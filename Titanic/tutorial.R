library(magrittr)
library(dplyr)
library(readr)

train <- train.orig <- read_csv('train.csv')
test <- test.orig <- read_csv('test.csv')

make.submission <- function(submission) {
  submission %>% select(PassengerId, Survived)
}

survival.ratios <- function(x) {
  x %>% summarise(Survivors=sum(Survived), Total=n(), SurvivalRatio=Survivors/Total)
}

# prediction: all died
submission.alldied <- test %>%
  mutate(Survived=0) %>%
  select(PassengerId, Survived) %>%
  make.submission %T>%
  write_csv(path='1_alldied.csv')

# prediction: men died, women survived
submission.mendied <- test %>%
  mutate(Survived = as.integer(Sex=='female')) %>%
  select(PassengerId, Survived) %>%
  make.submission %T>%
  write_csv(path='2_mendied.csv')

# age analysis
mean.age <- mean(train$Age, na.rm=T)
train %<>%
  mutate(Age = ifelse(is.na(Age), mean.age, Age)) %>%
  mutate(Child = Age < 18)
train %>%
  group_by(Sex, Child) %>%
  survival.ratios

# bin ticket prices
bin.fare <- function(fare) {
  if (fare < 10) {
    return('<10')
  } else if (fare < 20) {
    return('10-20')
  } else if (fare < 30) {
    return('20-30')
  } else {
    return('>30')
  }
}

train %<>%
  mutate(FareBinned = Vectorize(bin.fare)(Fare))
train %>%
  group_by(Sex, FareBinned, Pclass) %>%
  survival.ratios

submission.sex.fare.class <- test %>%
  mutate(Survived = ifelse(Sex == 'female', 1, 0)) #%>%
  mutate(Survived = ifelse(Sex == 'female' && Pclass == 3 && Fare >= 20, 0, 1)) %>%
  make.submission %T>%
  write_csv(path='3_sex.fare.class.csv')

submission.sex.fare.class$Survived
