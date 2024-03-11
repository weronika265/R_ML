# 1.
library(tidyverse)
donors <- read_csv("C:/Users/Admin/RStudio/lab9/donors.csv",
                   col_types = cols(...1 = col_skip()))
glimpse(donors)

# 2.
library(dplyr)
donors_numeric <- donors %>% select(where(is.numeric), respondedMailing)

summary(donors_numeric)
# sprawdź, gdzie są braki danych
colSums(is.na(donors_numeric))
# braki danych dla: age, numberChildren, incomeRating, wealthRating

# 3.
donors_numeric$age[is.na(donors_numeric$age)] <- mean(donors_numeric$age, na.rm = TRUE)
donors_numeric$numberChildren[is.na(donors_numeric$numberChildren)] <- median(donors_numeric$numberChildren, na.rm = TRUE)
donors_numeric <- na.omit(donors_numeric)
donors_numeric <- filter(donors_numeric, wealthRating != 0)

# 4.
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

donors_numeric_norm <- lapply(donors_numeric, normalize)

# 5.
# przekształć na ramkę danych
donors_numeric_norm <- as.data.frame(donors_numeric_norm)

library(rpart)
library(caTools)
set.seed(123)
# podziel dane na treningowe i testowe 75:25
sample <- sample.split(donors_numeric_norm$respondedMailing, SplitRatio = 0.75)
train_set <- subset(donors_numeric_norm, sample == TRUE)
test_set <- subset(donors_numeric_norm, sample == FALSE)

# porównaj rozkłady wszystkich zbiorów danych
print(prop.table(table(donors$respondedMailing)))
print(prop.table(table(train_set$respondedMailing)))
print(prop.table(table(test_set$respondedMailing)))

# 6.
library(performanceEstimation)
train_set_balanced <- smote(respondedMailing ~ ., 
                            perc.over = 5,
                            train_set,
                            k = 3)
                            # perc.under = 5)
print(prop.table(table(train_set_balanced$respondedMailing)))

train_set_balanced <- na.omit(train_set_balanced)
# any(is.na(train_set_balanced))

# 7.
library(tidyverse)
train_label <- train_set_balanced %>% pull(respondedMailing) %>% as.factor()
test_label <- test_set %>% pull(respondedMailing) %>% as.factor()

# 8.
train_set_balanced <- select(train_set_balanced, -respondedMailing)
test_set <- select(test_set, -respondedMailing)

# 9.
library(class)
preds <- knn(train_set_balanced, test_set, cl = train_label, k = 5)

# 10.
preds[1:6]

# 11.
conf_matrix <- table(preds, test_label)
conf_matrix

pred_accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
pred_accuracy
# dokładność predykcji wynosi 94%

# 12.
# w pliku lab9_cat