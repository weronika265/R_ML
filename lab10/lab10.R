# https://rpubs.com/mmazurek/346331
# https://rpubs.com/franzbischoff/ensemble
# https://rpubs.com/guptadeepak/ensemble

# 1.
library(tidyverse)
movies <- read_csv("C:/Users/Admin/RStudio/lab10/Movie_classification.csv")

# 2.
library(dplyr)
colSums(is.na(movies))
# zastąp wartości N/A średnią wartością
movies$Time_taken[is.na(movies$Time_taken)] <- mean(movies$Time_taken, na.rm = TRUE)

# zmień wartości w kolumnie 3D_available na kodowane binarnie
# zakoduj wartości kolumny Genre wykorzystując kodowanie One-Hot
movies2 <- movies %>%
  mutate(`3D_availableYES` = ifelse(`3D_available` == 'YES', 1, 0)) %>%
  mutate(GenreThriller = ifelse(Genre == 'Thriller', 1, 0)) %>%
  mutate(GenreDrama = ifelse(Genre == 'Drama', 1, 0)) %>%
  mutate(GenreComedy = ifelse(Genre == 'Comedy', 1, 0)) %>%
  mutate(GenreAction = ifelse(Genre == 'Action', 1, 0))

# usuń zmienione kolumny
movies2 <- subset(movies2, select = -c(`3D_available`, Genre))

# konwersja cechy klasy na factor
movies2$Start_Tech_Oscar <- as.factor(movies2$Start_Tech_Oscar)

# podziel zbiór danych na zbiór treningowy (80%) i testowy (20%)
library(caTools)
set.seed(123)
sample <- sample.split(movies2$Start_Tech_Oscar, SplitRatio = 0.8)
train_set <- subset(movies2, sample == TRUE)
test_set <- subset(movies2, sample == FALSE)

# 3.
# bagging
# https://www.geeksforgeeks.org/perform-bagging-in-r/
# https://www.statology.org/bagging-in-r/
library(ipred)
set.seed(123)
fit.bag <- bagging(Start_Tech_Oscar ~ ., train_set)
fit.bag

# random forest
# https://www.geeksforgeeks.org/random-forest-approach-in-r-programming/
# https://www.r-bloggers.com/2021/04/random-forest-in-r/
# library(randomForest)
set.seed(123)
# rf <- randomForest(Start_Tech_Oscar ~ ., data = train_set)
# rf

library(caret)
fit.rf <- train(Start_Tech_Oscar ~ .,
                data = train_set,
                method = "rf",
                verbose = FALSE
)
fit.rf

# boosting
# library(gbm)
set.seed(123)
# boost <- gbm(Start_Tech_Oscar ~ ., train_set, 50, "bernoulli")
# boost
fit.boost <- train(Start_Tech_Oscar ~ .,
                 data = train_set,
                 method = "gbm",
                 verbose = FALSE,
                 distribution = "adaboost"
)
fit.boost

# 4.
library(caret)
preds_bag <- predict(fit.bag, test_set)
preds_rf <- predict(fit.rf, test_set)
preds_boost <- predict(fit.boost, test_set)
# preds_boost <- predict.gbm(fit.boost, test_set, 50, "response")
# konwersja wyników do wartości binarnych
# preds_boost <- ifelse(preds_boost > 0.5, 1, 0)

conf_matrix_bag <- confusionMatrix(preds_bag, test_set$Start_Tech_Oscar)
conf_matrix_rf <- confusionMatrix(preds_rf, test_set$Start_Tech_Oscar)
conf_matrix_boost <- confusionMatrix(preds_boost, test_set$Start_Tech_Oscar)

conf_matrix_bag # 62% (poprawnie zakwalifikowane dane)
conf_matrix_rf # 70% <- w tym przypadku najlepszy okazał się algorytm random forest
conf_matrix_boost # 64%
