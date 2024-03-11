# 1. Funkcja ctree
iris
summary(iris)
library(party)
names(iris)

iris_ctree <- ctree(Species ~ ., data = iris)
print(iris_ctree)
plot(iris_ctree)

# 2. Funkcja rpart
library(rpart)

set.seed(123)
model <- rpart(iris$Species ~ ., 
               data = iris, 
               subset = c(sample(1:150, 75)), 
               method = "class")
model


#use.n=TRUE - podaje liczebności klas w liściach
#all=TRUE - podaje klasę w węzłach
#cex - określa rozmiar czcionki
plot(model)
text(model, use.n = TRUE, all = TRUE, cex = 0.7)
library(rpart.plot)
rpart.plot(model)


# 3. ZADANIA

# 1.
library(tidyverse)
movies <- read_csv("C:/Users/Admin/RStudio/lab7/Movie_classification.csv")

# 2.
library(dplyr)
unique(movies$Genre)
colSums(is.na(movies))
# zastąp wartości N/A średnią wartością
movies$Time_taken[is.na(movies$Time_taken)] <- mean(movies$Time_taken, na.rm = TRUE)

# zmień wartości w kolumnie 3D_available na kodowane binarnie
# zakoduj wartości kolumny Genre wykorzystując kodowanie One-Hot
# oznacz wartości dla kolumny Start_Tech_Oscar w postaci słownej, a nie binarnej
movies2 <- movies %>%
  mutate(`3D_availableYES` = ifelse(`3D_available` == 'YES', 1, 0)) %>%
  mutate(GenreThriller = ifelse(Genre == 'Thriller', 1, 0)) %>%
  mutate(GenreDrama = ifelse(Genre == 'Drama', 1, 0)) %>%
  mutate(GenreComedy = ifelse(Genre == 'Comedy', 1, 0)) %>%
  mutate(GenreAction = ifelse(Genre == 'Action', 1, 0)) %>%
  mutate(Start_Tech_Oscar = ifelse(Start_Tech_Oscar == 1, 'YES', 'NO'))

# usuń zmienionych kolumn
movies2 <- subset(movies2, select = -c(`3D_available`, Genre))

# 3.
library(rpart)
library(caTools)
# podziel zbiór danych na zbiór treningowy (80%) i testowy (20%)
set.seed(123)
sample <- sample.split(movies2$Start_Tech_Oscar, SplitRatio = 0.8)
train_set <- subset(movies2, sample == TRUE)
test_set <- subset(movies2, sample == FALSE)

# utwórz model na podstawie danych treningowych
model_movies <- rpart(Start_Tech_Oscar ~ ., data = train_set, method = "class")
model_movies

# 4.
# wyświetl utworzone drzwo w postaci graficznej
plot(model_movies)
text(model_movies, use.n = TRUE, all = TRUE, cex = 0.7)
library(rpart.plot)
rpart.plot(model_movies)

# 5.
# stwórz zbiór predykcji bazujących na modelu
preds <- predict(model_movies, test_set, type = "class")

# 6.
# stwórz macierz pomyłek porównującą dane rzeczywiste z przewidywanymi przez model
conf_matrix <- table(test_set$Start_Tech_Oscar, preds)
conf_matrix

# oblicz dokładność predykcji
pred_accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
pred_accuracy
# dokładność predykcji wynosi 57%
