# Celem badania jest określenie ryzyka zachorowania populacji indianek z plemienia Pima na cukrzycę
# Wczytanie danych
library(mlbench)
data(PimaIndiansDiabetes)

# Podsumowanie statystyczne danych
summary(PimaIndiansDiabetes)

# Wstępne przetwarzanie danych
# brak niekompletnych danych
sum(is.na(PimaIndiansDiabetes))

# zmiana na zmienną binarną
library(dplyr)
pima_indians_diabetes <- PimaIndiansDiabetes %>% 
  mutate(diabetes_pos = ifelse(diabetes == 'pos', 1, 0))

# usuń niepotrzebne kolumny (diabetes)
pima_indians_diabetes <- subset(pima_indians_diabetes, select = -c(diabetes))

# uzupełnianie danych
pima_indians_diabetes$mass[pima_indians_diabetes$mass == 0.0] <- mean(pima_indians_diabetes$mass, na.rm = TRUE)
pima_indians_diabetes$pressure[pima_indians_diabetes$pressure == 0] <- median(pima_indians_diabetes$pressure, na.rm = TRUE)
pima_indians_diabetes$pressure[pima_indians_diabetes$pressure == 0] <- median(pima_indians_diabetes$pressure, na.rm = TRUE)
pima_indians_diabetes$glucose[pima_indians_diabetes$glucose == 0] <- mean(pima_indians_diabetes$glucose, na.rm = TRUE)

# podziel dane na zbiory treningowy, walidacyjny, testowy
library(caret)
set.seed(123)

# dane treningowe 70%
index_train <- createDataPartition(pima_indians_diabetes$diabetes_pos, p = 0.7, list = FALSE)
# dane testowe 15%, walidacyjne 15% (podziel resztę z danych na pół)
index_test <- createDataPartition(pima_indians_diabetes$diabetes_pos[-index_train], p = 0.5, list = FALSE)

train_data <- pima_indians_diabetes[index_train, ]
data_rest <- pima_indians_diabetes[-index_train, ]
test_data <- data_rest[index_test, ]
validation_data <- data_rest[-index_test, ]

# Budowanie modelu regresji
glm.fit = glm(diabetes_pos ~ ., data = train_data, family = binomial)
summary(glm.fit)

# Poprawa modelu z wybranymi zmiennymi wpływowymi
model_koncowy = glm(diabetes_pos ~ pregnant+glucose+pressure+insulin+mass+pedigree, data = train_data, family = binomial)
summary(model_koncowy)

# predykcje na podstawie danych testowych
predictions_test <- predict(model_koncowy, newdata = test_data, type = "response")
predictions_test <- as.factor(ifelse(predictions_test > 0.5, 1, 0))

# predykcje na podstawie danych walidacyjnych
predictions_validation <- predict(model_koncowy, newdata = validation_data, type = "response")
predictions_validation <- as.factor(ifelse(predictions_validation > 0.5, 1, 0))

# Ocena jakości modelu
# ocena jakości modelu dla danych testowych
confMatrix_test <- confusionMatrix(predictions_test, as.factor(test_data$diabetes_pos))
print(confMatrix_test)

# ocena jakości modelu dla danych walidacyjnych
confMatrix_validation <- confusionMatrix(predictions_validation, as.factor(validation_data$diabetes_pos))
print(confMatrix_validation)

library(car)
vif(model_koncowy)
# nie wykryto współliniowości

exp(coef(model_koncowy))

# Omówienie uzyskanych rezultatów
# dokładność predykcji modelu jest dobra, na poziomie 77%, model ma jednak trudności w identyfikacji negatywnych przypadków 
# model jest dobry w przewidywaniu, ma jednak trudności w identyfikacji negatywnych przypadków 
# liczba razy, kiedy kobieta zaszła w ciążę zwiększa ryzyko zachoworowania na cukrzycę o ~13%
# zwiększony poziom glukozy wpływa na ryzyko cukrzycy o ~4.27%
# ciśnienie i poziom insuliny w krwi nie wpływa zbytnio na wynik, ale im mniejsza wartość, tym mniejsze ryzyko choroby
# zwiększenie masy zwiększa ryzyko cukrzycy o ~11%
# rodowód ma największy wpływ, gdzie obciążenie genetyczne zwiększa ryzyko cukrzycy o ~214%