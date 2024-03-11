# 1. Prosta regresja liniowa
# Wczytywanie danych
library(tidyverse)
rowery <- read_csv("./lab4/dane/bikes.csv", col_types = "Dffffddddd")

glimpse(rowery)

summary(rowery)

library(ggplot2)
humid <- ggplot(data=rowery, aes(x=humidity, y=rentals)) + geom_point() # przy wilgotności niższej niż ~40% jest znacznie mniej wynajmów. Przy wilgotności ~75% można zauważyć zwiększoną liczbę wynajmów
wind <- ggplot(data=rowery, aes(x=windspeed, y=rentals)) + geom_point() # przy dużej prędkości wiatru jest znacznie mniej wynajmów. Dużo wynajmów (~5000) dokonuje się przy lekkim wietrze
temp <- ggplot(data=rowery, aes(x=temperature, y=rentals)) + geom_point() # więcej wynajmów przy wyższej temperaturze, przy niższej jest ich mniej


library(gridExtra)
grid.arrange(humid, wind, temp, nrow=1, ncol=3)


# Korelacja
cov(rowery$rentals, rowery$humidity)

sd(rowery$rentals)
sd(rowery$humidity)

pearson <- cov(rowery$rentals, rowery$humidity) / (sd(rowery$rentals) * sd(rowery$humidity))

pearson

# wniosek: w dniach bardziej wilgotnych jest mniej wynajmów, chociaż zależność ta jest bardzo słaba, czynnik ten jest niewielki

cor(rowery$temperature, rowery$rentals)
cor(rowery$windspeed, rowery$rentals)

# Istnieje zależność, dla temperatury im jest większa, tym więcej wynajmów, a dla prędkości wiatru im większy wiatr, tym mniej wynajmów
# Dla temperatury zależność jest umiarkowana, dla prędkości wiatru jest słaba

rowery_liczbowe <- rowery %>%
  select(-(date:weather))

rowery_korelacje <- cor(rowery_liczbowe)

library(corrplot)
corrplot(rowery_korelacje)

corrplot(rowery_korelacje, type = "upper", method = "number")


# Współczynniki prostej regresji
b1 <- cov(rowery$temperature, rowery$rentals) / var(rowery$temperature)
b1

b0 <- mean(rowery$rentals) - b1 * mean(rowery$temperature)
b0


# Model prostej regresji liniowej
rowery_model1 <- lm(data = rowery, rentals~temperature)
rowery_model1

summary(rowery_model1)

# różnice między zaobserwowanymi a przewidywanymi wartościami modelu


# 2. Wielokrotna regresja liniowa
rowery_model2 <- lm(data = rowery, rentals~humidity+windspeed+temperature)

summary(rowery_model2)

# model jest dokładniejszy, więcej nam mówi o relacjach analizowanych danych
# y = b0 + b1 * humidity + b2 * windspeed + b3 * temperature


# 3. Zadania
# 3.1
library(tidyverse)
ogloszenia <- read_csv("./lab4/dane/Advertising.csv", col_types = cols(...1 = col_skip()))

# 3.2
pearson_tv <- cor(ogloszenia$TV, ogloszenia$sales)
pearson_tv

pearson_radio <- cor(ogloszenia$radio, ogloszenia$sales)
pearson_radio

pearson_newspaper <- cor(ogloszenia$newspaper, ogloszenia$sales)
pearson_newspaper

ogloszenia_korelacje <- cor(ogloszenia)
library(corrplot)
corrplot(ogloszenia_korelacje, type = "upper", method = "number")

# Istnieje zależność między sprzedażą a budżetem reklamowym dla usług
# Istnieje duża zależność między sprzedażą a budżetem reklamowym dla TV, umiarkowana dla radia, a mała dla gazety

rowery_model_tv <- lm(data = ogloszenia, sales~TV)
rowery_model_radio <- lm(data = ogloszenia, sales~radio)
rowery_model_newspaper <- lm(data = ogloszenia, sales~newspaper)

summary(rowery_model_tv)
summary(rowery_model_radio)
summary(rowery_model_newspaper)

ogloszenia_model <- lm(data = ogloszenia, sales~TV+radio+newspaper)
summary(ogloszenia_model)

# Ze sprzedażą powiązany jest budżet na reklamy w TV i w radiu
# Zależności dla TV i radia są znaczące, dla radia zależność jest nieistotna
# Zależność wskazuje na liniową



# --- LAB 5 --- (cd. lab 4)

# 1. Testy diagnostyczne reszt
mean(rowery_model2$residuals) # średnia jest bardzo bliska 0, więc kryterium jest spełnione

# rozkład normalny
library(olsrr)
ols_plot_resid_hist(rowery_model2)
ols_plot_resid_qq(rowery_model2)

# homoskedastyczność (wartości zmiennej niezależnej mają taką samą wariancję)
ols_plot_resid_fit(rowery_model2)

# brak korelacji
library(car)
# Test Durbina-Watsona
# 0 < DW < 2: dodatnia autokorelacja
# DW = 2: zerowa autokorelacja
# 2 < DW < 4: autokorelacja ujemna

durbinWatsonTest(rowery_model2)


# 2. Analiza punktów wpływowych
library(olsrr)
# Wykres odległości Cooka
ols_plot_cooksd_chart(rowery_model2)

# Wyświetlenie wartości odstających według malejącej odległości Cooka
cooks_outliers <- ols_plot_cooksd_chart(rowery_model2, print_plot = FALSE)$outliers
arrange(cooks_outliers, desc(cooks_distance))


# ZADANIE:
# 1.
cook_max <- arrange(cooks_outliers, desc(cooks_distance))[1,]
idx_cook_max <- cook_max$observation

# porównanie obserwacji o najwyższej wartości cooka z resztą danych
rowery[idx_cook_max, ]
summary(rowery[-idx_cook_max, ])
# obserwacja nie odstaje znacząco poza wilgotnością o wartości 0 -> może to sugerować błąd (też mało wynajmów, ale min. to 22)


# 2.
outlier_index <- arrange(cooks_outliers, desc(cooks_distance))$observation
# outlier_index <- arrange(cooks_outliers, desc(cooks_distance))$observation[1:25]


# 3.
summary(rowery[outlier_index, ])
summary(rowery[-outlier_index, ])
# temperatura, temperatura odczuwalna i siła wiatru są większe w wartościach odstających, szczególnie minimalna
# minimalna wilgotność w wartościach odstających może wynosić 0, co jest mało prawdopodobne
# wynajmów w wartościach odstających jest średnio mniej, a mininalna wartość wynosi 22 w porównaniu do reszty wyników, gdzie jest to 431


# 4.
summary(rowery)
summary(rowery[-outlier_index, ])

# wartości odstajace nie mają znaczącego wpływu na rozkład statystyczny danych, więc wartości te można usunąć
rowery2 <- rowery[-outlier_index, ]


# * Dla zbioru danych Advertising: *
# 1.
ols_plot_cooksd_chart(ogloszenia_model)
cooks_outliers_ads <- ols_plot_cooksd_chart(ogloszenia_model, print_plot = FALSE)$outliers

cook_max_ads <- arrange(cooks_outliers_ads, desc(cooks_distance))[1, ]$observation
ogloszenia[cook_max_ads, ]
summary(ogloszenia[-cook_max_ads, ])
# obserwacja odstaje dosyć znacząco dla reklam dla TV (0.7, a min. 4.1)


# 2.
outlier_index_ads <- arrange(cooks_outliers_ads, desc(cooks_distance))$observation


# 3.
summary(ogloszenia[outlier_index_ads, ])
summary(ogloszenia[-outlier_index_ads, ])
# w wartościach odstających statystyki dla reklamy dla TV i radia (szczególnie mediana) są dla TV - mniejsze, a dla radia - większe w porównaniu do reszty danych


# 4.
summary(ogloszenia)
summary(ogloszenia[-outlier_index_ads, ])
# wartości odstajace nie mają znaczącego wpływu na rozkład statystyczny danych, więc wartości te można usunąć
ogloszenia2 <- ogloszenia[-outlier_index_ads, ]



# 3. Współliniowość
ols_vif_tol(rowery_model2)
# ols_vif_tol(ogloszenia_model)


# 4. Ulepszanie modelu
# Uwzględnienie relacji nieliniowych - regresja wielomianowa
#library(ggplot2)

#df <- data.frame(x,y)
humid <- ggplot(data=rowery2, aes(x=humidity, y=rentals)) + geom_point() +
  geom_smooth(method = 'lm', se = TRUE) +
  geom_smooth(method = 'gam', colour = 'red')
wind <- ggplot(data=rowery2, aes(x=windspeed, y=rentals)) + geom_point() +
  geom_smooth(method = 'lm', se = TRUE) +
  geom_smooth(method = 'gam', colour = 'red')
temp <- ggplot(data=rowery2, aes(x=temperature, y=rentals)) + geom_point() +
  geom_smooth(method = 'lm', se = TRUE) +
  geom_smooth(method = 'gam', colour = 'red')

library(gridExtra)
grid.arrange(humid, wind, temp, nrow=1, ncol=3)

# Dodanie nowych predyktorów
rowery2 <- rowery2 %>%
  mutate(humidity2 = humidity^2) %>%
  mutate(windspeed2 = windspeed^2) %>%
  mutate(temperature2 = temperature^2)

# Utworzenie nowego modelu liniowego
rowery_model3 <- lm(data = rowery2,
                    rentals ~ humidity + windspeed + temperature + 
                      humidity2 + windspeed2 + temperature2)

summary(rowery_model3)

# temperatura, wilgotność^2 i temperatura^2 są najbardziej istotne, nieznacznie wilgotność i prędkość wiatru

rowery_model4 <- lm(data = rowery2,
                    rentals ~ humidity + windspeed + temperature + 
                      humidity2 + temperature2)

summary(rowery_model4)
# zwiększyła się istotność predyktora prędkości wiatru, chociaż wilgotność nadal jest mniej istotna


# Uwzględnianie zmiennych kategorialnych
summary(rowery2$season)
summary(rowery2$holiday)
summary(rowery2$weekday)
summary(rowery2$weather)

rowery2 <- rowery2 %>%
  mutate(season = plyr::revalue(season, c("1" = "Zima", "2" = "Wiosna",
                                          "3" = "Lato", "4" = "Jesien"))) %>%
  mutate(holiday = plyr::revalue(holiday, c("0" = "Nie", "1" = "Tak"))) %>%
  mutate(weekday = plyr::revalue(weekday, c("0" = "Niedziela","1" = "Poniedzialek",
                                            "2" = "Wtorek", "3" = "Sroda", "4" = "Czwartek",
                                            "5" = "Piatek", "6" = "Sobota"))) %>%
  mutate(weather = plyr::revalue(weather, c("1" = "Ladna pogoda", "2" = "Lekkie opady",
                                            "3" = "Obfite opady")))


rowery_model5 <- lm(data = rowery2,
                    rentals ~ humidity + windspeed + temperature + 
                      humidity2 + temperature2 + season)

summary(rowery_model5)
# season - najlepsza zmienna do uwzględnienia
# wprwadzenie zmiennej zwiększyło istotność poprzednich czynników, a Jesień z pór roku ma największy wpływ na wynajem rowerów


# Interakcje między zmiennymi
rowery_model6 <- lm(data = rowery2,
                    rentals ~ humidity + windspeed + temperature + 
                      humidity2 + temperature2 + season + weather * windspeed)

summary(rowery_model6)


# Wybieranie ważnych zmiennych
library(lubridate)

rowery2 <- rowery2 %>%
  mutate(dzien = as.numeric(date - min(date))) %>%
  mutate(miesiac = as.factor(month(date))) %>%
  mutate(rok = as.factor(year(date))) %>%
  select(-date)

ols_step_both_p(
  model = lm(
    data = rowery2,
    rentals ~ humidity + weekday + holiday +
      temperature + humidity2 + temperature2 + season +
      windspeed * weather + realfeel + dzien + miesiac + rok
  ),
  pent = 0.2, # wartość progowa p zmiennych uwzględnianych w modelu
  prem = 0.01, # progowa wartość p zmiennych usuwanych z modelu
  details = FALSE # flaga wskazująca jak dużo informacji wyświetlać
)

rowery_model7 <- lm(data = rowery2,
                    rentals ~ windspeed * weather + miesiac + weekday + season + 
                      holiday + temperature2 + temperature + rok + windspeed + humidity + humidity2)

# windspeed mogłoby zostać usunięte?
summary(rowery_model7)

ols_plot_resid_fit(rowery_model7) # jest lepiej, punkty są mniej rozproszone - mniejsze rozproszenie od linii, więc większa homoskedastyczność modelu?
ols_plot_resid_fit(rowery_model2)
