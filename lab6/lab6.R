# Regresja logistyczna
library(tidyverse)
house_price <- read_csv("./lab6/dane/House-Price.csv")

# - Wstępne przetwarzanie danych -
colSums(is.na(house_price)) # n_hos_beds (8)
house_price$n_hos_beds[is.na(house_price$n_hos_beds)] <- mean(house_price$n_hos_beds, na.rm = TRUE) # zastąpienie wartości N/A średnią wartością

sum(house_price$bus_ter == 'NO') # zmienna monotoniczna
house_price2 <- subset(house_price, select = -bus_ter) # usuwanie zmiennej monotonicznej

house_price2$avg_dist <- rowMeans(house_price2[,c('dist1', 'dist2', 'dist3', 'dist4')]) # uśrednienie zmiennych dist

# kodowanie jeden do wielu zmiennych kategorycznych
library(dplyr)
house_price2 <- house_price2 %>%
  mutate(airportYES = ifelse(airport == 'YES', 1, 0)) %>%
  mutate('waterbodyLake and River' = ifelse(waterbody == 'Lake and River', 1, 0)) %>%
  mutate(waterbodyLake = ifelse(waterbody == 'Lake', 1, 0)) %>%
  mutate(waterbodyRiver = ifelse(waterbody == 'River', 1, 0))

house_price2 <- subset(house_price2, select = -c(airport, waterbody, dist1, dist2, dist3, dist4)) # usuń niepotrzebne (airport i waterbody)

house_price_model <- lm(data = house_price2, price ~ .)

# usuwanie danych odstajacych (na wykładzie było, na ćwiczeniach jest przykład bez)
###
library(olsrr)
cooks_outliers <- ols_plot_cooksd_chart(house_price_model, print_plot = FALSE)$outliers
outlier_index <- arrange(cooks_outliers, desc(cooks_distance))$observation

summary(house_price2)
summary(house_price2[-outlier_index, ])

house_price3 <- house_price[-outlier_index,]

###

# tworzenie modelu regresji logistycznej
glm.fit = glm(Sold~price, data = house_price3, family = binomial)
summary(glm.fit)

glm.fit$coefficients

summary(glm.fit)$coef

glm.fit$fitted.values
glm.fit$residuals
glm.fit$family
glm.fit$iter
glm.fit$converged


# Regresja logistyczna dla wielu zmiennych objaśniających
glm.fit = glm(Sold~., data = house_price3, family = binomial)
summary(glm.fit)

model_koncowy <- glm(formula = Sold~price+air_qual+room_num+teachers+poor_prop+n_hos_beds+avg_dist, data = house_price3, family = binomial)
summary(model_koncowy)

step(model_koncowy, direction = "backward")

library(car)
vif(model_koncowy)

exp(coef(model_koncowy))
# < 1 -> 1 - x + o tyle maleje
# > 1 -> x - 1 + o tyle wzrasta



# --- ZADANIE ---
# 1.
library(tidyverse)
wady_wrodzone <- read_csv2("./lab6/dane/lab4-dane_a.csv")

library(plyr)
colnames(wady_wrodzone)[colnames(wady_wrodzone) == 'Wiek M'] <- 'WiekM'


# 2.
summary(wady_wrodzone)

hist(wady_wrodzone$WiekM, main = "Wiek matki", xlab = "WiekM")
hist(wady_wrodzone$MasaUr, main = "Masa dziecka przy urodzeniu", xlab = "MasaUr")

# Niektóre dane są nieprawidłowe, np. wiek matki nie może wynosić -1, a masa urodzonego dziecka 0.
# Jeśli chodzi o wiek matki, średnia wieku to 28 lat, najwięcej jest kobiet w wieku 25-30 lat. Niektóre kobiety mają wiek 40-45, nieliczne 45-50.
# Jeśli chodzi o masę dziecka przy urodzeniu, to średnia waga wynosi 3.306kg, najwięcej dzieci waży właśnie w okolicach 3-3.5kg. Nieliczne dzieci miały wagę 0.5-1.5 oraz 4.5-5.5


# 3.
# wstępne przetwarzanie danych
colSums(is.na(wady_wrodzone))

# kodowanie jeden do wielu zmiennych kategorycznych i usuwanie wierszy z nieprawidłowościami
library(dplyr)
wady_wrodzone2 <- wady_wrodzone %>%
  filter(WiekM > 0) %>%
  filter(MasaUr > 0) %>%
  mutate(WadaTak = ifelse(GRUPA == 'badana', 1, 0)) %>%
  mutate(MiejsceZamMiasto = ifelse(MiejsceZam == 'miasto', 1, 0)) %>%
  # mutate(MiejsceZamWies = ifelse(MiejsceZam == 'wies', 1, 0)) %>%
  mutate(PlecKobieta = ifelse(Płeć == 'K', 1, 0)) %>%
  mutate(PoronSamoTak = ifelse(PoronSamo == 'tak', 1, 0)) %>%
  mutate(InfOddechTak = ifelse(InfOddech == 'tak', 1, 0)) %>%
  mutate(PalenieTak = ifelse(Palenie == 'tak', 1, 0)) %>%
  mutate(WyksztMPodst = ifelse(WyksztM == 'podst', 1, 0)) %>%
  mutate(WyksztMZawod = ifelse(WyksztM == 'zawod', 1, 0)) %>%
  mutate(WyksztMSrednie = ifelse(WyksztM == 'srednie', 1, 0))
  # mutate(WyksztMWyzsze = ifelse(WyksztM == 'wyzsze', 1, 0))

wady_wrodzone2 <- subset(wady_wrodzone2, select = -c(GRUPA, MiejsceZam, Płeć, PoronSamo, InfOddech, Palenie, WyksztM))

glm.fit = glm(WadaTak ~ ., data = wady_wrodzone2, family = binomial)
summary(glm.fit)

model_koncowy <- glm(WadaTak ~ MasaUr+WiekM+KolCiazy+PlecKobieta+InfOddechTak+PalenieTak+WyksztMPodst, 
                     data = wady_wrodzone2, family = binomial)


# 4.
library(car)
vif(model_koncowy)
# nie wykryto współliniowości

# 5.
exp(coef(model_koncowy))
# MasaUr - wraz ze wzrostem masy o jedną jednostkę, szansa wady wrodzonej maleje o ~36%
# WiekM - wyższy wiek minimalnie wpływa na szansę wystąpienia wady wrodzonej (~2.3%)
# KolCiazy - każdy kolejny miesiąc zwiększa szansę wady wrodzonej o ~23%
# PlecKobieta - szansa wystąpienia wady u kobiet jest o ~39% niższa od mężczyzn
# InfOddechTak - przy infeksji oddechowej szansa wystąpienia wady wzrasta o ~308%
# PalenieTak - przy paleniu przez matkę szansa wystąpienia wady wzrasta o ~318%
# WyksztMPodst - przy matce o wykształceniu podstawowym szansa wystąpienia wady wzrasta o ~137%