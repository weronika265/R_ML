# 1.
# wyświetl zbiór danych anscombe
# data(anscombe)
# anscombe


# 2.
# podziel kolumny zbioru danych na podramki z odpowiadającymi parami kolumn x, y
ans1 <- data.frame(anscombe[1], anscombe[5])
ans2 <- data.frame(anscombe[2], anscombe[6])
ans3 <- data.frame(anscombe[3], anscombe[7])
ans4 <- data.frame(anscombe[4], anscombe[8])


# 3.
# oblicz średnią dla każdej kolumny (osobno dla wartości zmiennych x i y) w poszczególnych podramkach
mean_x <- sapply(list(ans1$x1, ans2$x2, ans3$x3, ans4$x4), mean)
mean_y <- sapply(list(ans1$y1, ans2$y2, ans3$y3, ans4$y4), mean)
# lub
colsMean <- c(colMeans(ans1), colMeans(ans2), colMeans(ans3), colMeans(ans4))

# oblicz średnią dla każdego wiersza (wspólnie dla wartości zmiennych x i y) w poszczególnych podramkach
# rowsMean <- c(rowMeans(ans1), rowMeans(ans2), rowMeans(ans3), rowMeans(ans4))

# oblicz średnią kolejno dla każdej kolumny, następnie średnią ze wszystkich wartości średnich x i y
mean_dataset <- mean(sapply(anscombe, mean))


# 4.
# odchylenie standardowe dla x1 i y1
sd1 <- lapply(ans1, sd)

# odchylenie standardowe dla x2 i y2
sd2 <- lapply(ans2, sd)

# odchylenie standardowe dla x3 i y3
sd3 <- lapply(ans3, sd)

# odchylenie standardowe dla x4 i y4
sd4 <- lapply(ans4, sd)


# 5.
# użyj biblioteki knitr
library(knitr)
# wyświetl wartości odchyleń standardowych dla ramek danych
table_sds <- kable(data.frame(sd1, sd2, sd3, sd4))


# 6.
# pobierz ramkę danych i jej wartości dla kolumn x i y
fit_models <- function(ans, x, y) {
  lm_model <- lm(y~x, data=ans) # utwórz model regresji
  print(lm_model) # wypisz współczynniki modelu
  plot(lm_model) # narysuj wykres modelu
  abline(lm_model) # dodaj linię do funkcji regresji
}


# 7.
# dla każdej ramki danych wyświetl utworzone dla niej wykresy w formacie 2x2
par(mfrow = c(2,2))
fit_models(ans1, ans1$x1, ans1$y1)
fit_models(ans2, ans2$x2, ans2$y2)
fit_models(ans3, ans3$x3, ans3$y3)
fit_models(ans4, ans4$x4, ans4$y4)
