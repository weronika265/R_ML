# --- Typy danych ---
# 1.8
# temp <- "36.5"
# temp

# 1.9
# temp + 0.1
# nie można dodawać do siebie zmiennych napisowych i liczbowych

# 1.10
# class(temp) # character

# 1.11
# temp <- as.numeric(temp)
# temp # typ numeric, wyświetla bez nawiasów

# 1.12

# temp <- as.integer(temp)
# temp # typ integer, z sufiksem L

# 1.13
# class("36") # character
# class(36) # numeric
# class(36L) # integer
# class(36+0i) # complex
# class(TRUE) # logical


# --- Wektory ---
# x <- c(2, 5, 8) # wektor z liczbami 2, 5, 8
# c(2:4, c(5, 8)) # wektor w przedziale 2-4 oraz 5-8
# 2:5 # przedział liczb 2-5
# seq(2, 5, by = 0.75) # sekwencja liczba w przedziale 2-5, z inkrementacją co 0.75
# rep(x, times = 3) # powtarzaj ciąg liczb 3 razy
# rep(x, each = 3) # powstarzaj każdą liczbę z ciągu 3 razy

# 2.1
# w1 <- 3:0
# w1 # przedział liczbowy od 3 do 0

# 2.2
# w1[1] # 1 el. wektora
# w1[length(w1)] # el. na miejscu 4
# w1[c(T, T, T, F)] # wyświetl wszystkie el. z TRUE
# w1[3:1] # wyświetl kolejno el. o indeksach 3, 2, 1
# w1[-2] # wyświetl wszystkie liczby w wektorze oprócz drugiego

# 2.3
# w2 <- c(0/0, 1/0, 1/Inf, TRUE, as.numeric("abc")) # NaN Inf 0 1 NA
# w3 <- as.logical(c("T", "False", "abc")) # TRUE FALSE NA
# NaN - Not a Number (nie jest określoną liczbą)
# NA - Not Available (brakuje wartości dla zmiennej)

# 2.4
# w4 <- vector("numeric", length = 9) # stwórz wektor z 9 liczbami (inicjowane zerami)
# w4[1:4] <- 1:4 # w pierwszych czterech indeksach umieść liczby kolejno 1-4
# w4[5] <- "null" # typ wartości musi być taki sam, więc dodając napis, wszystkie wartości zostają także zmienione na napisy
# w4[6:9] <- 6:9 # wartości muszą się zgadzać, więc przy "null" najlepiej wszystko konwertować na napis
# class(w4) # typ character
# w4 <- as.numeric(w4) # zmień wartości na liczby, "null" się nie da, więc NA
# bad <- is.na(w4) # dla każdej wartości wektora sprawdź, czy jest NA. Jeśli tak - TRUE wpp FALSE
# w4 <- w4[!bad] # zostaw wszystko, co nie jest NA (jeden ze sposobów usuwania niechcianych wartości)

# 2.5
# w5 <- 1:5
# w6 <- 6:10
# w5 + w6
# w5 - w6
# w5 * w6
# w5 / w6
# w5 == w6 # czy kolejne wartości w wektorach są równe (TRUE/ FALSE)
# w5 >= 3 # jeśli wartość w wektorze jest niemniejsza niż 3 (TRUE/ FALSE)
