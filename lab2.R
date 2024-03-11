# --- 1. MACIERZE ---
macierz1 <- matrix(1:12, 3, 4)
# macierz1

# 1.1
macierz1
macierz1[1, 2] # row, col
macierz1[1, 2, drop=FALSE] # ekstrakcja pojedyńczego wiersza, nie upraszczaj (jedna wartość, ale i tak zostaw jako macierz, a nie integer)

# 1.2
macierz1[2:3,] # wypisz wiersze 2 i 3

# 1.3
diag(macierz1) # wypisz przekątną macierzy
diag(5) # stworz przekątna dla macierzy 5x5 / macierz z przekątną 5x val 1
macierz2 <- diag(1:3) # przekątna z wartościami 1, 2, 3
solve(macierz2) # zwróć odwrotność macierzy

# 1.4
macierz3 <- 1:12 # wartości od 1 do 12
dim(macierz3) <- c(3, 4) # stwórz macierz 3x4 (rows, cols)
attributes(macierz3) # wypisz rozmiar macierzy
attributes(macierz1) # ten sam wynik, bo ten sam rozmiar

# 1.5
macierz4 <- matrix(1:12, 3, 4, byrow = TRUE) # wpisz wartości od 1-12 w macierzy 3x4, rozoczynając od wierszy (a nie kolumn by default)
macierz4

# 1.6
macierz5 <- cbind(macierz1, macierz4) # łącz od strony kolumn
macierz5
macierz6 <- rbind(macierz1, macierz4) # łącz od strony wierszy
macierz6

# --- 2. LISTY ---
# 2.1
lista1 <- list(lp = 1L, imie = "Jan", nazwisko = "Kowalski", oceny = c(4.0, 3.5, 5.0))
lista1
class(lista1) # typ - lista
class(lista1$lp) # integer
class(lista1$imie) # character
class(lista1$nazwisko) #character
class(lista1$oceny) # numeric

# 2.2
lista1$imie # "Jan"
lista1[[2]] # "Jan" - wartość drugiego elementu listy
lista1[2] # $imie [1]"Jan" - drugi element listy: nazwa elementu i wartość
lista1$przedmiot <- "WAD" # zapisz pod elementem listy 'przedmiot' wartość "WAD" (jeśli element jeszcze nie istnieje - stwórz go, wpp zmień wartość)
lapply(lista1, toupper) # zmień wszystkie wartości na duże litery, jeśli są to liczy, zmień je na napisy

# --- 3. RAMKI DANYCH ---
# 3.1
dane <- data.frame(rok = c(2020, 2020, 2021, NA, 2022),
                   pora.roku = c("lato", "jesien", "lato", "jesien", "wiosna"),
                   maks.temp = c(32, 21, 33, 15, 22))
attributes(dane) # wypisz rozmiar ramki danych (nazwy elementów, typ obiektu (data.frame), nazwy kolumn (1, 2, 3, 4, 5))
nrow(dane) # liczba wierzy ('krotki')
ncol(dane) # liczba kolumn (atrybuty)

# 3.2
pelne <- complete.cases(dane) # wyświetl, które wiersze zawierają wszystkie wartości
dane <- dane[pelne,] # usuń wiersze z NA

# 3.3
m <- data.matrix(dane) # konwersja ramki danych na macierz

# 3.4
dane[dane$maks.temp > 30,] # wyświetl dane z ramki danych, w których temperatura jest większa od 30

attach(dane) # dołącz ramkę danych do ścieżki wyszukiwań języka R, dzięki czemu można podawać tylko nazwy elementów ramek, a nie także nazwy samych ramek
maks.temp # jak tutaj

dane[pora.roku != "lato" & maks.temp > 20,] # wyświetl dane, które nie są latem i mają temperaturę wyższą od 20

subset(dane, rok == 2020) # zwróć podzbór ramki danych, gdzie rok to 2020

dane$min.temp <- c(18, 7, 16, -1) # do ramki danych 'dane' dodaj element min.temp z wartościami

data() # wyświetl zbiory danych z pakietu 'datasets'
data("iris") # dane dla "iris" (irysów)
head(iris, 3) # wyświetl pierwsze 3 krotki/ wpisy

# --- 4. FUNKCJE STERUJĄCE ---
# 4.1
# wypisz nazwy miesięcy kolejnych wpisów
for (i in month.name) {
  print(i)
}

# wypisz nazwy miesięcy pierwszych ośmiu wpisów
for (i in 1:8) {
  print(month.name[i])
}

# wypisz nazwy miesięcy, gdzie długość sekwencji podana jest jako długość wektora
v <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
for (i in seq_along(v)) {
  print(month.name[i])
}

# 4.2
# wypisz kolejne cyfry od 1 do 10 przez inkrementację zmiennej w pętli, dopóki nie napotkasz liczby większej od 10
i <- 1
while (i <= 10) {
  print(i)
  i <- i + 1
}

# 4.3
# wypisuj liczby zaczynając od 1, gdzie program powtarza operację i:
# inkrementuje zmienną o 1,
# omija wartość 5
# wychodzi z pętli, gdy dojdzie do wartości 11 i nie wypisuje jej
# w porównaniu do pętli while nie ma tu warunku stopu przy kolejnej iteracji, całkowite wyjście z pętli następuje po wywołaniu instrukcji break
i <- 1
repeat {
  i <- i + 1
  if (i == 11)
    break
  else if (i == 5)
    next
  else
    print(i)
}

# --- 5. FUNKCJE ---
# 5.1
# utwórz i wypisz utworzoną funkcję
y <- function(x, a = 1, b = 0) {
  a * x + b
}
y

# 5.2
y() # błąd, argument x nie jest podany
y(1) # zwróć funkcję dla argumentów 1, 1, 0

# 5.3
y(1, 2, 3) # dopasuj wartości dla kolenych parametrów
y(b = 3, a = 2, x = 1) # można zmieniać kolejność podawanych argumentów
y(b = 3, 1, a = 2) # można zmieniać kolejność i dla brakujących wartości nie wskazywać nazwy zmiennej (podstawiane są kolejno)

# --- ZADANIA ---
# a)
y <- function(x, a = 1, b = 0, c) {
  a * x + b
  # a * x + b + c
}
y(1) # jest możliwe, nie wyskoczy błąd dopóki nie użyjemy argumentu w ciele funkcji

# b)
konwersja_temp <- function(F) {
  (F - 32) / 1.8
}
konwersja_temp(200)
