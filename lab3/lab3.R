# 1.
# 1.1
library(tidyverse)
pomiary_sierpien <- read_csv("lab3/dane/pomiary_sierpien.csv", 
                             col_types = cols(...1 = col_skip()))

# 1.2
pomiary_wrzesien <- read_csv("lab3/dane/pomiary_wrzesien.csv",
                            col_types = cols(...1 = col_skip()))
# 1.3
powietrze <- rbind(pomiary_sierpien, pomiary_wrzesien)

# 2.
# 2.1
names(powietrze)
names(powietrze)[1:7] <- c("PM25", "PM10", "NO2", "NO", "NOx", "CO", "C6H6")
head(powietrze)

# 2.2
summary(powietrze)
str(powietrze)
quantile(powietrze$PM25, probs=seq(0,1,0.1), na.rm=TRUE)

# 2.3
for(i in names(powietrze))
  print(class(powietrze[i]))

# 2.4
powietrze[,1:7] <- round(powietrze[,1:7],2)

# 2.5
powietrze$data <- as.Date(paste(powietrze$rok, powietrze$miesiac, powietrze$dzien, sep="- "))
powietrze <- powietrze[, -(8:10)]

# 2.6
today <- Sys.Date()
days <- julian(today)
format(today, "Dzisiaj jest %A (%d %B %y)")
weekdays(today)
as.Date(c("01.03.20"), "%d.%m.%y")
diff <- as.numeric(today - powietrze$data)

# 2.7
head(powietrze, 5)
powietrze[2:3,]
powietrze[seq(1, 31, by=5),]
tail(powietrze)

# 2.8
any(is.na(powietrze$PM25))
!all(!is.na(powietrze$PM25))

# 2.9
powietrze[(powietrze$PM10 > 20),]
powietrze[which(powietrze$PM10 > 20),]

# 2.10
powietrze[which(powietrze$PM10 > 25 | powietrze$PM25 < 10),]

# 2.11
powietrze[which(powietrze$PM25 < quantile(powietrze$PM25, na.rm=T, probs=0.25)),
          "poziom_PM25"] <- "L"
powietrze[which(powietrze$PM25 >= quantile(powietrze$PM25, na.rm=T, probs=0.25) & powietrze$PM25 <= quantile(powietrze$PM25, na.rm=T, probs=0.75)),
          "poziom_PM25"] <- "M"
powietrze[which(powietrze$PM25 > quantile(powietrze$PM25, na.rm=T, probs=0.75)),
          "poziom_PM25"] <- "H"

# 2.12
powietrze[which(powietrze$PM10 < quantile(powietrze$PM10, na.rm=T, probs=0.25)),
          "poziom_PM10"] <- "L"
powietrze[which(powietrze$PM10 >= quantile(powietrze$PM10, na.rm=T, probs=0.25) & powietrze$PM10 <= quantile(powietrze$PM10, na.rm=T, probs=0.75)),
          "poziom_PM10"] <- "M"
powietrze[which(powietrze$PM10 > quantile(powietrze$PM10, na.rm=T, probs=0.75)),
          "poziom_PM10"] <- "H"

# 2.13
table(powietrze$poziom_PM25, powietrze$poziom_PM10)

# 2.14
sort(powietrze$PM25, decreasing=TRUE)

# 2.15
powietrze[order(powietrze$PM25, decreasing = T),]

# 2.16
library(dplyr)
powietrze %>% # weź dane ze zbioru powietrze
  filter(month(data) == 8) %>% # pozostaw tylko dane z sierpnia
  select(-(poziom_PM25:poziom_PM10)) %>% # ukryj kolumny poziom_25 oraz poziom_PM10
  mutate(dzien_tyg=weekdays(data)) %>% # wyznacz dni tygodnia
  group_by(dzien_tyg) %>% # pogrupuj po dniu tygodnia
  summarize(srednie_PM25 = mean(PM25), srednie_PM10 = mean(PM10)) %>% # wyświetl wartości średnie dla każdego dnia tygodnia
  arrange(desc(srednie_PM25)) # posortuj malejąco wg PM25)
        
# 2.17
library(reshape2)
powietrze_waska <- melt(powietrze, id=c("data"),
                        measure.vars=c("PM25", "PM10", "NO2", "NO", "NOx", "CO","C6H6"))
dcast(powietrze_waska, data ~ variable)

# 3. ZADANIA
# 2.
library(tidyverse)
# wczytaj dane pomiarowe do obiektu tibble i usuń wiersze z wartościami NA
gios_pjp_data1 <- na.omit(read_csv("lab3/dane/gios-pjp-data1.csv", col_types = cols(...1 = col_skip())))
# zmień nazwy kolumn na bardziej czytelne
names(gios_pjp_data1)[2:8] <- c("PM25", "PM10", "NO2", "NO", "NOx", "CO", "C6H6")


# 3.
library(dplyr)
# utwórz nową ramkę danych
gios_pjp_data1_dobowe <- gios_pjp_data1
# zostaw tylko datę, bez godziny, konwertuj na typ Date
gios_pjp_data1_dobowe$Data <- as.Date(gios_pjp_data1_dobowe$Data)

# pogrupuj dane po dacie i wylicz średnią dla każdego pomiaru
gios_pjp_data1_dobowe <- gios_pjp_data1_dobowe %>%
  group_by(Data) %>%
  summarize(srednie_PM25 = mean(PM25), srednie_PM10 = mean(PM10), srednie_NO2 = mean(NO2), srednie_NO = mean(NO), srednie_NOx = mean(NOx), srednie_CO = mean(CO), srednie_C6H6 = mean(C6H6))

# 4.
gios_pjp_data1_dobowe %>% # weź dane ze zbioru gios_pjp_data_dobowe
  mutate(dzien_tyg=weekdays(Data)) %>% # wyznacz dni tygodnia
  group_by(dzien_tyg) %>% # pogrupuj po dniu tygodnia
  summarize(srednie_NO2 = mean(srednie_NO2)) %>% # wyświetl wartość średnią NO2 dla każdego dnia tygodnia
  arrange(match(dzien_tyg, c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))) # posortuj wg dnia tygodnia
