# 1.
library(tidyverse)
college <- read_csv("D:/RStudio_files/lab11/college.csv")

college$loan_default_rate <- as.numeric(college$loan_default_rate)

# 2.
college_Meryland <- college[college$state == 'MD',]
college_Meryland <- remove_rownames(college_Meryland)

univ_names <- column_to_rownames(college_Meryland, var="name")

summary(univ_names$admission_rate)
summary(univ_names$sat_avg)

# 3.
college_scaled <- scale(univ_names[c("admission_rate", "sat_avg")])
summary(college_scaled)

# 4.
set.seed(1)
km <- kmeans(college_scaled, centers = 3, nstart = 25)

# 5.
km
km$size # 1) 8, 2) 9, 3) 2
km$centers
# 1) 0.6608405, 0.4894679
# 2) -0.2001854, -0.8322366
# 3) -1.7425275, 1.7871932

# 6.
library(factoextra)
fviz_cluster(km, college_scaled, repel = TRUE)
# grupa 1: uniwersytety, do których łatwiej się dostać - mają wysoki wskaźnik przyjęcia przy średnim wyniku SAT (uczelnie, na które bardzo ciężko jest się dostać)
# grupa 2: uniwersytety, które nie wymagają wysokiej średniej z egzaminu SAT, jednak tutaj wskaźnik przyjęcia kandydatów jest bliski średniej (uczelnie, na które jest się łatwo dostać w założeniem, że nasz wynik testu był w okolicach średniej)
# grupa 3: uniwersytety, które przyjmują mało studentów, nawet gdy ich średnia z egzaminu SAT jest bardzo wysoka (uczelnie, które nie wymagają wysokiego wyniku egzaminu)

# 7.
# przypisz etykiety grup do obserwacji w zbiorze
college_Meryland$cluster <- as.factor(km$cluster)

# grupuj według skupień i generuj średnie wartości dla każdego z atrybutów
grouped_data <- college_Meryland %>% 
  select(undergrads, tuition, loan_default_rate, faculty_salary_avg, median_debt, cluster) %>%
  group_by(cluster) %>%
  summarize(undergrads_mean = mean(undergrads), tuition_mean = mean(tuition), loan_default_rate_mean = mean(loan_default_rate), faculty_salary_avg_mean = mean(faculty_salary_avg), median_debt_mean = mean(median_debt))
# grupa 1: najmniej zarabiający pracownicy, średnia wysokość czesnego zbliżona do grupy 3, a podobno do grupy 2 pod względem zarobków pracowników i spłacalności kredytów
# grupa 2: najniższa średnia osób na studiach licencjackich, najniższe czesne, najlepsza spłacalność kredytów przez absolwentów
# grupa 3: najwyższa średnia osób na studiach licencjackich, najlepiej zarabiający pracownicy, najmniej kredytów absolwentów, jednak najmniejsza ich spłacalność

# 8.
wss_dist <- fviz_nbclust(college_scaled, kmeans, method = "wss")
print(wss_dist)
# k = 4

silhouette_dist <- fviz_nbclust(college_scaled, kmeans, method = "silhouette")
print(silhouette_dist)
# k = 4

gap_stat_dist <- fviz_nbclust(college_scaled, kmeans, method = "gap_stat")
print(gap_stat_dist)
# k = 1

opt_k <- 4 # lub wcześniejsze 3?

final_km <- kmeans(college_scaled, centers = opt_k, nstart = 25)
fviz_cluster(final_km, college_scaled, repel = TRUE)
