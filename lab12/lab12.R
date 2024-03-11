# 1.
load("D:/RStudio_files/lab12/nasiona.RData")

# 2.
data_selection <- nasiona[, c("powierzchnia", "obwod", "wspolczynnik_asymetrii")]
summary(data_selection)

# 3.
std_data <- scale(data_selection)
summary(std_data)

# 4.
sim_measure <- "euclidean"
data_dist <- dist(std_data, method = sim_measure)

# 5.
hier_grp <- hclust(data_dist, method = "ward.D2")

# 6.
plot(hier_grp, cex = 0.5)

# 7.
rect.hclust(hier_grp, k = 5, border = 2:6)

# 8.
groups <- cutree(hier_grp, k = 5)
std_data <- cbind(std_data, nr_grupy = groups)
std_data <- data.frame(std_data)

# 9.
# wartości średnie atrybutów w grupach
mean_values <- aggregate(. ~ nr_grupy, data = std_data, mean)

# liczebności utworzonych grup
group_count <- data.frame(table(std_data$nr_grupy))
colnames(group_count) <- c("nr_grupy", "suma")

data_summary <- merge(mean_values, group_count, by = "nr_grupy", all = TRUE)

# 10.
library(ggplot2)
nasiona_wykres <- ggplot(data = std_data, aes(x = powierzchnia, y = obwod, color = nr_grupy)) + 
  geom_point() + 
  labs(title = "Wykres powierzchni do obwodu nasion", x = "Powierzchnia", y = "Obwód")

pods_grup_wykres <- ggplot(data = data_summary, aes(x = powierzchnia, y = obwod, color = nr_grupy)) + 
  geom_point() + 
  labs(title = "Wykres powierzchni do obwodu grup nasion", x = "Powierzchnia", y = "Obwód")

nasiona_wykres
pods_grup_wykres
