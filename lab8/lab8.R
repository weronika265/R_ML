# Naiwny klasyfikator Bayesa
library("Przewodnik")
head(titanic)

library("e1071")
nb <- naiveBayes(Survived~Sex+Pclass+Age, data = titanic)
nb

library("klaR")
nb <- NaiveBayes(Survived~Age, data=na.omit(titanic))
plot(nb)

###

# 1.
library(tidyverse)
email <- read.csv("C:/Users/Admin/RStudio/lab8/email.csv")
head(email)
str(email)

# 2.
# przekształć zmienną message_label do typu kategorialnego
email$message_label <- factor(email$message_label)
str(email)

# 3.
library(tidyr)
email2 <- email %>% gather(word, count, -message_index, -message_label)

# 4.
library(dplyr)
email2 %>%
  group_by(word) %>%
  summarise(occurence = sum(count)) %>%
  arrange(desc(occurence)) %>%
  slice(0:10)

# 5.
# dla wiadomości zwykłych
email2 %>%
  filter(message_label == "ham") %>%
  group_by(word) %>%
  summarise(occurence = sum(count)) %>%
  arrange(desc(occurence)) %>%
  slice(0:10)

# dla wiadomości spam
email2 %>%
  filter(message_label == "spam") %>%
  group_by(word) %>%
  summarise(occurence = sum(count)) %>%
  arrange(desc(occurence)) %>%
  slice(0:10)


# 6.
library(rpart)
library(caTools)
# podziel zbiór danych na zbiór treningowy (75%) i testowy (25%)
set.seed(123)
sample <- sample.split(email2$message_label, SplitRatio = 0.75)
train_set <- subset(email2, sample == TRUE)
test_set <- subset(email2, sample == FALSE)
# wyświetl rozkłady klas dla wszystkich zbiorów danych
print(prop.table(table(email2$message_label)))
print(prop.table(table(train_set$message_label)))
print(prop.table(table(test_set$message_label)))

# 7.
library(e1071)
email_model_nb <- naiveBayes(message_label ~ ., data=train_set)
email_model_nb

# 8.
preds <- predict(email_model_nb, test_set, type = "class")

conf_matrix <- table(test_set$message_label, preds)
conf_matrix

pred_accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
pred_accuracy
# dokładność predykcji wynosi 73%

# 9.
# Aby zwiększyć dokładność predykcji klasyfikatora można m.in.:
# - zwiększyć ilość danych treningowych
# - usunąć lub zredukować dane, które mają zbyt dużo szumu lub są nieistotne
# - eksperymentować z różnymi kombinacjami cech
# - spróbować wyekstrahować dodatkowe dane z wiadomości


# Sieć Bayesa
library(bnlearn)
# załadowanie danych
data(coronary)

# utworzenie i wizualizacja sieci Bayesa
bn_df <- data.frame(coronary)
res <- hc(bn_df)
plot(res)

# modyfikacja struktury sieci
res$arcs <- res$arcs[-which((res$arcs[,'from'] == "M..Work" &
                               res$arcs[,'to'] == "Family")),]

# znalezienie tablic prawdopodobieństwa warunkowego (CPT) w każdym węźle
fittedbn <- bn.fit(res, data = bn_df)
print(fittedbn$Proteins)

cpquery(fittedbn, event = (Proteins=="<3"), evidence = ((Smoking=="no") &
                                                          (Pressure == ">140")))
