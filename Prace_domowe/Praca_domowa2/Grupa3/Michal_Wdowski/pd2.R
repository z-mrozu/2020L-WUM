# wczytanie
library(dplyr)
library(ggplot2)
data <- read.csv("Prace_domowe/Praca_domowa2/Grupa3/Michal_Wdowski/allegro-api-transactions.csv")

# wstępne spojrzenie
summary(data)

# zmiana na zmienne kategoryczne to co w istocie powinno nimi być
data$date <- as.POSIXct(data$date)
data$pay_option_on_delivery <- as.factor(data$pay_option_on_delivery)
data$pay_option_transfer <- as.factor(data$pay_option_transfer)
data$it_is_allegro_standard <- as.factor(data$it_is_allegro_standard)
data$it_is_brand_zone <- as.factor(data$it_is_brand_zone)

summary(data)

# kodowanie zmiennych kategorycznych
# te zmienne to:

# categories - listy z kilkoma kategoriami
table(data$categories)
# ma ponad 8k kategorii - thank you allegro, very cool
# ale na szczęscie jest main_cathegory, może to się do czegoś przyda

# pay_option_on_delivery
table(data$pay_option_on_delivery)
# same zera i jedynki - cool

# pay_option_transfer
table(data$pay_option_transfer)
# same zera i jedynki - cool

# seller
table(data$seller)
# ponad 50k rekordów, ale może da się znależć kilka najbardziej dominujących
data %>%
  group_by(seller) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) -> sellers_summary
# no nie wiem, nie robi sie ich sporo mniej, ale może spróbujemy

# it_is_allegro_standard
table(data$it_is_allegro_standard)
# same zera i jedynki - cool

# it_is_brand_zone
table(data$it_is_brand_zone)
# same zera i jedynki - cool

# it_location
table(data$it_location)
# ponad 9k lokalizacji (w tym takie jak "CAŁOWANIE" albo "cały kraj" zapisane na 10 różnych sposobów - małymi i wielkimi literami, z polskimi znakami i bez...)
data %>%
  group_by(it_location) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) -> localization_summary
# o tak, 12 i 13 pozycja to "internet" oraz "INTERNET". No ale też spróbujemy

# main_category
table(data$main_category)
# o, tych jest mało - 27
data %>%
  group_by(main_category) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) -> main_category_summary
# no to dużo mniej, ale też zobaczymy

# dobra, robimy tak - zostaje main_cathegory i te z 0/1 wartościami, a reszta - wyjazd z budowy
data %>%
  select(-it_location, -seller, -categories) -> data_dropped

# one-hot encoding
library(mlr)
data_onehot_encoded1 <- createDummyFeatures(data_dropped, target = "price", method = "1-of-n")
data_onehot_encoded2 <- createDummyFeatures(data_dropped, target = "price", method = "reference")


# imputacja
data2 <- data %>% select(price, it_seller_rating, it_quantity)

library("missForest")
library("mice")
library(Metrics)

set.seed(2137)

values <- rep(0, 10)
data_predeleted <- head(data2, 42000)

for(i in seq(1, 10, 1))
{
    print(i)
    data_deleted <- cbind(data_predeleted, prodNA(as.data.frame(data_predeleted$it_seller_rating), noNA = 0.1))
    colnames(data_deleted) <- c("price", "usun_to", "it_quantity", "it_seller_rating")
    data_deleted <- select(data_deleted, -usun_to)
    data_imputed <- mice(data_deleted, m = 3, method = "pmm", maxit = 5)
    data_imputed_1 <- complete(data_imputed, 2)
    values[i] <- rmse(data_predeleted$it_seller_rating, data_imputed_1$it_seller_rating)
}

values2 <- rep(0, 10)

for(i in seq(1, 10, 1))
{
    print(i)
    data_deleted <- cbind(data_predeleted, prodNA(as.data.frame(data_predeleted$it_seller_rating), noNA = 0.1), prodNA(as.data.frame(data_predeleted$it_quantity), noNA = 0.1))
    colnames(data_deleted) <- c("price", "usun_to", "to_tez", "it_seller_rating", "it_quantity")
    data_deleted <- select(data_deleted, -usun_to, -to_tez)
    data_imputed <- mice(data_deleted, m = 3, method = "pmm", maxit = 5)
    data_imputed_1 <- complete(data_imputed, 2)
    values2[i] <- rmse(data_predeleted$it_seller_rating, data_imputed_1$it_seller_rating)
}

values2

library(ggplot2)
results <- cbind(seq(1, 10, 1), values, values2)
ggplot(as.data.frame(results), aes(x = V1)) +
    geom_line(aes(y = values), color = "red") + 
    geom_line(aes(y = values2), color = "blue")

results <- as.data.frame(results)
x <- c(mean(results$values), mean(results$values2))

ggplot(as.data.frame(x), aes(x = c("1 kolumna", "2 kolumny"), y = x)) +
    geom_bar(stat = "identity") +
    xlab("") +
    ylab("RMSE")
