# Test chi-kwadrat
# Będzie na statystyce, pokażemy tylko jak go użyć. Używamy go do zmiennych kategorycznych by sprawdzić
# czy mają coś wspólnegoz targetem

library("DALEX")

data <- titanic_imputed

chisq.test(table(data$class, data$survived))

#p-value < 0.05 - można przyjąć, że zmienne zależne

x <- cut(rnorm(10000), c(-Inf,-2, -1, -0.5, 0, 0.5, 1, 2, Inf))
y <- cut(rnorm(10000), c(-Inf,-2, -1, -0.5, 0, 0.5, 1, 2, Inf))
chisq.test(table(x, y))

# p-value > 0.05 - Nie ma podstaw by powiedzieć, że są zależne
# Wykres analizy korespondencji, (ca plot). Pakiet ca, funkcja ca. Dla chętnych, jeżeli będzie ich więcej to mogę
# wyjaśnić na następnych zajęciach

# Missing values
# Fajny tutorial dla Rowcow. My skupimy się tylko na MICE
# https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/

# MICE
# Uzupełniamy brakujące wartości przewidując czego nam brakuje na podstawie pozostałych rozkładów

data <- apartments
#Tworzymy NA
#install.packages("missForest")
library("missForest")
set.seed(123)
data_na <- prodNA(apartments, noNA = 0.1)
summary(data_na)

#install.packages("mice")
library("mice")
data_imputed <- mice(data_na, m = 3, method = "pmm", maxit = 10)
summary(data_imputed)
data_imputed_1 <- complete(data_imputed, 2)
sqrt(mean((data$m2.price - data_imputed_1$m2.price)^2))

data_imputed_2 <- complete(data_imputed, 3)
sqrt(mean((data$m2.price - data_imputed_2$m2.price)^2))

# Nie tak źle! Czy można lepiej?
data_imputed_3 <- data_na


# Po średniej dla danej kategorii
for (r in 1:nrow(data_imputed_3)) {
  if (is.na(data_imputed_3[r,]$m2.price)) {
    data_imputed_3[r,]$m2.price <- mean(data[data$district == data[r,]$district,]$m2.price)
  }
}

sqrt(mean((data$m2.price - data_imputed_3$m2.price)^2))

data_imputed_4 <- data_na
data_imputed_4[is.na(data_imputed_4$m2.price),]$m2.price <- mean(data_imputed_4$m2.price, na.rm = TRUE)
sqrt(mean((data$m2.price - data_imputed_4$m2.price)^2))

# Wyszło dużo gorzej?

# Czy dla factorow da się?
barplot(table(data_na$district))
# Sa mniejwiecej rowno rozrzucone

plot(x = unique(data_na$district)[-1], y = sapply(unique(data_na$district)[-1], function(x){
  mean(data_na[data_na$district == x,]$m2.price, na.rm = TRUE)
}))
# Możemy podzielić faktory na 3 grupy i na podstawie tego uzupełniać w żalezności od ceny
# (z rówynm prawdopodobieństwem uzupełnienia dzielnicą z danej grupy)

data_imputed_5 <- complete(data_imputed, 1)
mean(data[is.na(data_na$district),]$district == data_imputed_5[is.na(data_na$district),]$district)
# Wyszło słabo :( Ale nie powinniśmy się przejmować, częściowa odpowiedź czemu w linii 71, niektóre dzielnice
# nie różnicują zbioru

# Brak danych to nie zawsze brak informacji? Czy znamy jakiś przykład?

# Laczenie zmiennych
# Czasem zmienna kategoryczna ma za dużo poziomów

data <- titanic
table(data$country)
library("mlr")
createDummyFeatures(titanic$country)
#Jak zareaguje na to model? Spoiler: źle

#Rozwiązanie
library(forcats)
new_var_1 <- fct_lump(data$country, n = 5)
table(new_var_1)
createDummyFeatures(new_var_1)
new_var_2 <- fct_lump(data$country, prop = 0.05)
table(new_var_2)
createDummyFeatures(new_var_2)

#PCA

data <- na.omit(createDummyFeatures(titanic_imputed))

data_pca <- princomp(~., data[,-5])
summary(data_pca)
data_pca$loadings[,1]



