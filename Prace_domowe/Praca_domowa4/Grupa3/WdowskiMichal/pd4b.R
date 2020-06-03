
# wczytanie danych
data <- read.csv("Prace_domowe/Praca_domowa4/Grupa3/WdowskiMichal/census_income_dataset.csv")

head(data)
summary(data)

# podział na zbiór treningowy i testowy
ktore <- sample(1:nrow(data), 0.7*nrow(data))
train <- data[ktore, ]
test <- data[-ktore, ]

cv <- makeResampleDesc("CV", iter = 7)
task <- makeClassifTask(data = train, target = "income_level")
learner <- makeLearner("classif.ksvm")
model <- train(learner, task)
predictions <- predict(model, newdata = test[, -ncol(test)])

# wydobycie wyników i porównanie z oczekiwanymi wynikami
wyniki <- cbind(predictions$data, test[, ncol(test)])

# poprawnosc dla wszystkich
sum(wyniki[, 1] == wyniki[, 2]) / nrow(wyniki)
table(wyniki[, 1], wyniki[, 2])


