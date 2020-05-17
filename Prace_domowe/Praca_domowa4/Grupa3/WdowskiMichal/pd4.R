library(DALEX)
library(mlr)

# wczytanie danych
data <- apartments
summary(data)

# podział na zbiór treningowy i testowy
ktore <- sample(1:nrow(data), 0.7*nrow(data))
train <- data[ktore, ]
test <- data[-ktore, ]

# modelowanie
cv <- makeResampleDesc("CV", iter = 4)
task <- makeClassifTask(data = train, target = "district")
learner <- makeLearner("classif.ksvm")
model <- train(learner, task)
predictions <- predict(model, newdata = test[, -ncol(test)])

# wydobycie wyników i porównanie z oczekiwanymi wynikami
wyniki <- cbind(predictions$data, test[, ncol(test)])

# poprawnosc dla wszystkich
sum(wyniki[, 1] == wyniki[, 2]) / nrow(wyniki)

# poprawnosc tylko dla Srodmiescia
sum(wyniki[wyniki$response == "Srodmiescie", 1] == wyniki[wyniki$response == "Srodmiescie", 2]) / nrow(wyniki[wyniki$response == "Srodmiescie", ])

# strojenie parametrów
getParamSet(learner)
params <- makeParamSet(
    makeDiscreteParam("C", seq(from = 0.5, to = 10, by = 0.5)),
    makeDiscreteParam("sigma", seq(from = 0.5, to = 10, by = 0.5))
)

ctrl_random <- makeTuneControlRandom(maxit = 100)
res_random <- tuneParams(learner = learner, task = task, par.set = params, control = ctrl_random, measures = acc, resampling = cv)
