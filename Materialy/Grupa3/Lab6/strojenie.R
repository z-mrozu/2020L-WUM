library(mlr)
library(DALEX)

# Wczytanie danych oraz zamian kolumny numerycznej na factor (target)
data <- titanic_imputed
data$survived <- as.factor(data$survived)
m <- sample(1:nrow(data), 0.7*nrow(data))
data_train <- data[m,]
data_test <- data[-m,]

# Tworzymy task oraz learner
task <- makeClassifTask(data = data_train, target = "survived")
lrn <- makeLearner("classif.gbm", par.vals = list(distribution = "bernoulli"), predict.type = "prob")

cv <- makeResampleDesc("CV", iter = 6)

model_raw <- train(lrn, task)
predict(model_raw, newdata = data_test)$data$prob.1
mltools::auc_roc(preds = predict(model_raw, newdata = data_test)$data$prob.1,
                 actuals = as.numeric(as.character(data_test$survived)))

# Tworzymy zbiór parametrów które będziemy stroić

#makeNumericParam()
#makeIntegerParam()
#makeLogicalParam()
#makeDiscreteParam()

gbm_ps = makeParamSet(
  makeIntegerParam("n.trees", lower = 500, upper = 2000),
  makeIntegerParam("interaction.depth", lower = 1, upper = 10),
  makeIntegerParam("n.minobsinnode", lower = 3, upper = 14),
  makeNumericParam("shrinkage", lower = -10, upper = -1, trafo = function(x) 2^x)
  )

# Grid Search

# Resolution jak sama nazwa wskazuje definiuje nam rozdzielczość siatki, tj. dzieli przedział dostępnych wartości
# na r a następnie sprawdza każdy punkt siatki. Łatwo policzyć, ze dla r = 2 mamy 16 iteracji, zaś dla r = 4 już 256

ctrl_grid <- makeTuneControlGrid(resolution = 2)
res_grid <- tuneParams(lrn, task = task, resampling = cv,
                       par.set = gbm_ps, control = ctrl_grid, measures = auc)

model_grid <- train(res_grid$learner, task)
mltools::auc_roc(preds = predict(model_grid, newdata = data_test)$data$prob.1,
                 actuals = as.numeric(as.character(data_test$survived)))


# Mamy learner z którym możemy robić co chcemy

# Random Search

# Ogólnie maxit im więcej tym lepiej, tutaj taka mała wartość, żeby skończyć w skończonym czasie
ctrl_random <- makeTuneControlRandom(maxit = 30)
res_random <- tuneParams(lrn, task = task, resampling = cv,
                       par.set = gbm_ps, control = ctrl_random, measures = auc)

model_random <- train(res_random$learner, task)
mltools::auc_roc(preds = predict(model_random, newdata = data_test)$data$prob.1,
                 actuals = as.numeric(as.character(data_test$survived)))




# Random Search w ogólnym przypadku jest dużo lepszy


# Do domu poczytać makeTuneControlMBO() - będzie w następnej pracy domowej :)
