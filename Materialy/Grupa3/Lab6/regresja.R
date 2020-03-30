library(DALEX)
library(mlr)


data <- apartments

# model
task <- makeRegrTask(data = data, target = "m2.price")
lrn <- makeLearner("regr.rpart", par.vals = list(maxdepth = 5))

# audyt modelu
cv <- makeResampleDesc("CV", iters = 5)
r <- resample(lrn, task, cv, measures = list(mse, mae, rmse, rsq))
MSE <- r$aggr
MSE

sd(apartments$m2.price)


listMeasures(obj = "regr")
