#install.packages("OpenML")
#install.packages(mlr)

library(OpenML)
library(mlr)

set.seed(1)

# pobranie danych
monks <- getOMLDataSet(data.id = 334L)
monks <- monks$data
head(monks)

# model
classif_task <- makeClassifTask(id = "lvr", data = monks, target = "class")
# listowanie learnerow ze wsparciem dla prawdopodobieństw
listLearners(properties = "prob")$class
# listowanie zbioru hiperparametrów
getLearnerParamSet("classif.randomForest")

classif_lrn <- makeLearner("classif.ranger", par.vals = list(num.trees = 500, mtry = 3), predict.type = "prob")

# jak sprawdzic mozliwe parametry
getParamSet(classif_lrn)
helpLearnerParam(classif_lrn)
getHyperPars(classif_lrn)

# audyt modelu
cv <- makeResampleDesc("CV", iters = 7)
r <- resample(classif_lrn, classif_task, cv, measures = list(auc), models = TRUE)
r$models
AUC <- r$aggr
AUC

listMeasures()
?listMeasures()
listMeasures(obj = "classif")

# predict

model <- r$models[[1]]

model

# BARDZO WAZNA UWAGA O KOLEJNOSCI ARGUMENTOW
p <- predict(model, newdata = monks)
p

m <- sample(1:nrow(monks), 0.7*nrow(monks))
monks_train <- monks[m,]
monks_test <- monks[-m,]

classif_task <- makeClassifTask(id = "lvr", data = monks_train, target = "class")
classif_lrn <- makeLearner("classif.ranger", par.vals = list(num.trees = 500, mtry = 3), predict.type = "prob")
model <- train(classif_lrn, classif_task)
predict(model, newdata = monks_train)
