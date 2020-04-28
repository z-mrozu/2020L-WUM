source("load&preprocessing.R")

library(xgboost)
library(ada)
library(mlr)
library(DALEX)
library(DALEXtra)
library(OpenML)

set.seed(123)

# Modifying dataset for XGBoost and ADABoost

# train and test datasets
m <- sample(1:nrow(data_mod), 0.7*nrow(data_mod))
data_train <- data_mod[m,]
data_test <- data_mod[-m,]

matrix_train <- data.matrix(data_train)
matrix_test <- data.matrix(data_test)

# tasks
task_train <- mlr::makeClassifTask(
  id = "german_credit",
  data = data_train,
  target = "customer_type"
)
task_test <- mlr::makeClassifTask(
  id = "german_credit_test",
  data = data_test,
  target = "customer_type"
)

resample_desc <- makeResampleDesc("CV", iters = 4)

# XGBoost
learner_xgb <- mlr::makeLearner(
  "classif.xgboost",
  predict.type = "prob"
)
model_xgb <- mlr::train(learner_xgb, task_train)
explainer_xgb <- explain(getLearnerModel(model_xgb), matrix_test[,-which(colnames(matrix_test) == 'customer_type')], matrix_test[,'customer_type'], label = "XGBoost",
                            verbose = FALSE, precalculate = FALSE)
res_xgb <- resample(learner_xgb, task_train, resample_desc, measures = list(auc, acc, ppv), models = TRUE)
res_xgb$aggr
# auc: 0.7258615

# ADABoost
learner_ada <- mlr::makeLearner(
  "classif.ada",
  predict.type = "prob"
)
model_ada <- mlr::train(learner_ada, task_train)
explainer_ada <- explain_mlr(model_ada, data_test, data_test$customer_type, label = "ADABoost",
                            verbose = FALSE, precalculate = FALSE)

res_ada <- resample(learner_ada, task_train, resample_desc, measures = list(auc, acc, ppv), models = TRUE)
res_ada$aggr
# auc: 0.7621625

# not working for these models (?)
# plot_data <- funnel_measure(explainer_xgb, explainer_ada, 
#                             nbins = 5, measure_function = DALEX::loss_root_mean_square, show_info = FALSE)

# hyper parameters tuning

control <- makeTuneControlRandom(maxit = 30)

# getParamSet("classif.xgboost")
# getParamSet("classif.ada")

params_xgb <- makeParamSet(
  makeIntegerParam("nrounds", lower = 100, upper = 500),
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  makeNumericParam("eta", lower = 0.1, upper = 0.5),
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
)

params_ada <- makeParamSet(
  makeIntegerParam("iter", lower = 10, upper = 300),
  makeIntegerParam("maxdepth", lower = 1, upper = 30),
  makeNumericParam("cp", lower = 0.001, upper = 0.1),
  makeNumericParam("delta", lower = -15, upper = 0, trafo = function(x) 10^x)
)

params_xgb_tuned <- tuneParams(
  learner = learner_xgb,
  task = task_train,
  resampling = resample_desc,
  par.set = params_xgb,
  control = control
)

params_ada_tuned <- tuneParams(
  learner = learner_ada,
  task = task_train,
  resampling = resample_desc,
  par.set = params_ada,
  control = control
)

learner_xgb_tuned <- setHyperPars(
  learner = learner_xgb,
  par.vals = params_xgb_tuned$x
)

learner_ada_tuned <- setHyperPars(
  learner = learner_ada,
  par.vals = params_ada_tuned$x
)

# params_xgb_tuned$x
# params_ada_tuned$x

model_xgb_tuned <- train(learner_xgb_tuned, task_train)

model_ada_tuned <- train(learner_ada_tuned, task_train)

mltools::auc_roc(preds = predict(model_xgb_tuned, newdata = data_test)$data$prob.Good,
                 actuals = as.numeric(data_test$customer_type) - 1)
# auc: 0.7691993 (previously 0.7258615)

mltools::auc_roc(preds = predict(model_ada_tuned, newdata = data_test)$data$prob.Good,
                 actuals = as.numeric(data_test$customer_type) - 1)
# auc: 0.7740502 (previously 0.7621625)

# save(model_xgb_tuned, file = 'models/xgb_tuned.rda')
# save(model_ada_tuned, file = 'models/ada_tuned.rda')

# DALEX not working (?)

xgb_tuned_raw <- getLearnerModel(model_xgb_tuned)
ada_tuned_raw <- getLearnerModel(model_ada_tuned)

explainer_xgb <- explain(xgb_tuned_raw,
                      data = matrix_test[,-which(colnames(matrix_test) == 'customer_type')],
                      y = matrix_test[,'customer_type'],
                      label = 'XGBoost')
explainer_ada <- explain(ada_tuned_raw,
                   data = data_test[,-which(names(data_test) == 'customer_type')],
                   y = data_test$customer_type,
                   label = 'ADABoost')


feature_i_xgb <- variable_importance(explainer_xgb, loss_function = loss_one_minus_auc)
feature_i_ada <- variable_importance(explainer_ada, loss_function = loss_one_minus_auc)
