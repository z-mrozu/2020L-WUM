source('load&preprocessing.R')
library(mlr)
library(ranger)
library(gbm)
library(OpenML)
library(DALEX)

# data_mod <- data2
# data_mod <- normalizeFeatures(data_mod, target = "customer_type")
# data_mod <- createDummyFeatures(
#   data_mod, target = "customer_type",
#   cols = c(
#     "checking_account_status",
#     "credit_history",
#     "purpose",
#     "savings",
#     "present_employment",
#     "personal",
#     "other_debtors",
#     "property",
#     "other_installment_plans",
#     "housing",
#     "job",
#     "telephone",
#     "foreign_worker"
#   )
# )

# Zbiór treningowy i testowy
set.seed(123)

m <- sample(1:nrow(data2), 0.7*nrow(data2))
data_train <- data2[m,]
data_test <- data2[-m,]


task <- makeClassifTask(id = "german_credit", data = data_train, target = "customer_type")

ranger_lrn <- makeLearner("classif.ranger", predict.type = "prob")
gbm_lrn <- makeLearner('classif.gbm', par.vals = list(distribution = 'bernoulli'),predict.type = 'prob')

cv <- makeResampleDesc("CV", iters = 5)
# r1 <- resample(ranger_lrn, task, cv, measures = list(auc, acc, ppv), models = TRUE)
# r1$aggr

# najważniejsze parametry dla ranger:
# num.trees
# mtry
# max.depth
# min.node.size
# splitrule
# importance

ranger_ps <- makeParamSet(
  makeIntegerParam("num.trees", lower = 200, upper = 1000),
  makeIntegerParam("mtry", lower = 2, upper = 7),
  makeIntegerParam("min.node.size", lower = 1, upper = 10),
  makeDiscreteParam("importance", values = c('none', 'impurity', 'permutation')),
  makeDiscreteParam("splitrule", values = c('gini', 'extratrees'))
)
ctrl_random <- makeTuneControlRandom(maxit = 30)
res_random <- tuneParams(ranger_lrn, 
                         task = task, 
                         resampling = cv,
                         par.set = ranger_ps, 
                         control = ctrl_random, 
                         measures = list(auc, acc, ppv))

ranger_tuned <- train(res_random$learner, task)
mltools::auc_roc(preds = predict(ranger_tuned, newdata = data_test)$data$prob.Good,
                 actuals = as.numeric(data_test$customer_type) - 1)
# save(ranger_tuned, file = 'models/ranger_tuned.rda')
# auc: 0.786

# najważniejsze parametry dla gbm:
# n.trees
# interaction.depth
# n.minobsinnode
# shrinkage
# bag.fraction

gbm_ps <- makeParamSet(
  makeIntegerParam("n.trees", lower = 200, upper = 1000),
  makeIntegerParam("interaction.depth", lower = 1, upper = 10),
  makeIntegerParam("n.minobsinnode", lower = 5, upper = 20),
  makeNumericParam("shrinkage", lower = 0.001, upper = 0.2),
  makeNumericParam("bag.fraction", lower = 0.1, upper = 0.9)
)
gbm_random <- tuneParams(gbm_lrn, 
                         task = task, 
                         resampling = cv,
                         par.set = gbm_ps, 
                         control = ctrl_random, 
                         measures = list(auc, acc, ppv))

gbm_tuned <- train(gbm_random$learner, task)
mltools::auc_roc(preds = predict(gbm_tuned, newdata = data_test)$data$prob.Good,
                 actuals = as.numeric(data_test$customer_type) - 1)
# auc_roc: 0.78
# save(gbm_tuned, file = 'models/gbm_tuned.rda')
# Pokombinujmy
ranger_tuned_raw <- getLearnerModel(ranger_tuned)
gbm_tuned_raw <- getLearnerModel(gbm_tuned)

ranger_exp <- explain(ranger_tuned_raw,
                      data = data_test[,-which(names(data_test) == 'customer_type')],
                      y = data_test$customer_type,
                      label = 'Random forest')
gbm_exp <- explain(gbm_tuned_raw,
                      data = data_test[,-which(names(data_test) == 'customer_type')],
                      y = data_test$customer_type,
                      label = 'Gradient Boost')

ranger_fi <- variable_importance(ranger_exp, loss_function = loss_one_minus_auc)
gbm_fi <- variable_importance(gbm_exp, loss_function = loss_one_minus_auc)

plot(ranger_fi)
plot(gbm_fi)
# Najważniejsze dla ranger: checking_account_status, duration, credit_amount, credit_history
# Najważniejsze dla gbm: checking_account_status, duration, putpose, credit_history

ranger_pd_nums <- variable_effect(ranger_exp, variables = c('duration',
                                                            'credit_amount'))
ranger_pd_cats <- variable_effect(ranger_exp, variables = c('checking_account_status',
                                                            'credit_history'))

gbm_pd_nums <- variable_effect(gbm_exp, variables = 'duration')
gbm_pd_cats <- variable_effect(gbm_exp, variables = c('checking_account_status',
                                                      'purpose',
                                                      'credit_history'))


plot(ranger_pd_nums)
plot(ranger_pd_cats)
plot(gbm_pd_nums)
plot(gbm_pd_cats)
