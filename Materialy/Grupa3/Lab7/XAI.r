library(DALEX)
library(ranger)

# Stwórzmy dwa modele, liniowy oraz las losowy
apartments_lm_model <- lm(m2.price ~ ., data = apartments)
predicted_mi2_lm <- predict(apartments_lm_model, apartmentsTest)
sqrt(mean((predicted_mi2_lm - apartmentsTest$m2.price)^2))

apartments_rf_model <- ranger(m2.price ~ ., data = apartments)

# Residuals
predicted_mi2_rf <- predict(apartments_rf_model, apartmentsTest)$predictions
sqrt(mean((predicted_mi2_rf - apartmentsTest$m2.price)^2))

explainer_lm <- explain(apartments_lm_model,
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)


explainer_rf <- explain(apartments_rf_model,
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

mp_lm <- model_performance(explainer_lm)
mp_rf <- model_performance(explainer_rf)
plot(mp_lm)
plot(mp_lm, geom = "boxplot")
plot(mp_lm, mp_rf)
plot(mp_lm, mp_rf, geom = "boxplot")

md_lm <- model_diagnostics(explainer_lm)
md_rf <- model_diagnostics(explainer_rf)


plot(md_lm, md_rf, variable = "y", yvariable = "y_hat")

library(ggplot2)

plot(md_lm, md_rf, variable = "y", yvariable = "y_hat") +
  facet_wrap(~label) +
  geom_abline(slope = 1)



# Variable Importance
vi_lm <- variable_importance(explainer_lm, loss_function = loss_root_mean_square)
plot(vi_lm)

vi_rf <- variable_importance(explainer_rf, loss_function = loss_root_mean_square)
plot(vi_rf)

plot(vi_lm, vi_rf)


# Czy zadziała dla klasyfikacji? Tak

titanic_glm_model <- glm(survived ~., data = titanic_imputed, family = "binomial")
titanic_rf_model <- ranger(as.factor(survived)~., data = titanic_imputed, classification = TRUE, probability = TRUE)

explainer_classif_lm <- explain(titanic_glm_model, data = titanic_imputed[,-8], y = titanic_imputed$survived)
explainer_classif_rf <- explain(titanic_rf_model, data = titanic_imputed[,-8], y = titanic_imputed$survived)

vi_class_lm <- variable_importance(explainer_classif_lm, loss_function = loss_one_minus_auc)
plot(vi_class_lm)

vi_class_rf <- variable_importance(explainer_classif_rf, loss_function = loss_one_minus_auc)
plot(vi_class_rf)

plot(vi_class_lm, vi_class_rf)

# Partial Dependence Plots

pdp_regr_rf  <- variable_effect(explainer_rf, variable =  "construction.year", type = "partial_dependency")
pdp_regr_lm  <- variable_effect(explainer_lm, variable =  "construction.year", type = "partial_dependency")
plot(pdp_regr_rf, pdp_regr_lm)

pdp_classif_rf  <- variable_effect(explainer_classif_lm, variable =  "fare", type = "partial_dependency")
pdp_classif_lm  <- variable_effect(explainer_classif_rf, variable =  "fare", type = "partial_dependency")
plot(pdp_classif_rf, pdp_classif_lm)

pdp_regr_rf_factor  <- variable_effect(explainer_rf, variable =  "district",
                                       type = "partial_dependency", variable_type = "categorical")
pdp_regr_lm_factor  <- variable_effect(explainer_lm, variable =  "district",
                                       type = "partial_dependency", variable_type = "categorical")
plot(pdp_regr_rf_factor, pdp_regr_lm_factor)

# iBreakDown

bd_lm <- predict_parts(explainer_lm, new_observation = apartments[1,])
bd_rf <- predict_parts(explainer_rf, new_observation = apartments[1,])
plot(bd)
plot(bd_rf)

library(patchwork)

plot(bd) | plot(bd_rf)

# modelStudio

library(modelStudio)
new_observations <- titanic_imputed[1:4,]
rownames(new_observations) <- c("Lucas", "James", "Thomas", "Nancy")
modelStudio(explainer_classif_rf, new_observations)

# DALEXtra

library(DALEXtra)


library("mlr")
library("DALEXtra")
task <- mlr::makeRegrTask(
  id = "R",
  data = apartments,
  target = "m2.price"
)
learner_lm <- mlr::makeLearner(
  "regr.lm"
)
model_lm <- mlr::train(learner_lm, task)
explainer_lm <- explain_mlr(model_lm, apartmentsTest, apartmentsTest$m2.price, label = "LM")

learner_rf <- mlr::makeLearner(
  "regr.randomForest"
)
model_rf <- mlr::train(learner_rf, task)
explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF")


plot_data <- funnel_measure(explainer_lm, explainer_rf,
                            nbins = 5, measure_function = DALEX::loss_root_mean_square)
plot(plot_data)

