

library("counterfactuals")
library("iml")
library("randomForest")
library("mlr3")
library("mlr3learners")

data(german, package = "rchallenge")  
credit = german[, c("duration", "amount", "purpose", "age", 
                    "employment_duration", "housing", "number_credits", "credit_risk")]


set.seed(20210816)
rf = randomForest::randomForest(credit_risk ~ ., data = credit[-998L,])


predictor = iml::Predictor$new(rf, type = "prob")
x_interest = credit[998L, ]
predictor$predict(x_interest)


moc_classif = MOCClassif$new(
  predictor, epsilon = 0, fixed_features = c("age", "employment_duration"), 
  quiet = TRUE, termination_crit = "genstag", n_generations = 10L
)


cfactuals = moc_classif$find_counterfactuals(
  x_interest, desired_class = "good", desired_prob = c(0.6, 1)
)


head(cfactuals$evaluate(show_diff = TRUE, 
                        measures = c("dist_x_interest", "dist_target", "no_changed", "dist_train")), 3L)




