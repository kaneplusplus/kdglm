
context("Survival Model Testing")

library(devtools)
library(survival)
data(lung)

surv <- Surv(lung$time, lung$status)

surv <- survfit(Surv(time, status), data = lung)

cox_fit <- coxph(Surv(time, status) ~ age + sex, data = lung)

library(devtools)
library(survival)
library(tensorflow)
library(keras)
library(dplyr)
document()
data(lung)

lung <- lung %>%
  mutate(status = status - 1,
         sex = as.factor(sex),
         ph.ecog = as.factor(ph.ecog))

tf$config$run_functions_eagerly(TRUE)
dc_fit <- dlcp(
  lung, 
  Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + 
    meal.cal + wt.loss, 
#  hidden_layers = c(5, 2),
#  hidden_layers_activation = c("sigmoid", "linear"),
  output_activation = "linear",
  verbose = TRUE, epochs=10,
  optimizer = "sgd")

lung_na <- na.omit(lung)

m <- cbind(lung_na$time, lung_na$status, lung_na$ph.karno)
m[,3] <- m[,3] / 100.
neg_log_prop_haz_lik(m[,1:2, drop = FALSE], m[,3, drop = FALSE])
neg_log_prop_haz_lik_ref(m[, 1:2], m[, 3])


