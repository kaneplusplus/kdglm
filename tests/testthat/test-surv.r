
context("Survival Model Testing")

library(devtools)
library(survival)
data(lung)

surv <- Surv(lung$time, lung$status)

surv <- survfit(Surv(time, status), data = lung)

cox_fit <- coxph(Surv(time, status) ~ age + sex, data = lung)

library(keras)
library(devtools)
library(survival)
library(tensorflow)
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
  Surv(time, status) ~ sex + age + meal.cal + wt.loss + ph.ecog, 
#  hidden_layers = c(16),
#  hidden_layers_activation = c("linear"),
  verbose = TRUE, 
  epochs = 500,
  validation_split = .2)
get_weights(dc_fit$model)
coxph(Surv(time, status) ~ sex + age + meal.cal + wt.loss + ph.ecog, lung)
plot(dc_fit$history)

lung_na <- na.omit(lung)

m <- cbind(lung_na$time, lung_na$status, lung_na$ph.karno)
m[,3] <- m[,3] / 100.
neg_log_prop_haz_lik(m[,1:2, drop = FALSE], m[,3, drop = FALSE])
neg_log_prop_haz_lik_ref(m[, 1:2], m[, 3])


