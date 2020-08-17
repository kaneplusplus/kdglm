
context("Survival Model Testing")

library(keras)
#library(devtools)
library(survival)
library(tensorflow)
library(dplyr)
#document()
data(lung)

lung <- lung %>%
  mutate(status = status - 1,
         sex = as.factor(sex),
         ph.ecog = as.factor(ph.ecog))

tf$config$run_functions_eagerly(TRUE)
dc_fit <- dcoxph(
  lung, 
  Surv(time, status) ~ sex + age + wt.loss,
#  hidden_layers = c(6),
#  hidden_layers_activation = c("sigmoid"),
  verbose = TRUE, 
  epochs = 200,
  validation_split = .2)
get_weights(dc_fit$model)
cph <- coxph(Surv(time, status) ~ sex + age + meal.cal + wt.loss + ph.ecog, 
             lung)

plot(dc_fit$history)

predict(dc_fit$model, lung)
