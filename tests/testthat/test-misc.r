context("Tests that need to be cleaned up.")

fit_linear <- lm(Sepal.Length ~ ., iris)
sd(iris$Sepal.Length - predict(fit_linear, iris))

m1 <- dglm(iris, 
           Sepal.Length ~ ., 
           hidden_layers = c(16, 2),
           hidden_layers_activation = c("linear", "linear"),
           verbose = TRUE, epochs = 1000)

sd(iris$Sepal.Length - dglm_predict(iris, m1))

library(dplyr)

dglm_predict(iris, m1)

m2 <- dglm(iris, 
           Species ~ ., 
           hidden_layers = c(24, 12),
           epochs = 100,
           verbose = FALSE)

dglm_predict(iris, m2)
dglm_predict(iris, m2, type = "one-hot")

m3 <- dglm(iris,
           Sepal.Length ~ .,
           verbose = TRUE, epochs = 1000)
get_weights(m3$model)
fit_linear


#expect_equal(predict(fit_dl1, iris), predict(fit_linear, iris))
