context("Continuous Supervised Regression Works")

fit_linear <- lm(Sepal.Length ~ . - 1, iris)
flp <- predict(fit_linear, iris)

dlr(iris, Sepal.Length ~ . - 1)


expect_equal(predict(fit_dl1, iris), predict(fit_linear, iris))
