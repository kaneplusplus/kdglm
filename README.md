
<!-- README.md is generated from README.Rmd. Please edit that file -->

dlrm
====

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of dlm is to …

Installation
------------

You can install the development version of this package from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("kaneplusplus/dlm")

Regression onto a categorical variable
--------------------------------------

    library(keras)
    library(dlrm)

    # Regress iris variables onto Species (categorical) and get the accuracy.
    fit_dlmcat <- dlr(iris, Species ~ ., hidden_layers = 16, epochs = 2000)
    sum(iris$Species == dlr_predict(iris, fit_dlmcat)) / nrow(iris)
    #> [1] 0.94

Regression onto a continous variable
------------------------------------

    # Regress iris variables onto Sepal.Length using a linear model and get the 
    # in-sample prediction accuracy.
    fit_linear <- lm(Sepal.Length ~ ., iris)
    sd(predict(fit_linear, iris))
    #> [1] 0.7711747

    # Perform the same operation with a deep learner with two hidden layers
    # with 24 and 2 nodes respectively.
    fit_dlmc <- dlr(iris, Sepal.Length ~., hidden_layers = c(24, 2))
    sd(iris$Sepal.Length - dlr_predict(iris, fit_dlmc))
    #> [1] 0.3141947

A deep survival analysis
------------------------

    library(survival)
    library(dplyr)
    #> 
    #> Attaching package: 'dplyr'
    #> The following objects are masked from 'package:stats':
    #> 
    #>     filter, lag
    #> The following objects are masked from 'package:base':
    #> 
    #>     intersect, setdiff, setequal, union

    data(lung)

    # Handle the categorical variables in lung
    lung <- lung %>%
      mutate(status = status - 1,
             sex = as.factor(sex),
             ph.ecog = as.factor(ph.ecog))

    # Fit the deep survival model.
    dc_fit <- dlcp(
      lung,
      Surv(time, status) ~ sex + age + meal.cal + wt.loss + ph.ecog,
      epochs = 150,
      validation_split = .2)

    dl_weights <- get_weights(dc_fit$model)[[1]]

    # Compare the deep survival model coefficients to coxph.
    cfit <- summary(coxph(Surv(time, status) ~ sex + age + meal.cal + wt.loss + ph.ecog, lung))$conf.int
    comp <- round(cbind(dl_weights, log(cfit)[,-2]), 3)
    colnames(comp) <- c("Deep Learner Coefs", "Cox Coef", "Cox Lower .95", "Cox Upper .95")
    comp
    #>          Deep Learner Coefs Cox Coef Cox Lower .95 Cox Upper .95
    #> sex2                 -0.383   -0.535        -0.926        -0.143
    #> age                  -0.130    0.008        -0.015         0.030
    #> meal.cal             -0.002    0.000        -0.001         0.000
    #> wt.loss              -0.021   -0.012        -0.027         0.003
    #> ph.ecog1             -0.214    0.336        -0.122         0.793
    #> ph.ecog2              0.225    1.002         0.429         1.576
    #> ph.ecog3              0.232    2.086         0.030         4.141

    # Plot the training history.
    plot_training_history(dc_fit)
    #> `geom_smooth()` using formula 'y ~ x'

<img src="man/figures/README-survival-1.png" width="100%" />
