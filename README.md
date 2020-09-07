
<!-- README.md is generated from README.Rmd. Please edit that file -->

Overview
========

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The {dglm} package provide “deep” versions of (generalized) linear
models with an interface that is designed to follow those of standard
regression routines (`lm()`, `glm()`, and `coxph()`) as closely as
possible while providing the ability to fit non-linear relationships
among data. The package implements these model fitting routines through
two functions that prepend a “d” in front of their linear-model
analogues.

-   `dglm()` regress variables onto count, continuous, or categorical
    variables.
-   `dcoxph()` regress variables onto survival data.

In addition common associated methods including `summary()`, `update()`,
and `predict()` are implemented along with others to facilitate the
validating, examining, characterizing, updating, and predicting with
these models.

Approach
========

Deep learners, regressing tensors of order two (matrics) onto tensors of
order one (vectors), can be thought of as non-linear generalizations of
linear models. For example, consider the linear model:

*y*<sub>*i*</sub> = *x*<sub>*i*1</sub> *β*<sub>1</sub> + *x*<sub>*i*2</sub> *β*<sub>2</sub> + *x*<sub>*i*3</sub> *β*<sub>3</sub>+ ...

We can rewrite this as

*y*<sub>*i*</sub> = *f*<sub>0</sub>( *x*<sub>*i*⋅</sub> )

where *f*<sub>0</sub> is a linear combination of the values in the
vector *x*<sub>*i*⋅</sub> – a deep learner with no hidden layers.
Likewise, a logistic regression can be written as:

log ( *y*<sub>*i*</sub> / (1−*y*<sub>*i*</sub>) ) = *f*<sub>0</sub>( *x*<sub>*i*⋅</sub> )

where *y*<sub>*i*</sub> is a zero-one variable. Any generalized linear
model, including survival models, fits into a similar construction and
we can extend these models to capture non-linear predictive information
with respect to the dependent variable.

Installation
============

The package is not currently available on
[CRAN](https://cran.r-project.org/) but you can install the development
version of this package from [GitHub](https://github.com/) with the
following code

    devtools::install_github("kaneplusplus/dlm")

Examples with the `penguins` data set
=====================================

Regression onto a categorical variable
--------------------------------------

    library(keras)
    library(dlrm)

    # Regress iris variables onto Species (categorical) and get the accuracy.
    fit_dlmcat <- dlr(iris, Species ~ ., hidden_layers = 16, epochs = 100)
    sum(iris$Species == dlr_predict(iris, fit_dlmcat)) / nrow(iris)
    #> [1] 0.6933333

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
    #> [1] 0.3086302

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
    #> sex2                 -0.115   -0.535        -0.926        -0.143
    #> age                  -0.072    0.008        -0.015         0.030
    #> meal.cal             -0.003    0.000        -0.001         0.000
    #> wt.loss              -0.041   -0.012        -0.027         0.003
    #> ph.ecog1             -0.055    0.336        -0.122         0.793
    #> ph.ecog2              0.059    1.002         0.429         1.576
    #> ph.ecog3              0.227    2.086         0.030         4.141

    # Plot the training history.
    plot_training_history(dc_fit)
    #> `geom_smooth()` using formula 'y ~ x'

<img src="man/figures/README-survival-1.png" width="100%" />
