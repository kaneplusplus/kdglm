context("Tests that need to be cleaned up.")

fit_linear <- lm(Sepal.Length ~ ., iris)
sd(iris$Sepal.Length - predict(fit_linear, iris))

m1 <- dglm(iris, 
           Sepal.Length ~ ., 
           hidden_layers = c(16, 2),
           hidden_layers_activation = c("linear", "linear"),
           verbose = TRUE, epochs = 1000)

# tf$keras$utils$plot_model(m1$model, "test.png", show_shapes = TRUE)

sd(iris$Sepal.Length - dglm_predict(iris, m1))

library(dplyr)

#metric_space_embedding(iris, m1) %>%
#  `colnames<-`(c("x1", "x2")) %>%
#  as_tibble() %>%
#  mutate(species = iris$Species) %>%
#  ggplot(aes(x = x1, y = x2, color = species)) + geom_point()

dglm_predict(iris, m1)

m2 <- dglm(iris, 
           Species ~ ., 
           hidden_layers = c(24, 12),
           epochs = 100,
           verbose = FALSE)

#d <- metric_space_embedding(iris, m2)[,1:3] %>% 
#  `colnames<-`(c("x1", "x2", "x3")) %>%
#  as_tibble() %>%
#  mutate(species = iris$Species) 
#
#library(threejs)
#scatterplot3js(x = d$x1, y = d$x2, z = d$x3, color = as.numeric(d$species))
#ggplot(d, aes(x  = x1, y = x2, color = species)) + geom_point()


dglm_predict(iris, m2)
dglm_predict(iris, m2, type = "one-hot")

m3 <- dglm(iris,
           Sepal.Length ~ .,
           verbose = TRUE, epochs = 1000)
get_weights(m3$model)
fit_linear


#expect_equal(predict(fit_dl1, iris), predict(fit_linear, iris))
