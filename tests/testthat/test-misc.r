context("Tests that need to be cleaned up.")

fit_linear <- lm(Sepal.Length ~ ., iris)
sd(iris$Sepal.Length - predict(fit_linear, iris))

m1 <- dlr(iris, Sepal.Length ~ ., hidden_layers = c(24, 2),
          verbose = FALSE, epochs = 1000)

sd(iris$Sepal.Length - dlr_predict(iris, m1))

library(dplyr)
library(ggplot2)

metric_space_embedding(iris, m1) %>%
  `colnames<-`(c("x1", "x2")) %>%
  as_tibble() %>%
  mutate(species = iris$Species) %>%
  ggplot(aes(x = x1, y = x2, color = species)) + geom_point()

dlr_predict(iris, m1)

m2 <- dlr(iris, 
          Species ~ ., 
          hidden_layers = c(24, 12),
          epochs = 1000,
          verbose = FALSE)

d <- latent_space_embedding(iris, m2) %>% 
  `colnames<-`(c("x1", "x2", "x3")) %>%
  as_tibble() %>%
  mutate(species = iris$Species) 

library(threejs)
scatterplot3js(x = d$x1, y = d$x2, z = d$x3, color = as.numeric(d$species))
ggplot(d, aes(x  = x1, y = x2, color = species)) + geom_point()


dlr_predict(iris, m2)
dlr_predict(iris, m2, type = "one-hot")


expect_equal(predict(fit_dl1, iris), predict(fit_linear, iris))
