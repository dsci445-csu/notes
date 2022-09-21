## ----echo = FALSE, message = FALSE------------------------------------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ISLR)
library(knitr)
library(dplyr)
library(tidyr)
library(tidymodels)

opts_chunk$set(fig.height = 3, message = FALSE, warning = FALSE)
theme_set(theme_bw())

set.seed(445)


## ---- echo = FALSE----------------------------------------------------------------------------------------------------------------
## generate training data
n <- 50
x <- runif(n, 0, 100)
f <- function(x) 20 + .0001*(-1*(x - 20) - 2*(x - 70)^2 - (x - 50)^3)
train <- data.frame(x = x, y = f(x) + rnorm(n, 0, 2))

## fit models of varying levels of flexibility
lm_spec <- linear_reg()
m0 <- lm_spec |> fit(y ~ x, data = train)
m1 <- workflow() |> add_recipe(recipe(y ~ x, data = train) |> step_ns(x, deg_free = 6)) |> add_model(lm_spec) |> fit(data = train)
m2 <- workflow() |> add_recipe(recipe(y ~ x, data = train) |> step_ns(x, deg_free = 25)) |> add_model(lm_spec) |> fit(data = train)

## get training MSE
m0 |> augment(new_data = train) |> mutate(model = "Linear Regression", df = 2) |>
  bind_rows(m1 |> augment(new_data = train) |> mutate(model = "Smoothing Spline", df = 6)) |>
  bind_rows(m2 |> augment(new_data = train) |> mutate(model = "Smoothing Spline", df = 25)) |>
  mutate(SE = (.pred - y)^2) |>
  group_by(model, df) |>
  summarise(`Train MSE` = mean(SE)) -> mse

## plot predictions vs. training data
ggplot() + 
  geom_point(aes(x, y), data = train) +
  geom_line(aes(x, f(x)), colour = "black") +
  geom_line(aes(x, .pred), colour = "blue", data = m0 |> augment(new_data = data.frame(x = seq(0, 100, length.out = 200)))) +
  geom_line(aes(x, .pred), colour = "red", data = m1 |> augment(new_data = data.frame(x = seq(0, 100, length.out = 200)))) +
  geom_line(aes(x, .pred), colour = "darkgreen", data = m2 |> augment(new_data = data.frame(x = seq(0, 100, length.out = 200))))

## generate test data
test <- data.frame(x = runif(1000, 0, 100))
test$y <- f(x) + rnorm(n, 0, 2)

## predictions for test data
m0.pred <- m0 |> augment(new_data = test)
m1.pred <- m1 |> augment(new_data = test)
m2.pred <- m2 |> augment(new_data = test)

## get test MSE
m0.pred |> mutate(model = "Linear Regression", df = 2) |>
  bind_rows(m1.pred |> mutate(model = "Smoothing Spline", df = 6)) |>
  bind_rows(m2.pred |> mutate(model = "Smoothing Spline", df = 25)) |>
  mutate(SE = (.pred - y)^2) %>%
  group_by(model, df) %>%
  summarise(`Test MSE` = mean(SE)) %>%
  left_join(mse) %>%
  knitr::kable(digits = 4) ## pretty table


## ---- echo = FALSE----------------------------------------------------------------------------------------------------------------
ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  geom_smooth(aes(displ, hwy))


## ---------------------------------------------------------------------------------------------------------------------------------
## get index of training observations
# take 60% of observations as training and 40% for validation
mpg_val <- validation_split(mpg, prop = 0.6)

## models
lm_spec <- linear_reg()

linear_recipe <- recipe(hwy ~ displ, data = mpg)
quad_recipe <- linear_recipe |> step_mutate(displ2 = displ^2)
cubic_recipe <- quad_recipe |> step_mutate(displ3 = displ^3)
quart_recipe <- cubic_recipe |> step_mutate(displ4 = displ^4)

m0 <- workflow() |> add_model(lm_spec) |> add_recipe(linear_recipe) |> fit_resamples(resamples = mpg_val)
m1 <- workflow() |> add_model(lm_spec) |> add_recipe(quad_recipe) |> fit_resamples(resamples = mpg_val)
m2 <- workflow() |> add_model(lm_spec) |> add_recipe(cubic_recipe) |> fit_resamples(resamples = mpg_val)
m3 <- workflow() |> add_model(lm_spec) |> add_recipe(quart_recipe) |> fit_resamples(resamples = mpg_val)

## estimate test MSE
collect_metrics(m0) |> mutate(model = "linear") |>
  bind_rows(collect_metrics(m1) |> mutate(model = "quadratic")) |>
  bind_rows(collect_metrics(m2) |> mutate(model = "cubic")) |>
  bind_rows(collect_metrics(m3) |> mutate(model = "quartic")) |> 
  select(model, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  select(-rsq) |>
  kable()


## ---- echo = FALSE, message = FALSE-----------------------------------------------------------------------------------------------
res <- data.frame() ## store results

for(i in 1:10) { # repeat 10 times
  mpg_val <- validation_split(mpg, prop = 0.6)

  m0 <- workflow() |> add_model(lm_spec) |> add_recipe(linear_recipe) |> fit_resamples(resamples = mpg_val)
  m1 <- workflow() |> add_model(lm_spec) |> add_recipe(quad_recipe) |> fit_resamples(resamples = mpg_val)
  m2 <- workflow() |> add_model(lm_spec) |> add_recipe(cubic_recipe) |> fit_resamples(resamples = mpg_val)
  m3 <- workflow() |> add_model(lm_spec) |> add_recipe(quart_recipe) |> fit_resamples(resamples = mpg_val)

## estimate test MSE
collect_metrics(m0) |> mutate(model = "linear", terms = 2) |>
  bind_rows(collect_metrics(m1) |> mutate(model = "quadratic", terms = 3)) |>
  bind_rows(collect_metrics(m2) |> mutate(model = "cubic", terms = 4)) |>
  bind_rows(collect_metrics(m3) |> mutate(model = "quartic", terms = 5)) |> 
  select(model, terms, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  select(-rsq) |>
  mutate(iter = i) |>
  bind_rows(res) -> res
}

res %>%
  mutate(iter = as.character(iter)) %>%
  ggplot() +
  geom_line(aes(terms, rmse, group = iter, colour = iter)) +
  theme(legend.position = "none")


## ---- message = FALSE, cache = TRUE-----------------------------------------------------------------------------------------------
## perform LOOCV on the mpg dataset
mpg_loocv <- vfold_cv(mpg, v = nrow(mpg))

## models
m0 <- workflow() |> add_model(lm_spec) |> add_recipe(linear_recipe) |> fit_resamples(resamples = mpg_loocv)
m1 <- workflow() |> add_model(lm_spec) |> add_recipe(quad_recipe) |> fit_resamples(resamples = mpg_loocv)
m2 <- workflow() |> add_model(lm_spec) |> add_recipe(cubic_recipe) |> fit_resamples(resamples = mpg_loocv)
m3 <- workflow() |> add_model(lm_spec) |> add_recipe(quart_recipe) |> fit_resamples(resamples = mpg_loocv)

## estimate test MSE
collect_metrics(m0) |> mutate(model = "linear") |>
  bind_rows(collect_metrics(m1) |> mutate(model = "quadratic")) |>
  bind_rows(collect_metrics(m2) |> mutate(model = "cubic")) |>
  bind_rows(collect_metrics(m3) |> mutate(model = "quartic")) |> 
  select(model, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  select(-rsq) |>
  kable()


## ---- cache = TRUE----------------------------------------------------------------------------------------------------------------
## perform k-fold on the mpg dataset
mpg_10foldcv <- vfold_cv(mpg, v = 10)

## models
m0 <- workflow() |> add_model(lm_spec) |> add_recipe(linear_recipe) |> fit_resamples(resamples = mpg_10foldcv)
m1 <- workflow() |> add_model(lm_spec) |> add_recipe(quad_recipe) |> fit_resamples(resamples = mpg_10foldcv)
m2 <- workflow() |> add_model(lm_spec) |> add_recipe(cubic_recipe) |> fit_resamples(resamples = mpg_10foldcv)
m3 <- workflow() |> add_model(lm_spec) |> add_recipe(quart_recipe) |> fit_resamples(resamples = mpg_10foldcv)

## estimate test MSE
collect_metrics(m0) |> mutate(model = "linear") |>
  bind_rows(collect_metrics(m1) |> mutate(model = "quadratic")) |>
  bind_rows(collect_metrics(m2) |> mutate(model = "cubic")) |>
  bind_rows(collect_metrics(m3) |> mutate(model = "quartic")) |> 
  select(model, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  select(-rsq) |>
  kable()


## ---- echo = FALSE, message = FALSE, cache = TRUE---------------------------------------------------------------------------------
## repear k-fold on the mpg dataset 10x
res_cv <- data.frame() ## store results

for(i in 1:10) { # repeat 10 times
  ## perform k-fold on the mpg dataset
  mpg_10foldcv <- vfold_cv(mpg, v = 10)
  
  ## models
  m0 <- workflow() |> add_model(lm_spec) |> add_recipe(linear_recipe) |> fit_resamples(resamples = mpg_10foldcv)
  m1 <- workflow() |> add_model(lm_spec) |> add_recipe(quad_recipe) |> fit_resamples(resamples = mpg_10foldcv)
  m2 <- workflow() |> add_model(lm_spec) |> add_recipe(cubic_recipe) |> fit_resamples(resamples = mpg_10foldcv)
  m3 <- workflow() |> add_model(lm_spec) |> add_recipe(quart_recipe) |> fit_resamples(resamples = mpg_10foldcv)

## estimate test MSE
collect_metrics(m0) |> mutate(model = "linear", terms = 2) |>
  bind_rows(collect_metrics(m1) |> mutate(model = "quadratic", terms = 3)) |>
  bind_rows(collect_metrics(m2) |> mutate(model = "cubic", terms = 4)) |>
  bind_rows(collect_metrics(m3) |> mutate(model = "quartic", terms = 5)) |> 
  select(model, terms, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  select(-rsq) |>
  mutate(iter = i) |>
  bind_rows(res_cv) -> res_cv
}

res_cv %>%
  mutate(iter = as.character(iter)) %>%
  ggplot() +
  geom_line(aes(terms, rmse, group = iter, colour = iter)) +
  theme(legend.position = "none")


## ---- echo = FALSE----------------------------------------------------------------------------------------------------------------
library(mvtnorm)


## ---- echo = FALSE, cache = TRUE, fig.height=5------------------------------------------------------------------------------------
mu_1 <- c(-1, 1)
mu_2 <- c(2, -1)
mu_3 <- c(-3, .1)
mu_4 <- c(1, -1)
sigma_1 <- matrix(c(1, .5, .5, 1), nrow = 2)
sigma_2 <- matrix(c(1, -.5, -.5, 1), nrow = 2)

sample_mixture2 <- function(n, mu_1, mu_2, sigma, p) {
  z <- rbinom(n, 1, p)
  z*rmvnorm(n, mu_1, sigma) + (1 - z)*rmvnorm(n, mu_2, sigma) 
}

d_mixture2 <- function(x, mu_1, mu_2, sigma, p) {
  p*dmvnorm(x, mu_1, sigma) + (1 - p)*dmvnorm(x, mu_2, sigma) 
}

# training data from the mixture
train <- data.frame(class = "1", sample_mixture2(100, mu_1, mu_2, sigma_1, 0.2)) |>
  bind_rows(data.frame(class = "2", sample_mixture2(100, mu_3, mu_4, sigma_1, 0.7))) |>
  mutate(class = as.factor(class))

names(train) <- c("class", "x1", "x2")

bayes_classifier <- function(x) {
  as.character(as.numeric(d_mixture2(x, mu_1, mu_2, sigma_1, 0.2)*0.5 < d_mixture2(x, mu_3, mu_3, sigma_1, 0.7)*0.5) + 1)
}

## create plot data to separate the space
expand.grid(x1 = seq(-6, 6, length.out = 100), 
            x2 = seq(-6, 6, length.out = 100),
            class = "1") |>
  data.frame() -> plot_dat

all_dat <- bind_rows(train |> mutate(type = "train"), plot_dat |> mutate(type = "plot"))

grid_small <- tibble(neighbors = c(1, 10, 100))
train_rs <- validation_time_split(all_dat, prop = nrow(train)/nrow(all_dat))

## knn plots for each k
knn_spec <- nearest_neighbor(mode = "classification", neighbors = tune("neighbors"))
knn_control <- control_resamples(save_pred = TRUE)
knn_spec |>
  tune_grid(class ~ x1 + x2, resamples = train_rs, grid = grid_small, control = knn_control) -> knn_fit

knn_fit |> augment(parameters = data.frame(neighbors = 1)) |> filter(type == "plot") |> select(x1, x2, .pred_class) |> mutate(model = "KNN, K = 1") |>
  bind_rows(knn_fit |> augment(parameters = data.frame(neighbors = 10)) |> filter(type == "plot") |> select(x1, x2, .pred_class) |> mutate(model = "KNN, K = 10")) |>
  bind_rows(knn_fit |> augment(parameters = data.frame(neighbors = 100)) |> filter(type == "plot") |> select(x1, x2, .pred_class) |> mutate(model = "KNN, K = 100")) |>
  bind_rows(plot_dat[, -3] |> mutate(.pred_class = apply(plot_dat[, -3], 1, bayes_classifier), model = "Bayes Classifier")) |>
  ggplot() +
  geom_tile(aes(x1, x2, fill = .pred_class), alpha = 0.5) +
  geom_point(aes(x1, x2, colour = class), data = train) +
  facet_wrap(~model) +
  theme(text = element_text(size = 20))


## ---------------------------------------------------------------------------------------------------------------------------------
k_fold <- 10
train_cv <- vfold_cv(train, v = k_fold)

grid_large <- tibble(neighbors = seq(1, 100, by = 2))

knn_spec <- nearest_neighbor(mode = "classification", neighbors = tune("neighbors"))
knn_spec |>
  tune_grid(class ~ x1 + x2, resamples = train_cv, grid = grid_large) |>
  collect_metrics() |>
  filter(.metric == "accuracy") |>
  mutate(error = 1 - mean) -> knn_err


## ---- echo = FALSE----------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_line(aes(neighbors, error), knn_err) +
  geom_point(aes(neighbors, error), data = knn_err |> filter(error == min(error)), shape = 3) +
  xlab("KNN K") + ylab("K-Fold CV Error")

