library(tidyverse)
library(scales)
library(ISLR)
library(knitr)
library(dplyr)
library(tidyr)
library(tidymodels)

set.seed(445)

## validation deprecated ----
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

## validation not deprecated ----
mpg_val <- initial_split(mpg, prop = 0.6)
mpg_train <- training(mpg_val)
mpg_test <- testing(mpg_val)

## models
lm_spec <- linear_reg()

linear_recipe <- recipe(hwy ~ displ, data = mpg)
quad_recipe <- linear_recipe |> step_mutate(displ2 = displ^2)
cubic_recipe <- quad_recipe |> step_mutate(displ3 = displ^3)
quart_recipe <- cubic_recipe |> step_mutate(displ4 = displ^4)

m0 <- workflow() |> add_model(lm_spec) |> add_recipe(linear_recipe) |> fit(mpg_train)
m1 <- workflow() |> add_model(lm_spec) |> add_recipe(quad_recipe) |> fit(mpg_train)
m2 <- workflow() |> add_model(lm_spec) |> add_recipe(cubic_recipe) |> fit(mpg_train)
m3 <- workflow() |> add_model(lm_spec) |> add_recipe(quart_recipe) |> fit(mpg_train)

## estimate test MSE
get_rmse <- function(model) {
  augment(model, new_data = mpg_test) |>
    mutate(resid2 = .resid^2) |>
    summarise(rmse = sqrt(mean(resid2))) |>
    pull(rmse)
}

data.frame(model = c("linear", "quadratic", "cubic", "quartic"),
           rmse = c(get_rmse(m0), get_rmse(m1), get_rmse(m2), get_rmse(m3))) |>
  kable()

## LOOCV ----
lm_spec <- linear_reg()

linear_recipe <- recipe(hwy ~ displ, data = mpg)
quad_recipe <- linear_recipe |> step_mutate(displ2 = displ^2)
cubic_recipe <- quad_recipe |> step_mutate(displ3 = displ^3)
quart_recipe <- cubic_recipe |> step_mutate(displ4 = displ^4)

mpg_loocv <- loo_cv(mpg)

get_mse_i <- function(model) {
  augment(model, new_data = mpg_test) |>
    mutate(resid2 = .resid^2) |>
    pull(resid2)
}

## gotta do this manually now?
mse <- data.frame()
for(i in seq_len(nrow(mpg_loocv))) {
  mpg_train <- training(mpg_loocv[i,]$splits[[1]])
  mpg_test <- testing(mpg_loocv[i,]$splits[[1]])
  
  ## models
  m0 <- workflow() |> add_model(lm_spec) |> add_recipe(linear_recipe) |> fit(mpg_train)
  m1 <- workflow() |> add_model(lm_spec) |> add_recipe(quad_recipe) |> fit(mpg_train)
  m2 <- workflow() |> add_model(lm_spec) |> add_recipe(cubic_recipe) |> fit(mpg_train)
  m3 <- workflow() |> add_model(lm_spec) |> add_recipe(quart_recipe) |> fit(mpg_train)
  
  ## estimate test MSE
  data.frame(model = c("linear", "quadratic", "cubic", "quartic"),
             mse_i = c(get_mse_i(m0), get_mse_i(m1), get_mse_i(m2), get_mse_i(m3)),
             i = i) |>
    rbind(mse) -> mse
}

## take mean over n estimates
mse |>
  group_by(model) |>
  summarise(rmse = sqrt(mean(mse_i))) |>
  kable()
