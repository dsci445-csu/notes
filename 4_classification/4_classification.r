## ----echo = FALSE, message = FALSE-----------------------------------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ISLR)
library(knitr)
library(dplyr)
library(tidyr)
library(tidymodels)
library(discrim)

opts_chunk$set(fig.height = 3, message = FALSE, warning = FALSE)
theme_set(theme_bw())

set.seed(445)


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------
head(Default)

ggplot(Default) +
  geom_point(aes(balance, income, colour = default, shape = default), alpha = 0.5)

Default |>
  select (-student) |>
  gather(feature, value, -default) |>
  ggplot() +
  geom_boxplot(aes(default, value, fill = default)) +
  facet_wrap(.~feature, scales = "free_y") +
  theme(legend.position = "hide")



## ---- echo = FALSE, fig.height = 2.5---------------------------------------------------------------------------------------------
Default2 <- Default
Default2$default_num <- as.numeric(Default2$default) - 1

lm_spec <- linear_reg(engine = "lm")

lm_fit0 <- lm_spec |>
  fit(default_num ~ balance, data = Default2)

lm_fit0 |>
  tidy() |>
  select(term, estimate) |>
  pivot_wider(names_from = "term", values_from = "estimate") |>
  ggplot() +
  geom_point(aes(balance, default_num), alpha = 0.5, data = Default2) +
  geom_abline(aes(intercept = `(Intercept)`, slope = balance), colour = "blue") +
  xlab("Balance") +
  ylab("Probability of Default")


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------
logistic_reg() |>
  fit(default ~ balance, data = Default2) |>
  augment(new_data = Default2) |>
  ggplot() +
  geom_point(aes(balance, default_num), alpha = 0.5) +
  geom_line(aes(balance, .pred_Yes), colour = "blue") +
  xlab("Balance") +
  ylab("Probability of Default")


## --------------------------------------------------------------------------------------------------------------------------------
logistic_spec <- logistic_reg()

logistic_fit <- logistic_spec |>
  fit(default ~ balance, family = "binomial", data = Default)

logistic_fit |>
  pluck("fit") |>
  summary()


## --------------------------------------------------------------------------------------------------------------------------------
logistic_fit2 <- logistic_spec |>
  fit(default ~ ., family = "binomial", data = Default)

logistic_fit2 |>
  pluck("fit") |>
  summary()


## Let $K = 2$ and $\pi_1 = \pi_2$. When does the Bayes classifier assign an observation to class $1$?


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------
x <- seq(-6, 6, length.out = 1000)
ggplot() +
  geom_line(aes(x, dnorm(x, 1.25, 1)), colour = "red") +
  geom_line(aes(x, dnorm(x, -1.25, 1)), colour = "blue") +
  geom_vline(aes(xintercept = 0), lty = 2) +
  xlab("") + ylab("")


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------
df <- data.frame(x = rnorm(20, 1.25, 1), y = "1") |>
  bind_rows(data.frame(x = rnorm(20, -1.25, 1), y = "2"))

df |>
  group_by(y) |>
  summarise(mu = mean(x), pi = n()/nrow(df)) |>
  data.frame() -> ests

ests$sigma2 <- df |>
  left_join(ests) |>
  mutate(summand = (x - mu)^2) |>
  group_by(y) |>
  summarise(summand = sum(summand)) |>
  summarise(sigma2 = sum(summand)/(nrow(df) - nrow(ests))) |> 
  pull(sigma2)


ggplot() +
  geom_histogram(aes(x, group = y, fill = y), data = df, bins = 30, position = "dodge") +
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_vline(aes(xintercept = sum(ests$mu)/2))



## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------
## test data
test_df <- data.frame(x = rnorm(20000, 1.25, 1), y = "1") |>
  bind_rows(data.frame(x = rnorm(20000, -1.25, 1), y = "2"))

test_df |>
  mutate(pred = as.numeric(x <= sum(ests$mu)/2) + 1) |>
  select(y, pred) |>
  table() -> confusion

test_df |>
  mutate(pred = as.numeric(x <= 0) + 1) |>
  select(y, pred) |>
  table() -> confusion_bayes

confusion


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------
library(mvtnorm)


## ---- echo = FALSE, cache = TRUE-------------------------------------------------------------------------------------------------
expand.grid(x1 = seq(-10, 10, length.out = 1000), 
            x2 = seq(-10, 10, length.out = 1000)) |>
  data.frame() %>%
  mutate(indep = dmvnorm(.),
         corr = dmvnorm(., mean = c(0, 0), sigma = matrix(c(1, .5, .5, 1), ncol = 2))) |> 
  gather(density, f, indep, corr) |> 
  ggplot() +
  geom_contour(aes(x1, x2, z = f)) +
  facet_wrap(.~density)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------
lda_spec <- discrim_linear(engine = "MASS")

lda_fit <- lda_spec |>
  fit(default ~ student + balance, data = Default)

lda_fit |>
  pluck("fit")

# training data confusion matrix
lda_fit |>
  augment(new_data = Default) |>
  conf_mat(truth = default, estimate = .pred_class)



## --------------------------------------------------------------------------------------------------------------------------------
lda_fit |>
  augment(new_data = Default) |>
  mutate(pred_lower_cutoff = factor(ifelse(.pred_Yes > 0.2, "Yes", "No"))) |>
  conf_mat(truth = default, estimate = pred_lower_cutoff)


## ---- echo = FALSE, cache = TRUE, fig.height=1.9---------------------------------------------------------------------------------
get_errors <- function(model, y, tau) {
  conf <- table(predict(model)$posterior[, 2] > tau, y)

  data.frame(error_1 = conf[2, 1]/sum(conf[, 1]),
             error_2 = conf[1, 2]/sum(conf[, 2]),
             error_tot = (conf[2, 1] + conf[1, 2])/sum(conf))
}

res <- data.frame()
for(tau in seq(0.0001, 0.5, by = .0001)) {
  res <- rbind(res, data.frame(threshold = tau, get_errors(lda_fit$fit, Default$default, tau)))
}

res |>
  gather(error, value, -threshold) |>
  ggplot() +
  geom_line(aes(threshold, value, colour = error))


## ---- echo = FALSE, cache=TRUE, fig.height = 1.8---------------------------------------------------------------------------------
## make fake data
mu_1 <- c(-1, -1)
mu_2 <- c(1, 1)
sigma_1 <- matrix(c(1, .5, .5, 1), nrow = 2)
sigma_2 <- matrix(c(1, -.5, -.5, 1), nrow = 2)

# lda will be better because this data is generated from the same sigma
lda_better_train <- data.frame(class = "1", rmvnorm(100, mu_1, sigma_1)) |>
  bind_rows(data.frame(class = "2", rmvnorm(100, mu_2, sigma_1))) |>
  mutate(class = factor(class))

# lqa will be better because this data is generated from different sigmas
qda_better_train <- data.frame(class = "1", rmvnorm(100, mu_1, sigma_1)) |>
  bind_rows(data.frame(class = "2", rmvnorm(100, mu_2, sigma_2))) |>
  mutate(class = factor(class))

names(lda_better_train) <- names(qda_better_train) <- c("class", "x1", "x2")

## models -- lda and qga fit
qda_spec <- discrim_quad()

lda_fit_lda_better <- lda_spec |> fit(class ~ x1 + x2, data = lda_better_train)
qda_fit_lda_better <- qda_spec |> fit(class ~ x1 + x2, data = lda_better_train)
lda_fit_qda_better <- lda_spec |> fit(class ~ x1 + x2, data = qda_better_train)
qda_fit_qda_better <- qda_spec |> fit(class ~ x1 + x2, data = qda_better_train)

## function to calculate the Bayes classifier boundary
delta <- function(x, mu, sigma, pi) {
  -0.5 * t(x) %*% solve(sigma) %*% x + t(x) %*% solve(sigma) %*% mu - 0.5 * t(mu) %*% solve(sigma) %*% mu - 0.5 * log(sum(diag(sigma))) + log(pi)
}

## create plot data to separate the space
expand.grid(x1 = seq(-4, 4, length.out = 100), 
            x2 = seq(-4, 4, length.out = 100)) |>
  data.frame() -> plot_dat

## lda better situation data  
lda_plot_dat <- data.frame(plot_dat, 
                           delta_1 = apply(plot_dat, 1, delta, mu = mu_1, sigma = sigma_1, pi = 0.5),
                           delta_2 = apply(plot_dat, 1, delta, mu = mu_2, sigma = sigma_1, pi = 0.5)) |>
  mutate(bayes_classifier = ifelse(delta_1 >= delta_2, "1", "2")) |>
  mutate(lda_classifier = as.character(predict(lda_fit_lda_better, plot_dat)$`.pred_class`)) |>
  mutate(qda_classifier = as.character(predict(qda_fit_lda_better, plot_dat)$`.pred_class`))
  
## qda better situation data
qda_plot_dat <- data.frame(plot_dat, 
                           delta_1 = apply(plot_dat, 1, delta, mu = mu_1, sigma = sigma_1, pi = 0.5),
                           delta_2 = apply(plot_dat, 1, delta, mu = mu_2, sigma = sigma_2, pi = 0.5)) |>
  mutate(bayes_classifier = ifelse(delta_1 >= delta_2, "1", "2")) |>
  mutate(lda_classifier = as.character(predict(lda_fit_qda_better, plot_dat)$`.pred_class`)) |>
  mutate(qda_classifier = as.character(predict(qda_fit_qda_better, plot_dat)$`.pred_class`))

## make plots
lda_plot_dat |>
  dplyr::select(-delta_1, -delta_2) |>
  gather(classifier, class, -x1, -x2) |>
  ggplot() +
  geom_tile(aes(x1, x2, fill = class), alpha = 0.5) +
  facet_wrap(.~classifier) +
  geom_point(aes(x1, x2, colour = class), data = lda_better_train)
  
qda_plot_dat |>
  dplyr::select(-delta_1, -delta_2) |>
  gather(classifier, class, -x1, -x2) |>
  ggplot() +
  geom_tile(aes(x1, x2, fill = class), alpha = 0.5) +
  facet_wrap(.~classifier) +
  geom_point(aes(x1, x2, colour = class), data = qda_better_train)



## ---- echo = FALSE, cache = TRUE, fig.show='hold', out.width="50%", fig.height=4-------------------------------------------------
## make fake data
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
  mutate(class = factor(class))

names(train) <- c("class", "x1", "x2")

## function to calculate the Bayes classifier boundary
bayes_classifier <- function(x) {
  as.character(as.numeric(d_mixture2(x, mu_1, mu_2, sigma_1, 0.2)*0.5 < d_mixture2(x, mu_3, mu_3, sigma_1, 0.7)*0.5) + 1)
}

## create plot data to separate the space
expand.grid(x1 = seq(-6, 6, length.out = 500), 
            x2 = seq(-6, 6, length.out = 500)) |>
  data.frame() -> plot_dat

## knn plots for each k
for(k in c(1, 10, 100)) {
  ## fit knn
  nearest_neighbor(mode = "classification", neighbors = k) |>
    fit(class ~ ., data = train) |>
    augment(new_data = plot_dat) |>
    ggplot() +
    geom_tile(aes(x1, x2, fill = .pred_class), alpha = 0.5) +
    geom_point(aes(x1, x2, colour = class), data = train) +
    ggtitle(paste("KNN, K =", k)) +
    theme(text = element_text(size = 20), legend.position = 'hide') -> p
  
  print(p)
}

plot_dat |>
  mutate(class = apply(plot_dat, 1, bayes_classifier)) |>
  ggplot() +
  geom_tile(aes(x1, x2, fill = class), alpha = 0.5) +
  geom_point(aes(x1, x2, colour = class), data = train) +
  ggtitle("Bayes Classifier") +
  theme(text = element_text(size = 20))

