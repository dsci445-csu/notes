## ----echo = FALSE, message = FALSE---------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ISLR)
library(knitr)
library(dplyr)
library(tidyr)

theme_set(theme_bw())

set.seed(445)


## ---- echo = FALSE-------------------------------------------------------------------------------------
## generate training data
n <- 50
x <- runif(n, 0, 100)
f <- function(x) 20 + .0001*(-1*(x - 20) - 2*(x - 70)^2 - (x - 50)^3)
train <- data.frame(x = x, y = f(x) + rnorm(n, 0, 2))

## fit models of varying levels of flexibility
m0 <- lm(y ~ x, data = train)
m1 <- smooth.spline(train$x, train$y, df = 6)
m2 <- smooth.spline(train$x, train$y, df = 25)

## get training MSE
data.frame(model = "Linear Regression", df = 2, pred = m0$fitted.values, true = train$y) %>%
  bind_rows(data.frame(model = "Smoothing Spline", df = 6, pred = predict(m1, x)$y, true = train$y)) %>%
  bind_rows(data.frame(model = "Smoothing Spline", df = 25, pred = predict(m2, x)$y, true = train$y)) %>%
  mutate(SE = (pred - true)^2) %>%
  group_by(model, df) %>%
  summarise(`Train MSE` = mean(SE)) -> mse

## plot predictions vs. training data
ggplot() + 
  geom_point(aes(x, y), data = train) +
  geom_line(aes(x, f(x)), colour = "black") +
  geom_abline(aes(intercept = m0$coefficients[1], slope = m0$coefficients[2]), colour = "blue") +
  geom_line(aes(m1$x, m1$y), colour = "red") +
  geom_line(aes(m2$x, m2$y), colour = "darkgreen")

## generate test data
test <- data.frame(x = runif(1000, 0, 100))
test$y <- f(x) + rnorm(n, 0, 2)

## predictions for test data
m0.pred <- predict(m0, test)
m1.pred <- predict(m1, test$x)
m2.pred <- predict(m2, test$x)

## get test MSE
data.frame(model = "Linear Regression", df = 2, pred = m0.pred, true = test$y) %>%
  bind_rows(data.frame(model = "Smoothing Spline", df = 6, pred = m1.pred$y, true = test$y)) %>%
  bind_rows(data.frame(model = "Smoothing Spline", df = 25, pred = m2.pred$y, true = test$y)) %>%
  mutate(SE = (pred - true)^2) %>%
  group_by(model, df) %>%
  summarise(`Test MSE` = mean(SE)) %>%
  left_join(mse) %>%
  knitr::kable(digits = 4) ## pretty table


## ---- echo = FALSE-------------------------------------------------------------------------------------
ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  geom_smooth(aes(displ, hwy))


## ------------------------------------------------------------------------------------------------------
## get index of training observations
# take 60% of observations as training and 40% for validation
n <- nrow(mpg)
trn <- seq_len(n) %in% sample(seq_len(n), round(0.6*n)) 

## fit models
m0 <- lm(hwy ~ displ, data = mpg[trn, ])
m1 <- lm(hwy ~ displ + I(displ^2), data = mpg[trn, ])
m2 <- lm(hwy ~ displ + I(displ^2) + I(displ^3), data = mpg[trn, ])
m3 <- lm(hwy ~ displ + I(displ^2) + I(displ^3) + I(displ^4), data = mpg[trn, ])

## predict on validation set
pred0 <- predict(m0, mpg[!trn, ])
pred1 <- predict(m1, mpg[!trn, ])
pred2 <- predict(m2, mpg[!trn, ])
pred3 <- predict(m3, mpg[!trn, ])

## estimate test MSE
true_hwy <-  mpg[!trn, ]$hwy # truth vector

data.frame(terms = 2, model = "linear", true = true_hwy, pred = pred0) %>%
  bind_rows(data.frame(terms = 3, model = "quadratic", true = true_hwy, pred = pred1)) %>%
  bind_rows(data.frame(terms = 4, model = "cubic", true = true_hwy, pred = pred2)) %>%
  bind_rows(data.frame(terms = 5, model = "quartic", true = true_hwy, pred = pred3)) %>% ## bind predictions together
  mutate(se = (true - pred)^2) %>% # squared errors
  group_by(terms, model) %>% # group by model
  summarise(test_mse = mean(se)) %>% ## get test mse
  kable() ## pretty table


## ---- echo = FALSE, message = FALSE--------------------------------------------------------------------
res <- data.frame() ## store results

for(i in 1:10) { # repeat 10 times
  trn <- seq_len(n) %in% sample(seq_len(n), round(0.9*n)) 

  ## fit models
  m0 <- lm(hwy ~ displ, data = mpg[trn, ])
  m1 <- lm(hwy ~ displ + I(displ^2), data = mpg[trn, ])
  m2 <- lm(hwy ~ displ + I(displ^2) + I(displ^3), data = mpg[trn, ])
  m3 <- lm(hwy ~ displ + I(displ^2) + I(displ^3) + I(displ^4), data = mpg[trn, ])
  
  ## predict on validation set
  pred0 <- predict(m0, mpg[!trn, ])
  pred1 <- predict(m1, mpg[!trn, ])
  pred2 <- predict(m2, mpg[!trn, ])
  pred3 <- predict(m3, mpg[!trn, ])
  
  ## estimate test MSE
  data.frame(iter = i, terms = 2, model = "linear", true = mpg[!trn, ]$hwy, pred = pred0) %>%
    bind_rows(data.frame(iter = i, terms = 3, model = "quadratic", true = mpg[!trn, ]$hwy, pred = pred1)) %>%
    bind_rows(data.frame(iter = i, terms = 4, model = "cubic", true = mpg[!trn, ]$hwy, pred = pred2)) %>%
    bind_rows(data.frame(iter = i, terms = 5, model = "quartic", true = mpg[!trn, ]$hwy, pred = pred3)) %>% ## bind predictions together
    mutate(se = (true - pred)^2) %>% # squared errors
    group_by(iter, terms, model) %>% # group by model
    summarise(test_mse = mean(se)) -> test_mse
  
  res %>%
    bind_rows(test_mse) -> res
}

res %>%
  mutate(iter = as.character(iter)) %>%
  ggplot() +
  geom_line(aes(terms, test_mse, group = iter, colour = iter)) +
  theme(legend.position = "none")


## ---- message = FALSE----------------------------------------------------------------------------------
## perform LOOCV on the mpg dataset
res <- data.frame() ## store results
for(i in seq_len(n)) { # repeat for each observation
  trn <- seq_len(n) != i # leave one out

  ## fit models
  m0 <- lm(hwy ~ displ, data = mpg[trn, ])
  m1 <- lm(hwy ~ displ + I(displ^2), data = mpg[trn, ])
  m2 <- lm(hwy ~ displ + I(displ^2) + I(displ^3), data = mpg[trn, ])
  m3 <- lm(hwy ~ displ + I(displ^2) + I(displ^3) + I(displ^4), data = mpg[trn, ])
  
  ## predict on validation set
  pred0 <- predict(m0, mpg[!trn, ])
  pred1 <- predict(m1, mpg[!trn, ])
  pred2 <- predict(m2, mpg[!trn, ])
  pred3 <- predict(m3, mpg[!trn, ])
  
  ## estimate test MSE
  true_hwy <- mpg[!trn, ]$hwy # get truth vector
  
  res %>% ## store results for use outside the loop
    bind_rows(data.frame(terms = 2, model = "linear", true = true_hwy, pred = pred0)) %>%
    bind_rows(data.frame(terms = 3, model = "quadratic", true = true_hwy, pred = pred1)) %>%
    bind_rows(data.frame(terms = 4, model = "cubic", true = true_hwy, pred = pred2)) %>%
    bind_rows(data.frame(terms = 5, model = "quartic", true = true_hwy, pred = pred3)) %>% ## bind predictions together
    mutate(mse = (true - pred)^2) -> res
}

res %>%
  group_by(terms, model) %>%
  summarise(LOOCV_test_MSE = mean(mse)) %>%
  kable()


## ------------------------------------------------------------------------------------------------------
## perform k-fold on the mpg dataset
res <- data.frame() ## store results

## get the folds
k <- 10
folds <- sample(seq_len(10), n, replace = TRUE) ## approximately equal sized

for(i in seq_len(k)) { # repeat for each observation
  trn <- folds != i # leave ith fold out

  ## fit models
  m0 <- lm(hwy ~ displ, data = mpg[trn, ])
  m1 <- lm(hwy ~ displ + I(displ^2), data = mpg[trn, ])
  m2 <- lm(hwy ~ displ + I(displ^2) + I(displ^3), data = mpg[trn, ])
  m3 <- lm(hwy ~ displ + I(displ^2) + I(displ^3) + I(displ^4), data = mpg[trn, ])
  
  ## predict on validation set
  pred0 <- predict(m0, mpg[!trn, ])
  pred1 <- predict(m1, mpg[!trn, ])
  pred2 <- predict(m2, mpg[!trn, ])
  pred3 <- predict(m3, mpg[!trn, ])
  
  ## estimate test MSE
  true_hwy <- mpg[!trn, ]$hwy # get truth vector
  
  data.frame(terms = 2, model = "linear", true = true_hwy, pred = pred0) %>%
    bind_rows(data.frame(terms = 3, model = "quadratic", true = true_hwy, pred = pred1)) %>%
    bind_rows(data.frame(terms = 4, model = "cubic", true = true_hwy, pred = pred2)) %>%
    bind_rows(data.frame(terms = 5, model = "quartic", true = true_hwy, pred = pred3)) %>% ## bind predictions together
    mutate(mse = (true - pred)^2) %>%
    group_by(terms, model) %>%
    summarise(mse = mean(mse)) -> test_mse_k
  
  res %>% bind_rows(test_mse_k) -> res
}




res %>%
  group_by(terms, model) %>%
  summarise(kfoldCV_test_MSE = mean(mse)) %>%
  kable()


## ---- echo = FALSE, message = FALSE, cache = TRUE------------------------------------------------------
## repear k-fold on the mpg dataset 10x
res_cv <- data.frame() ## store results
k <- 10

for(j in 1:10) {
  res <- data.frame()
  ## get the folds
  folds <- sample(seq_len(10), n, replace = TRUE) ## approximately equal sized
  
  for(i in seq_len(k)) { # repeat for each observation
    trn <- folds != i # leave ith fold out
  
    ## fit models
    m0 <- lm(hwy ~ displ, data = mpg[trn, ])
    m1 <- lm(hwy ~ displ + I(displ^2), data = mpg[trn, ])
    m2 <- lm(hwy ~ displ + I(displ^2) + I(displ^3), data = mpg[trn, ])
    m3 <- lm(hwy ~ displ + I(displ^2) + I(displ^3) + I(displ^4), data = mpg[trn, ])
    
    ## predict on validation set
    pred0 <- predict(m0, mpg[!trn, ])
    pred1 <- predict(m1, mpg[!trn, ])
    pred2 <- predict(m2, mpg[!trn, ])
    pred3 <- predict(m3, mpg[!trn, ])
    
    ## estimate test MSE
    true_hwy <- mpg[!trn, ]$hwy # get truth vector
    
    data.frame(terms = 2, model = "linear", true = true_hwy, pred = pred0) %>%
      bind_rows(data.frame(terms = 3, model = "quadratic", true = true_hwy, pred = pred1)) %>%
      bind_rows(data.frame(terms = 4, model = "cubic", true = true_hwy, pred = pred2)) %>%
      bind_rows(data.frame(terms = 5, model = "quartic", true = true_hwy, pred = pred3)) %>% ## bind predictions together
      mutate(mse = (true - pred)^2) %>%
      group_by(terms, model) %>%
      summarise(mse = mean(mse)) -> test_mse_k
    
    res %>% bind_rows(test_mse_k) -> res
  }
  
  res %>%
    mutate(iter = j) %>%
    group_by(iter, terms, model) %>%
    summarise(kfoldCV_test_MSE = mean(mse)) %>%
    bind_rows(res_cv) -> res_cv
}

res_cv %>%
  mutate(iter = as.character(iter)) %>%
  ggplot() +
  geom_line(aes(terms, kfoldCV_test_MSE, group = iter, colour = iter)) +
  theme(legend.position = "none")


## ---- echo = FALSE-------------------------------------------------------------------------------------
library(class)
library(mvtnorm)


## ---- echo = FALSE, cache = TRUE, fig.show='hold', out.width="50%", fig.height=5-----------------------
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
train <- data.frame(class = "1", sample_mixture2(100, mu_1, mu_2, sigma_1, 0.2)) %>%
  bind_rows(data.frame(class = "2", sample_mixture2(100, mu_3, mu_4, sigma_1, 0.7)))

names(train) <- c("class", "x1", "x2")

bayes_classifier <- function(x) {
  as.character(as.numeric(d_mixture2(x, mu_1, mu_2, sigma_1, 0.2)*0.5 < d_mixture2(x, mu_3, mu_3, sigma_1, 0.7)*0.5) + 1)
}

## create plot data to separate the space
expand.grid(x1 = seq(-6, 6, length.out = 100), 
            x2 = seq(-6, 6, length.out = 100)) %>%
  data.frame() -> plot_dat

## knn plots for each k
for(k in c(1, 10, 100)) {
  ## fit knn
  knn_fit <- knn(train[, -1], plot_dat, train$class, k = k)
  
  knn_plot_dat <- plot_dat %>%
    mutate(class = as.character(knn_fit))
  
  ## make plots
  knn_plot_dat %>%
    ggplot() +
    geom_tile(aes(x1, x2, fill = class), alpha = 0.5) +
    geom_point(aes(x1, x2, colour = class), data = train) +
    ggtitle(paste("KNN, K =", k)) +
    theme(text = element_text(size = 20), legend.position = 'hide') -> p
  
  print(p)
}

plot_dat %>%
  mutate(class = apply(plot_dat, 1, bayes_classifier)) %>%
  ggplot() +
  geom_tile(aes(x1, x2, fill = class), alpha = 0.5) +
  geom_point(aes(x1, x2, colour = class), data = train) +
  ggtitle("Bayes Classifier") +
  theme(text = element_text(size = 20))


## ------------------------------------------------------------------------------------------------------
k_fold <- 10
cv_label <- sample(seq_len(k_fold), nrow(train), replace = TRUE)
err <- rep(NA, k) # store errors for each flexibility level

for(k in seq(1, 100, by = 2)) {
  err_cv <- rep(NA, k_fold) # store error rates for each fold
  for(ell in seq_len(k_fold)) {
    trn_vec <- cv_label != ell # fit model on these
    tst_vec <- cv_label == ell # estimate error on these
    
    ## fit knn
    knn_fit <- knn(train[trn_vec, -1], train[tst_vec, -1], train[trn_vec, ]$class, k = k)
    ## error rate
    err_cv[ell] <- mean(knn_fit != train[tst_vec, ]$class)
  }
  err[k] <- mean(err_cv)
}
err <- na.omit(err)


## ---- echo = FALSE-------------------------------------------------------------------------------------
ggplot() +
  geom_line(aes(seq(1, 100, by = 2), err)) +
  geom_point(aes(seq(1, 100, by = 2)[which.min(err)], err[which.min(err)]), shape = 3) +
  xlab("KNN K") + ylab("K-Fold CV Error")

