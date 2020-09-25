## ----echo = FALSE, message = FALSE---------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ISLR)
library(knitr)
library(dplyr)
library(tidyr)


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

