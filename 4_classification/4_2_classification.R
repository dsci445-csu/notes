## ----echo = FALSE, message = FALSE-------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ISLR)
library(knitr)
library(dplyr)
library(tidyr)

theme_set(theme_bw())

set.seed(445)


## Let $K = 2$ and $\pi_1 = \pi_2$. When does the Bayes classifier assign an observation to class $1$?


## ---- echo = FALSE-----------------------------------------------------------------------------------
x <- seq(-6, 6, length.out = 1000)
ggplot() +
  geom_line(aes(x, dnorm(x, 1.25, 1)), colour = "red") +
  geom_line(aes(x, dnorm(x, -1.25, 1)), colour = "blue") +
  geom_vline(aes(xintercept = 0), lty = 2) +
  xlab("") + ylab("")


## ---- echo = FALSE-----------------------------------------------------------------------------------
df <- data.frame(x = rnorm(20, 1.25, 1), y = "1") %>%
  bind_rows(data.frame(x = rnorm(20, -1.25, 1), y = "2"))

df %>%
  group_by(y) %>%
  summarise(mu = mean(x), pi = n()/nrow(df)) %>%
  data.frame() -> ests

ests$sigma2 <- df %>%
  left_join(ests) %>%
  mutate(summand = (x - mu)^2) %>%
  group_by(y) %>%
  summarise(summand = sum(summand)) %>%
  summarise(sigma2 = sum(summand)/(nrow(df) - nrow(ests))) %>% .$sigma2


ggplot() +
  geom_histogram(aes(x, group = y, fill = y), data = df, bins = 30, position = "dodge") +
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_vline(aes(xintercept = sum(ests$mu)/2))



## ---- echo = FALSE-----------------------------------------------------------------------------------
## test data
test_df <- data.frame(x = rnorm(20000, 1.25, 1), y = "1") %>%
  bind_rows(data.frame(x = rnorm(20000, -1.25, 1), y = "2"))

test_df %>%
  mutate(pred = as.numeric(x <= sum(ests$mu)/2) + 1) %>%
  select(y, pred) %>%
  table() -> confusion

test_df %>%
  mutate(pred = as.numeric(x <= 0) + 1) %>%
  select(y, pred) %>%
  table() -> confusion_bayes

confusion

