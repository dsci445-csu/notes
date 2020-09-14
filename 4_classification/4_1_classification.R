## ----echo = FALSE, message = FALSE-------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ISLR)
library(knitr)
library(dplyr)
library(tidyr)

theme_set(theme_bw())


## ---- echo = FALSE-----------------------------------------------------------------------------------
head(Default)

ggplot(Default) +
  geom_point(aes(balance, income, colour = default, shape = default), alpha = 0.5)

Default %>%
  select (-student) %>%
  gather(feature, value, -default) %>%
  ggplot() +
  geom_boxplot(aes(default, value, fill = default)) +
  facet_wrap(.~feature, scales = "free_y") +
  theme(legend.position = "hide")



## ---- echo = FALSE, fig.height = 2.5-----------------------------------------------------------------
Default2 <- Default
Default2$default_num <- as.numeric(Default2$default) - 1

m0 <- lm(default_num ~ balance, data = Default2)

ggplot(Default2) +
  geom_point(aes(balance, default_num), alpha = 0.5) +
  geom_abline(aes(intercept = m0$coefficients[1], slope = m0$coefficients[2]), colour = "blue") +
  xlab("Balance") +
  ylab("Probability of Default")


## ---- echo = FALSE-----------------------------------------------------------------------------------
m1 <- glm(default ~ balance, family = "binomial", data = Default)

ggplot(Default2) +
  geom_point(aes(balance, default_num), alpha = 0.5) +
  geom_line(aes(balance, m1$fitted.values), colour = "blue") +
  xlab("Balance") +
  ylab("Probability of Default")


## ----------------------------------------------------------------------------------------------------
m1 <- glm(default ~ balance, family = "binomial", data = Default)

summary(m1)


## ----------------------------------------------------------------------------------------------------
m2 <- glm(default ~ ., family = "binomial", data = Default)
summary(m2)

