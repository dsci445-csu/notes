## ----echo = FALSE, message = FALSE-------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ISLR)
library(knitr)

opts_chunk$set(fig.height = 3, message = FALSE, warning = FALSE)
theme_set(theme_bw())


## ----------------------------------------------------------------------------------------------------
## load the data in
ads <- read_csv("../data/Advertising.csv")

## fit the model
model <- lm(sales ~ TV, data = ads)

summary(model)


## ---- echo=FALSE-------------------------------------------------------------------------------------
ggplot(aes(TV, sales), data = ads) +
  geom_point() +
  geom_smooth(method = "lm")


## ----------------------------------------------------------------------------------------------------
summary(model)


## ----------------------------------------------------------------------------------------------------
# model_2 <- lm(sales ~ TV + radio + newspaper, data = ads)
model_2 <- lm(sales ~ ., data = ads[, -1])

summary(model_2)


## ----------------------------------------------------------------------------------------------------
# model with TV, radio, and newspaper
summary(model_2)

# model without newspaper
summary(lm(sales ~ TV + radio, data = ads))


## ----------------------------------------------------------------------------------------------------
ggplot() +
  geom_point(aes(model_2$fitted.values, model_2$residuals))

