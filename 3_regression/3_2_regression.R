e## ----echo = FALSE, message = FALSE-------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ISLR)
library(knitr)

theme_set(theme_bw())


## ----------------------------------------------------------------------------------------------------
## load the data in
ads <- read_csv("data/Advertising.csv")


## ----------------------------------------------------------------------------------------------------
head(mpg)


## ---- message = FALSE, fig.height=9, cache=TRUE------------------------------------------------------
library(GGally)

mpg %>% 
  select(-model) %>% # too many models
  ggpairs() # plot matrix


## ----------------------------------------------------------------------------------------------------
lm(hwy ~ displ + cty + drv, data = mpg) %>%
  summary()


## ----------------------------------------------------------------------------------------------------
lm(sales ~ TV + radio + TV*radio, data = ads) %>%
  summary()


## ----------------------------------------------------------------------------------------------------
ggplot(data = mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "red") +
  geom_smooth(method = "loess", colour = "blue")


## ----------------------------------------------------------------------------------------------------
lm(hwy ~ displ + I(displ^2), data = mpg) %>%
  summary()


## ---- fig.show="hold", out.width = "33%", fig.height = 7---------------------------------------------
library(caret) # package for knn
set.seed(445) #reproducibility

x <- rnorm(100, 4, 1) # pick some x values
y <- 0.5 + x + 2*x^2 + rnorm(100, 0, 2) # true relationship
df <- data.frame(x = x, y = y) # data frame of training data

for (k in seq(2, 10, by = 2)) {
  knn_model <- knnreg(y ~ x, data = df, k = k) # fit knn model
  
  ggplot(df) +
    geom_point(aes(x, y)) +
    geom_line(aes(x, predict(knn_model, df)), colour = "red") +
    ggtitle(paste("KNN, k = ", k)) +
    theme(text = element_text(size = 30)) -> p
  
  print(p) # knn plots
}

ggplot(df) +
    geom_point(aes(x, y)) +
    geom_line(aes(x, lm(y ~ x, df)$fitted.values), colour = "red") +
    ggtitle("Simple Linear Regression") +
    theme(text = element_text(size = 30)) # slr plot


