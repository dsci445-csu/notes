## ----echo = FALSE, message = FALSE-----------------------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ISLR)
library(knitr)
library(dplyr)
library(tidyr)
library(splines)


set.seed(445)


## ---- 
head(Wage, 4) %>% knitr::kable(row.names = FALSE)


## ---- 
## piecewise linear model of wage on age with cutpoints at 30 and 60
df <- Wage[, c("age", "wage")] %>%
  mutate(c1 = as.numeric(age >= 30 & age < 60),
         c2 = as.numeric(age >= 60)) %>%
  select(-age)

m0 <- lm(wage ~ ., data = df)
interval <- predict(m0, newdata = df, interval = 'confidence') %>% data.frame()

ggplot() +
  geom_point(aes(age, wage), alpha = .3, data = Wage) +
  geom_ribbon(aes(x = Wage$age, ymin = interval$lwr, ymax = interval$upr), fill = "red", alpha = .2) +
  geom_line(aes(Wage$age, m0$fitted.values), colour = "red", size = 2) +
  theme(text = element_text(size = 30))
  
df2 <- Wage[, c("age", "wage")] %>%
  mutate(c1 = as.numeric(age >= 30 & age < 60),
         c3 = as.numeric(age >= 60),
         high_wage = wage > 250) %>%
  select(-age, -wage)

## piecewise linear model of high wage on age with cutpoints at 30 and 60
m1 <- glm(high_wage ~ ., family = "binomial", data = df2)
interval <- predict(m1, newdata = df2, type = "response", se.fit = TRUE) %>% data.frame()

ggplot() +
  geom_ribbon(aes(x = Wage$age, ymin = interval$fit - 1.96*interval$se.fit, ymax = interval$fit + 1.96*interval$se.fit), fill = "red", alpha = .2) +
  geom_line(aes(Wage$age, m1$fitted.values), colour = "red", size = 2) +
  xlab("age") +
  ylab("Pr(wage > 250 | age)") +
  theme(text = element_text(size = 30))


## ---- 
## Wage subset
Wage_small <- Wage[sample(1:nrow(Wage), 100),]

## cubic spline
spline_cubic <- lm(wage ~ bs(age, knots = c(50), degree = 3), data = Wage_small)

## continuous piecewise
df3 <- Wage_small[, c("age", "wage")] %>%
  mutate(age_low = age*(age < 50),
         age_high = age*(age >= 50)) %>%
  select(-age)
cts_piecewise_cubic <- lm(wage ~ poly(age_low, 3) + poly(age_high, 3), data = df3)

## piecewise cubic
piecewise_cubic1 <- lm(wage ~ poly(age, 3), data = Wage_small %>% filter(age < 50))
piecewise_cubic2 <- lm(wage ~ poly(age, 3), data = Wage_small %>% filter(age >= 50))

## plot data
x1s <- data.frame(age = seq(min(Wage_small$age), 50))
x2s <- data.frame(age = seq(50, max(Wage_small$age)))
y1s <- predict(piecewise_cubic1, newdata = x1s)
y2s <- predict(piecewise_cubic2, newdata = x2s)

## plots
ggplot() +
  geom_point(aes(age, wage), alpha = .3, data = Wage_small) +
  geom_line(aes(x1s$age, y1s), colour = "red", size = 2) +
  geom_line(aes(x2s$age, y2s), colour = "red", size = 2) +
  geom_vline(aes(xintercept = 50), lty = 2) +
  theme(text = element_text(size = 30))

ggplot() +
  geom_point(aes(age, wage), alpha = .3, data = Wage_small) +
  geom_line(aes(Wage_small$age, cts_piecewise_cubic$fitted.values), colour = "red", size = 2) +
  geom_vline(aes(xintercept = 50), lty = 2) +
  theme(text = element_text(size = 30))

ggplot() +
  geom_point(aes(age, wage), alpha = .3, data = Wage_small) +
  geom_line(aes(Wage_small$age, spline_cubic$fitted.values), colour = "red", size = 2) +
  geom_vline(aes(xintercept = 50), lty = 2) +
  theme(text = element_text(size = 30))

## ---- 
## splines vs polynomial regression
spline_fit <- lm(wage ~ ns(age, df = 15), data = Wage)
poly_fit <- lm(wage ~ poly(age, 15), data = Wage)

## plot data
xs <- seq(16, 81)

## plots
ggplot() +
  geom_point(aes(age, wage), alpha = .3, data = Wage) +
  geom_line(aes(xs, predict(spline_fit, data.frame(age = xs))), colour = "red") +
  geom_line(aes(xs, predict(poly_fit, data.frame(age = xs))), colour = "blue")



## ---- 
## generalizaed additive models
library(gam)

# plot function
plot_f <- function(fit) {
  preplot_obj <- gam:::preplot.Gam(fit)
  for(var in names(preplot_obj)) {
    obj <- preplot_obj[[var]]
    if(class(obj$x) %in% c("integer", "numeric")) {
      p <- ggplot() +
        geom_ribbon(aes(obj$x, ymin = obj$y - 1.96*obj$se.y, ymax = obj$y + 1.96*obj$se.y), alpha = 0.2) +
        geom_line(aes(obj$x, obj$y)) +
        xlab(obj$xlab) + ylab(obj$ylab) +
        theme(text = element_text(size = 30))
    } else if(class(obj$x) %in% c("factor", "character")) {
      p <- ggplot() +
        geom_errorbar(aes(obj$x, ymin = obj$y - 1.96*obj$se.y, ymax = obj$y + 1.96*obj$se.y), alpha = 0.8) +
        geom_point(aes(obj$x, obj$y)) +
        xlab(obj$xlab) + ylab(obj$ylab) +
        theme(text = element_text(size = 30), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    print(p)
  }
}

## make plots
gam_fit <- gam(wage ~ ns(year, df = 4) + ns(age, df = 5) + education, data = Wage)
plot_f(gam_fit)

## ---- 
## logistic GAM model
gam_fit_logit <- gam(I(wage > 250) ~ ns(year, df = 4) + ns(age, df = 5) + education, data = Wage, family = "binomial")
plot_f(gam_fit_logit)

