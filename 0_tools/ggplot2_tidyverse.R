## ----eval = FALSE--------------------------------------------------------
## install.packages("ggplot2")


## ------------------------------------------------------------------------
library(ggplot2)


## ------------------------------------------------------------------------
anscombe


## ------------------------------------------------------------------------
head(diamonds)
ggplot(data = diamonds)


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price))


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point()


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut))


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut)) +
  geom_smooth(aes(color = cut), method = "lm")


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut)) +
  geom_smooth(aes(color = cut), method = "lm") +
  scale_y_sqrt()
  


## ------------------------------------------------------------------------
dim(mpg)
summary(mpg)
head(mpg)


## ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_point(mapping = aes(displ, hwy))


## ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_point(mapping = aes(displ, hwy, colour = class))

## ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_point(mapping = aes(displ, hwy), colour = "darkgreen", size = 2)

## ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_histogram(mapping = aes(hwy), bins = 30) 
## boxplots will look very different sometimes with different binwidths

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(drv, hwy)) 
## boxplots allow us to see the distribution of a cts rv conditional on a discrete one
## we can also show the actual data at the same time
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(drv, hwy)) +
  geom_jitter(mapping = aes(drv, hwy), alpha = .5)

ggplot(data = mpg) +
  geom_bar(mapping = aes(drv)) 
## shows us the distribution of a categorical variable


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut))


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut)) +
  facet_wrap(. ~ cut)


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(carat, price)) +
  geom_point(aes(color = cut)) +
  facet_grid(color ~ cut)


## ------------------------------------------------------------------------
library(tidyverse)


## ------------------------------------------------------------------------
# load readr
library(readr)

# read a csv
recent_grads <- read_csv(file = "https://raw.githubusercontent.com/fivethirtyeight/data/master/college-majors/recent-grads.csv")

## ---- eval=FALSE---------------------------------------------------------
## a %>% b()


## ---- eval=FALSE---------------------------------------------------------
## b(a)


## ------------------------------------------------------------------------
recent_grads %>% filter(Major == "STATISTICS AND DECISION SCIENCE")


## ------------------------------------------------------------------------
recent_grads %>% filter(Major_category == "Computers & Mathematics")


## ------------------------------------------------------------------------
math_grads <- recent_grads %>% filter(Major_category == "Computers & Mathematics")


## ------------------------------------------------------------------------
math_grads %>% arrange(ShareWomen)


## ------------------------------------------------------------------------
math_grads %>% arrange(desc(ShareWomen))


## ------------------------------------------------------------------------
math_grads %>% select(Major, ShareWomen, Total, Full_time, P75th)


## ------------------------------------------------------------------------
math_grads %>% select(Major, College_jobs:Low_wage_jobs)


## ------------------------------------------------------------------------
math_grads %>% rename(Code_major = Major_code)


## ------------------------------------------------------------------------
math_grads %>% mutate(Full_time_rate = Full_time_year_round/Total)

# we can't see everything
math_grads %>% 
  mutate(Full_time_rate = Full_time_year_round/Total) %>% 
  select(Major, ShareWomen, Full_time_rate)


## ------------------------------------------------------------------------
math_grads %>% summarise(mean_major_size = mean(Total))


## ------------------------------------------------------------------------
math_grads %>% summarise(mean_major_size = mean(Total), num_majors = n())


## ------------------------------------------------------------------------
recent_grads %>%
  group_by(Major_category) %>%
  summarise(mean_major_size = mean(Total, na.rm = TRUE)) %>%
  arrange(desc(mean_major_size))

## ------------------------------------------------------------------------
table1

table2

table3

# spread across two data frames
table4a

table4b


## ------------------------------------------------------------------------
# Compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)

# Visualize cases over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country)) + 
  geom_point(aes(colour = country))


## ------------------------------------------------------------------------
table4a


## ------------------------------------------------------------------------
table4a


## ------------------------------------------------------------------------
table4a %>%
  gather(-country, key = "year", value = "cases")


## ------------------------------------------------------------------------
table4a %>%
  gather(-country, key = "year", value = "cases") %>%
  left_join(table4b %>% gather(-country, key = "year", value = "population"))


## ------------------------------------------------------------------------
table2

table2 %>%
  spread(key = type, value = count)


## ------------------------------------------------------------------------
table3


## ------------------------------------------------------------------------
table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/")

