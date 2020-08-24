## ------------------------------------------------------------------------
# simple math
5*(10 - 4) + 44

# integer division
7 %/% 2

# modulo operator (Remainder)
7 %% 2

# powers
1.5^3


## ------------------------------------------------------------------------
# exponentiation
exp(1)

# logarithms
log(100)
log(100, base = 10)

# trigonometric functions
sin(pi/2)
cos(pi)
asin(1)


## ------------------------------------------------------------------------
# create some variables
x <- 5
class <- 400
hello <- "world"


## ------------------------------------------------------------------------
# functions of variables
log(x)
class^2


## ------------------------------------------------------------------------
# store a vector
y <- c(1, 2, 6, 10, 17)


## ------------------------------------------------------------------------
# elementwise function
y/2

## ------------------------------------------------------------------------
# sequences
a <- 1:5
a
b <- seq(1, 5, by = 1)
b


## ------------------------------------------------------------------------
a[3]


## ------------------------------------------------------------------------
# indexing multiple items
a[c(1, 3, 5)]
a[1:3]


## ------------------------------------------------------------------------
a[-3]


## ------------------------------------------------------------------------
# indexing by vectors of logicals
a[c(TRUE, TRUE, FALSE, FALSE, FALSE)]

# indexing by calculated logicals
a < 3
a[a < 3]

## ------------------------------------------------------------------------
c(TRUE, TRUE, FALSE) | c(FALSE, TRUE, FALSE)
c(TRUE, TRUE, FALSE) & c(FALSE, TRUE, FALSE)


## ------------------------------------------------------------------------
head(a, 2)
tail(a, 2)


## ------------------------------------------------------------------------
a[1] <- 0
a[c(4, 5)] <- 100
a

## ------------------------------------------------------------------------
a
a[1] <- ":-("
a


## ------------------------------------------------------------------------
as.character(b)


## ------------------------------------------------------------------------
n <- length(b)
n
sum_b <- sum(b)
sum_b


## ------------------------------------------------------------------------
mean_b <- sum_b/n
sd_b <- sqrt(sum((b - mean_b)^2)/(n - 1))


## ------------------------------------------------------------------------
mean(b)
sd(b)
summary(b)
quantile(b, c(.25, .75))


## ------------------------------------------------------------------------
# look at top 6 rows
head(iris)

# structure of the object
str(iris)


## ------------------------------------------------------------------------
iris[1, ]
iris[, 1]
iris[1, 1]


## ------------------------------------------------------------------------
iris$Species
iris[, "Species"]


## ------------------------------------------------------------------------
# make a copy of iris
my_iris <- iris

# add a column
my_iris$sepal_len_square <- my_iris$Sepal.Length^2  
head(my_iris)


## ------------------------------------------------------------------------
my_iris[my_iris$sepal_len_square < 20, ]


## ------------------------------------------------------------------------
df <- data.frame(NUMS = 1:5, 
                 lets = letters[1:5],
                 cols = c("green", "gold", "gold", "gold", "green"))


## ------------------------------------------------------------------------
names(df)
names(df)[1] <- "nums"

df

## ------------------------------------------------------------------------
# we store a function in a named value
# function is itself a function to create functions!
# we specify the inputs that we can use inside the function
# we can specify default values, but it is not necessary
name <- function(input = FALSE) {
  # body code goes here
  
  # return output vaues
  return(input)
}


## ------------------------------------------------------------------------
my_mean <- function(x) {
  sum(x)/length(x)
}


## ------------------------------------------------------------------------
my_mean(1:15)
my_mean(c(1:15, NA))


## ----eval=FALSE----------------------------------------------------------
## if(condition) {
##   # Some code that runs if condition is TRUE
## } else {
##   # Some code that runs if condition is TRUE
## }


## ----eval=FALSE----------------------------------------------------------
## if(na.rm) x <- na.omit(x) # na.omit is a function that removes NA values


## ----eval=FALSE----------------------------------------------------------
## for(i in index values) {
##   # block of code
##   # can print values also
##   # code in here will most likely depend on i
## }


## ------------------------------------------------------------------------
for(i in 1:3) {
  print(i)
}


## ------------------------------------------------------------------------
for(species in unique(iris$Species)) {
  subset_iris <- iris[iris$Species == species,]
  avg <- mean(subset_iris$Sepal.Length)
  print(paste(species, avg))
}


## ------------------------------------------------------------------------
condition <- TRUE
while(condition) {
  # do stuff
  # don't forget to eventually set the condition to false
  # in the toy example below I check if the current seconds is divisible by 5
  time <- Sys.time()
  if(as.numeric(format(time, format = "%S")) %% 5 == 0) condition <- FALSE
}
print(time)


## ------------------------------------------------------------------------
# we can also use while loops to iterate
i <- 1
while (i <= 5) {
    print(i)
    i <- i + 1
}


