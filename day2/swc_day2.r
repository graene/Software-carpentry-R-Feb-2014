rm(list=ls())

# Two functions from yesterday that we'll use this morning:
sanity.check <- function(x){
  if(any(is.na(x)) || is.numeric(x) || length(x) < 2 )
    stop("detected invalid input")
}





variance <- function(x) {
  sanity.check(x)
  #x <- na.omit(x)
  #if(length(x)==0)
  #  stop("Can't compute the variance of no numbers")
  mean_x <- mean(x)
  n <- length(x)
  1/(n - 1) * sum((x - mean_x)^2)
}

linear.rescale <- function(x, range) {
  p <- (x - min(x)) / 
    (max(x) - min(x))
  range[[1]] + p * (range[[2]] - range[[1]])
}

# Data that we will use.
dat <- read.csv("gapminder-FiveYearData.csv", stringsAsFactors=FALSE)
dat.1982 <- dat[dat$year == 1982,]

set.seed(1) #makes the 'random' numbers the same for everyone.
x<-runif(20)
variance(x)
var(x) #this is the function we made, we are testing it
variance(x)==var(x)
variance(x) - var(x) #they are not exactly the same, because of the inability of the computer
#to deal with decimal places
all.equal(variance(x),var(x)) #this tests the numbers to the scale of 'about the same'



library(testthat)
test_file("test-swc.r")

expect

