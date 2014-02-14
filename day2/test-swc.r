library(testthat)

set.seed(1) #makes the 'random' numbers the same for everyone.
x<-runif(20)
variance(x)
var(x) #this is the function we made, we are testing it
variance(x)==var(x)
variance(x) - var(x) #they are not exactly the same, because of the inability of the computer
#to deal with decimal places
all.equal(variance(x),var(x)) #this tests the numbers to the scale of 'about the same'
expect_that(variance(x),equals(var(x)))

expect_that(variance(x+1), equals(variance(x))) # first argument is something u create, second argument is a condition function
expect_that(variance(x+100), equals(variance(x)))

# POSSIBLE PROBLEMS:
# missing values
# not numerical e.g., characters dataframe
# pretend numbers
# no numeric
# length1
# true/false

# CONDITION FUNCTIONS:
# throws_error()
# is_a()
# is_true()



# missing values
set.seed(1)
x.with.nas <- c(runif(10),NA)
expect_that(variance(x.with.nas),equals(variance(na.omit(x.with.nas))))


# check that the variance function won't work with a length-0 input
expect_that(variance(numeric(0)),throws_error())





