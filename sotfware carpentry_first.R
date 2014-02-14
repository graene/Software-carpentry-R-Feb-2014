x <- TRUE
x <- 1
x <- c(1,TRUE)
x <- c(1, TRUE, "1")


x <- list(1, TRUE, "1") # lists don't have to be all the same type of data, but vectors do

typeof(c(TRUE,FALSE))
class(c(TRUE,FALSE))
storage.mode(c(TRUE,FALSE))

str(x)

NA
NA==1
NA==NA
is.na(NA)

x <- c(1,2,3,NA,4,5)
x
x==3
which(x==3)
x == 3 & !is.na(x)

FALSE & NA
FALSE | NA
Inf# infinity
-Inf
Inf+1

m <- matrix(1,4,5)
m
m[5] <- "hello"
m
m[1,2]
m == "hello"

m[m=="hello"]

m
m[1,] #returns vector by default (because only 1 row)
m[1:2,] # returns matrix
m[1,,drop=FALSE] # force it to return matrix

m <- matrix(1:25,5,5)
m[c(TRUE,FALSE)]



d <- data.frame(apple=1:10,aardvark=letters[1:10],c=runif(10))
d[2] # returns second column of data frame
d[[2]] # returns content of second column of data frame
d[["apple"]]
d$apple


# when read.csv use strings.as.factors=F to avoid things being coerced as factors


# functions

f <- function(x){
  x
}

x <- 10
y <- 20
f(y)

double <- function(x){
  x <- x*2
  x
}
double(pi)

setwd("~/Desktop/SWC")
file.exists("gapminder-FiveYearData.csv.txt")

dat <- read.csv("gapminder-FiveYearData.csv.txt",stringsAsFactors=F)
head(dat)
dat.1982 <- dat[dat$year==1982,]

average <- function(x) {
  av <- sum(x)/length(x)
  av
}

variance <- function(x){
  av <- sum(x)/length(x)
  xx <- sum((x-av)^2)
  vv <- xx*(1/(length(x)-1))
  zz
}

skew <- function(x,noisy=F) {
if(noisy){
  message("I'm computing the skew for you!")
}
  
  nn <- 1/(length(x)-2)
  top <- sum((x-mean(x))^3)
  bottom <- (var(x))^(3/2)
  sk <- nn*top/bottom
  sk
}  

# PLOTTING

plot(lifeExp ~ gdpPercap, dat.1982, log="x")

# scaling
range <- c(0.2,10)
x <- sqrt(dat.1982$pop)
p <- (x - min(x)) /
  (max(x) - min(x))
cex <- r[1] + p*(r[2]-r[1])

plot(lifeExp ~ gdpPercap, dat.1982, log="x",cex=cex)

linear.rescale <- function(x,range) {
  x <- sqrt(x)
  p <- (x - min(x)) /
    (max(x) - min(x))
  cex <- range[1] + p*(range[2]-range[1])
}


cex <- linear.rescale(dat.1982$pop,range=c(0.2,10))

plot(lifeExp ~ gdpPercap, dat.1982, log="x",cex=cex)

col.table <- c(Asia="tomato",
               Europe="chocolate4",
               Africa="dodgerblue",
               Americas="darkgoldenrod1",
               Oceania="green4")

xx <- function(things,table) {
col <- unname(table[things])
}

plot(lifeExp ~ gdpPercap, dat.1982, log="x",cex=cex,col= xx(dat.1982$continent,col.table))





table <- c("tomato","chocolate4","dodgerblue","darkgoldenrod1","green4")


colour.by.category <- function(things,table) {
  cc <- data.frame(Things=unique(things),Col=unique(table))
  col <- unname(things[cc])

}




### restart

rm(list=ls())

dat <- read.csv("gapminder-FiveYearData.csv", stringsAsFactors=FALSE)
dat.1982 <- dat[dat$year == 1982,]

linear.rescale <- function(x, range) {
  p <- (x - min(x)) / 
    (max(x) - min(x))
  range[[1]] + p * (range[[2]] - range[[1]])
}

col.table <- c(Asia="tomato",
               Europe="chocolate4",
               Africa="dodgerblue2",
               Americas="darkgoldenrod1",
               Oceania="green4")

colour.by.category <- function(things, table) {
  unname(table[things])
}

col <- colour.by.category(dat.1982$continent, col.table)
cex <- linear.rescale(sqrt(dat.1982$pop), range=c(0.2, 10))
plot(lifeExp ~ gdpPercap, dat.1982, log="x", cex=cex, col=col)



add.trend.line <- function(x, y, d, col) {
  fit <- lm(d[[y]] ~ log10(d[[x]]))
  abline(fit, col=col)
}

for (continent in unique(dat.1982$continent)) {
  add.trend.line("gdpPercap", "lifeExp",
                 d=dat.1982[dat.1982$continent==continent,],
                 col=col.table[[continent]]) 
}



add.trend.line("gdpPercap", "lifeExp", dat.1982)
add.trend.line("gdpPercap", "lifeExp", dat.1982[dat.1982$continent=="Asia",])
add.trend.line("gdpPercap", "lifeExp", dat.1982[dat.1982$continent=="Africa",])



my.plot <- function(year, data, cols) {
  dat.year <- data[data$year == year,]
  col <- colour.by.category(dat.year$continent, cols)
  cex <- linear.rescale(sqrt(dat.year$pop), range=c(0.2, 10))
  plot(lifeExp ~ gdpPercap, dat.year, log="x", cex=cex, col=col)
  for (continent in unique(dat.year$continent)) {
    add.trend.line("gdpPercap", "lifeExp",
                   d=dat.year[dat.year$continent == continent,],
                   col=cols[[continent]])
  }}

par(mfrow=(c(4,3)))
for (i in unique(dat$year)) {
  my.plot(year=i,data=dat,cols=col.table)
  }


par(mfrow=(c(1,1)))




