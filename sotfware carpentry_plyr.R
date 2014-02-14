library(plyr)
rm(list=ls())
dat <- read.csv("gapminder-FiveYearData.csv", stringsAsFactors=FALSE)

dat.asia <- dat[dat$continent=="Asia",]

get.n.countries <- function(x) {
  length(unique(x$country))
}

dat.asia <- dat[dat$continent=="Asia",]
get.n.countries(dat.asia)

dat.eu <- dat[dat$continent=="Europe",]
get.n.countries(dat.eu)

dat.africa<- dat[dat$continent=="Africa",]
get.n.countries(dat.africa)

dat.africa<- dat[dat$continent=="Africa",]
get.n.countries(dat.africa)

ddply(dat,.(continent,year),get.n.countries)
ddply(dat,c("continent","year"),get.n.countries)
ddply(dat,~continent*year,get.n.countries)

get.total.pop <- function(x) {
  sum(x$pop)
}

ddply(dat,.(continent,year),get.total.pop)
ddply(dat,.(continent,year),function(x) max(x$pop))

get.countries <- function(x) unique(x$country)
  

mylist <- dlply(dat,.(continent),get.countries)

mylist[["Europe"]]



model <- function(x){
  fit <- lm(lifeExp ~ log10(gdpPercap), data=x)
  c(n= length(x$lifeExp),r2=summary(fit)$r.squared, a= coef(fit)[[1]],b=coef(fit)[[2]])
}

model(dat)

out <- ddply(dat,.(continent,year),model)



summarise(dat,pop.mean=mean(pop),pop.var=var(pop))

ddply(dat,.(continent,year),summarise,
      n=length(pop),
      pop.mean=mean(pop),
      pop.var=var(pop),
      min=min(pop),
      max=max(pop))











