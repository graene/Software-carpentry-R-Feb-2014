
model <- function(x){
  fit <- lm(lifeExp ~ log10(gdpPercap), data=x)
  c(n= length(x$lifeExp),r2=summary(fit)$r.squared, a= coef(fit)[[1]],b=coef(fit)[[2]])
}

f <- function(x){
  x
}

double <- function(x){
  x <- x*2
  x
}

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
  
  
  linear.rescale <- function(x, range) {
    p <- (x - min(x)) / 
      (max(x) - min(x))
    range[[1]] + p * (range[[2]] - range[[1]])
  }
  
  
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
  
  xx <- function(things,table) {
    col <- unname(table[things])
  }
  
  get.total.pop <- function(x) {
    sum(x$pop)
  }
  
  add.trend.line <- function(x, y, d, col) {
    fit <- lm(d[[y]] ~ log10(d[[x]]))
    abline(fit, col=col)
  }
  
  colour.by.category <- function(things, table) {
    unname(table[things])
  }
  
  
  get.n.countries <- function(x) {
    length(unique(x$country))
  }
  
  
  