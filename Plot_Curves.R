library(tidyverse)


scurve <- function(x){
  y <- exp(x) / (1 + exp(x))
  return(y)
}

pcurve <- function(x){
 y <-  exp(x) / (1 + exp(x))
 return(y)
}

p <- ggplot(data = data.frame(x = c(0, 1)), aes(x))



p + stat_function(fun = scurve, n = 100) 

p + stat_function(fun = scurve, n = 100) 



curve(200 * x/(0.25 + x))

micmen <- function(x, a, b) {
  (a * x)/(b + x)
}


curve(micmen(x, 200, 0.15^0.5))


logist <- function(x, a, b) {
  exp(a + b*x)/( 1 + exp(a + b*x))
}

curve(logist(x, 2, 5))


holl3 <- function(x, a, b) {
  (a*x^2)/(b^2 + x^2)
}

curve(holl3(x, 100, 0.2))

holl4 <- function(x, a, b, c) {
  (a*x^2)/(b + c*x^2 + x^2)
}

curve(holl4(x, 100, 0.04, 0.05))

q <- 0.5
curve(q*x^.75)




g  <- tibble(x = 1:10,
             y = rnorm(10, x^(0.2), 0.05))


library(ggplot2)

mod <- lm(log(y) ~ log(x), data = g)

pwr <- function(x) 
 exp(predict(mod, newdata = data.frame(x=x)))

ggplot(g,
       aes(x = x, y = y)) +
 geom_point() +
 stat_function(fun = pwr)



ggplot(data.frame(x=c(0,5000)), aes(x)) +
 stat_function(fun=function(x)x^2, geom="line", aes(colour="square")) +
 stat_function(fun=pwr, geom="line", aes(colour="exp")) +
 scale_colour_manual("Function", value=c("blue","red"), breaks=c("square","exp"))



x <- 1:500
lbda <- 0.75

pwr <- function(x) x^lbda
dat <- data.frame(x, y = pwr(x))

p <- ggplot(dat, aes(x = x, y = y)) + 
 geom_point()

p + stat_function(fun = pwr)
