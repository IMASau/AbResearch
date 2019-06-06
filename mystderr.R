stderr <- function(x) {
 sqrt(var(x[!is.na(x)]) / length(x[!is.na(x)]))
}

my.stderr <- function(x) {
 meany <- mean(x)
 ymin  <- mean(x) - stderr(x)
 ymax  <- mean(x) + stderr(x)
 # assemble the named output
 out <- c(y = meany, ymin = ymin, ymax = ymax)
 return(out)
}
