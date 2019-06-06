errorUpper2 <- function(x){
 if(mean(x) < 0){
  x.mean <- mean(x) 
  x.sd <- sd(x) 
  SEM <- x.sd / (sqrt(length(x))) 
  return(x.mean - SEM)} else {
   x.mean <- mean(x) 
   x.sd <- sd(x) 
   SEM <- x.sd / (sqrt(length(x))) 
   return((x.mean + SEM))}
}
