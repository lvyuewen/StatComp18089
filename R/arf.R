#' @title  Beta(a,b) distribution by the acceptance-rejection method
#' @description the required times of getting n points which satidfy Beta(a,b) distribution by the acceptance-rejection method
#' @param n the whole number of Beta distribution
#' @param a the first parameter of Beta distribution
#' @param b the second parameter of Beta distribution
#' @return times  \code{n}
#' @examples
#' \dontrun{
#' n=1000
#' a=3
#' b=2
#' arf(1000,3,2)
#' }
#' @export
arf<-function(n,a,b){
  j<-k<-0;
  y <- numeric(n)
  while (k < n){
    u <- runif(1)
    j <- j + 1
    x <- runif(1) #random variate from g
    if (x^(a-1)* (1-x)^(b-1) > u){
       #we accept x
      k <- k + 1
      y[k] <- x
     }
   }
  j
}
