#' @title Pearson's Chi-squared Test for Count Data
#' @description chisq.test performs chi-squared contingency table tests and goodness-of-fit tests,the faster version
#' @param x1 the first vector,any length
#' @param x2 the second vector,needing same length
#' @return X-squared  \code{n}
#' @examples
#' \dontrun{
#' a=c(12,24)
#' b=c(25,10)
#' chisq2=chisq.test2(a,b)
#' chisq2
#' }
#' @export
chisq.test2<-function(x1,x2){
  x<-cbind(x1,x2)
  sr <- rowSums(x)
  sc <- colSums(x)
  n<-sum(sr)
  E <- outer(sr, sc, "*")/n
  YATES <- min(0.5, abs(x - E))
  STATISTIC <- sum((abs(x - E) - YATES)^2/E)
  return(STATISTIC)
}
