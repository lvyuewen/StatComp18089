#' @title A chi-square test using R
#' @description A chi-square test using R
#' @param x1 vector
#' @param x2 vector
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
