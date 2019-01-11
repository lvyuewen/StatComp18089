#' @title Cross Tabulation and Table Creation
#' @description table2 uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels.
#' @param x vector
#' @param y vector
#' @return a contingency table \code{n}
#' @examples
#' \dontrun{
#' a=c(0,1,2,1,1,0,0,1)
#' b=c(0,0,1,1,1,0,1,1)
#' table2(a,b)
#' }
#' @export
table2 <- function(x, y) {
  x_val <- unique(x)
  y_val <- unique(y)
  mat <- matrix(0L, length(x_val), length(y_val))
  for (i in seq_along(x)) {
    mat[which(x_val == x[[i]]), which(y_val == y[[i]])]<-mat[which(x_val == x[[i]]),which(y_val == y[[i]])] + 1L
  }
  dimnames <- list(x_val, y_val)
  names(dimnames) <- as.character(as.list(match.call())[-1])
  tab <- array(mat, dim = dim(mat), dimnames = dimnames)
  class(tab) <- "table"
  tab
}
