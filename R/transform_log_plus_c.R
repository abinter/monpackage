#'  Transformation log(x+c) with offset
#'
#' Log-tranform a variable x with a offset to prevent errors with zero values. Offset chosen to
#' minimize distribution skewness.
#'
#' @param x a vector of continuous variables
#'
#' @return log(x+best.c), vector with log-transformed variables
#'
#'
#' @import e1071
#'
#' @examples
#' transform.log.plus.c(c(15,7,5,1,8,0,85,0))
#'
#' @export



transform_log_plus_c <- function(x) {
  best.c <- optimise(function(c, x) (skewness(log(x + c)))^2, c(0.001, 20), x = x)$minimum
  print(best.c)
  return(log(x+best.c))
}


