#' Mode calculation
#'
#' Calculate the mode of a categorical variable
#'
#' @param v Factor
#'
#' @examples
#' getmode (c(112,15,18,15,415,41,5,4,44,51,51,2,65,4,8,5,5,8,45,489))
#'
#' @export

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

