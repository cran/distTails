#' Quantile function
#'
#' This function computes the quantile function of the tailW.
#' \deqn{Q(p,\alpha, \beta, \nu) = \left(\frac{-\log(1 - p)}{\alpha} + \nu^\beta\right)^{1 / \beta}}
#' @param p Probability.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @return Gives the quantiles of the TailW. The length of the result is determined by the length of x.
#' @keywords TailW
#' @export
#' @examples
#' qtailw(0.5, 1, 1, 1)
qtailw <- function(p, threshold, scale, shape) {
  q <- ((-log(1 - p) / scale) + threshold^shape)^(1 / shape)
  return(q)
}
