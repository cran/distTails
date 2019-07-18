#' TailW Log-likelihood function
#'
#' This function computes the log-likelihood of the tailW with the input sample data.
#' \deqn{l(x;\alpha,\beta) = n(\log(\alpha)+\log(\beta))+(\beta-1)\sum_{i=1}^{n}\log(x+\nu)-\alpha\sum_{i=1}^{n}((x+\nu)^\beta-\nu^\beta)}
#' @param x Sample data.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @return Gives the log-likelihood of the TailW. The length of the result is determined by the length of x.
#' @keywords TailW
#' @export
#' @examples
#' ltailw(1,1,1,1)
ltailw <- function(x, threshold, scale, shape) {
  l <- length(x) * log(scale) + length(x) * log(shape) + (shape - 1) * sum(log(x)) - scale * sum(x^shape - threshold^shape)
  return(l)
}
