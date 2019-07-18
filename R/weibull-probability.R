#' TailW Probability Function
#'
#' This function computes the cumulative density function of the tailW with the input sample data.
#' \deqn{F(x,\alpha,\beta,\nu)=1-\exp(-\alpha(x+\nu)^\beta+\alpha\nu^\beta).}
#' @param x Sample data.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @return Gives the distribution function of the TailW. The length of the result is determined by the length of x.
#' @keywords TailW
#' @export
#' @examples
#' # Using the probability function to show the fitting.
#'samp <- rtailw(1000, 1, 2, 3)
#'emp_cdf <- ecdf(samp)(samp)
#'pars <- fittail(samp, dist = "TailW")
#'x_seq <- seq(min(samp), max(samp), length.out = 250)
#'p <- ptailw(x_seq, threshold = 1, scale = pars$scale, shape = pars$shape)
#'plot(samp, 1-emp_cdf, log = "y")
#'lines(x_seq, 1-p, col = "red")
ptailw <- function(x, threshold, scale, shape) {
  p <- 1 - exp(-scale * x^shape + scale * threshold^shape)
  return(p)
}
