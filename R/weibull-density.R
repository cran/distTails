#' TailW Density function
#'
#' This function computes the density of the tailW with the input sample data. The expression for the density used is:
#' \deqn{f(x, \alpha, \beta, \nu) = \alpha \beta (x + \nu)^{\beta -1} \exp(-\alpha(x + \nu)^{\beta} + \alpha \nu^{\beta})}
#' @param x Sample data.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @return Gives the density of the TailW. The length of the result is determined by the length of x.
#' @keywords TailW
#' @export
#' @examples
#' # Generate random deviates from a weibull tail and plot the theoretical density.
#' scale <- 2
#' shape <- 1
#' threshold <- 1
#' x_seq <- seq(threshold, 5, length.out = 500)
#' theo_density <- dtailw(x_seq, threshold = threshold, scale = scale, shape = shape)
#' sample <- rtailw(500, threshold = threshold, scale = scale, shape = shape)
#' hist(sample, probability = TRUE)
#' lines(x = x_seq, y = theo_density, col = "red")
dtailw <- function(x, threshold, scale, shape) {
  d <- scale * shape * x^(shape - 1) * exp(-scale * x^shape + scale * threshold^shape)
  return(d)
}
