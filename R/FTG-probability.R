#' FTG Probability Function
#'
#' This function computes the probability of the full-tail gamma with the input sample data. The expression for the probability used is:
#' \deqn{G(x; \alpha, \theta, \rho) = 1 - \Gamma(\alpha, \rho + \theta x)/\Gamma(\alpha, \rho).}
#' @param x Sample data.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @return Gives the distribution function of the FTG. The length of the result is determined by the length of x.
#' @keywords FTG
#' @export
#' @references del Castillo, Joan & Daoudi, Jalila & Serra, Isabel. (2012). The full-tails gamma distribution applied to model extreme values. ASTIN Bulletin. <doi:10.1017/asb.2017.9>.
#' @examples
#' pFTG(1,1,1,1)
pFTG <- function(x, threshold, scale, shape) {
  a <- shape
  s <- scale
  r <- threshold
  cdf <- 1 - gsl::gamma_inc(a, r + r / s * x) / gsl::gamma_inc(a, r)
  return(cdf)
}
