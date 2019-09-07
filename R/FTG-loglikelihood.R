#' FTG Log-likelihood Function
#'
#' This function computes the loglikelihood of the full-tail gamma with the input sample data. The expression used is:
#' \deqn{l(x; \alpha, \sigma, \rho) = -n\left(\log\Gamma(\alpha, \rho) + \log(\sigma) -\alpha \log(\rho) - \frac{\alpha - 1}{n}\sum_{i = 1}^{n}\log\left(1 + \frac{x_{i}}{\sigma}\right) + \frac{\rho}{n} \sum_{i = 1}^{n}\left(1 + \frac{x_{i}}{\sigma}\right)\right)}
#' @param x Sample data.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @return Gives the log-likelihood of the FTG. The length of the result is determined by the length of x.
#' @keywords FTG
#' @export
#' @references del Castillo, Joan & Daoudi, Jalila & Serra, Isabel. (2012). The full-tails gamma distribution applied to model extreme values. ASTIN Bulletin. <doi:10.1017/asb.2017.9>.
#' @examples
#' lFTG(1,1,1,1)
lFTG <- function(x, threshold, scale, shape) {
  a <- shape
  s <- scale
  r <- threshold
  l <- -length(x) * (log(gsl::gamma_inc(a, r)) + log(s) - a * log(r) -
    (a - 1) * mean(log(1 + x / s)) + r * mean(1 + x / s))
  return(-l)
}
