#' FTG Density Function
#'
#' This function computes the density of the full-tail gamma with the input sample data. The expression for the density used is:
#' \deqn{g(x; \alpha, \theta, \rho) = \frac{\rho^{\alpha}}{\sigma}\left(\rho + \frac{x}{\sigma}\right)^{\alpha - 1}\exp\left(-\left(\rho + \frac{x}{\sigma}\right)\right)/\Gamma(\alpha, \rho).}
#' @param x Sample data.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @return Gives the density of the FTG. The length of the result is determined by the length of x.
#' @keywords FTG
#' @export
#' @references del Castillo, Joan & Daoudi, Jalila & Serra, Isabel. (2012). The full-tails gamma distribution applied to model extreme values. ASTIN Bulletin. <doi:10.1017/asb.2017.9>.
#' @examples
#' a <- 0.3
#' t <- 0.3
#' r <- 0.8
#' n <- 1000
#' sample <- rFTG(n, a, t, r)
#' x <- seq(min(sample), max(sample), length.out = 200)
#' d <- dFTG(x, a, t, r)
#' hist(sample, breaks = "FD", probability = TRUE)
#' lines(x, d, col = "red")
dFTG <- function(x, threshold, scale, shape) {
  a <- shape
  s <- scale
  r <- threshold

  pdf <- ((r^a) / s) * (1 + x / s)^(a - 1) * exp(-r * (1 + x / s)) / gsl::gamma_inc(a, r)
  return(pdf)
}
