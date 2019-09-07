#' FTG Quantile function
#'
#' This function computes the quantiles of the full-tail gamma with the input sample data.
#' @param p Probability.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @param interval a vector containing the end-points of the interval to be searched for the minimum.
#' @return Gives the quantiles of the FTG. The length of the result is determined by the length of x.
#' @keywords FTG
#' @export
#' @references del Castillo, Joan & Daoudi, Jalila & Serra, Isabel. (2012). The full-tails gamma distribution applied to model extreme values. ASTIN Bulletin. <doi:10.1017/asb.2017.9>.
#' @examples
#' qFTG(0.5,1,1,1, c(0,10))
qFTG <- function(p, threshold, scale, shape, interval) {
  a <- shape
  s <- scale
  r <- threshold

  equation <- function(x) {
    (pFTG(x, a, s, r) - p)^2
  }

  result <- stats::optimize(f = equation, interval = interval)
  return(result$minimum)
}
#' @return Gives the density of the FTG. The length of the result is determined by the length of x.
