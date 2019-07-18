#' FTG Random Sample Generation
#'
#' This function computes n random variates from full-tail gamma with a rejection method.
#' @param n Sample size.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @return Gives random deviates of the FTG. The length of the result is determined by n.
#' @keywords FTG
#' @export
#' @references del Castillo, Joan & Daoudi, Jalila & Serra, Isabel. (2012). The full-tails gamma distribution applied to model extreme values. ASTIN Bulletin. <doi:10.1017/asb.2017.9>.
#' @examples
#' x <- rFTG(100, 1, 1, 1)
#' hist(x, breaks = "FD")
rFTG <- function(n, threshold, scale, shape) {
  sample <- c()
  m <- 0
  while (m < n) {
    x <- stats::rexp(1, rate = threshold)
    u <- stats::runif(1)
    if (u <= (1 + x)^(shape - 1)) {
      sample[m + 1] <- x
    }
    m <- length(sample)
  }
  sample * threshold / scale
}
