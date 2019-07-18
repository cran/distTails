#' TailW Maximum Likelihood Estimation
#'
#' This function scales the input data w.r.t. the threshold and performs MLE with a tailW.
#' @param sample Sample data.
#' @param dist Name of the distribution to fit.
#' @return Gives a list of the estimated parameters fo the function fitted. For the TailW it returns, scale and shape. Fot the FTG it returns the parameters scale, shape, and threshold.
#' @keywords Tail fitting
#' @export
#' @examples
#' scale <- 2
#' shape <- 1
#' threshold <- 1
#' s <- rtailw(1000, threshold = threshold , scale = scale, shape = shape)
#' fits <- fittail(s, dist = "TailW")
#' x_seq <- seq(threshold, max(s), length.out = 500)
#' theo_density <- dtailw(x_seq, threshold = threshold, scale = fits$scale, shape = fits$shape)
#' hist(s, probability = TRUE, breaks = "FD")
#' lines(x = x_seq, y = theo_density, col = "red")
fittail <- function(sample, dist = "TailW") {
  if (dist == "TailW") {
    tailweib <- sample / min(sample)
    # Compute initial value for shape
    aux <- function(p) {
      -ltailw(tailweib, threshold = 1, scale = 1, shape = p)
    }
    solfit <- stats::nlm(aux, 1)
    shape0 <- solfit$estimate
    # Compute initial value for scale
    aux <- function(p) {
      -ltailw(tailweib, threshold = 1, scale = p, shape = 1)
    }
    solfit <- stats::nlm(aux, 1)
    scale0 <- solfit$estimate
    # Compute values for shape and scale with the initial values
    aux <- function(p) {
      -ltailw(tailweib, threshold = 1, scale = p[1], shape = p[2])
    }
    solfit <- stats::nlm(aux, c(scale0, shape0))
    fits <- list(scale = solfit$estimate[1], shape = solfit$estimate[2])
    return(fits)
  }

  if (dist == "FTG") {

    eFTG <- function(x, par)
    {
      # Minus the loglikelihood function.
      ftg_loglikelihood <- function(parameters)
      {
        a <- parameters[1]
        s <- parameters[2]
        r <- parameters[3]

        logl <- lFTG(sample, r, s, a)
        return(logl)
      }
      base::tryCatch(
        {
          result <- stats::optim(par = c(par[1], par[2], par[3]), f = ftg_loglikelihood)
          returning <- list(a = result$par[1], s = result$par[2], r = result$par[3])
        },
        error = function(e) {returning <<- list(a = NA, s = NA, r = NA)})

      return (returning)
    }

    # Function used to initialize the rho.
    r_initialize <- function(par) {
      a <- par[1]
      s <- par[2]

      d_r <- function(a, r) {
        (-r^(a - 1) * base::exp(-r)) / gsl::gamma_inc(a, r)
      }

      equation <- function(r) {
        (d_r(a, r) - a / r + 1 + 1 / s)^2
      }

      result <- stats::optimize(f = equation, interval = c(1e-20, 100))
      return(result$minimum)
    }

    # Fit a GPD to the data to initialize the parameters.
    fit.gpd <- ercv::fitpot(sample, threshold = 0)

    # Obtain the standarized data.
    m <- mean(sample)
    y <- sample / m

    if (fit.gpd[1] > 0 && 1 / fit.gpd[1] < 100) {
      # Before fitting the FTG we initialize rho.
      a <- -1 / fit.gpd[1]
      s <- (fit.gpd[2] / fit.gpd[1]) / m
      r <- r_initialize(c(a, s))
      fit.FTG <- eFTG(x = y, par = c(a, s, r))
    }
    else {
      # We better fit a Gamma distribution to the data to initialize the parameters.
      fit.gamma <- MASS::fitdistr(y, "gamma")
      fit.FTG <- eFTG(x = y, par = c(fit.gamma$estimate[1], 1, fit.gamma$estimate[2]))
    }

    result <- list(shape = fit.FTG$a, scale = fit.FTG$s * m, threshold = fit.FTG$r)
    return(result)
  }
}
