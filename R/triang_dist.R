#min: a (value where the triangle starts)
#max: b (value where the triangle finishes)
#mode: c (value in x where is the top of the triangle, it is the most
#common one)

#To calculate the area, meaning, the density, the triangle must be divided in
#two subtriangles (drawing a vertical line from c to the base).
#Height is in: 2/(b-a)
#The area of each triangle is calculated separately.

#' Triangular Distribution
#'
#' Density, distribution, quantile function and random generation for the
#' triangular distribution with parameters min, max and mode.
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param n Number of observations to generate.
#' @param min Minimum value (lower bound a).
#' @param max Maximum value (upper bound b).
#' @param mode Mode value (c).
#'
#' @name triang_dist
NULL

# Validates the (min, max, mode) parameter triple with specific error messages.
.validate_triang_params <- function(min, max, mode) {
  if (min >= max) {
    stop("min must be strictly less than max (got min = ", min,
         ", max = ", max, ").")
  }
  if (mode < min || mode > max) {
    stop("mode must be between min and max (got mode = ", mode,
         ", min = ", min, ", max = ", max, ").")
  }
  invisible(TRUE)
}

#' Density function.
#' @rdname triang_dist
#' @return \code{dtriang} Returns density value(s) for each element of \code{x}.
#' @export
dtriang <- function(x, min, max, mode) {
  .validate_triang_params(min, max, mode)

  val <- numeric(length(x))
  #Upward slope, between min and mode (skipped when mode == min)
  if (mode > min) {
    left <- x >= min & x < mode
    val[left] <- 2 * (x[left] - min) / ((max - min) * (mode - min))
  }
  #Downward slope, between mode and max (skipped when mode == max)
  if (mode < max) {
    right <- x > mode & x <= max
    val[right] <- 2 * (max - x[right]) / ((max - min) * (max - mode))
  }
  #At the mode (top of the triangle)
  val[x == mode] <- 2 / (max - min)
  val
}

#' Distribution function. P(X <= q).
#' @rdname triang_dist
#' @return \code{ptriang} Returns cumulative density value(s) for each element of \code{q}.
#' @export
ptriang <- function(q, min, max, mode) {
  .validate_triang_params(min, max, mode)

  val <- numeric(length(q))
  val[q >= max] <- 1
  #Left of the mode (skipped when mode == min)
  if (mode > min) {
    left <- q > min & q <= mode
    val[left] <- (q[left] - min) ^ 2 / ((max - min) * (mode - min))
  }
  #Right of the mode (skipped when mode == max)
  if (mode < max) {
    right <- q > mode & q < max
    val[right] <- 1 - (max - q[right]) ^ 2 / ((max - min) * (max - mode))
  }
  val
}

#' Quantile function (inverse of ptriang).
#' @rdname triang_dist
#' @return \code{qtriang} Returns the quantile value(s) for each element of \code{p}.
#' @export
qtriang <- function(p, min, max, mode) {
  .validate_triang_params(min, max, mode)
  if (any(p < 0 | p > 1)) {
    stop("p must be between 0 and 1.")
  }

  p_crit <- (mode - min) / (max - min) #cdf at the mode
  val <- numeric(length(p))
  lower <- p <= p_crit
  upper <- p > p_crit
  val[lower] <- min + sqrt(p[lower] * (max - min) * (mode - min))
  val[upper] <- max - sqrt((1 - p[upper]) * (max - min) * (max - mode))
  val
}

#' Random generation.
#' @rdname triang_dist
#' @return Vector of length \code{n} with randomly generated values.
#'
#' @importFrom stats runif
#' @export
rtriang <- function(n, min, max, mode) {
  .validate_triang_params(min, max, mode)
  qtriang(runif(n, 0, 1), min, max, mode)
}
