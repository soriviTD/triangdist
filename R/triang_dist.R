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
#'Density, distribution , quantile function and random generation for the
#'triangular distribution with parameters min, max and mode.
#'
#' @param x,q Quantile.
#' @param p Probability.
#' @param n Observations to generate.
#' @param min Minimum value.
#' @param max Maximum value.
#' @param mode Mode value.
#' #'
#' @name triang_dist
NULL

#'Density function.
#Height in a specific point.
#' @rdname triang_dist
#' @return \code{dtriang} Returns density value.
#' @export
dtriang <- function(x, min, max, mode) {
  if (min >= max || mode < min || mode > max) {
    stop("One or more values are wrong.")
  } else {
    if (x == mode) {
      val <- 2 / (max - min)
    } else if (x < min || x > max) {
      val <- 0
    } else if (x < mode) {
      val <- 2 * (x - min) / ((max - min) * (mode - min))
    } else if (x > mode) {
      val <- 2 * (max - x) / ((max - min) * (max - mode))
    }
    val
  }
}

#'Distribution function.
#P(X<q).
#' @rdname triang_dist
#' @return \code{ptriang} Returns cumulative density value.
#' @export
ptriang <- function(q, min, max, mode) {
  if (min >= max || mode < min || mode > max) {
    stop("One or more values are wrong.")
  } else {
    h <- 0
    if (q == mode) {
      h <- 2 / (max - min)
      val <- (mode - min) * h / 2
    } else if (q <= min) {
      val <- 0
    } else if (q >= max) {
      val <- 1
    } else if (q < mode) {
      h <- 2 * (q - min) / ((max - min) * (mode - min))
      val <- 0.5 * h * (q - min)
    } else if (q > mode) {
      h <- 2 * (max - q) / ((max - min) * (max - mode))
      val <- 1 - (max - q) * h * 0.5
    }
    val
  }
}

#'Quantile function.
#You give to it the probability and it tells you which x corresponds to it
#(ptriang^-1).
#' @rdname triang_dist
#' @return \code{qtriang} Returns the quantile value.
#' @export
qtriang <- function(p, min, max, mode) {
  if (min >= max || mode < min || mode > max || p > 1 || p < 0) {
    stop("One or more values are wrong.")
  } else {
    p_mode <- ptriang(mode, min, max, mode)
    if (p == p_mode) {
      val <- mode
    } else if (p < p_mode) {
      val <- sqrt(p * (max - min) * (mode - min)) + min
    } else if (p > p_mode) {
      val <- -sqrt((1 - p) * (max - min) * (max - mode)) + max
    }
    val
  }
}

#' Random generation.
#' @rdname triang_dist
#' @return Vector of length \code{n} with randomly generated values.
#'
#' @importFrom stats runif
#' @export
rtriang <- function(n, min, max, mode) {
  if (any(min >= max || mode < min || mode > max)) {
    stop("One or more values are wrong.")
  } else {
    aux <- runif(n, 0, 1)
    val <- rep(NA, n)
    for (i in seq_along(aux)) {
      val[i] <- qtriang(aux[i], min, max, mode)
    }
    val
  }
}
