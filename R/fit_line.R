#' Linear regression on low-light points to estimate AQY and Rd
#'
#' Sorts data by light intensity (Qin), takes the first `n` points,
#' fits A ~ Qin, and returns the slope (AQY) and dark respiration (Rd = -intercept).
#'
#' @param data Data frame containing the columns.
#' @param Qin Name or index of light intensity column.
#' @param A Name or index of net photosynthesis column.
#' @param n Number of lowest-light points to use (default 5).
#' @param min_points Minimum number of points required; if fewer, returns NA.
#'
#' @return A list with elements:
#'   \item{AQY}{Apparent quantum yield (slope of regression).}
#'   \item{Rd}{Dark respiration (positive value, = -intercept).}
#'   \item{intercept}{Raw intercept.}
#'   \item{n_used}{Actual number of points used.}
#'   \item{r_squared}{R-squared of the fit.}
#' @export
#'
#' @examples
#' \dontrun{
#' res <- low_light_coefs(df, "Qin", "A", n = 5)
#' print(res$AQY)
#' }
low_light_coefs <- function(data, Qin, A, n = 5, min_points = 3) {
  # Extract and clean
  Q <- data[[Qin]]
  A_vals <- data[[A]]
  valid <- !is.na(Q) & !is.na(A_vals)
  Q <- Q[valid]
  A_vals <- A_vals[valid]

  if (length(Q) < min_points) {
    warning("Not enough valid points (", length(Q), ") to perform regression. Returning NA.")
    return(list(AQY = NA_real_, Rd = NA_real_, intercept = NA_real_,
                n_used = 0, r_squared = NA_real_))
  }

  # Sort by Q
  ord <- order(Q)
  Q_sorted <- Q[ord]
  A_sorted <- A_vals[ord]

  # Use at most n points, but ensure at least min_points
  n_use <- min(n, length(Q_sorted))
  if (n_use < min_points) {
    warning("Requested n=", n, " but only ", n_use, " points available. Need at least ", min_points)
    return(list(AQY = NA_real_, Rd = NA_real_, intercept = NA_real_,
                n_used = n_use, r_squared = NA_real_))
  }

  low_df <- data.frame(x = Q_sorted[1:n_use], y = A_sorted[1:n_use])
  fit <- lm(y ~ x, data = low_df)
  slope <- coef(fit)[["x"]]
  intercept <- coef(fit)[["(Intercept)"]]
  rd <- -intercept  # dark respiration as positive

  # Compute R-squared
  r2 <- summary(fit)$r.squared

  list(AQY = slope,
       Rd = rd,
       intercept = intercept,
       n_used = n_use,
       r_squared = r2)
}
