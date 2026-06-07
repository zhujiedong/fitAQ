#' Linear regression on low-light points to estimate AQY and Rd
#'
#' @param data Data frame.
#' @param Qin Name of light intensity column (optional if type given).
#' @param A Name of photosynthesis column (optional if type given).
#' @param n Number of lowest-light points (default 5).
#' @param type Instrument type: "LI-6800", "LI-6400", or "other". Default "LI-6800".
#' @param min_points Minimum points required.
#' @return List with AQY, Rd, intercept, n_used, r_squared.
#' @export
low_light_coefs <- function(data,
                            Qin = NULL,
                            A = NULL,
                            n = 5,
                            type = c("LI-6800", "LI-6400", "other"),
                            min_points = 3) {
  type <- match.arg(type)

  # Set default column names based on type
  if (type == "LI-6800") {
    if (is.null(Qin)) Qin <- "Qin"
    if (is.null(A)) A <- "A"
  } else if (type == "LI-6400") {
    if (is.null(Qin)) Qin <- "PARi"
    if (is.null(A)) A <- "Photo"
  } else { # "other"
    if (is.null(Qin) || is.null(A)) {
      stop("For type = 'other', you must provide both Qin and A column names.")
    }
  }

  # Extract and clean
  Q <- data[[Qin]]
  A_vals <- data[[A]]
  valid <- !is.na(Q) & !is.na(A_vals)
  Q <- Q[valid]
  A_vals <- A_vals[valid]

  if (length(Q) < min_points) {
    warning("Not enough valid points (", length(Q), ") to perform regression. Returning NA.")
    return(list(
      AQY = NA_real_, Rd = NA_real_, intercept = NA_real_,
      n_used = 0, r_squared = NA_real_
    ))
  }

  # Sort by Q
  ord <- order(Q)
  Q_sorted <- Q[ord]
  A_sorted <- A_vals[ord]

  n_use <- min(n, length(Q_sorted))
  if (n_use < min_points) {
    warning("Requested n=", n, " but only ", n_use, " points available. Need at least ", min_points)
    return(list(
      AQY = NA_real_, Rd = NA_real_, intercept = NA_real_,
      n_used = n_use, r_squared = NA_real_
    ))
  }

  low_df <- data.frame(x = Q_sorted[1:n_use], y = A_sorted[1:n_use])
  fit <- lm(y ~ x, data = low_df)
  slope <- coef(fit)[["x"]]
  intercept <- coef(fit)[["(Intercept)"]]
  rd <- -intercept
  r2 <- summary(fit)$r.squared

  list(
    AQY = slope, Rd = rd, intercept = intercept,
    n_used = n_use, r_squared = r2
  )
}
