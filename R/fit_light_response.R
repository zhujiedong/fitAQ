#' Fit light response curves using DEoptim
#'
#' @param data A data frame.
#' @param Qin Name or index of PPFD column. If `NULL` and `type` is specified,
#'   a default will be used.
#' @param A Name or index of net photosynthesis column. If `NULL` and `type` is specified,
#'   a default will be used.
#' @param model Model type.
#' @param sat_frac Fraction of Pmax for LSP.
#' @param type Instrument type: `"LI-6800"`, `"LI-6400"`, or `"other"`.
#'   If `"LI-6800"`, defaults: `Qin = "Qin"`, `A = "A"`.
#'   If `"LI-6400"`, defaults: `Qin = "PARi"`, `A = "Photo"`.
#'   If `"other"`, user must provide `Qin` and `A`.
#' @param lower,upper Optional bounds.
#' @param control List for DEoptim.
#' @param ... Additional arguments.
#' @export

fit_light_response <- function(data,
                               Qin = NULL,
                               A = NULL,
                               model = c(
                                 "rectangular", "nonrectangular",
                                 "exponential", "modified"
                               ),
                               sat_frac = 0.9,
                               type = c("LI-6800", "LI-6400", "other"),
                               lower = NULL,
                               upper = NULL,
                               control = list(trace = FALSE),
                               ...) {
  model <- match.arg(model)
  type <- match.arg(type)

  # Determine column names
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

  # Extract numeric vectors
  Q <- data[[Qin]]
  A_obs <- data[[A]]
  if (anyNA(Q) || anyNA(A_obs)) stop("Missing values not allowed.")
  Q <- as.numeric(Q)
  A_obs <- as.numeric(A_obs)

  # Compute low-light coefficients (using the same type and column names)
  low_coefs <- low_light_coefs(data, Qin = Qin, A = A, type = type, n = 5)
  aqy <- low_coefs$AQY
  rd <- low_coefs$Rd

  cl <- match.call()

  # Dispatch to model-specific fitter
  fit <- switch(model,
    rectangular    = fit_rectangular(Q, A_obs, lower, upper, control, ...),
    nonrectangular = fit_nonrectangular(Q, A_obs, lower, upper, control, ...),
    exponential    = fit_exponential(Q, A_obs, lower, upper, control, ...),
    modified       = fit_modified(Q, A_obs, lower, upper, control, ...)
  )

  deriv <- derive_light_points(model, fit$par, Q, A_obs, sat_frac)

  structure(
    list(
      model = model,
      par = fit$par,
      value = fit$value,
      residuals = A_obs - fit$fitted,
      fitted = fit$fitted,
      lcp = deriv$lcp,
      lsp = deriv$lsp,
      pmax = deriv$pmax,
      aqy = aqy,
      rd = rd,
      sat_frac = sat_frac,
      data = data.frame(Qin = Q, A = A_obs),
      call = cl
    ),
    class = "light_response_fit"
  )
}
