#' Fit light response curves using DEoptim
#'
#' Fits one of four light response models using Differential Evolution optimization.
#'
#' @param data A data frame.
#' @param Qin Name or index of PPFD column.
#' @param A Name or index of net photosynthesis column.
#' @param model Model type: "rectangular", "nonrectangular", "exponential", "modified".
#' @param sat_frac Fraction of Pmax to define saturation point (default 0.9).
#' @param lower,upper Optional named vectors of parameter bounds.
#' @param control List passed to \code{DEoptim.control}.
#' @param ... Additional arguments to \code{DEoptim}.
#'
#' @return An object of class \code{light_response_fit}.
#' @export
#'
#' @importFrom DEoptim DEoptim DEoptim.control
#'
#' @examples
#' \dontrun{
#' res <- fit_light_response(mydata, Qin = "PAR", A = "Photo", model = "exponential")
#' print(res)
#' plot(res)
#' light_points(res)
#' }
#' @importFrom stats lm coef
fit_light_response <- function(data,
                               Qin,
                               A,
                               model = c("rectangular", "nonrectangular",
                                         "exponential", "modified"),
                               sat_frac = 0.9,
                               lower = NULL,
                               upper = NULL,
                               control = list(trace = FALSE),   # 修改这里
                               ...){
  model <- match.arg(model)

  # Extract numeric vectors
  Q <- data[[Qin]]
  A_obs <- data[[A]]
  if (anyNA(Q) || anyNA(A_obs)) stop("Missing values not allowed.")
  Q <- as.numeric(Q)
  A_obs <- as.numeric(A_obs)

  # Low‑light linear regression (first 5 points sorted by Q)
  ord <- order(Q)
  Q_sorted <- Q[ord]
  A_sorted <- A_obs[ord]
  low_df <- data.frame(x = Q_sorted[1:5], y = A_sorted[1:5])
  low_lm <- lm(y ~ x, data = low_df)
  aqy <- coef(low_lm)[["x"]]
  rd <- -coef(low_lm)[["(Intercept)"]]

  cl <- match.call()

  # Dispatch to model-specific fitter (defined in fit_models.R)
  fit <- switch(model,
                rectangular    = fit_rectangular(Q, A_obs, lower, upper, control, ...),
                nonrectangular = fit_nonrectangular(Q, A_obs, lower, upper, control, ...),
                exponential    = fit_exponential(Q, A_obs, lower, upper, control, ...),
                modified       = fit_modified(Q, A_obs, lower, upper, control, ...))

  # Compute LCP, LSP, Pmax (defined in derive_points.R)
  deriv <- derive_light_points(model, fit$par, Q, A_obs, sat_frac)

  # Build return object
  structure(
    list(model     = model,
         par       = fit$par,
         value     = fit$value,
         residuals = A_obs - fit$fitted,
         fitted    = fit$fitted,
         lcp       = deriv$lcp,
         lsp       = deriv$lsp,
         pmax      = deriv$pmax,
         aqy       = aqy,
         rd        = rd,
         sat_frac  = sat_frac,
         data      = data.frame(Qin = Q, A = A_obs),
         call      = cl),
    class = "light_response_fit"
  )
}
