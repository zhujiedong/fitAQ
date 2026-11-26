#' Extract physiological parameters from a fitted light response curve
#'
#' Returns a data frame with Light Compensation Point (LCP),
#' Light Saturation Point (LSP), maximum net photosynthesis (Pmax),
#' Apparent Quantum Yield (AQY), and dark respiration (Rd).
#'
#' @param fit An object of class \code{light_response_fit}.
#' @return A data frame with one row (no row names).
#' @export
#'
#' @examples
#' # Simulate a small noise-free light-response curve
#' Qin <- c(0, 25, 50, 100, 250, 500, 1000, 1500)
#' A <- (0.05 * Qin * 20) / (0.05 * Qin + 20) - 1
#' dat <- data.frame(Qin = Qin, A = A)
#' fit <- fit_light_response(dat, model = "rectangular",
#'                           control = list(NP = 40, itermax = 60))
#'
#' lp <- light_points(fit)
#' lp
light_points <- function(fit) {
  if (!inherits(fit, "light_response_fit"))
    stop("Object must be of class 'light_response_fit'")
  data.frame(
    LCP  = fit$lcp,
    LSP  = fit$lsp,
    Pmax = fit$pmax,
    AQY  = fit$aqy,
    Rd   = fit$rd,
    row.names = NULL
  )
}
