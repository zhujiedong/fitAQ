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
#' \dontrun{
#' lp <- light_points(fit)
#' }
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
