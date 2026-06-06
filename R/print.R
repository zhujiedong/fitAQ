#' Print method for light_response_fit objects
#' @param x A \code{light_response_fit} object.
#' @param digits Number of digits to display.
#' @param ... Not used.
#' @export
print.light_response_fit <- function(x, digits = 3, ...) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Model:", x$model, "\n")
  cat("Optimized parameters:\n")
  print(round(x$par, digits))
  cat("\nResidual sum of squares:", round(x$value, digits), "\n")
  cat("Apparent quantum yield (AQY):", round(x$aqy, digits), "\n")
  cat("Dark respiration (Rd):", round(x$rd, digits), "\n")
  cat("Light compensation point (LCP):", round(x$lcp, digits), "\n")
  cat("Light saturation point (LSP,", x$sat_frac*100, "% Pmax):", round(x$lsp, digits), "\n")
  cat("Maximum net photosynthesis (Pmax):", round(x$pmax, digits), "\n")
  invisible(x)
}
