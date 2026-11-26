#' Plot a fitted light-response curve
#'
#' Base-graphics plot of the observed data, the fitted model curve, and the
#' derived physiological points (LCP, LSP, Pmax).
#'
#' @param x A `light_response_fit` object.
#' @param n Number of points used to draw the fitted curve.
#' @param expand See [predict_from_fit()].
#' @param col_point,col_line,col_mark Colors for the observations, the fitted
#'   curve, and the LCP/LSP markers.
#' @param pch Plotting character for the observations.
#' @param lty_mark Line type for the LCP/LSP markers.
#' @param annotate Logical; annotate model, LCP, LSP, and Pmax values.
#' @param ... Further graphical arguments passed to [graphics::plot()].
#' @returns `x`, invisibly.
#' @export
#'
#' @examples
#' # Simulate a small noise-free light-response curve
#' Qin <- c(0, 25, 50, 100, 250, 500, 1000, 1500)
#' A <- (0.05 * Qin * 20) / (0.05 * Qin + 20) - 1
#' dat <- data.frame(Qin = Qin, A = A)
#' fit <- fit_light_response(dat, model = "rectangular",
#'                           control = list(NP = 40, itermax = 60))
#' plot(fit)
plot.light_response_fit <- function(x, n = 200, expand = 1.05,
                                    col_point = "grey35",
                                    col_line = "#D55E00",
                                    col_mark = "grey55",
                                    pch = 19,
                                    lty_mark = 3,
                                    annotate = TRUE,
                                    ...) {
  if (!inherits(x, "light_response_fit")) {
    stop("Object must be of class 'light_response_fit'")
  }

  pred <- predict_from_fit(x, n = n, expand = expand)
  obs <- x$data

  dots <- list(...)
  xlim <- dots$xlim
  ylim <- dots$ylim
  dots$xlim <- NULL
  dots$ylim <- NULL
  if (is.null(xlim)) {
    xlim <- range(pred$Qin)
    if (is.finite(x$lcp)) xlim <- range(xlim, x$lcp)
    if (is.finite(x$lsp)) xlim <- range(xlim, x$lsp)
  }
  if (is.null(ylim)) {
    ylim <- range(c(obs$A, pred$A_pred))
  }

  xlab <- expression(Q["in"] ~ (mu * mol ~ m^{-2} ~ s^{-1}))
  ylab <- expression(A ~ (mu * mol ~ m^{-2} ~ s^{-1}))

  do.call(graphics::plot, c(
    list(x = obs$Qin, y = obs$A, pch = pch, col = col_point,
      xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim),
    dots
  ))
  graphics::abline(h = 0, col = "grey85")
  graphics::lines(pred$Qin, pred$A_pred, col = col_line, lwd = 2)

  # LCP / LSP markers
  if (is.finite(x$lcp)) {
    graphics::abline(v = x$lcp, col = col_mark, lty = lty_mark)
  }
  if (is.finite(x$lsp)) {
    graphics::abline(v = x$lsp, col = col_mark, lty = lty_mark)
  }

  graphics::legend("bottomright", bty = "n", cex = 0.9,
    legend = c("Observed", "Fitted", "LCP", "LSP"),
    pch = c(pch, NA, NA, NA),
    col = c(col_point, col_line, col_mark, col_mark),
    lty = c(NA, 1, lty_mark, lty_mark),
    lwd = c(NA, 2, 1, 1))

  if (annotate) {
    graphics::legend("topleft", bty = "n", cex = 0.9,
      title = paste("Model:", x$model),
      legend = c(
        sprintf("LCP  = %.2f", x$lcp),
        sprintf("LSP  = %.2f", x$lsp),
        sprintf("Pmax = %.2f", x$pmax)
      )
    )
  }

  invisible(x)
}
