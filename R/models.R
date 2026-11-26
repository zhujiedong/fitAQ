# Central registry of the four light-response models.
#
# Each model is defined once: parameter names, default DEoptim bounds, and a
# vectorised prediction function. Fitting (fit_models.R), point derivation
# (derive_points.R), and prediction (predict_line.R) all dispatch on this
# registry so the model formulas cannot drift out of sync.

.light_response_models <- list(
  rectangular = list(
    par_names = c("alpha", "Am", "Rd"),
    lower = function(Q, A) {
      c(alpha = 0.001, Am = 0.1 * max(A), Rd = -abs(max(A)))
    },
    upper = function(Q, A) {
      c(alpha = 1.0, Am = 2.0 * max(A), Rd = 0)
    },
    pred = function(par, Q) {
      (par[["alpha"]] * Q * par[["Am"]]) / (par[["alpha"]] * Q + par[["Am"]]) -
        par[["Rd"]]
    }
  ),

  nonrectangular = list(
    par_names = c("alpha", "Am", "Rd", "theta"),
    lower = function(Q, A) {
      c(alpha = 0.001, Am = 0.1 * max(A), Rd = -abs(max(A)), theta = 0.01)
    },
    upper = function(Q, A) {
      c(alpha = 1.0, Am = 2.0 * max(A), Rd = 0, theta = 0.999)
    },
    pred = function(par, Q) {
      term <- par[["alpha"]] * Q + par[["Am"]]
      # pmax() guards against NaN when custom bounds allow theta > 1
      sqrt_term <- sqrt(pmax(term^2 - 4 * par[["alpha"]] * par[["theta"]] *
        par[["Am"]] * Q, 0))
      (1 / (2 * par[["theta"]])) * (term - sqrt_term) - par[["Rd"]]
    }
  ),

  exponential = list(
    par_names = c("Am", "b", "Ic"),
    lower = function(Q, A) {
      c(Am = 0.1 * max(A), b = 0.001, Ic = 0)
    },
    upper = function(Q, A) {
      c(Am = 2.0 * max(A), b = 10, Ic = max(Q))
    },
    pred = function(par, Q) {
      Am <- par[["Am"]]; b <- par[["b"]]; Ic <- par[["Ic"]]
      ifelse(Q > Ic,
        Am * (1 - exp(-b * (Q - Ic))),
        -Am * (exp(b * (Ic - Q)) - 1)
      )
    }
  ),

  modified = list(
    par_names = c("alpha", "beta", "gamma", "Rd"),
    lower = function(Q, A) {
      c(alpha = 0.001, beta = 1e-6, gamma = 1e-6, Rd = -abs(max(A)))
    },
    upper = function(Q, A) {
      c(alpha = 1.0, beta = 0.1, gamma = 0.1, Rd = 0)
    },
    pred = function(par, Q) {
      par[["alpha"]] *
        ((1 - par[["beta"]] * Q) / (1 + par[["gamma"]] * Q)) * Q -
        par[["Rd"]]
    }
  )
)

model_pred <- function(model, par, Q) {
  .light_response_models[[model]]$pred(par, Q)
}

model_default_bounds <- function(model, Q, A) {
  def <- .light_response_models[[model]]
  list(lower = def$lower(Q, A), upper = def$upper(Q, A))
}

model_par_names <- function(model) {
  .light_response_models[[model]]$par_names
}
