# Internal generic fitter: optimises any registered model with DEoptim.

fit_model <- function(model, Q, A, lower, upper, control, ...) {
  def <- .light_response_models[[model]]
  par_names <- def$par_names

  if (is.null(lower)) {
    lower <- def$lower(Q, A)
  } else {
    lower <- as.numeric(lower)
    if (is.null(names(lower))) names(lower) <- par_names[seq_along(lower)]
  }
  if (is.null(upper)) {
    upper <- def$upper(Q, A)
  } else {
    upper <- as.numeric(upper)
    if (is.null(names(upper))) names(upper) <- par_names[seq_along(upper)]
  }
  if (length(lower) != length(upper)) {
    stop("`lower` and `upper` must have the same length.")
  }

  objective <- function(params, Q, A) {
    pred <- def$pred(stats::setNames(params, names(lower)), Q)
    sum((A - pred)^2)
  }

  res <- DEoptim::DEoptim(
    fn = objective,
    lower = lower, upper = upper,
    control = do.call(DEoptim::DEoptim.control, control),
    Q = Q, A = A, ...
  )

  best <- res$optim$bestmem
  names(best) <- names(lower)
  list(par = best, value = res$optim$bestval, fitted = def$pred(best, Q))
}
