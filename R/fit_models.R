# Internal functions for fitting each model with DEoptim
# None are exported.

# Rectangular hyperbola
fit_rectangular <- function(Q, A, lower, upper, control, ...) {
  if (is.null(lower)) {
    lower <- c(alpha = 0.001, Am = 0.1 * max(A), Rd = -abs(max(A)))
  }
  if (is.null(upper)) {
    upper <- c(alpha = 1.0,   Am = 2.0 * max(A), Rd = 0)
  }

  objective <- function(params, Q, A) {
    alpha <- params[1]; Am <- params[2]; Rd <- params[3]
    pred <- (alpha * Q * Am) / (alpha * Q + Am) - Rd
    sum((A - pred)^2)
  }

  res <- DEoptim::DEoptim(fn = objective,
                          lower = lower, upper = upper,
                          control = do.call(DEoptim::DEoptim.control, control),
                          Q = Q, A = A, ...)
  best <- res$optim$bestmem
  names(best) <- names(lower)
  fitted <- (best["alpha"] * Q * best["Am"]) / (best["alpha"] * Q + best["Am"]) - best["Rd"]
  list(par = best, value = res$optim$bestval, fitted = fitted)
}

# Non-rectangular hyperbola
fit_nonrectangular <- function(Q, A, lower, upper, control, ...) {
  if (is.null(lower)) {
    lower <- c(alpha = 0.001, Am = 0.1 * max(A), Rd = -abs(max(A)), theta = 0.01)
  }
  if (is.null(upper)) {
    upper <- c(alpha = 1.0,   Am = 2.0 * max(A), Rd = 0,            theta = 0.999)
  }

  objective <- function(params, Q, A) {
    alpha <- params[1]; Am <- params[2]; Rd <- params[3]; theta <- params[4]
    term <- alpha * Q + Am
    sqrt_term <- sqrt(term^2 - 4 * alpha * theta * Am * Q)
    pred <- (1 / (2 * theta)) * (term - sqrt_term) - Rd
    sum((A - pred)^2)
  }

  res <- DEoptim::DEoptim(fn = objective,
                          lower = lower, upper = upper,
                          control = do.call(DEoptim::DEoptim.control, control),
                          Q = Q, A = A, ...)
  best <- res$optim$bestmem
  names(best) <- names(lower)
  term <- best["alpha"] * Q + best["Am"]
  sqrt_term <- sqrt(term^2 - 4 * best["alpha"] * best["theta"] * best["Am"] * Q)
  fitted <- (1 / (2 * best["theta"])) * (term - sqrt_term) - best["Rd"]
  list(par = best, value = res$optim$bestval, fitted = fitted)
}

# Exponential model
fit_exponential <- function(Q, A, lower, upper, control, ...) {
  if (is.null(lower)) {
    lower <- c(Am = 0.1 * max(A), b = 0.001, Ic = 0)
  }
  if (is.null(upper)) {
    upper <- c(Am = 2.0 * max(A), b = 10,    Ic = max(Q))
  }

  objective <- function(params, Q, A) {
    Am <- params[1]; b <- params[2]; Ic <- params[3]
    pred <- ifelse(Q > Ic, Am * (1 - exp(-b * (Q - Ic))), -Am * (exp(b * (Ic - Q)) - 1))
    sum((A - pred)^2)
  }

  res <- DEoptim::DEoptim(fn = objective,
                          lower = lower, upper = upper,
                          control = do.call(DEoptim::DEoptim.control, control),
                          Q = Q, A = A, ...)
  best <- res$optim$bestmem
  names(best) <- names(lower)
  pred <- ifelse(Q > best["Ic"], best["Am"] * (1 - exp(-best["b"] * (Q - best["Ic"]))),
                 -best["Am"] * (exp(best["b"] * (best["Ic"] - Q)) - 1))
  list(par = best, value = res$optim$bestval, fitted = pred)
}

# Modified rectangular hyperbola
fit_modified <- function(Q, A, lower, upper, control, ...) {
  if (is.null(lower)) {
    lower <- c(alpha = 0.001, beta = 1e-6, gamma = 1e-6, Rd = -abs(max(A)))
  }
  if (is.null(upper)) {
    upper <- c(alpha = 1.0,   beta = 0.1,   gamma = 0.1,   Rd = 0)
  }

  objective <- function(params, Q, A) {
    alpha <- params[1]; beta <- params[2]; gamma <- params[3]; Rd <- params[4]
    pred <- alpha * ((1 - beta * Q) / (1 + gamma * Q)) * Q - Rd
    sum((A - pred)^2)
  }

  res <- DEoptim::DEoptim(fn = objective,
                          lower = lower, upper = upper,
                          control = do.call(DEoptim::DEoptim.control, control),
                          Q = Q, A = A, ...)
  best <- res$optim$bestmem
  names(best) <- names(lower)
  fitted <- best["alpha"] * ((1 - best["beta"] * Q) / (1 + best["gamma"] * Q)) * Q - best["Rd"]
  list(par = best, value = res$optim$bestval, fitted = fitted)
}
