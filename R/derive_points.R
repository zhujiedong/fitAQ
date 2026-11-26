# Internal functions to compute LCP, LSP, and Pmax from fitted parameters.

# uniroot with graceful failure: returns NA instead of erroring when the
# function has no sign change in the interval (e.g. curve never saturates).
safe_uniroot <- function(f, interval) {
  tryCatch(
    uniroot(f, interval = interval, extendInt = "yes")$root,
    error = function(e) NA_real_
  )
}

# Rectangular and non-rectangular hyperbolae share the same derivation:
# pmax = Am - Rd, photosynthesis P(Q) is model_pred().
derive_hyperbola <- function(model, par, Q, sat_frac) {
  pmax <- unname(par[["Am"]] - par[["Rd"]])
  P <- function(x) model_pred(model, par, x)

  lcp <- safe_uniroot(P, interval = c(0, max(Q) * 1.2))
  lcp <- max(0, lcp)

  target <- sat_frac * pmax
  lcp_for_lsp <- if (is.na(lcp)) 0 else lcp
  lsp <- safe_uniroot(function(x) P(x) - target,
    interval = c(lcp_for_lsp, max(Q) * 5))
  if (is.na(lsp)) {
    warning("Could not find LSP (curve may not reach ", sat_frac,
      " * Pmax within range). Setting LSP = NA.")
  }

  list(lcp = unname(lcp), lsp = unname(lsp), pmax = pmax)
}

derive_exponential <- function(par, Q, sat_frac) {
  pmax <- unname(par[["Am"]])
  lcp <- max(0, unname(par[["Ic"]]))
  lsp <- par[["Ic"]] - log(1 - sat_frac) / par[["b"]]
  if (is.na(lsp) || is.infinite(lsp) || lsp < 0) {
    warning("Invalid LSP for exponential model, using 2 * max Q")
    lsp <- max(Q) * 2
  }
  list(lcp = lcp, lsp = unname(lsp), pmax = pmax)
}

derive_modified <- function(par, sat_frac) {
  alpha <- par[["alpha"]]; beta <- par[["beta"]]
  gamma <- par[["gamma"]]; Rd <- par[["Rd"]]

  if (beta <= 0 || gamma <= 0) {
    warning("beta or gamma <= 0, LSP/LCP may be unreliable. beta = ",
      beta, ", gamma = ", gamma)
  }

  # LSP: original closed-form formula
  lsp <- (sqrt((beta + gamma) / beta) - 1) / gamma
  if (is.na(lsp) || is.infinite(lsp) || lsp < 0) {
    warning("Invalid LSP for modified model, setting LSP = NA")
    lsp <- NA_real_
  }

  # LCP: solve alpha*beta*Q^2 + (gamma*Rd - alpha)*Q - Rd = 0
  a <- alpha * beta
  b_val <- gamma * Rd - alpha
  disc <- b_val^2 + 4 * a * Rd
  if (disc < 0 || a == 0) {
    warning("Discriminant negative or a = 0 for LCP calculation, setting LCP = NA")
    lcp <- NA_real_
  } else {
    roots <- c(
      (-b_val - sqrt(disc)) / (2 * a),
      (-b_val + sqrt(disc)) / (2 * a)
    )
    pos_roots <- roots[roots > 0]
    if (length(pos_roots) == 0) {
      warning("No positive root for LCP, setting LCP = NA")
      lcp <- NA_real_
    } else {
      lcp <- min(pos_roots) # smallest positive root is the compensation point
    }
  }

  # Pmax evaluated at the LSP
  pmax <- if (!is.na(lsp)) {
    unname(alpha * ((1 - beta * lsp) / (1 + gamma * lsp)) * lsp - Rd)
  } else {
    NA_real_
  }

  list(lcp = unname(lcp), lsp = unname(lsp), pmax = pmax)
}

derive_light_points <- function(model, par, Q, A, sat_frac) {
  switch(model,
    rectangular = ,
    nonrectangular = derive_hyperbola(model, par, Q, sat_frac),
    exponential = derive_exponential(par, Q, sat_frac),
    modified = derive_modified(par, sat_frac)
  )
}
