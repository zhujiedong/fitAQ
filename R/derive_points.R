# Internal function to compute LCP, LSP, and Pmax from fitted parameters
derive_light_points <- function(model, par, Q, A, sat_frac) {
  switch(
    model,
    rectangular = {
      alpha <- par["alpha"]; Am <- par["Am"]; Rd <- par["Rd"]
      pmax <- Am - Rd
      P <- function(x) (alpha * x * Am) / (alpha * x + Am) - Rd
      lcp <- uniroot(P, interval = c(0, max(Q) * 1.2), extendInt = "yes")$root
      lcp <- max(0, lcp)  # 避免负数
      target <- sat_frac * pmax
      lsp <- uniroot(function(x) P(x) - target,
                     interval = c(lcp, max(Q) * 5), extendInt = "yes")$root
      list(lcp = unname(lcp), lsp = unname(lsp), pmax = unname(pmax))
    },
    nonrectangular = {
      alpha <- par["alpha"]; Am <- par["Am"]; Rd <- par["Rd"]; theta <- par["theta"]
      pmax <- Am - Rd
      P <- function(x) {
        term <- alpha * x + Am
        sqrt_term <- sqrt(term^2 - 4 * alpha * theta * Am * x)
        (1 / (2 * theta)) * (term - sqrt_term) - Rd
      }
      lcp <- uniroot(P, interval = c(0, max(Q) * 1.2), extendInt = "yes")$root
      lcp <- max(0, lcp)
      target <- sat_frac * pmax
      lsp <- uniroot(function(x) P(x) - target,
                     interval = c(lcp, max(Q) * 5), extendInt = "yes")$root
      list(lcp = unname(lcp), lsp = unname(lsp), pmax = unname(pmax))
    },
    exponential = {
      Am <- par["Am"]; b <- par["b"]; Ic <- par["Ic"]
      pmax <- Am
      lcp <- Ic
      lcp <- max(0, lcp)
      lsp <- Ic - log(1 - sat_frac) / b
      if (is.na(lsp) || is.infinite(lsp) || lsp < 0) {
        warning("Invalid LSP for exponential model, using 2 * max Q")
        lsp <- max(Q) * 2
      }
      list(lcp = unname(lcp), lsp = unname(lsp), pmax = unname(pmax))
    },
    modified = {
      alpha <- par["alpha"]; beta <- par["beta"]; gamma <- par["gamma"]; Rd <- par["Rd"]
      if (beta <= 0 || gamma <= 0) {
        warning("beta or gamma <= 0, LSP/LCP may be unreliable. beta = ", beta, ", gamma = ", gamma)
      }
      # LSP: 原始公式
      lsp <- (sqrt((beta + gamma) / beta) - 1) / gamma
      if (is.na(lsp) || is.infinite(lsp) || lsp < 0) {
        warning("Invalid LSP for modified model, using NA")
        lsp <- NA_real_
      }
      # LCP: 解二次方程 A*beta*Q^2 + (gamma*Rd - alpha)*Q - Rd = 0
      a <- alpha * beta
      b_val <- gamma * Rd - alpha
      c <- -Rd
      disc <- b_val^2 - 4 * a * c
      if (disc < 0 || a == 0) {
        warning("Discriminant negative or a=0 for LCP calculation, setting LCP = NA")
        lcp <- NA_real_
      } else {
        root1 <- (-b_val - sqrt(disc)) / (2 * a)
        root2 <- (-b_val + sqrt(disc)) / (2 * a)
        pos_roots <- c(root1, root2)[c(root1 > 0, root2 > 0)]
        if (length(pos_roots) == 0) {
          warning("No positive root for LCP, setting LCP = NA")
          lcp <- NA_real_
        } else {
          lcp <- min(pos_roots)   # 取最小正根作为补偿点
        }
      }
      # Pmax 在 LSP 处计算
      if (!is.na(lsp)) {
        pmax <- alpha * ((1 - beta * lsp) / (1 + gamma * lsp)) * lsp - Rd
      } else {
        pmax <- NA_real_
      }
      list(lcp = unname(lcp), lsp = unname(lsp), pmax = unname(pmax))
    }
  )
}
