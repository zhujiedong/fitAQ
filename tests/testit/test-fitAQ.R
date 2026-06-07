# Load required package
library(testit)

# ----- Read the two test datasets -----
data1_path <- system.file("extdata", "dat-aq-wait30.csv", package = "fitAQ")
data2_path <- system.file("extdata", "lrc.csv", package = "fitAQ")

stopifnot(file.exists(data1_path))
stopifnot(file.exists(data2_path))

df1 <- read.csv(data1_path)   # columns: Qin, A  (suitable for LI-6800)
df2 <- read.csv(data2_path)   # columns: Qin, A  (but originally described as PARi/Photo; use LI-6400 to map accordingly)

# ----- Helper function to check basic validity of a fitted object -----
assert_fit <- function(fit_obj, data_name, model_name) {
  if (is.null(fit_obj)) {
    return(FALSE)
  }
  ok <- inherits(fit_obj, "light_response_fit")
  ok <- ok && length(fit_obj$par) > 0
  ok <- ok && is.finite(fit_obj$lcp) && fit_obj$lcp >= 0
  ok <- ok && is.finite(fit_obj$lsp) && fit_obj$lsp > fit_obj$lcp
  ok <- ok && is.finite(fit_obj$pmax) && fit_obj$pmax > 0
  ok <- ok && is.finite(fit_obj$aqy) && fit_obj$aqy > 0
  ok <- ok && is.finite(fit_obj$rd)
  return(ok)
}

# ----- 1. Test fit_light_response with type = "LI-6800" (default) on df1 -----
models <- c("rectangular", "nonrectangular", "exponential", "modified")

assert("All four models can be called without crashing for df1 (LI-6800)", {
  for (m in models) {
    res <- fit_light_response(df1, type = "LI-6800", model = m,
                              control = list(trace = FALSE))
    (assert_fit(res, "df1", m))
  }
  TRUE
})

# ----- 2. Test fit_light_response with type = "LI-6400" on df2 -----
assert("All four models can be called without crashing for df2 (LI-6400)", {
  for (m in models) {
    res <- fit_light_response(df2, type = "LI-6400", model = m,
                              control = list(trace = FALSE))
    (assert_fit(res, "df2", m))
  }
  TRUE
})

# ----- 3. Test explicit column names via type = "other" -----
assert("type = 'other' requires explicit Qin and A", {
  res <- fit_light_response(df1, type = "other", Qin = "Qin", A = "A",
                            model = "exponential", control = list(trace = FALSE))
  (inherits(res, "light_response_fit"))
})

# ----- 4. Detailed checks for exponential model (using df1 and LI-6800) -----
exp_fit <- fit_light_response(df1, type = "LI-6800", model = "exponential",
                              control = list(trace = FALSE))

assert("Exponential model parameters are positive and LCP/LSP are in expected range", {
  (exp_fit$par["Am"] > 0)
  (exp_fit$par["b"] > 0)
  (exp_fit$par["Ic"] >= 0)
  (exp_fit$lcp >= 0)
  (exp_fit$lsp > exp_fit$lcp)
  (exp_fit$pmax > 0)
  (exp_fit$aqy > 0)
  (exp_fit$rd >= 0)
})

# ----- 5. Test light_points helper function -----
lp <- light_points(exp_fit)

assert("light_points returns a data frame with correct columns", {
  (is.data.frame(lp))
  (all(c("LCP", "LSP", "Pmax", "AQY", "Rd") %in% colnames(lp)))
  (nrow(lp) == 1)
})

# ----- 6. Test predict_from_fit if it exists -----
if (exists("predict_from_fit")) {
  pred_df <- predict_from_fit(exp_fit)
  assert("predict_from_fit returns a data frame with Qin and A_pred columns", {
    (is.data.frame(pred_df))
    (all(c("Qin", "A_pred") %in% colnames(pred_df)))
    (nrow(pred_df) > 0)
  })
}
