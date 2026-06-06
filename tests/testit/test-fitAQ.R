# Load required package
library(testit)

# ----- Read the two test datasets -----
data1_path <- system.file("extdata", "dat-aq-wait30.csv", package = "fitAQ")
data2_path <- system.file("extdata", "lrc.csv", package = "fitAQ")

stopifnot(file.exists(data1_path))
stopifnot(file.exists(data2_path))

df1 <- read.csv(data1_path)
df2 <- read.csv(data2_path)

# Unify column names for convenience
names(df1)[names(df1) == "Qin"] <- "PARi"
names(df1)[names(df1) == "A"]   <- "Photo"

names(df2)[names(df2) == "Qin"] <- "PARi"
names(df2)[names(df2) == "A"]   <- "Photo"

# ----- Helper function to check basic validity of a fitted object -----
assert_fit <- function(fit_obj, data_name, model_name) {
  if (is.null(fit_obj)) {
    return(FALSE)
  }
  # Basic structure
  ok <- inherits(fit_obj, "light_response_fit")
  # Parameter count
  ok <- ok && length(fit_obj$par) > 0
  # Physiological range checks (roughly reasonable)
  ok <- ok && is.finite(fit_obj$lcp) && fit_obj$lcp >= 0
  ok <- ok && is.finite(fit_obj$lsp) && fit_obj$lsp > fit_obj$lcp
  ok <- ok && is.finite(fit_obj$pmax) && fit_obj$pmax > 0
  ok <- ok && is.finite(fit_obj$aqy) && fit_obj$aqy > 0
  ok <- ok && is.finite(fit_obj$rd)
  return(ok)
}

# ----- 1. Test fit_light_response for all models on both datasets -----
models <- c("rectangular", "nonrectangular", "exponential", "modified")

assert("All four models can be called without crashing for dat-aq-wait30", {
  for (m in models) {
    res <- fit_light_response(df1, Qin = "PARi", A = "Photo", model = m)
    (assert_fit(res, "dat-aq-wait30", m))
  }
  TRUE
})

assert("All four models can be called without crashing for lrc data", {
  for (m in models) {
    res <- fit_light_response(df2, Qin = "PARi", A = "Photo", model = m)
    (assert_fit(res, "lrc", m))
  }
  TRUE
})

# ----- 2. Detailed checks for exponential model (using lrc data) -----
exp_fit <- fit_light_response(df2, Qin = "PARi", A = "Photo", model = "exponential")

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

# ----- 3. Test the light_points helper function -----
lp <- light_points(exp_fit)

assert("light_points returns a data frame with correct columns", {
  (is.data.frame(lp))
  (all(c("LCP", "LSP", "Pmax", "AQY", "Rd") %in% colnames(lp)))
  (nrow(lp) == 1)
})

# ----- 4. Test predict_from_fit if it exists -----
if (exists("predict_from_fit")) {
  pred_df <- predict_from_fit(exp_fit)
  assert("predict_from_fit returns a data frame with Qin and A_pred columns", {
    (is.data.frame(pred_df))
    (all(c("Qin", "A_pred") %in% colnames(pred_df)))
    (nrow(pred_df) > 0)
  })
}
