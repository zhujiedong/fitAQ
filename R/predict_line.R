#' 从拟合结果生成预测数据
#'
#' @param fit 拟合结果对象（类 light_response_fit）
#' @param n 生成的 Qin 序列长度（默认 100）
#' @param expand 扩展系数，默认 1.05（超出原始最大 Qin 的 5%）
#' @return 数据框，包含两列：Qin 和 A_pred（预测的光合速率）
#' @export
#'
#' @examples
#' # Simulate a small noise-free light-response curve
#' Qin <- c(0, 25, 50, 100, 250, 500, 1000, 1500)
#' A <- (0.05 * Qin * 20) / (0.05 * Qin + 20) - 1
#' dat <- data.frame(Qin = Qin, A = A)
#' fit <- fit_light_response(dat, model = "rectangular",
#'                           control = list(NP = 40, itermax = 60))
#'
#' pred_df <- predict_from_fit(fit)
#' head(pred_df)
predict_from_fit <- function(fit, n = 100, expand = 1.05) {
  if (!inherits(fit, "light_response_fit")) {
    stop("Object must be of class 'light_response_fit'")
  }

  # 提取原始数据中的 Qin 范围
  Qin_obs <- fit$data$Qin
  q_min <- min(Qin_obs, na.rm = TRUE)
  q_max <- max(Qin_obs, na.rm = TRUE)

  # 生成用于预测的 Qin 序列
  Qin_seq <- seq(q_min, q_max * expand, length.out = n)

  # 根据模型类型计算预测 A（模型公式统一定义于 R/models.R）
  A_pred <- model_pred(fit$model, fit$par, Qin_seq)

  data.frame(Qin = Qin_seq, A_pred = A_pred)
}
