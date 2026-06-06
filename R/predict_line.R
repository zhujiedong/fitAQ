#' 从拟合结果生成预测数据
#'
#' @param fit 拟合结果对象（类 light_response_fit）
#' @param n 生成的 Qin 序列长度（默认 100）
#' @param expand 扩展系数，默认 1.05（超出原始最大 Qin 的 5%）
#' @return 数据框，包含两列：Qin 和 A_pred（预测的光合速率）
#' @export
#'
#' @examples
#' pred_df <- predict_from_fit(result4)
#' head(pred_df)
predict_from_fit <- function(fit, n = 100, expand = 1.05) {
  # 提取原始数据中的 Qin 范围
  Qin_obs <- fit$data$Qin
  q_min <- min(Qin_obs, na.rm = TRUE)
  q_max <- max(Qin_obs, na.rm = TRUE)
  
  # 生成用于预测的 Qin 序列
  Qin_seq <- seq(q_min, q_max * expand, length.out = n)
  
  # 根据模型类型计算预测 A
  model <- fit$model
  par <- fit$par
  
  A_pred <- switch(model,
    rectangular = {
      alpha <- par["alpha"]; Am <- par["Am"]; Rd <- par["Rd"]
      (alpha * Qin_seq * Am) / (alpha * Qin_seq + Am) - Rd
    },
    nonrectangular = {
      alpha <- par["alpha"]; Am <- par["Am"]; Rd <- par["Rd"]; theta <- par["theta"]
      term <- alpha * Qin_seq + Am
      sqrt_term <- sqrt(term^2 - 4 * alpha * theta * Am * Qin_seq)
      (1 / (2 * theta)) * (term - sqrt_term) - Rd
    },
    exponential = {
      Am <- par["Am"]; b <- par["b"]; Ic <- par["Ic"]
      ifelse(Qin_seq > Ic, 
             Am * (1 - exp(-b * (Qin_seq - Ic))),
             -Am * (exp(b * (Ic - Qin_seq)) - 1))
    },
    modified = {
      alpha <- par["alpha"]; beta <- par["beta"]; gamma <- par["gamma"]; Rd <- par["Rd"]
      alpha * ((1 - beta * Qin_seq) / (1 + gamma * Qin_seq)) * Qin_seq - Rd
    }
  )
  
  data.frame(Qin = Qin_seq, A_pred = A_pred)
}
