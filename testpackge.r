library("devtools")
library("usethis")

load_all()

df <- read.csv("./inst/extdata/dat-aq-wait30.csv")

result1 <- fit_light_response(
  data = df,
  Qin = "Qin",
  A = "A",
  model = "exponential"
)

result2 <- fit_light_response(
  data = df,
  Qin = "Qin",
  A = "A",
  model = "rectangular"
)
result3 <- fit_light_response(
  data = df,
  Qin = "Qin",
  A = "A",
  model = "nonrectangular"
)
result4 <- fit_light_response(
  data = df,
  Qin = "Qin",
  A = "A",
  model = "modified"
)

for (i in 1:4) {
  var_name <- paste0("result", i)
  print(get(var_name))
  print("------------------------------")
}

library(tinyplot)
pred_data <- predict_from_fit(result4)
coefs <- low_light_coefs(df, Qin = "Qin", A = "A", n = 40)


# Set1 四色分配：点用深灰避免喧宾夺主，三线用 Set1 核心色
cols <- c(
  points = "#4D4D4D", # 深灰：观测点
  fit    = "#E41A1C", # Set1 红：拟合曲线（最突出）
  reg    = "#4DAF4A", # Set1 绿：AQY 回归线
  pmax   = "#377EB8" # Set1 蓝：Pmax 渐近线
)

# 1. 基础散点图
tinyplot(A ~ Qin,
  data = df,
  pch = 19,
  col = cols["points"],
  cex = 1.3, # 更大的点
  xlab = "PPFD (µmol photons m⁻² s⁻¹)",
  ylab = expression(A[net] ~ (µmol ~ m^-2 ~ s^-1)),
  main = "Light Response Curve",
  grid = TRUE, # 添加网格
  xlim = c(0, max(df$Qin) * 1.05),
  ylim = c(min(df$A, na.rm = TRUE) * 1.05, max(df$A, na.rm = TRUE) * 1.1),
  frame.plot = FALSE # 去除外框，更现代
)

# 2. 拟合曲线（实线，最粗）
tinyplot_add(A_pred ~ Qin,
  data = pred_data,
  type = "l", col = cols["fit"], lwd = 2.5
)

# 3. AQY 回归线 — 限制在低光区域（只画到 n_low 对应的最大 Qin）
if (!is.na(coefs$AQY)) {
  qin_sorted <- sort(df$Qin)
  x_max_low <- qin_sorted[min(40, length(qin_sorted))]
  x_seq <- seq(0, x_max_low, length.out = 50)
  y_reg <- coefs$AQY * x_seq + coefs$intercept
  tinyplot_add(x_seq, y_reg,
    type = "l",
    col = cols["reg"], lwd = 1.5, lty = 2
  ) # 虚线
}

# 4. Pmax 渐近线（注意：建议用 result4$pmax 而非 max(result3$data$A)）
pmax_val <- ifelse(!is.null(result4$pmax), result4$pmax, max(df$A, na.rm = TRUE))
tinyplot_add(
  type = type_hline(pmax_val),
  col = cols["pmax"], lwd = 1.5, lty = 3
) # 点划线

# 5. 手动图例（tinyplot_add 不会自动进图例）
legend("bottomright",
  legend = c("Observed", "Fitted curve", "AQY regression", expression(A[max])),
  col = cols,
  pch = c(19, NA, NA, NA),
  lty = c(NA, 1, 2, 3),
  lwd = c(NA, 2.5, 1.5, 1.5),
  pt.cex = 1.3,
  bty = "n", inset = 0.02, seg.len = 2
)

light_points(result3)
