extract_predictor <- function(model, predictor) {
  coefs <- coef(summary(model))
  p_value <- coefs[predictor, 4]
  stars <- ifelse(p_value < 0.001, "$^{***}$", ifelse(p_value < 0.01, "$^{**}$", ifelse(p_value < 0.05, "$^{*}$", "")))
  coef_se <- round(coefs[predictor, 1:2], 3)
  coef_se_star <- paste0(round(coef_se[1], 3), stars)
  return(c(coef_se_star, coef_se[2]))
}