
#' Value at Risk and Conditional Value at risk.
#'
#' Estimates losses distribution, VaR amd CVaR.
#' @param series data frame with return series.
#' @param w Portfolio weights.
#' @param quant Quantile.
#' @param normal Indicator if returns are normaly distributed.
#' @return Losses distribution, covar matrix, volatility, VaR and CVaR.
#' @export

var_cvar <- function(series, w, quant, normal = FALSE, port_name = 'Portolio') {
  port_rets <- as.numeric(series %*% w)
  fact_mean_ret <- apply(series, 2, mean)
  port_mean_ret <- as.numeric(mean(port_rets))
  neg_rets <- -1 * port_rets
  covar_mat <- cov(series)
  pos_valid <- w != 0
  cor_mat <- round(100 * cor(merge.xts(xts(port_rets, order.by = index(series)), series[,pos_valid])), 3)
  colnames(cor_mat) <- rownames(cor_mat) <- c(port_name, colnames(series)[pos_valid])

  port_vol <- sqrt(as.numeric(t(w) %*% covar_mat %*% w))

  if(normal){
    var <- qnorm(quant) * port_vol
    cvar <- dnorm(qnorm(quant))/(1-quant) * port_vol
  }else{
    var <- quantile(neg_rets, probs = quant)
    cvar <- sapply(var, function(x) mean(neg_rets[neg_rets > x]))
  }
  return(list(covar_mat = covar_mat, cor_mat = cor_mat, port_neg_rets = neg_rets, fact_mean_ret = fact_mean_ret, port_mean_ret = port_mean_ret, port_vol = port_vol, var = var, cvar = cvar))
}
