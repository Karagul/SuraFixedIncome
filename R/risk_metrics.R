
#' Value at Risk and Conditional Value at risk.
#'
#' Estimates losses distribution, VaR amd CVaR.
#' @param series Series.
#' @param quant Quantile.
#' @param normal Indicator if returns are normaly distributed.
#' @return VaR and CVaR.
#' @export

risk_metrics <- function(series, quant, normal = FALSE) {

  vol <- sqrt(sd(series))

  if(normal){
    var <- qnorm(quant) * vol
    cvar <- dnorm(qnorm(quant))/(1-quant) * vol
  }else{
    var <- quantile(series, probs = quant)
    cvar <- sapply(var, function(x) mean(series[series > x]))
  }
  return(list(var = var, cvar = cvar))
}
