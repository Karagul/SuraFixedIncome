#' Price
#'
#' Price
#' @param ytm Yield to maturity
#' @param cf Cash flows
#' @param times cf times in years
#' @return Price.
#' @export

cf_price_ytm <-function(ytm, cf, freq, times)
{
  df <- 1/(1 + ytm/freq)^(freq * times)
  pr <- sum(cf * df)

  return(pr)
}
