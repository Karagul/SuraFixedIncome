#' Price
#'
#' Price
#' @param par_ns Nelson Siegel Pars
#' @param cf Cash flows
#' @param times cf times in years
#' @return Price.
#' @export

cf_price_ns <-function (par_ns, cf, times)
{
  cc <- nelson_siegel(par_ns, times)/100
  df <- exp(-cc * times)
  if(!is.null(dim(times))){
    pr <- apply(cf * df, 2, sum)
  }else{
    pr <- sum(cf * df)
  }
  return(pr)
}
