#' Bond price
#'
#' Calculates full price using cc_curve and cash flows data frame.
#' @param cf Cash flows dataframe
#' @param cc_curve Cero Coupon Curve
#' @param cc_spread Spread
#' @param base Discount rate base
#' @param round_val round decimals.
#' @return Bond price.
#' @export

bond_price_cf_cc <-function (cf, cc_curve, cc_spread=0, base=360, round_val=5)
{
  cc_rates <- approx(x=cc_curve[,1],y=cc_curve[,2], xout=cf[,1])$y
  df = 1/(1 + (cc_rates+cc_spread))**(cf[,1]/base)
  full_price = round(sum(cf[,2] * df), round_val)
  return(full_price)
}
