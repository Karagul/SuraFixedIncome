#' Bond price to cc spread
#'
#' Calculates discount margin over cc curve.
#' @param mk_price Market full price
#' @param cf Cash flows dataframe
#' @param cc_curve Cero Coupon Curve
#' @param base Discount rate base
#' @param round_val round decimals.
#' @return Bond price.
#' @export

price_cf_to_cc_spread <-function(mk_price, cf, cc_curve, base=360, round_val=3, y_lim = c(-0.1, 0.1))
{
  diff_price <- function(x) {
    ref_pr <- bond_price_cf_cc(cf, cc_curve, x, base, round_val)
    return(mk_price - ref_pr)
  }
  spr = round(uniroot(f=diff_price, interval=y_lim, tol=10^(-round_val))$root,round_val)
  return(spr)
}
