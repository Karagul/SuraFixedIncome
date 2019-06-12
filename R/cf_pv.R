
#' Cash Flow Present Value.
#'
#' PV.
#' @param cf Cash flows.
#' @param days Days.
#' @param cc Cero coupon curve.
#' @param spr Spread
#' @param base Base.
#' @return Present Value
#' @export

cf_pv <- function(cf, days, cc, spr, base) {
  disc_cf <- cf * df_efec(approx(x=cc[,1], y=cc[,2], xout=days)$y + spr, days, base)
  return(sum(disc_cf))
}
