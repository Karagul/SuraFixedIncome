diff_ytm_ns <- function (betas, date_ini, ytm, coupon, matur, freq, conv, y_lim=c(0,20))
{
  pr = bond_price_ns_fixed(betas, date_ini = date_ini, coupon = coupon, matur = matur, freq = freq, conv=conv)
  ytm_ns = price_to_ytm(date_ini = date_ini, mk_price = pr, coupon = coupon, matur = matur, freq = freq, conv=conv, y_lim = y_lim)
  aux = (ytm - ytm_ns)
  aux = as.numeric(crossprod(aux))
  return(aux)
}
