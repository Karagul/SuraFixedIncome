diff_pr_ns <-
function (betas, date_ini, mk_price, coupon, matur, freq, conv, y_lim=c(0,20)) 
{
  ##mk_price: Precio sucio
  pr = bond_price_ns_fixed(betas, date_ini = date_ini, coupon = coupon, matur = matur, freq = freq, conv=conv)
  aux = pr - mk_price
  aux = crossprod(aux)
  return(aux)
}
