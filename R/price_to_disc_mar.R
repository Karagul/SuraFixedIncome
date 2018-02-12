price_to_disc_mar <-
function(date_ini, mk_price, coupon, matur, freq, conv, serie_float_rate=NULL, issue_date=NULL, amort_mat = NULL, in_arrears=0, round_val=5, y_lim = c(-20, 20), clean=1) 
{
  diff_price <- function(x) {
    bond_val=bond_price_float(date_ini, x, coupon, matur, freq, conv, serie_float_rate, issue_date, amort_mat, in_arrears, round_val)
    ref_pr=ifelse(clean,bond_val$clean_price,bond_val$full_price)
    return(mk_price - ref_pr)
  }
  spr = round(uniroot(f=diff_price, interval=y_lim, tol=10^(-round_val))$root,round_val)
  return(spr)
}
