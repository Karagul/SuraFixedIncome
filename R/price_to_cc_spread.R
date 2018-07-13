price_to_cc_spread <-function(date_ini, mk_price, cc_curve=NULL, betas_ns=NULL, coupon, matur, freq, conv, serie_float_rate=NULL, issue_date=NULL, cpn_days=NA, amort_mat = NULL, in_arrears=0, round_val=5, y_lim = c(-0.05, 0.05), clean=1) 
{
  diff_price <- function(x) {
    bond_val=bond_price_cc(date_ini, cc_curve, betas_ns, x, coupon, matur, freq, conv, serie_float_rate, issue_date, cpn_days, amort_mat, in_arrears, round_val)
    ref_pr=ifelse(clean,bond_val$clean_price,bond_val$full_price)
    return(mk_price - ref_pr)
  }
  spr = round(uniroot(f=diff_price, interval=y_lim, tol=10^(-round_val))$root,round_val)
  return(spr)
}
