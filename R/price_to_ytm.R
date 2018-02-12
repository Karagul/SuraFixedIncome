price_to_ytm <- function (date_ini, mk_price, coupon, matur, freq, conv, serie_float_rate = NULL,
                          issue_date = NULL, cpn_days=NA,  amort_mat = NULL, in_arrears = 0, round_val = 5, y_lim = c(0, 20), clean = 1) {
  diff_pr <- function(x, mk_pricei, couponi, maturi, freqi,convi, issue_datei, cpn_daysi) {
    bond_val = bond_risk_nom(date_ini, x, couponi, maturi, freqi, convi, serie_float_rate, issue_datei, cpn_daysi, amort_mat, in_arrears, round_val)
    ref_pr = ifelse(clean, bond_val$clean_price, bond_val$full_price)
    return(mk_pricei - ref_pr)
  }
  lbonds = length(mk_price)
  ym = rep(0, lbonds)
  for (i in 1:lbonds) {
    if (sign(diff_pr(y_lim[1], mk_pricei = mk_price[i], couponi = coupon[i],maturi = matur[i], freqi = freq[i], convi = conv[i],
                     issue_datei = issue_date[i], cpn_daysi=cpn_days[i])) * sign(diff_pr(y_lim[2],
                     mk_pricei = mk_price[i], couponi = coupon[i], maturi = matur[i],
                    freqi = freq[i], convi = conv[i], issue_datei = issue_date[i], cpn_daysi=cpn_days[i])) == 1) {next}
    ym[i] = round(uniroot(f = diff_pr, interval = y_lim,
                          mk_pricei = mk_price[i], couponi = coupon[i], maturi = matur[i],
                          freqi = freq[i], convi = conv[i], issue_datei = issue_date[i], cpn_daysi=cpn_days[i],
                          tol = 10^(-round_val))$root, round_val)
  }
  return(ym)
}
