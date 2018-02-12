bond_option_pr_port<-function (date_ini, date_mat, sigma, c_p="call", strike, cc_curve, price, coupon, matur, freq, conv, base_vol=365, serie_float_rate = NULL, issue_date = NULL, cpn_days=NA, bullet = 1, in_arrears = 0, round_val = 5, clean_pr=TRUE){
  lbonds = length(date_mat)
  fwd_price = option_price = option_delta=option_disc_rate=rep(0, lbonds)
  for (i in 1:lbonds) {
    bond_data = bond_option_pr(date_ini, date_mat[i], sigma[i], c_p[i], strike[i], cc_curve, price[i], coupon[i], matur[i], freq[i], conv[i], base_vol=base_vol, serie_float_rate = serie_float_rate, issue_date = issue_date[i], cpn_days=cpn_days, bullet = bullet, in_arrears = in_arrears, round_val = round_val, clean_pr=clean_pr)
    fwd_price[i] = bond_data$fwd_price
    option_price[i] = bond_data$option_price
    option_delta[i] = bond_data$option_delta
    option_disc_rate[i] = bond_data$option_disc_rate
    }
  return(list(fwd_price = fwd_price, option_price = option_price, option_delta = option_delta, option_disc_rate=option_disc_rate))
}