ns_optim_ytm_fixed <- function(betas_ini, date_ini, ytm, coupon, matur, freq, conv, issue_date, round_val = 5, y_lim=c(0,5), min_beta=c( 0 , -15 , -10 ,5), max_beta=c(15 , 0, 10 ,10)){

  cf_times <- bond_cf_fixed(date_ini = date_ini, coupon = coupon, matur = matur, freq = freq, conv = conv, issue_date = issue_date)
  freq[freq == 0] <- 1
  betas_optim=optim(par=betas_ini, fn=diff_cf_ytm_ns, date_ini=date_ini, ytm=ytm, cf_times = cf_times, freq=freq, round_val = round_val, y_lim=y_lim, control=list(abs.tol=0), lower=min_beta, upper=max_beta)$par
  #betas_optim <- nlminb(start=betas_ini, objective=diff_cf_ytm_ns, date_ini=date_ini, ytm=ytm, cf_times = cf_times, freq=freq, round_val = round_val, y_lim=y_lim, lower=min_beta, upper=max_beta)
  return(betas_optim)
}
