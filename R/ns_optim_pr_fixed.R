ns_optim_pr_fixed <- function(betas_ini, date_ini, mk_price, coupon, matur, freq, conv, issue_date, y_lim=c(0,5), delta_beta=0.01, min_beta=c( 0 , -15 , -10 ,5), max_beta=c(15 , 0, 10 ,10)){

  cf_times <- bond_cf_fixed(date_ini = date_ini, coupon = coupon, matur = matur, freq = freq, conv = conv, issue_date = issue_date)

  diff<-function(betas, mk_price, cf, times){
    res <- as.numeric(crossprod(cf_price_ns(par_ns = betas, cf = cf, times = times) - mk_price) + delta_beta*crossprod(betas-betas_ini))
    return(res)
  }

  #betas_optim=optim(par=betas_ini, fn=diff, date_ini=date_ini, mk_price=mk_price, coupon=coupon, matur=matur, freq=freq, conv=conv, y_lim=y_lim, lower=min_beta, upper=max_beta, control=list(abs.tol=0))$par
  betas_optim=nlminb(start=betas_ini, objective=diff, mk_price=mk_price, cf = cf_times$cf, times = cf_times$times, lower=min_beta, upper=max_beta)$par
  return(betas_optim)
}
