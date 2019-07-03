ns_optim_pr_reg <- function(betas_ini, date_ini, mk_price, coupon, matur, freq, conv, y_lim=c(0,5), delta_beta=0.01, min_beta=c( 0 , -15 , -10 ,5), max_beta=c(15 , 0, 10 ,10)){

  diff<-function(betas,date_ini, mk_price, coupon, matur, freq, conv, y_lim){
    res=as.numeric(diff_pr_ns(betas, date_ini, mk_price, coupon, matur, freq, conv, y_lim)+delta_beta*crossprod(betas-betas_ini))
    return(res)
  }

  #betas_optim=optim(par=betas_ini, fn=diff, date_ini=date_ini, mk_price=mk_price, coupon=coupon, matur=matur, freq=freq, conv=conv, y_lim=y_lim, lower=min_beta, upper=max_beta, control=list(abs.tol=0))$par
  betas_optim=nlminb(start=betas_ini, objective=diff, date_ini=date_ini, mk_price=mk_price, coupon=coupon, matur=matur, freq=freq, conv=conv, y_lim=y_lim, lower=min_beta, upper=max_beta)
  return(betas_optim)
}
