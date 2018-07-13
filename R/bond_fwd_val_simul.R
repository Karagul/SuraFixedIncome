bond_fwd_val_simul<-function(date_ini, simul_days, strike, cv, nominal, date_fwd, cc_curve, 
                             price, coupon, matur, freq, conv, serie_float_rate = NULL, 
                             issue_date = NULL, cpn_days = NA, bullet = 1, in_arrears = 0, 
                             round_val = 5, clean_pr = TRUE, base = 360, M=1000){
  max_days=max(as.numeric(date_fwd-date_ini))
  simul_days_port=simul_days[simul_days<=max_days]
  pr_sim = array(0, dim = c(length(simul_days),M, length(nominal)))
  for(i in 1:length(simul_days_port)){
    d=simul_days_port[i]
    pr_sim[d,,]=t(repc(bond_fwd_val(date_ini+d, strike, cv, nominal, date_fwd, cc_curve, 
               price, coupon, matur, freq, conv, serie_float_rate = serie_float_rate, 
               issue_date = issue_date, cpn_days = cpn_days, bullet = bullet, in_arrears = in_arrears, 
               round_val = round_val, clean_pr = clean_pr, base = base)$fwd_val,M))
  }
  return(pr_sim)
}