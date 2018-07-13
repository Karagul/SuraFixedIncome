bond_fwd_val<-function(date_ini, strike, cv, nominal, date_fwd, cc_curve, price, coupon, matur, 
                       freq, conv, serie_float_rate = NULL, issue_date = NULL, cpn_days = NA, 
                       bullet = 1, in_arrears = 0, round_val = 5, clean_pr = TRUE, base=360){
  
  days=as.numeric(date_fwd-date_ini)
  nfwd=length(strike)
  vp_pay_leg=vp_rec_leg=fwd_pr=disc_rate=rep(0,nfwd)
  
  for(i in 1:nfwd){
  fwd_pr[i]=bond_fwd_pr(date_ini=date_ini, date_fwd=date_fwd[i], cc_curve=cc_curve, price=price, coupon=coupon, matur=matur, 
                     freq=freq, conv=conv, serie_float_rate = serie_float_rate, issue_date = issue_date, cpn_days = cpn_days, 
                     bullet = bullet, in_arrears = in_arrears, round_val = round_val, clean_pr = clean_pr)[[1]]
  disc_rate[i]=approx(x=cc_curve[,1],y=cc_curve[,2],xout=days[i])$y
  vp_fwd_leg=fwd_pr[i]/(1+disc_rate[i]*days[i]/base)
  vp_fix_leg=strike[i]/(1+disc_rate[i]*days[i]/base)
  vp_pay_leg[i]=nominal[i]*((cv[i]=="V")*vp_fwd_leg+(cv[i]=="C")*vp_fix_leg)
  vp_rec_leg[i]=nominal[i]*((cv[i]=="C")*vp_fwd_leg+(cv[i]=="V")*vp_fix_leg)
  }
  fwd_val=vp_rec_leg-vp_pay_leg
  return(list(fwd_pr=fwd_pr, dif_fwd_spot=fwd_pr-price, vp_pay_leg=vp_pay_leg,vp_rec_leg=vp_rec_leg, fwd_val=fwd_val, disc_rate=disc_rate))
  
  }


