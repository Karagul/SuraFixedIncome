bond_risk_nom_port <-function (date_ini, ytm, coupon, matur, freq, conv, serie_float_rate=NULL, issue_date=NULL, cpn_days=NA, amort_mat=NULL, in_arrears=0, round_val=5)
{
  lbonds=length(ytm)
  full_price=clean_price=cum_coupon=coupon1=mac_dur=mod_dur=convex=dtm=rep(0,lbonds)
  for(i in 1:lbonds){
  bond_data=bond_risk_nom(date_ini, ytm[i], coupon[i], matur[i], freq[i], conv[i], serie_float_rate, issue_date[i], cpn_days[i], amort_mat, in_arrears, round_val)
  full_price[i]=bond_data$full_price
  clean_price[i]=bond_data$clean_price
  cum_coupon[i]=bond_data$cum_coupon
  coupon1[i]=bond_data$coupon1
  mac_dur[i]=bond_data$mac_dur
  mod_dur[i]=bond_data$mod_dur
  convex[i]=bond_data$convex
  dtm[i]=bond_data$dtm
  }
  return(list(full_price = full_price, clean_price = clean_price, cum_coupon=cum_coupon, coupon1=coupon1, mac_dur = mac_dur, mod_dur = mod_dur, convex=convex, dtm=dtm))  
}
