
bond_fwd_pr<-function (date_ini, date_fwd, cc_curve, price, coupon, matur, freq, conv, serie_float_rate = NULL, issue_date = NULL, cpn_days=NA, bullet = 1, in_arrears = 0, round_val = 5, clean_pr=TRUE){
  if (bullet == 0) {
    stop("Esta funcion no soporta titulos amortizables")
  }
  coupon0 = coupon
  model_count = switch(conv, act_360 = days_act_360, act_365 = days_act_365,
                       act_act = days_act_act, `30_360` = days_30_360, `30_360I` = days_30_360I,
                       `30_360E` = days_30_360E, nl_365 = days_nl_365)
  model_diff = switch(conv, act_360 = diff_act_360, act_365 = diff_act_365,
                      act_act = diff_act_act, `30_360` = diff_30_360, `30_360I` = diff_30_360I,
                      `30_360E` = diff_30_360E, nl_365 = diff_nl_365)
  if(is.na(cpn_days)){
    per = 1
    length = 1
    if (freq != 0) {
      per = 12/freq
      length = as.numeric(freq * (matur - date_ini)/365) + 2
    }
    if (freq == 0) {
      freq = 1
    }
    cpn_dates = sort(seq(matur, by = paste0("-", per, " months"), length = length))
  }
  if(!is.na(cpn_days)){
    cpn_dates = sort(seq(from=matur, to=issue_date, by = -cpn_days))
  }
  fut_cpn_dates = cpn_dates[cpn_dates > date_ini]

  times_count=model_count(fut_cpn_dates, date_ini)
  days=times_count$days
  times = times_count$times
  delta = tail(model_diff(cpn_dates)$times, length(fut_cpn_dates))
  full_price=price

  if (clean_pr==TRUE){
  if (!is.null(serie_float_rate)) {
    date_prev = tail(cpn_dates[cpn_dates < date_ini], 1)
    float_rate_1 = serie_float_rate[which(serie_float_rate[,1] == date_ini), 2]
    if (length(float_rate_1) == 0) {
      stop("Actualizar series de ?ndices!!")
    }
    coupon = rep(float_rate_1 + coupon, lfutcpn)
    if (in_arrears == 0) {
      float_rate_0 = serie_float_rate[which(serie_float_rate[,1] == date_prev), 2]
      coupon[1] = float_rate_0 + coupon0
    }
  }
  coupon1 = coupon[1]
  if (is.na(delta) & coupon1 != 0) {
    delta = model_count(fut_cpn_dates, issue_date)$times
  }
  delta = ifelse(is.na(delta), 1, delta)
  cum_coupon = round(coupon1 * (delta[1] - model_count(fut_cpn_dates[1], date_ini)$times), round_val)
  full_price = round(price + cum_coupon, round_val)
  }
  pv_int_coupon=0
  time_fwd=model_count(date_fwd, date_ini)
  if(fut_cpn_dates[1]<date_fwd){
    df_int=1/(1+approx(x=cc_curve[,1],y=cc_curve[,2],xout=days[1])$y*times[1])
    pv_int_coupon=coupon1*delta[1]*df_int
  }

  fwd_price=(full_price-pv_int_coupon)*(1+approx(x=cc_curve[,1],y=cc_curve[,2],xout=time_fwd$days)$y*time_fwd$times)
  return(list(fwd_price))
}
