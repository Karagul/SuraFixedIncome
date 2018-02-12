
bond_risk_nom<-function (date_ini, ytm, coupon, matur, freq, conv, serie_float_rate = NULL, issue_date = NULL, cpn_days=NA, amort_mat = NULL, in_arrears = 0, round_val = 5)
{
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
  cpn_dates = sort(unique(c(seq(matur, by = paste0("-", per, " months"), length = length),issue_date)))
  }
  if(!is.na(cpn_days)){
    cpn_dates = sort(seq(from=matur, to=issue_date, by = -cpn_days))
  }
  fut_cpn_dates = cpn_dates[cpn_dates > date_ini]
  lfutcpn = length(fut_cpn_dates)
  times = model_count(fut_cpn_dates, date_ini)$times
  delta = tail(model_diff(cpn_dates)$times, length(fut_cpn_dates))
  df = 1/(1 + ytm/freq)^(freq * times)
  if (!is.null(serie_float_rate)) {
    date_prev = tail(cpn_dates[cpn_dates < date_ini], 1)
    float_rate_1 = as.numeric(serie_float_rate[findInterval(date_ini, index(serie_float_rate))])
    if (length(float_rate_1) == 0) {
      stop("Actualizar series de ?ndices!!")
    }
    coupon = rep(float_rate_1 + coupon, lfutcpn)
    if (in_arrears == 0) {
      float_rate_0 = as.numeric(serie_float_rate[findInterval(date_prev, index(serie_float_rate))])
      coupon[1] = float_rate_0 + coupon0
    }
  }
  coupon1 = coupon[1]
  if (any(is.na(delta)) & coupon1 != 0) {
    delta = model_count(fut_cpn_dates, issue_date)$times
  }
  delta = ifelse(is.na(delta), 1, delta)
  if(!is.null(amort_mat)){
    cf_cap=rep(0,lfutcpn)
    pos_amort=match(amort_mat[,1],fut_cpn_dates)
    if(any(is.na(pos_amort))){stop("Fechas de amortizacion no coinciden con fechas de flujos.")}
    cf_cap[pos_amort]=amort_mat[,2]
    cf = coupon * delta + cf_cap
  }else{
    cf = coupon * delta
    cf[lfutcpn] = cf[lfutcpn] + 1
  }

  cum_coupon = round(coupon1 * (delta[1] - model_count(fut_cpn_dates[1], date_ini)$times), round_val)
  full_price = round(sum(cf * df), round_val)
  clean_price = round(full_price - cum_coupon, round_val)
  tdf = times * df
  mac_dur = round(-sum(cf * tdf)/full_price, round_val)
  mod_dur = round(mac_dur/(1 + ytm/freq), round_val)
  convex = round(sum((times/freq + times^2) * cf * df)/(full_price * (1 + ytm/freq)^2), round_val)
  dtm = as.numeric(matur - date_ini)
  return(list(full_price = full_price, clean_price = clean_price,
              cum_coupon = cum_coupon, coupon1 = coupon1, mac_dur = mac_dur,
              mod_dur = mod_dur, convex = convex, dtm = dtm))
}
