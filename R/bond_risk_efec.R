bond_risk_efec <-function (date_ini, ytm, coupon, matur, freq, conv, serie_float_rate=NULL, issue_date=NULL, bullet = 1,  in_arrears=0, round_val=5)
{
  ##Esta funci?n calcula precio y medidas de sensibilidad para un t?tulo asumiendo que el ytm es una tasa efectiva anual.
  ## serie_float_rate: Matrix donde columna 1 es fecha, columna 2 es valor.
  ##Si serie_float_rate == NULL, entonces, el cup?n es fijo.

  if (bullet == 0) {
    stop("Esta funcion no soporta titulos amortizables")
  }

  if (l_bonds != length(matur)|l_bonds != length(freq)|l_bonds != length(conv)) {
    stop("El n?mero de bonos no coincide con los par?metros insertados!!")
  }

  model_count=switch(conv, "act_360"=days_act_360, "act_365"=days_act_365,"act_act"=days_act_act,"30_360"=days_30_360,"30_360I"=days_30_360I,"30_360E"=days_30_360E,"nl_365"=days_nl_365)
  model_diff=switch(conv, "act_360"=diff_act_360, "act_365"=diff_act_365,"act_act"=diff_act_act,"30_360"=diff_30_360,"30_360I"=diff_30_360I,"30_360E"=diff_30_360E,"nl_365"=diff_nl_365)

  coupon0=coupon
  per = 1
  length = 1
  if (freq != 0) {
    per = 12/freq
    length = as.numeric(freq * (matur - date_ini)/365) + 2
  }

  cpn_dates = sort(unique(c(seq(matur, by = paste0("-", per, " months"), length = length),issue_date)))
  fut_cpn_dates = cpn_dates[cpn_dates > date_ini]

  times = model_count(fut_cpn_dates, date_ini)$times
  delta = tail(model_diff(cpn_dates)$times, length(fut_cpn_dates))

  df = 1/(1 + ytm)^times

  if(!is.null(serie_float_rate)){
    date_prev=tail(cpn_dates[cpn_dates < date_ini],1)
    float_rate_1=serie_float_rate[which(serie_float_rate[,1]==date_ini),2]
    coupon=rep(float_rate_1+coupon,lfutcpn)
    if(in_arrears==0){
      float_rate_0=serie_float_rate[which(serie_float_rate[,1]==date_prev),2]
      coupon[1]=float_rate_0+coupon0
    }
  }
  coupon1=coupon[1]
  if(is.na(delta) & coupon1!=0){
    delta = model_count(fut_cpn_dates, issue_date)$times}
  delta=ifelse(is.na(delta),1,delta)
  cf = coupon * delta
  cum_coupon=round(coupon[1]*(delta[1]-(fut_cpn_dates[1]-date_ini)/base),round_val)
  full_price = round(sum(cf * df) + tail(df, 1),round_val)
  clean_price=round(full_price-cum_coupon, round_val)

  tdf = times * df
  mac_dur = round(-(sum(cf * tdf) + tail(tdf, 1))/full_price,round_val)
  mod_dur = round(mac_dur/(1 + ytm),round_val)
  convex = round(sum((times+times^2)*cf*df)/(full_price*(1+ytm)^2),round_val)
  dtm=as.numeric(matur-date_ini)
  return(list(full_price = full_price, clean_price = clean_price, cum_coupon=cum_coupon, coupon1=coupon1, mac_dur = mac_dur, mod_dur = mod_dur, convex=convex, dtm=dtm))
}
