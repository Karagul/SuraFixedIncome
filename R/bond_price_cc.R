bond_price_cc <-function (date_ini, cc_curve=NULL, betas_ns=NULL, cc_spread, coupon, matur, freq, conv, serie_float_rate=NULL, issue_date=NULL, cpn_days=NA, amort_mat = NULL, in_arrears=0, round_val=5)
{
  ##Esta funcion calcula precio para un titulo a partir de una curva cero cupon y un zeta-spread.
  ##cc_curve: Data frame con dias y tasas
  ## serie_float_rate: Matrix donde columna 1 es fecha, columna 2 es valor.
  ##Si serie_float_rate == NULL, entonces, el cup?n es fijo.
  ##conv=c("act_360","act_365","act_act","30_360","30_360I","30_360E","nl_365"  )
  ##si in_arrears==1, todos los flujos se proyectan con indicador actual. Si in_arrears=0, el primer flujo se proyecta con tasa previa.

  if(is.null(cc_curve) & is.null(betas_ns)){
    stop("Se requiere curva cero cupon o betas!")
  }
  coupon0=coupon
  model_count=switch(conv, "act_360"=days_act_360, "act_365"=days_act_365,"act_act"=days_act_act,"30_360"=days_30_360,"30_360I"=days_30_360I,"30_360E"=days_30_360E,"nl_365"=days_nl_365)
  model_diff=switch(conv, "act_360"=diff_act_360, "act_365"=diff_act_365,"act_act"=diff_act_act,"30_360"=diff_30_360,"30_360I"=diff_30_360I,"30_360E"=diff_30_360E,"nl_365"=diff_nl_365)

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
  lfutcpn=length(fut_cpn_dates)

  count = model_count(fut_cpn_dates, date_ini)
  times = count$times
  delta = tail(model_diff(cpn_dates)$times, lfutcpn)
  if(is.na(delta) & coupon[1]!=0){
    delta = model_count(fut_cpn_dates, issue_date)$times}
  delta=ifelse(is.na(delta),1,delta)


  if(!is.null(betas_ns)){
    cc_rates=nelson_siegel(betas_ns, count$times)/100
    df = exp(-(cc_rates+cc_spread)*times)
  }else{
  cc_rates=approx(x=cc_curve[,1],y=cc_curve[,2],xout=count$days)$y
  df = 1/(1 + (cc_rates+cc_spread)*times)}

  df=ifelse(df<0,1,df)

  if(is.null(serie_float_rate)){cf = coupon * delta}
  if(!is.null(serie_float_rate)){
    date_prev=tail(cpn_dates[cpn_dates < date_ini],1)
    float_rate_1=serie_float_rate[which(serie_float_rate[,1]==date_ini),2]
    if(length(float_rate_1)==0){stop("Actualizar series de ?ndices!!")}
    coupon=rep(float_rate_1+coupon,lfutcpn)
    if(in_arrears==0){
      float_rate_0=serie_float_rate[which(serie_float_rate[,1]==date_prev),2]
      coupon[1]=float_rate_0+coupon0
    }
  }
  if(!is.null(amort_mat)){
    cf_cap=rep(0,lfutcpn)
    pos_amort=match(amort_mat[,1],fut_cpn_dates)
    if(any(is.na(pos_amort))){stop("Fechas de amortizaci?n no coinciden con fechas de flujos.")}
    cf_cap[pos_amort]=amort_mat[,2]
    cf = coupon * delta + cf_cap
  }else{
    cf = coupon * delta
    cf[lfutcpn] = cf[lfutcpn] + 1
  }

  cum_coupon=round(coupon[1]*(delta[1]-model_count(fut_cpn_dates[1], date_ini)$times),round_val)
  full_price = round(sum(cf * df),round_val)

  clean_price=round(full_price-cum_coupon, round_val)
  return(list(full_price = full_price, clean_price = clean_price, cum_coupon=cum_coupon))
}
