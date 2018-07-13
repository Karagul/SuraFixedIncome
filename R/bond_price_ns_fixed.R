bond_price_ns_fixed <-
function (par_ns, date_ini, coupon, matur, freq, conv, bullet = 1)
{
  ##conv=c("act_360","act_365","act_act","30_360","30_360I","30_360E","nl_365"  )

  if (bullet == 0) {
    stop("Esta funcion no soporta titulos amortizables")
  }

  lbonds=length(coupon)
  if (lbonds != length(matur)|lbonds != length(freq)|lbonds != length(conv)) {
    stop("El n?mero de bonos no coincide con los par?metros insertados!!")
  }
  bond_prices = rep(0, lbonds)
  for(i in 1:lbonds){
    model_count=switch(conv[i], "act_360"=days_act_360, "act_365"=days_act_365,"act_act"=days_act_act,"30_360"=days_30_360,"30_360I"=days_30_360I,"30_360E"=days_30_360E,"nl_365"=days_nl_365)
    model_diff=switch(conv[i], "act_360"=diff_act_360, "act_365"=diff_act_365,"act_act"=diff_act_act,"30_360"=diff_30_360,"30_360I"=diff_30_360I,"30_360E"=diff_30_360E,"nl_365"=diff_nl_365)

    per = 1
    length = 1
    if (freq[i] != 0) {
      per = 12/freq[i]
      length = as.numeric(freq[i] * (matur[i] - date_ini)/365) + 2

    }
    cpn_dates = sort(seq(matur[i], by = paste0("-", per,
                                               " months"), length = length))
    fut_cpn_dates = cpn_dates[cpn_dates > date_ini]

    times = model_count(fut_cpn_dates, date_ini)$times

    delta = tail(model_diff(cpn_dates)$times, length(fut_cpn_dates))
    delta=ifelse(is.na(delta),1,delta)
    cc = nelson_siegel(par_ns, times)/100
    df = exp(-cc * times)
    cf = coupon[i] * delta
    bond_prices[i] = sum(cf * df) + tail(df, 1)}
  return(bond_prices)
}
