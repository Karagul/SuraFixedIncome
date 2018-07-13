bond_price_ns <- function(par_ns, date_ini, coupon, matur, freq, bullet=1, base=365,  base_num="365"){
  #freq: {0, 1, 2, 4, 12}. Annual payment frequecy. If (freq==0){zero coupon bond}
  #base_num=365 sólo aplica para títulos con peridicidad ANUAL
  if (any(freq != 1) & base_num == "365") {
    stop("Esta función solo soporta conteo de días 365 cuando los pagos son ANUALES")
  }
  if (bullet == 0) {
    stop("Esta función no soporta títulos amortizables")
  }
  l_bonds = length(coupon)
  if (l_bonds != length(matur)) {
    stop("Número de cupones diferente a número de vencimientos")
  }
  bond_prices = rep(0, l_bonds)
  for (i in c(1:l_bonds)) {
    per = 1
    length = 1
    if (freq[i] != 0) {
      per = 12/freq[i]
      length = as.numeric(freq[i] * (matur[i] - date_ini)/365) +2
    }
    cpn_dates = sort(seq(matur[i], by = paste0("-", per, " months"), length = length))
    fut_cpn_dates = cpn_dates[cpn_dates > date_ini]
    if (base_num == "Act") {
      times = as.numeric((fut_cpn_dates - date_ini)/base)
      delta = tail(as.numeric(diff(cpn_dates)/base), length(fut_cpn_dates))
    }
    else if (base_num == "365") {
      times = unlist(lapply(fut_cpn_dates, day_count_tes,date_ini))/base
      delta = 1
    }
    cc = nelson_siegel(par_ns, times)/100
    df = exp(-cc * times)
    cf = coupon[i] * delta/100
    bond_prices[i] = sum(cf * df) + tail(df, 1)
  }
  return(bond_prices)
}
