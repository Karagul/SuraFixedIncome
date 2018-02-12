days_30_365 <-function (date_fin, date_ini){
  d360=sapply(date_fin, days_360, date_ini=date_ini, tipo=0)
  t360=d360/365
  return(list(days=d360,times=t360))
}
