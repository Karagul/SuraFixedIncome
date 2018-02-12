days_nl_360 <-function (date_fin, date_ini){
  d365=sapply(date_fin, days_365, date_ini=date_ini)
  t365=d365/360
  return(list(days=d365,times=t365))
}
