days_nl_365 <-
function (date_fin, date_ini){
  d365=sapply(date_fin, days_365, date_ini=date_ini)
  t365=d365/365
  return(list(days=d365,times=t365))
}
