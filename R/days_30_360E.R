days_30_360E <-
function (date_fin, date_ini){
  d360=sapply(date_fin, days_360, date_ini=date_ini, tipo=2)
  t360=d360/360
  return(list(days=d360,times=t360))
}
