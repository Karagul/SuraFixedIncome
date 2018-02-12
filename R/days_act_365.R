days_act_365 <-
function (date_fin, date_ini){
  da=days_actual(date_fin, date_ini)
  ta=da/365
  return(list(days=da,times=ta))
}
