days_act_360 <-
function (date_fin, date_ini){
  da=days_actual(date_fin, date_ini)
  ta=da/360
  return(list(days=da,times=ta))
}
