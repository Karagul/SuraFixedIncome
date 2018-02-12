days_act_act <-
function (date_fin, date_ini){
  dact=days_actual(date_fin, date_ini)
  d365=days_365(date_fin, date_ini)
  taa=d365/365+(dact-d365)/366
  return(list(days=dact,times=taa))
}
