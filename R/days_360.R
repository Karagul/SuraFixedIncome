days_360 <-
function (date_fin, date_ini, tipo=0) {
  #tipo=c(0,1,2). Si(tipo=0, 30/360 Bond Basis | 30/360 ISDA | 30/360 US MUNI),
  ##si(tipo=1, 30E/360 Eurobond Basis | 30/360 ISMA | 30/360 Special German), 
  ##si(tipo=2, 30E/360 ISDA | 30/360 German)
  
  if(any(date_ini>=date_fin)){stop("Alguna fecha final es menor a la fecha inicial")}
    
  days = as.numeric(format(date_fin, "%d"))
  months = as.numeric(format(date_fin, "%m"))
  years = as.numeric(format(date_fin, "%Y"))
  
  day_ini = as.numeric(format(date_ini, "%d"))
  month_ini = as.numeric(format(date_ini , "%m"))
  year_ini = as.numeric(format(date_ini, "%Y"))
  
  last_day_m_ini=last_day_month(date_ini)
  last_day_m_fin=last_day_month(date_fin)
  
  if(tipo==0){
    d1=day_ini*(day_ini!=31)+30*(day_ini==31)
    d2=days*(days<31|day_ini<30)+30*(days==31)*(day_ini>=30)}
  if(tipo==1){
    d1=day_ini*(day_ini!=31)+30*(day_ini==31)
    d2=days*(days!=31)+30*(days==31)}
  if(tipo==2){
    d1=day_ini*(day_ini!=last_day_m_ini)+30*(day_ini==last_day_m_ini)
    d2=days*(days!=last_day_m_fin)+30*(days==last_day_m_fin)}
  
  days_count=(years-year_ini)*360+(months-month_ini)*30+d2-d1
  
return(days_count) 
}
