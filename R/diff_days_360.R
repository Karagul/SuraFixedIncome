diff_days_360 <-
function (dates, tipo=0) 
{
  ldates=length(dates)
  diff_days=rep(0,ldates)
  for(i in c(1:(ldates-1))){
    diff_days[i]=days_360(date_fin=dates[i+1],date_ini=dates[i], tipo=tipo)}
  return(diff_days)
}
