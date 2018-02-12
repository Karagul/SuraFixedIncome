diff_days_365 <-
function (dates,...) 
{
  ldates=length(dates)
  diff_days=rep(0,ldates)
  for(i in c(1:(ldates-1))){
    diff_days[i]=days_360(date_fin=dates[i+1],date_ini=dates[i])}
  return(diff_days)
}
