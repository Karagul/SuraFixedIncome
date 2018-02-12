diff_act_act <-
function(dates) {
  ldates=length(dates)
  dd=dt=NA
  if(ldates>1){
  dd=dt=rep(0,ldates-1)
  for(i in c(1:(ldates-1))){
    res=days_act_act(date_fin=dates[i+1],date_ini=dates[i])
    dd[i]=res$days
    dt[i]=res$times
  }}
  return(list(days=dd, times=dt))}
