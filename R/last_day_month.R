last_day_month<-function(date_in){
  date_initial=as.Date(paste0("01/",format(date_in,"%m/%Y")),"%d/%m/%Y")
  last_day=as.numeric(format(tail(seq(date_initial, by="month",length=2),1)-1,"%d"))  
return(last_day)
}
