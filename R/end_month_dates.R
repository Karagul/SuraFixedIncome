end_month_dates <- function(initial_date, final_date, freq = 12, include_final_date = TRUE){
  month_dates <- seq(initial_date, final_date, by = "month")
  l <- ifelse(as.numeric(format(tail(month_dates,1),"%m"))<as.numeric(format(final_date,"%m")), 3, 2)
  month_dates <- c(month_dates[-1], seq(tail(month_dates,1), by = "month", length = l)[2:l])

  end_dates <- dmy(paste("01-", format(month_dates,"%m-%Y")))-1
  end_dates <- end_dates[seq(1,length(end_dates), by = 12/freq)]

  if(include_final_date){all_dates <- sort(c(initial_date, final_date, end_dates))
  }else{all_dates <- sort(c(initial_date, end_dates))}

  return(all_dates)
}
