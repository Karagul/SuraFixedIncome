days_365 <-
function (date_fin, date_ini,...) 
{
  day_count = as.numeric(date_fin - date_ini)
  years = as.numeric(format(c(date_ini, date_fin), "%Y"))
  ref1 = as.Date(paste0(years[1], "02", "29"), format = "%Y%m%d")
  ref2 = as.Date(paste0(years[2], "02", "29"), format = "%Y%m%d")
  ajuste1=0;
  if (is.na(ref1) & !is.na(ref2) & date_fin > ref2) {    ajuste1 = -1
  } else if (!is.na(ref1) & is.na(ref2) & date_ini < ref1) {
    ajuste1 = -1
  }  else if (!is.na(ref1) & !is.na(ref2) & date_ini < ref1 & 
             date_fin > ref2 & years[1] == years[2]) {
    ajuste1 = -1
  }  else if (!is.na(ref1) & !is.na(ref2) & date_ini < ref1 & 
             date_fin > ref2 & years[1] != years[2]) {
    ajuste1 = -2
  }
  years_int = c(years[1]:years[2])[c(-1, -length(c(years[1]:years[2])))]
  ajuste2 = -sum(leap_year(years_int))
  day_count_365 = day_count + ajuste1 + ajuste2
  return(day_count_365)
}
