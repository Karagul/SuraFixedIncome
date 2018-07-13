leap_year <-
function (year) {
  ly = (ifelse((year%%4 == 0 & year%%100 != 0) | year%%400 == 0, 366, 365) == 366)
  return(ly)
}
