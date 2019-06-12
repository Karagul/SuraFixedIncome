df_efec=function(tasa_efec, days, base){
  df <- 1/((1+tasa_efec)^(days/base))
  return(df)
}
