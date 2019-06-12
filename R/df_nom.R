df_nom <- function(tasa_nom, days, base){
  df <- 1/(1+tasa_nom*days/base)
  return(df)
}
