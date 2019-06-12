df_cont=function(tasa_cont, days, base){
  df=exp(-tasa_cont*days/base)
  return(df)
}
