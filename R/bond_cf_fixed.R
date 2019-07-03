bond_cf_fixed <-function (date_ini, coupon, matur, freq, conv, issue_date)
{
  n_bonds <- length(coupon)

  cf_df <- times_df <- NULL
  cf_list <- times_list <- as.list(rep(NA,n_bonds))

  for(i in 1:n_bonds){
      model_count=switch(conv[i], "act_360"=days_act_360, "act_365"=days_act_365,"act_act"=days_act_act,"30_360"=days_30_360,"30_360I"=days_30_360I,"30_360E"=days_30_360E,"nl_365"=days_nl_365)
      model_diff=switch(conv[i], "act_360"=diff_act_360, "act_365"=diff_act_365,"act_act"=diff_act_act,"30_360"=diff_30_360,"30_360I"=diff_30_360I,"30_360E"=diff_30_360E,"nl_365"=diff_nl_365)

      per = 1
      length = 1
      if (freq[i] != 0) {
        per = 12/freq[i]
        length = as.numeric(freq[i] * (matur[i] - date_ini)/365) + 2
      }
      if (freq[i] == 0) {
        freq[i] = 1
      }
      cpn_dates = sort(unique(c(seq(matur[i], by = paste0("-", per, " months"), length = length),issue_date[i])))

      fut_cpn_dates <- cpn_dates[cpn_dates > date_ini]
      lfutcpn <- length(fut_cpn_dates)

      count <- model_count(fut_cpn_dates, date_ini)
      times_i <- count$times
      delta <- tail(model_diff(cpn_dates)$times, lfutcpn)
      if(is.na(delta) & coupon[i]!=0){
        delta = model_count(fut_cpn_dates, issue_date[i])$times}
      delta=ifelse(is.na(delta),1,delta)

      cf = coupon[i] * delta
      cf = coupon[i] * delta
      cf[lfutcpn] = cf[lfutcpn] + 1

      if(i>1){
        cf_df <- cbind.fill(cf_df, cf, fill = 0)
        times_df <- cbind.fill(times_df, times_i, fill = 1)
      }else{
        cf_df <- cf
        times_df <- times_i
      }
      cf_list[[i]] <- cf
      times_list[[i]] <- times_i
    }
  return(list(times = times_df, cf = cf_df, cf_list = cf_list, times_list = times_list))
}
