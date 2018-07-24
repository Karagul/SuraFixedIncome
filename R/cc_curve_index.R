
cc_curve_index <- function(cc_hist, index_ini = 1000, dates = NULL, target_matur_in_days = 1095, bond_freq = 2, base = 365, slippage = 5){

  cc_curve <- na.omit(cc_hist[, as.numeric(colnames(cc_hist)) <= target_matur_in_days])
  if(!is.null(dates)){
    cc_curve <- cc_curve[paste(dates, collapse = '/')]
  }
  cc_dates <- index(cc_curve)
  ini_date <- cc_dates[1]
  last_date <- tail(cc_dates, 1)
  target_mat <- target_matur_in_days/base

  per <- 1/bond_freq
  cpn_days <- round(seq(per, target_mat, per) * 365)
  cpn_times <- cpn_days/base
  ldates <- length(cc_dates)
  days <- as.numeric(colnames(cc_curve))
  index_val <- rep(0, ldates)
  index_val[1] <- index_ini

  ytm <- function(x, cpn_times, cpn_rates){
    df <- 1/((1 + cpn_rates) ** cpn_times)
    lcpn <- length(cpn_times)
    cf <- rep(x, lcpn) * c(cpn_times[1], diff(cpn_times))
    cf[lcpn] <- cf[lcpn] + 1
    obj <- sum(cf * df) - 1
    return(obj)
  }

  rates <- as.vector(cc_curve[1])/100
  cpn_rates <- approxExtrap(x = days, y = rates, xout = cpn_days)$y
  par_rate <- uniroot(ytm, interval = c(0,0.2), cpn_times, cpn_rates)$root
  mac_dur <- rep(0, length(cc_dates))

  for(i in 2:ldates){
    delta_days <- as.numeric(cc_dates[i] - cc_dates[i-1])
    cpn_daysi <- cpn_days - delta_days

    rates <- as.vector(cc_curve[i])/100
    cpn_ratesi <- approxExtrap (x = days, y = rates, xout = cpn_daysi)$y
    cpn_timesi <- cpn_daysi/base
    lcpn <- length(cpn_times)
    cf <- rep(par_rate, lcpn) * c(cpn_times[1], diff(cpn_times))
    cf[lcpn] <- cf[lcpn] + 1
    df <- 1/((1 + cpn_ratesi) ** cpn_timesi)
    bond_pr <- sum(cf * df)

    bond_exec_pr <- bond_pr + bond_pr * slippage/10000
    tc <- index_val[i-1] * (bond_exec_pr - bond_pr)
    index_val[i] <- index_val[i-1] * bond_pr - tc

    tdf <- cpn_timesi * df
    mac_dur[i] <- round(-sum(cf * tdf)/bond_pr, 3)

    cpn_rates <- approxExtrap(x = days, y = rates, xout = cpn_days)$y
    par_rate <- uniroot(ytm, interval = c(0,0.3), cpn_times, cpn_rates)$root
  }
  index_series <- xts(index_val, order.by = index(cc_curve))
  mac_dur <- xts(mac_dur, order.by = index(cc_curve))
  return(list(index_series=index_series, mac_dur=mac_dur[-1]))
}
