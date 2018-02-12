bond_report <- function (purchase_date, nominal, coupon, matur, freq, conv, isin = NA, serie_float_rate=NULL, amort_mat = NULL, in_arrears=0, diff_fix_value = 1)
  {
    ##Esta funcion genera las tablas para los reportes.
    ## serie_float_rate: Matrix donde columna 1 es fecha, columna 2 es valor.
  ##conv=c("act_360","act_365","act_act","30_360","30_360I","30_360E","nl_365"  )
  ##si in_arrears==1, todos los flujos se proyectan con indicador actual. Si in_arrears=0, el primer flujo se proyecta con tasa previa.
  ##diff_fix_value. Días hábiles entre la fecha fix del indicador y la fecha value.

  model_count <- switch(conv, "act_360"=days_act_360, "act_365"=days_act_365,"act_act"=days_act_act,"30_360"=days_30_360,"30_360I"=days_30_360I,"30_360E"=days_30_360E,"nl_365"=days_nl_365)
  model_diff <- switch(conv, "act_360"=diff_act_360, "act_365"=diff_act_365,"act_act"=diff_act_act,"30_360"=diff_30_360,"30_360I"=diff_30_360I,"30_360E"=diff_30_360E,"nl_365"=diff_nl_365)
  coupon0=coupon

  per <- 1
  length <- 1
  if (freq != 0) {
    per <- 12/freq
    length <- as.numeric(freq * (matur - purchase_date)/365) + 2
  }
  cpn_dates_all <- sort(unique(c(seq(matur, by = paste0("-", per, " months"), length = length))))
  cpn_dates <- cpn_dates_all[cpn_dates_all > purchase_date]

  lcpn <- length(cpn_dates)
  cpn_dates_fix <- tail(cpn_dates_all,lcpn+1) %>% head(lcpn)
  cpn_rates <- coupon

  #Días y fechas relativas a la fecha de compra.
  diff_dates_pur <- model_diff(c(purchase_date, cpn_dates))
  days_pur <- diff_dates_pur$days #Diff días desde fecha de compra.

  #Días y delta cupón.
  diff_dates <- model_diff(tail(cpn_dates_all,lcpn+1)) #Días de cada flujo desde fecha de compra
  days <- diff_dates$days
  delta <- diff_dates$times

  if(is.null(serie_float_rate)){
    cf = nominal * coupon * delta
  }else{
    if(in_arrears==0){#Las tasas pagan al vencimiento y se fijan en cpn anterior.
      float_rates <- as.vector(serie_float_rate[findInterval(cpn_dates_fix, index(serie_float_rate))-diff_fix_value])
      float_cpn <- float_rates + coupon
    }
    cf <- nominal * float_cpn * delta
    cpn_rates <- float_cpn
  }

  if(any(is.na(delta)) & coupon[1]!=0){delta <- model_count(cpn_dates, issue_date)$times}
  delta <- ifelse(is.na(delta),1,delta)

  cf_cap <- rep(0,lcpn)

  if(!is.null(amort_mat)){
    pos_amort <- match(amort_mat[,1],cpn_dates)
    if(any(is.na(pos_amort))){stop("Fechas de amortización no coinciden con fechas de flujos.")}
    cf_cap[pos_amort] <- amort_mat[,2] * nominal
  }else{
    cf_cap[lcpn] <- cf_cap[lcpn] + nominal
  }
  cf_total <- cf + cf_cap
  cf_tab <- data.frame(ISIN = isin, DAYS = c(0, days_pur), EVENT = c('purchase', rep('coupon', lcpn)),
                       DATE = c(purchase_date, cpn_dates), FIX_RATE = c(0,float_rates),
                       CPN_RATE = c(0,cpn_rates), PERIOD = c(0,delta),
                       COUPON = c(0, cf), PRINCIPAL = c(0, cf_cap),
                       CASH_FLOW = c(-nominal, cf_total))

  cf_tab <- cf_tab %>% mutate(AMORTIZED_COST_OPEN = c(nominal,c(nominal - cumsum(PRINCIPAL))[-(lcpn+1)])) %>%
    mutate(AMORTIZED_COST_CLOSE = nominal - cumsum(PRINCIPAL)) %>%
    mutate(COLLECTED_INTEREST = COUPON - CASH_FLOW * (CASH_FLOW > 0)) %>%
    mutate(DIFF_VS_REAL = (COLLECTED_INTEREST != 0) * (COLLECTED_INTEREST - COUPON))

  #Tabla mensual:
  dates_m <- sort(unique(c(end_month_dates(purchase_date, matur), cpn_dates)))
  diff_dates_m <- model_diff(dates_m)
  days_m <-  diff_dates_m$days
  cf_m <- cap_amort <- rep(0, length(dates_m))
  pos_dates <- match(cf_tab$DATE,dates_m)
      cf_m[pos_dates] <- cf_tab$CASH_FLOW
    cap_amort[pos_dates] <- cf_tab$PRINCIPAL

    cum_cpn <- cf[1] - float_cpn[1] * diff_dates_pur$times[1] * nominal ##Cupón acumulado en fecha de compra.

    pos_date_fix <- sapply(dates_m[-1], function(x) ifelse(any(x<=cf_tab$DATE[-1]),min(which(x<=cf_tab$DATE[-1])),0)) + 1
    cpn_rates <- cf_tab$CPN_RATE[pos_date_fix]

    cf_tab_mon <- data.frame(ISIN = isin, DATE = dates_m) %>% mutate(TOTAL_CPN_DAYS = c(0, diff_dates_m$days)) %>%
                  mutate(ACCOUNTING_YIELD = c(cum_cpn, cpn_rates * nominal * diff_dates_m$times)) %>%
                  mutate(COUPON = ACCOUNTING_YIELD) %>%
                  mutate(CASH_FLOW = cf_m) %>% mutate(CAPITAL_AMORT = cap_amort) %>%
                  mutate(AMORTIZED_COST_OPEN = c(nominal, cumsum(c(nominal, COUPON - c(0,CASH_FLOW[-1])))[-c(1,length(dates_m))])) %>%
                  mutate(AMORTIZED_COST_CLOSE = cumsum(c(nominal, COUPON - c(0,CASH_FLOW[-1])))[-1]) %>%
                  mutate(COLLECTED_INTEREST = cumsum(COUPON - c(0,CASH_FLOW[-1]) + CAPITAL_AMORT))
    return(list(cf_tab = cf_tab, cf_tab_mon = cf_tab_mon))
}




