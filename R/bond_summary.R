bond_summary <- function(curr_date, purchase_date, price_series, cf_tab_mon_array, id){

  #cf_tab_mon_array: Array de cuadros de marcha asociados a cada instrumento
  #price_series: Serie de precio de instrumento.
  cf_tab_mon <- cf_tab_mon_array

  dates_m <- end_month_dates(purchase_date, curr_date, include_final_date = FALSE)
  pos_cf_tab <- match(dates_m, cf_tab_mon$DATE)

  summary_tab <- data.frame(DATE = dates_m) %>%
                 mutate(PRICE = as.vector(price_series[findInterval(DATE, index(price_series))]/100)) %>%
                 mutate(MK_VALUE = nominal * PRICE) %>%
                 mutate(AMORTIZED_COST = cf_tab_mon$AMORTIZED_COST_CLOSE[pos_cf_tab]) %>%
                 mutate(INT_DEV = c(0, sapply(unique(format(dates_m, "%m%y")),
                        function(x) sum(cf_tab_mon$COUPON[format(cf_tab_mon$DATE,"%m%y")==x])))) %>%
                 mutate(INT_CF = c(0, sapply(unique(format(dates_m, "%m%y")),
                        function(x) sum(cf_tab_mon$CASH_FLOW[-1][format(cf_tab_mon$DATE,"%m%y")==x])))) %>%
                 mutate(ACCOUNT_SSN = cumsum(c(nominal, INT_DEV-INT_CF))[-1]) %>%
                 mutate(RESULT = MK_VALUE - ACCOUNT_SSN)

  debit_credit <- summary_tab %>% filter(DATE == curr_date) %>% select(RESULT, INT_DEV)

  report_tab <- data.frame("Imputación" = c(102061934, 401100701, 102061934, 401100701)) %>%
                mutate("nombre de cuenta" = c(id, "Actualización" , id, "Intereses T.P (renta) (Cr)")) %>%
                mutate("Débito" = c(ifelse(debit_credit[1,1]>0,debit_credit[1,1],0), ifelse(debit_credit[1,1]<0,debit_credit[1,1],0),
                                        ifelse(debit_credit[1,2]>0,debit_credit[1,2],0), ifelse(debit_credit[1,2]<0,debit_credit[1,2],0))) %>%
                mutate("Crédito" =Débito[c(2,1,4,3)])

  cnames <- c("cm",	"debe ME", "haber ME", "cambio", "cod_anal", "descripc", "canal prod", "cod depto", "cod linea", "cod secc", "cod subsecc", "cod grupo", "canal contable")
  for(cn in cnames){
    report_tab[,cn] = ""
  }
  return(list(summary_tab = summary_tab, report_tab = report_tab))
}
