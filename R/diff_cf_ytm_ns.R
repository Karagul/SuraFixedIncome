

diff_cf_ytm_ns <- function (betas, date_ini, ytm, cf_times, freq, round_val = 5, y_lim=c(0,20))
{
  pr = cf_price_ns(betas, cf_times$cf, cf_times$times)
  ytm_ns = cf_price_to_ytm(date_ini, pr, cf_times, freq, round_val, y_lim)
  aux = (ytm - ytm_ns)
  aux = as.numeric(crossprod(aux))
  return(aux)
}
