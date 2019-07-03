cf_price_to_ytm <- function (date_ini, mk_price, cf_times, freq, round_val = 5, y_lim = c(-0.1, 0.2)) {

  diff_pr <- function(ytm, mk_price, cf, freq, times){
    res <- cf_price_ytm(ytm = ytm, cf = cf, freq = freq, times = times) - mk_price
    return(res)
  }

  lbonds = length(mk_price)
  ym = rep(0, lbonds)
  for (i in 1:lbonds) {
    sol = try(round(uniroot(f = diff_pr, interval = y_lim,
                          mk_price = mk_price[i], cf = cf_times$cf_list[[i]], freq = freq[i], times = cf_times$times_list[[i]],
                          tol = 10^(-round_val))$root, round_val), silent = TRUE)
    if(class(sol) != "try-error"){
      ym[i] <- sol
    }
  }
  return(ym)
}
