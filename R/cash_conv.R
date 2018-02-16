#' Cash converter.
#'
#' Takes amount in some currency and converts ir to anoher currency.
#' @param cash_in Input amount.
#' @param curr_in Currency of input amount.
#' @param spot Spot rate.
#' @param spot_id Iso currency code.
#' @return Converted amount.
#' @export

cash_conv <-function(cash_in, curr_in, spot, spot_id){
  cash_out=NA
  if(substr(spot_id,1,3)==substr(spot_id,4,6)){
    cash_out <- cash_in
  }else{
    if(curr_in==substr(spot_id,1,3)){
      cash_out=cash_in*spot
    }
    if(curr_in==substr(spot_id,4,6)){
      cash_out=cash_in/spot
    }
  }
  return(cash_out)
}
