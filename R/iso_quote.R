#' ISO quote
#'
#' Takes id of two currencies amd generates an iso Quote.
#' @param curr1 Currency 1.
#' @param curr2 Currency 2.
#' @param base_currs Base currencies.
#' @return iso currency quote.
#' @export

iso_quote <-function(curr1, curr2 = NULL, base_currs = c("NZD", "AUD", "EUR", "GBP")){

  if(is.null(curr2)){
    iso <- ifelse(curr1 %in% base_currs, paste0(curr1, 'USD'), paste0('USD', curr1))
  }else{
    iso <- paste0(curr1, curr2)
    if(curr1 %in% base_currs){iso <- paste0(curr1, curr2)
    }else if (curr2 %in% base_currs){iso <- paste0(curr2, curr1)
    }else{
      if(curr1 == "USD"){iso <- paste0(curr1, curr2)
      }else if (curr2 == 'USD'){iso <- paste0(curr2, curr1)}
    }
  }
  return(iso)
}
