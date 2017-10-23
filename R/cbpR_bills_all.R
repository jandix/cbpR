#' Get all query parameters.
#' 
#' @param bill_id character. The id of a certain bill.
#' @param bill_type character. Bill type ('hr' (house bill); 's' (senate bill); 'hres' (House resolution); 'sres' (Senate resolution); 'hcon' (House Concurrent Resolution); 'scon' (Senate Concurrent Resolution); 'hjres' (House Joint Resolution); 'sjres' (Senate Joint Resolution).
#' @param name character. The last name of a representative or senator.
#' @param key character. A valid API key which can be requested at URL.
#' 
#' @export


cbpR_bills_all <- function(bill_id = NULL,
                           bill_type = NULL,
                           name = NULL,
                           key = Sys.getenv("CBPR_API_KEY")) {
  
  limit <- 2000
  offsets <- NULL
  
  res <- cbpR::cbpR_bills(bill_id = bill_id,
                          bill_type = bill_type,
                          name = name,
                          offset = 1,
                          limit = limit,
                          key = key)
  res_df <- res$result
  
  if (res$meta$rows > limit) offsets <- seq(limit, res$meta$rows, limit)
  
  for (offset in offsets) {
    res <- cbpR::cbpR_bills(bill_id = bill_id,
                            bill_type = bill_type,
                            name = name,
                            key = key,
                            limit = limit,
                            offset = offset)
    res_df <- rbind(res_df, res$result)
  }
  
  res$result <- res_df
  
  res
}
