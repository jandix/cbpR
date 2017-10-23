#' Request automatically all bills based on the query parameters
#' 
#' @param key character. A valid API key which can be requested at .
#' @param id character. The id of a certain bill.
#' @param name character. The last name of a representative or senator.
#' 
#' @export


cbpR_bills <- function(bill_id = NULL,
                       bill_type = NULL,
                       name = NULL,
                       key = Sys.getenv("CBPR_API_KEY")) {
  
  res <- cbpR::cbpR_bills(bill_id = bill_id,
                          bill_type = bill_type,
                          name = name,
                          offset = 1,
                          limit = 2000,
                          key = key)
  
  
  i <- res$meta$rows %/% 2000 + 1
  res_df <- res$result
  
  for (offset in 2:i) {
    res <- cbpR::cbpR_bills(bill_id = bill_id,
                            bill_type = bill_type,
                            name = name,
                            key = key,
                            limit = 2000,
                            offset = offset)
    res_df <- rbind(res_df, res$result)
  }
  
  
  res <- httr::GET(url)
  res <- jsonlite::fromJSON(httr::content(res, as = "text"))
  
  list(res)
}
