#' Request bills based on the query parameters
#' 
#' @param bill_id character. The id of a certain bill.
#' @param bill_type character. Bill type ('hr' (house bill); 's' (senate bill); 'hres' (House resolution); 'sres' (Senate resolution); 'hcon' (House Concurrent Resolution); 'scon' (Senate Concurrent Resolution); 'hjres' (House Joint Resolution); 'sjres' (Senate Joint Resolution).
#' @param name character. The last name of a representative or senator.
#' @param offset integer. Offset of the query.
#' @param limit integer. Limit of the query.
#' @param key character. A valid API key which can be requested at .
#' 
#' @export


cbpR_bills <- function(bill_id = NULL,
                       bill_type = NULL,
                       name = NULL,
                       offset = 1,
                       limit = 2000,
                       key = Sys.getenv("CBPR_API_KEY")) {
  
  url <- "http://ec2-34-215-165-118.us-west-2.compute.amazonaws.com/api/bill"
  
  parameters <- list(
    id = bill_id,
    billType = bill_type,
    name = name, 
    key = key,
    offset = offset,
    limit = limit
  )
  
  url <- httr::modify_url(url,
                          query = parameters)
  
  
  res <- httr::GET(url)
  res <- jsonlite::fromJSON(httr::content(res, as = "text"))
  
  res
}
