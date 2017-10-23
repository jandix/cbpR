#' Request bills based on the query parameters
#' 
#' @param key character. A valid API key which can be requested at .
#' @param id character. The id of a certain bill.
#' @param name character. The last name of a representative or senator.
#' @param offset integer.
#' @param limit integer.
#' @param key character.
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
  
  list(res)
}
