#' Request bills based on the query parameters
#' 
#' @param key character. A valid API key which can be requested at .
#' @param id character. The id of a certain bill.
#' @param name character. The last name of a representative or senator.

cbpR_bills <- function(key, id = NULL, name = NULL) {
  
  url <- "http://ec2-52-43-166-248.us-west-2.compute.amazonaws.com/api/bill?key=pOg8jYyRJo5aZAO523o3AG7e2Q3E4Dvn&name=Brown"
  
  
  res <- httr::GET(url)
  res <- jsonlite::fromJSON(httr::content(res, as = "text"))
  
  list(res)
}