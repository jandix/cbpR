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
  
  # check if an api key is provided
  if(is.null(key)) {
    stop("Please provide an API call.", call. = F)
  }
  
  # define base url
  url <- "http://ec2-34-215-165-118.us-west-2.compute.amazonaws.com/api/bill"
  
  # define parameters
  parameters <- list(
    id = bill_id,
    billType = bill_type,
    name = name, 
    key = key,
    offset = offset,
    limit = limit
  )
  
  # joining url using base url and parameters
  url <- httr::modify_url(url,
                          query = parameters)
  
  # GET request 
  res <- httr::GET(url)
  
  # check if correct response type
  if (httr::http_type(res) != "application/json") {
    stop("API did not return JSON", call. = F)
  }
  
  # parse json result to R Object
  res <- httr::content(res, as = "text")
  res <- jsonlite::fromJSON(res)
  
  # return list including meta information and result set
  res
}
