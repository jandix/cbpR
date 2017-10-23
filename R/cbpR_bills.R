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
    stop("Please provide an API key.", call. = F)
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
  parsed <- httr::content(res, as = "text")
  parsed <- jsonlite::fromJSON(parsed)
  
  # check if API return error
  if (httr::http_error(res)) {
    stop(
      sprintf(
        "API request failed [%s]\n%s",
        httr::status_code(res),
        parsed$meta$message
      )
    )
  }
  
  # return a simple S3 object including meta information and result set
  structure(
    list(
      meta = list (
        limit = parsed$meta$limit,
        offset = parsed$meta$offset,
        total_rows = parsed$meta$rows,
        url = url,
        rate_limit = list (
          limit = parsed$meta$rate_limit$limit,
          current = parsed$meta$rate_limit$current,
          remaining = parsed$meta$rate_limit$remaining
        )
      ),
      result_df = parsed$result
    ),
    class = "cbpR_api"
  )
}