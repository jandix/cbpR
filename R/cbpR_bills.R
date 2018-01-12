#' Request bills based on the query parameters
#' 
#' \code{cbpR_bills} returns US Congressional Bills depending on the provided query. The function uses the API of the extensive Congressional Bills Project. Information about the Congressional Bills Project can be found on the official website: \url{http://congressionalbills.org}.
#' 
#' @param q character. The search query. See \url{https://cbapi.dataflood.de/documentation/endpoints/bills.html} for detailed description.
#' @param offset integer. Offset of the query.
#' @param limit integer. Limit of the query.
#' @param order character. 
#' @param key character. A valid API key which can be requested at \url{https://cbapi.dataflood.de/start}.
#' 
#' @return The output is a cbpR API object. Which includes meta information about the query and the respective data set that is defined in the parameter \code{q}. 
#' 
#' @example cbpR_bills("NameLast=Washington", limit = 100)
#' 
#' @export


cbpR_bills <- function(q = NULL,
                       offset = 1,
                       limit = 2000,
                       order = NULL,
                       key = Sys.getenv("CBPR_API_KEY")) {
  
  # check if an api key is provided
  if(is.null(key)) {
    stop("Please provide an API key.", call. = F)
  }
  
  # check if query is provided
  if(is.null(q)) {
    stop("Please provide a query.", call. = F)
  }
  
  # define base url
  url <- "https://cbapi.dataflood.de/api/bills"
  
  # define parameters
  parameters <- list(
    q = q, 
    offset = offset,
    limit = limit,
    order = order,
    key = key
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