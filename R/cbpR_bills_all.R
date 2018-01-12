#' Request all bills based on the query parameters
#' 
#' \code{cbpR_bills} returns all US Congressional Bills depending on the provided query regardless of the number of rows. The function uses the API of the extensive Congressional Bills Project. Information about the Congressional Bills Project can be found on the official website: \url{http://congressionalbills.org}.
#' 
#' @param q character. The search query. See \url{https://cbapi.dataflood.de/documentation/endpoints/bills.html} for detailed description.
#' @param order character. 
#' @param key character. A valid API key which can be requested at \url{https://cbapi.dataflood.de/start}.
#' 
#' @return The output is a cbpR API object. Which includes meta information about the query and the respective data set that is defined in the parameter \code{q}. 
#' 
#' @example cbpR_bills_all("Cong=93&BillType=hr")
#' 
#' @export


cbpR_bills_all <- function(q = NULL,
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
  
  
  limit <- 2000
  offsets <- NULL
  
  res <- cbpR::cbpR_bills(q = q,
                          limit = 2000,
                          order = order,
                          key = key)
  res_df <- res$result
  
  if (res$meta$total_rows > limit) offsets <- seq(limit, res$meta$total_rows, limit)
  
  for (offset in offsets) {
    res <- cbpR::cbpR_bills(q = q,
                            offset = offset,
                            limit = limit,
                            order = order,
                            key = key)
    res_df <- rbind(res_df, res$result)
    # stop function if rate limit exceeded
    if (res$meta$rate_limit$remaining < 1) {
      stop("The rate limit is reached.", call. = F)
    }
  }
  
  res$result <- res_df
  
  structure(
    list(
      meta = list (
        limit = res$meta$limit,
        offset = NA,
        total_rows = res$meta$total_rows,
        url = url,
        rate_limit = list (
          limit = res$meta$rate_limit$limit,
          current = res$meta$rate_limit$current,
          remaining = res$meta$rate_limit$remaining
        )
      ),
      result_df = res_df
    ),
    class = "cbpR_api"
  )
}
