#' Query analysis to the API
#'
#' Given an url and a JSON object, it generates a \code{data.frame} object with 
#' the output of the query. 
#'
#' @param query A JSON query, created with my.query function or contained in a
#' text file. 
#' @param url  The url.
#' @param verbose By default \code{TRUE}.
#' @return An object of class \code{data.frame} with the query output. 
#' @examples
#' 
#' #query <- my.data( 
#' #              query  = system.file("extdata", "jsonQueryNhanes", package="Rcupcake"), 
#' #              url    = "https://nhanes.hms.harvard.edu/"
#' #              )
#' @author Alba Gutierrez, Gregoire Versmee
#' @export run.query

run.query <- function( query, url, verbose = TRUE ){
  
  IRCT_REST_BASE_URL <- url
  IRCT_CL_SERVICE_URL <- paste(IRCT_REST_BASE_URL,"rest/v1/",sep="")
  
  IRCT_QUERY_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"queryService/",sep="")
  IRCT_RESULTS_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"resultService/",sep="")
  
  IRCT_RUN_QUERY_URL <- paste(IRCT_QUERY_BASE_URL,"runQuery",sep="")
  IRCT_GET_RESULTS_STATUS_URL <- paste(IRCT_RESULTS_BASE_URL,"resultStatus",sep="")
  IRCT_GET_RESULTS_FORMATS_URL <- paste(IRCT_RESULTS_BASE_URL,"availableFormats",sep="")
  IRCT_GET_RESULTS_URL <- paste(IRCT_RESULTS_BASE_URL,"result",sep="")
  
  if( class(query) == "json"){
    body <- query
  }else{
    body <- paste(readLines(query), collapse = "")
  }
  
  result <- httr::content(httr::POST(IRCT_RUN_QUERY_URL, 
                                     body = body))
  if( class(result) != "list" ){
    message("Please revise the connection to the url of interest")
    stop()
  }
  
  if (result$resultId == "null") {
    message("Please, revise your query object. Query cannot be run.")
    stop()
  }
  if (verbose == TRUE) {
    message(paste("Your request is being processed, Query #", result$resultId))
  }
  
  resultId <- result$resultId
  resultStatus <- content(GET(paste(IRCT_RESULTS_BASE_URL, 'resultStatus/', resultId, sep='')))$status
  while(resultStatus != 'AVAILABLE') {
    if(resultStatus == 'ERROR'){
      message("Query Failed")
      break
    }else{
      Sys.sleep(2)
      resultStatus <- content(GET(paste(IRCT_RESULTS_BASE_URL, 'resultStatus/', resultId, sep='')))$status
      if (verbose == TRUE) {
        message(paste("...", resultStatus, sep=''))
      }
    }
  }
  
  response <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_FORMATS_URL, 
                                            result$resultId, sep = "/")))
  
  responseFormat <- "CSV"
  if( ! responseFormat %in% response ){
    message( "Sorry, the", "is not available for this query.")
  }
  
  response <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_URL, 
                                            result$resultId, responseFormat, sep = "/")), as = "text")
  results <- read.csv(text = response, na.strings = "")
  
  if (nrow(results) == 0) {
    message("There are not results for your query")
    stop()
  }
  colnames(results)[1] <- "patient_id"
  return(results)
}