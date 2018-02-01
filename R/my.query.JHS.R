#' !!!!!!! Fix an issue with The Jackson Heart Study Cohort on Topmed-dev
#' Query analysis to the API
#'
#' Given a vector with the fields of interest, and the vector generated with the paths obtained 
#' after applying the getchildren function, it returns a JSON query
#'
#' @param variables  A vector with the variables of interest
#' @param pathways  A vector with the paths of interest, the function will apply the \code{getchildren}
#' function
#' @param url  The url.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.
#' @return A JSON query. 
#' @author Alba Gutierrez, Gregoire Versmee, Gabor Korodi
#' @examples
#' query
# # fieldname1  <- "/nhanes/Demo/laboratory/laboratory/pcbs/"
# # fieldname2  <- "/nhanes/Demo/demographics/demographics/"
#  
# # queryExample <- my.query( variables = c("AGE", "PCB153")
# #                           pathways  = c(fieldname1, fieldname2),
# #                           url       = "https://nhanes.hms.harvard.edu/"
# #              )
#' @export my.query.JHS

my.query.JHS <- function(variables, pathways, url, verbose = FALSE) {
  
  
  if( verbose == TRUE){
    message(" Creating a list with the path from the vector list
            which contains all available paths for the resource")
  }
   
  # Use get.children() to build the path list 
  pathList <- c()
  for (i in 1:length(pathways))  { 
      pathways[i] <- URLencode(pathways[i])
      children <- get.children(pathways[i], url)
      pathList <- c(pathList, children)
  }
  
  myfields <- ""
  for (i in 1:(length(variables)-1))  {
      myfields <- paste0(myfields,variables[i], "|")
  }
  
  myfields <- paste0(myfields, variables[i+1])
  
  # Filter fields by values in vector using grep
  pathList <- grep(myfields, pathList, value=TRUE)
  
  
  if( verbose == TRUE){
    message(" Getting all the fields available from the pathlist")
  }
  
  # Create vector for query
  querySELECT<- c()
  
  # for each entry in filtered path list:
  #     create a field entry in the query
  for (i in 1:length(pathList))
  {
    if( verbose == TRUE ){
      # message( pathList[i] )
      message( "Get the fields for this particluar path" )
    }
    
    IRCT_REST_BASE_URL <- url
    IRCT_CL_SERVICE_URL <- paste(IRCT_REST_BASE_URL,"rest/v1/",sep="")
    IRCT_RESOURCE_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"resourceService/",sep="")
    IRCT_PATH_RESOURCE_URL <- paste(IRCT_RESOURCE_BASE_URL,"path",sep="")
    
    # format URL for current path entry to retrieve fields       
    nurlstr <- paste( IRCT_PATH_RESOURCE_URL, pathList[i], sep = "" )
    nurl <- gsub( "\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub("[(]","%28", URLencode(nurlstr)))))
    
    # httr::content(httr::GET(nurl))
    
    # get children of current path entry
    pathFields <- httr::content(httr::GET(nurl))
    # message(pathFields)
    
    if( verbose == TRUE ){
      message( "Generating {field} object for SELECT portion of the query" )
    }
    
    # if there were children, add a field entry to the query for each child path
    if (length(pathFields)>0) {
      for (j in 1:length(pathFields)) {
        # message("pathFields[j] : ", pathFields[j])
        
        myField <- list(
          list(
            field=list(
              pui = jsonlite::unbox(pathFields[[j]]$pui),
              dataType = jsonlite::unbox(pathFields[[j]]$dataType)
            ),
            alias = structure(strsplit(jsonlite::unbox(pathList[i]), "/")[[1]][length(unlist(strsplit(jsonlite::unbox(pathList[i]), "/")))], class = class(jsonlite::unbox(pathList[i])))
            )
          )
        querySELECT <- c( querySELECT, ( myField ) )
      }
    } else {
      
      pathSegs <- unlist(strsplit(pathList[[i]],"/"))
      leafnurlstr <- str_c( pathSegs[1:length(pathSegs)-1] , collapse = "/" )
      # message("leafnurlstr")
      # message(leafnurlstr)
      leafnurl <- gsub( "\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub("[(]","%28", URLencode(leafnurlstr)))))
      # message("leafnurl")
      # message(leafnurl)
      leafPathUrl <- paste0(IRCT_PATH_RESOURCE_URL, leafnurl, "/")
      # message(leafPathUrl)
      leafPathFields <- httr::content(httr::GET(leafPathUrl))
      
      leafPath <- grep(pathList[[i]], leafPathFields, value=TRUE)
      # message("leafPath")
      # message(leafPath)
      
      
      # get children of current path entry
      pathFields <- httr::content(httr::GET(nurl))
      # message("pathFields")
      # message(pathFields)
      
      entry <- NULL
      for(index in 1:length(leafPathFields)){
        if(leafPathFields[[index]]$pui==pathList[[i]]){
          entry <- leafPathFields[[index]]
        }
      }
      # message(entry)
      myField <- list(
        list(
          field=list(
            pui = jsonlite::unbox(entry$pui),
            dataType = jsonlite::unbox(entry$dataType)
          ),
          alias=structure(strsplit(jsonlite::unbox(entry$pui), "/")[[1]][length(unlist(strsplit(jsonlite::unbox(entry$pui), "/")))], class = class(jsonlite::unbox(entry$pui)))
        )
      )
      querySELECT <- c( querySELECT, ( myField ) )
    }
  }
  
  if( verbose == TRUE ){
    message( "Generating the WHERE portion of the query, from the first path selected" )
  }
  
  queryWHERE <- c()
  
  field <- unlist(strsplit(myfields, "[|]"))[1]
  whereClause <- grep(field, pathList, value=TRUE)
  
  
  # Assuming STRING variable, but not sure
  mylist      <- list( pui      = jsonlite::unbox( whereClause ),
                       dataType = jsonlite::unbox( "STRING" ) )
  
  queryWHERE  <- list( field     = mylist,
                       predicate = jsonlite::unbox( "CONTAINS" ),
                       fields    = list( ENOUNTER = jsonlite::unbox( "NO" )))
  
  querySTRING <- list( select = querySELECT,
                       where  = list( queryWHERE ) )
  
  query <- jsonlite::toJSON(querySTRING, pretty=TRUE)
  
  
  # run the query
  
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
  resultStatus <- httr::content(httr::GET(paste(IRCT_RESULTS_BASE_URL, 'resultStatus/', resultId, sep='')))$status
  while(resultStatus != 'AVAILABLE') {
      if(resultStatus == 'ERROR'){
          message("Query Failed")
          break
      }else{
          Sys.sleep(2)
          resultStatus <- httr::content(httr::GET(paste(IRCT_RESULTS_BASE_URL, 'resultStatus/', resultId, sep='')))$status
          if (verbose == TRUE) {
              message(paste("...", resultStatus, sep=''))
          }
      }
  }
  
  response <- suppressMessages(httr::content(httr::GET(paste(IRCT_GET_RESULTS_URL, result$resultId, "CSV", sep = "/")), as = "text"))
  results <- read.csv(text = response, na.strings = "", check.names = FALSE)
  
  if (nrow(results) == 0) {
      message("There are not results for your query")
      stop()
  }
  
  # order the columns according to the order of the  variable you asked for
  order <- c(1)
  for (i in 1:length(variables))  order <- c(order, grep(variables[i], colnames(results)))
  results <- results[order]
  
  # make column names
  cnames <- colnames(results)
  cnames <- gsub("\\(", ".", gsub("\\)", ".", gsub(" ", "_", cnames)))
  for (i in 1:length(cnames))  {
      if (grepl("0|1|2|3|4|5|6|7|8|9", substr(cnames[i], 1, 1)))  cnames[i] <- paste0("x", cnames[i])
  }
  cnames[1] <- "patient_id"
  colnames(results) <- cnames
  
  return(results)
}
  
