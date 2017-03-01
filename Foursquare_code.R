# This file retrieves Foursquare API calls to get locations around the census tracts
# run several times to circumvent the result limit by Foursquare

library(httr)
library(RPostgreSQL)

#options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

con1 <- dbConnect(dbDriver("PostgreSQL"), dbname = "crime-data", user = "root", host = "localhost")
centroids <- dbGetQuery(con1, "SELECT * FROM nyct2010_centroids;")

# 4SQ identification
# remove on Github
clientid <-"GFHY5FNLDIAHLGFRBSFRNNF3FCJ11BWCMMUSFE4JWMJ5CREL"
clientsecret <- "ENAB54Y51RH3L53X3P24SXE3PHDXVID2FRXKM0QTNNO5QQQH"

today_date <- format(Sys.Date(),"%Y%m%d")

# 4SQ Category IDS
category_ids <- c("4d4b7104d754a06370d81259", "4d4b7105d754a06372d81259", "4d4b7105d754a06374d81259", "4d4b7105d754a06376d81259",
                  "4d4b7105d754a06377d81259","4d4b7105d754a06375d81259","4e67e38e036454776db1fb3a","4d4b7105d754a06378d81259","4d4b7105d754a06379d81259")

# new query_input
query_input <- expand.grid(centroids$gid, category_ids)
query_input$lat <- rep(centroids$lat, 9)
query_input$long <- rep(centroids$long, 9)

GET_retry <- function(url, ..., times = 3) {
  res <- suppressWarnings(GET(url, ...))
  if (res$status_code > 226) {
    stat <- 500
    while (stat > 226) {
      res <- suppressWarnings(GET(url, ...))
      stat <- res$status_code
    }
  }
  return(res)
}

for (j in 1:30){
  venue_id = list()
  venue_lat = list()
  venue_long = list()
  venue_category = list()
  today_date <- format(Sys.Date(),"%Y%m%d")
  
  
  for (i in 6424:nrow(query_input)) {
    lat <- query_input$lat[i]
    long <- query_input$long[i]
    category <- as.character(query_input$Var2[i])
    
    # Do query and parse results
    query <- paste0("https://api.foursquare.com/v2/venues/search?categoryId=",category,"&intent=browse&client_id=",clientid,"&client_secret=",clientsecret,"&ll=",lat,",",long,"&radius=2000&v=",today_date)
    result <- GET_retry(query)
    result_object <- content(result) #fromJSON(result)
    
    if (length(result_object$response$venues) > 0){
      for (r in 1:length(result_object$response$venues)) {
        tmp <- result_object$response$venues[[r]]
        venue_id[[i]] <- tmp$id
        venue_lat[[i]] <- tmp$location$lat
        venue_long[[i]] <- tmp$location$lng
        venue_category[[i]] <- category
      }
    }
  }
  final_result <- data.frame(cbind(unlist(venue_id), unlist(venue_lat), unlist(venue_long), unlist(venue_category)))
  final_result <- subset(final_result, !duplicated(final_result))
  
  save(final_result, file = paste0("newrun", j, ".RData"))
}

ls_data <- list()

for (i in 1:30){
  load(paste0("newrun", i, ".Rdata"))
  ls_data[[i]] <- final_result
}

f4s_frame <- do.call("rbind", ls_data) 
f4s_frame <- f4s_frame[!duplicated(f4s_frame),]

write.csv(f4s_frame, file = "D:/crime-data/Foursquare/f4s_data.csv")
