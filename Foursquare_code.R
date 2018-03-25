# FOURSQUARE ------------------------------------------------------------------

# the first part of the file retrieves the data from the Foursquare API
# using the centroids of the census tracts and a huge radius around it

# the second part plots the distribution across a map of NY

# PART 1 ----------------------------------------------------------------------

library(httr)
library(RPostgreSQL)

# get the centroids
con1 = dbConnect(dbDriver("PostgreSQL"), 
                  dbname = "crime-data", 
                  user = "root", 
                  host = "localhost")
centroids = dbGetQuery(con1, "SELECT * FROM nyct2010_centroids;")

# set identification for Foursquare

# removed on Github
clientid ="redacted"
clientsecret = "redacted"

# Foursquare category IDS
category_ids = c("4d4b7104d754a06370d81259", "4d4b7105d754a06372d81259", 
                  "4d4b7105d754a06374d81259", "4d4b7105d754a06376d81259",
                  "4d4b7105d754a06377d81259", "4d4b7105d754a06375d81259",
                  "4e67e38e036454776db1fb3a", "4d4b7105d754a06378d81259",
                  "4d4b7105d754a06379d81259")

# input for the queries: centroids IDs plus coordinates
query_input = expand.grid(centroids$gid, category_ids)
query_input$lat = rep(centroids$lat, 9)
query_input$long = rep(centroids$long, 9)

# sample the rows
query_input = query_input[sample(nrow(query_input)),]

# make GET calls retry X times
GET_retry = function(url, ..., times = 3) {
  res = suppressWarnings(GET(url, ...))
  if (res$status_code > 226) {
    stat = 500
    while (stat > 226) {
      res = suppressWarnings(GET(url, ...))
      stat = res$status_code
    }
  }
  return(res)
}

# make call to API with all query inputs j times
for (j in 1:30){
  # preallocate results
  venue_id = list()
  venue_lat = list()
  venue_long = list()
  venue_category = list()
  
  # set current date
  today_date = format(Sys.Date(),"%Y%m%d")
  
  # use query input
  for (i in 1:nrow(query_input)) {
    lat = query_input$lat[i]
    long = query_input$long[i]
    category = as.character(query_input$Var2[i])
    
    # Do query and parse results
    query = paste0("https://api.foursquare.com/v2/venues/search?categoryId=",category,"&limit=50&intent=browse&client_id=",clientid,"&client_secret=",clientsecret,"&ll=",lat,",",long,"&radius=2000&v=",today_date)
    result = GET_retry(query, times = 30)
    result_object = content(result) #fromJSON(result)
    
    if (length(result_object$response$venues) > 0){
      for (r in 1:length(result_object$response$venues)) {
        tmp = result_object$response$venues[[r]]
        venue_id[[i]] = tmp$id
        venue_lat[[i]] = tmp$location$lat
        venue_long[[i]] = tmp$location$lng
        venue_category[[i]] = category
      }
    }

  }
  # save results
  final_result = data.frame(cbind(unlist(venue_id), 
                                   unlist(venue_lat), 
                                   unlist(venue_long), 
                                   unlist(venue_category)))
  # remove potential duplicates
  final_result = subset(final_result, !duplicated(final_result))
  
  # save results
  save(final_result, file = paste0("newrun", j, ".RData"))
}

# TRANSFORM DATA --------------------------------------------------------------

# load data again
ls_data = list()

for (i in 1:30){
  load(paste0("newrun", i, ".Rdata"))
  ls_data[[i]] = final_result
}

# remove duplicates
f4s_frame = do.call("rbind", ls_data) 
f4s_frame = f4s_frame[!duplicated(f4s_frame),]
# 55817 unique POI

# save as CSV file
# write.csv(f4s_frame, file = "D:/crime-data/Foursquare/f4s_data.csv")

# the final data was transformed in file data_preparation.

# PLOTTING FOURSQUARE ---------------------------------------------------------

# again, the NY shapefile is loaded and used to plot the different 
# distributions of POI venues

library(ggplot2)
library(viridis)  # for map colours
library(sp)
library(rgdal)
library(mapproj)  # necessary to ensure consistent coordinate mapping
library(grid)
library(scales)

# load function to save map
source("helper_visualisation.R")

# load feature function
source("feature_helperfunctions.R")

# load final data
load("foursquare_complete.Rdata")

# read shapefile and match to analysis tracts
census = fread("D:/crime-data/2010_NYC_Census_cleaned.csv")
tracts = sp:::spTransform(readOGR("D:/crime-data/nyc-taxi-data/nyct2010_15b/nyct2010.shp",
                                   layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))

# fortify as data.frame
tracts@data$id = as.numeric(rownames(tracts@data)) + 1
NY_layer = fortify(tracts, region = "id")
NY_layer$id = as.numeric(NY_layer$id)

# match to ID_mapping
NY_layer = subset(NY_layer, NY_layer$id %in% census$gid & !(NY_layer$id %in% c(1263, 2023, 2116)))
rm(tracts)

# remove census tracts not in the analysis but in the map
census = census[rowSums(is.na(census)) == 0,]

# transform foursquare data
foursquare = foursquare_aggregation(foursquare)

# subset to relevant census tracts
foursquare = subset(foursquare, foursquare$gid %in% census$gid)

# join map data and plot data
NY_layer = left_join(NY_layer, foursquare, by = c("id" = "gid"))

# this function creates a plot for each Foursquare category and saves it automatically
POI_plots(NY_layer, colnames(foursquare)[-1])


