# Taxi visualisation ----------------------------------------------------------

# this file loads pickup and dropoff data from the SQL data base
# and plots the coordinates of each on a map of NYC


# load required libraries
library(ggplot2)
library(data.table)
library(sp)
library(rgdal)
library(RPostgreSQL)
library(dplyr)
library(dtplyr)

source("helper_visualisation.R")

# load map
# load census file to limit subset shapefile
census = fread("D:/crime-data/2010_NYC_Census_cleaned.csv", stringsAsFactors = F)

# load shapefile and project
tracts = sp:::spTransform(readOGR("D:/crime-data/nyc-taxi-data/nyct2010_15b/nyct2010.shp",
                                   layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))

# fortify as data.frame
tracts@data$id = as.numeric(rownames(tracts@data)) + 1
NY_layer = fortify(tracts, region = "id")
NY_layer$id = as.numeric(NY_layer$id)

# subset to relevant areas
NY_layer = subset(NY_layer, NY_layer$id %in% census$gid & !(NY_layer$id %in% c(1263, 2023, 2116)))
rm(tracts)

# subset census to relevant areas
census = census[rowSums(is.na(census)) == 0,]

# load taxi data for one week (the full set cannot be plotted due to memory limits)
con2 = dbConnect(dbDriver("PostgreSQL"), 
                  dbname = "nyc-taxi-data", 
                  user = "root", 
                  host = "localhost")

pickups = dbGetQuery(con2, "SELECT * FROM plot_data_pickup_small"); setDT(pickups)
dropoffs = dbGetQuery(con2, "SELECT * FROM plot_data_dropoff_small"); setDT(dropoffs)

# subset taxi trips to analysis units
pickups = subset(pickups, pickups$pickup_nyct2010_gid %in% census$gid & 
                    pickups$dropoff_nyct2010_gid %in% census$gid)
dropoffs = subset(dropoffs, dropoffs$pickup_nyct2010_gid %in% census$gid & 
                     dropoffs$dropoff_nyct2010_gid %in% census$gid)

# create plot of pickups and dropoffs, turn off "save" if only interested in plotting
drop = taxi_map(mapdata = NY_layer, taxidata = pickups, type = "pickup", save = T)
drop = taxi_map(mapdata = NY_layer, taxidata = dropoffs, type = "dropoff", save = T)

