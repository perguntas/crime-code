<<<<<<< HEAD
#--------------------------#
#                          #
# Cleaning the Census data #
#                          #
#--------------------------#

library(RPostgreSQL)
library(sp)
library(rgdal)
library(ggplot2)

path <- "D:/crime-data/"
con1 <- dbConnect(dbDriver("PostgreSQL"), dbname = "crime-data", user = "postgres", host = "localhost")

census <- read.csv(paste0(path, "2010_NYC_Census_Tracts.csv"), stringsAsFactors = F)

keep <- c(# GEO.id2
          "GEO.id2",
          # total population
          "HD01_S001",
          # median age
          "HD01_S020",
          # % male population
          "HD02_S026",
          # % male age groups
          "HD02_S030", "HD02_S031", "HD02_S032", "HD02_S033",
          # % race
          "HD02_S078", "HD02_S079", "HD02_S080", "HD02_S081", "HD02_S089", "HD02_S107",
          # housing vacancy
          "HD02_S171",
          # household size (?)
          "HD01_S167",
          # female-headed households, add the two together
          "HD02_S157", "HD02_S158",
          # institutionalized population
          "HD02_S144"
          )

# subset data.table
census <- census[-1, colnames(census) %in% keep]
# make columns numeric
census[, -1] <- apply(census[, -1], 2, function(x) as.numeric(x))

NY_layer <- sp:::spTransform(readOGR("D:/crime-data/nyc-taxi-data/nyct2010_15b/nyct2010.shp", layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))
NY_layer <- fortify(NY_layer, region = "BoroCT2010")

tractID <- dbGetQuery(con1, "SELECT * FROM geo_mapping;")

NY_layer <- left_join(NY_layer, tractID, by = c("id" = "boroct2010"))

census <- census[census$GEO.id2 %in% NY_layer$ctlabel, ]

write.csv(census, file = paste0(path, "2010_NYC_Census_cleaned.csv"), row.names=FALSE)

# not needed right now
# census[1,] <- tolower(census[1,])
# 
# # Remove all columns without content (denoted by " ( X ) " in the census file)
# remove <- colnames(census)[grepl("( X )", census[2,])]
# census <- census[, !colnames(census) %in% remove]
# 
# # Remove all columns related to absolute number, excluding housing numbers and median age, excluding the first occurrence
# remove <- colnames(census)[(grepl("number", census[1,]) 
#                             & !grepl("hous|median", census[1,]))][-1]
=======
#--------------------------#
#                          #
# Cleaning the Census data #
#                          #
#--------------------------#

setwd("D:/crime-data")
library(foreign) # for dbf table

census <- read.csv("2010_NYC_Census_Tracts.csv", stringsAsFactors = F)

# Remove all columns related to absolute numbers, excluding housing numbers and median age, excluding the first occurrence
census[1,] <- tolower(census[1,])
remove <- colnames(census)[(grepl("number", census[1,]) & !grepl("hous|median|relationship", census[1,]))][-1]
census <- census[, !colnames(census) %in% remove]

remove <- colnames(census)[(grepl("percent", census[1,]) & grepl("occupancy|household", census[1,]))]
census <- census[, !colnames(census) %in% remove]

# remove all tenure columns, keep HD02_S181, HD02_S184
remove <- colnames(census)[grepl("tenure", census[1,])]
remove <- remove[!(remove %in% c("HD02_S181", "HD02_S184"))]
census <- census[, !colnames(census) %in% remove]

# delete explanative first line
census <- census[-1,]

# Remove all columns with only 100 (percent) or " ( X ) " (ie without content)
census <- Filter(function(x) (length(unique(x)) > 2), census)

#write.csv(census, "2010_NYC_Census_cleaned.csv")

# DONT' EVEN NEED THAT
# split census GEO.id
#census_string <- strsplit(census$GEO.display.label, ", ")
#census$tract <- sapply(census_string, "[[", 1)
#census$county <- sapply(census_string, "[[", 2)

# census$GEO.id2 and nyct$CT2010 match by 36005+CT2010
# NOPE

# check   
nyct2010 <- read.dbf("D:/crime-data/nyc-taxi-data/nyct2010_15b/nyct2010.dbf", as.is = T)


# to do:
# - remove tracts not in NYC
# - remove lines with minimal variation
# - correlation analysis?
>>>>>>> 56bfba16d5a6e19389e1b60dd7268ca411889e4e
