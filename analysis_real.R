library(RPostgreSQL)
library(lubridate)
library(data.table)
library(magrittr)
library(dplyr)
library(sp)
library(rgdal)
library(ggplot2)
#library(viridis)

#source("helper_visualisation.R")


con1 <- dbConnect(dbDriver("PostgreSQL"), dbname = "crime-data", user = "postgres", host = "localhost")

# load data
crime_data <- dbGetQuery(con1, "SELECT id, cmplnt_fr_dt, ofns_desc, ctlabel, loc_gid FROM crimes_sub"); setDT(crime_data)
census <- fread("D:/crime-data/2010_NYC_Census_cleaned.csv", stringsAsFactors = F)

#test <- fread("D:/crime-data/NYPD_Complaints16.csv", stringsAsFactors = F)

# subset for 2015
crime_data$cmplnt_fr_dt <- mdy(crime_data$cmplnt_fr_dt)
crime15 <- subset(crime_data, format.Date(cmplnt_fr_dt, "%Y")=="2015")

# redo offenses
violent_crime <- c("ROBBERY", "MURDER & NON-NEGL. MANSLAUGHTER", "FELONY ASSAULT")
property_crime <- c("BURGLARY", "ARSON", "GRAND LARCENY", "PETIT LARCENY", "GRAND LARCENY OF MOTOR VEHICLE")

violent_crime15 <-
  crime15 %>%
  filter(ofns_desc %in% violent_crime) %>%
  mutate(ofns_desc = "violent") %>%
  group_by(loc_gid, ctlabel) %>%
  summarise(occ=n())

property_crime15 <- 
  crime15 %>%
  filter(ofns_desc %in% property_crime) %>%
  mutate(ofns_desc = "property") %>%
  group_by(loc_gid, ctlabel) %>%
  summarise(occ=n())




tractID <- dbGetQuery(con1, "SELECT * FROM geo_mapping;")
colnames(tractID) <- c("id", "gid", "ctlabel")
tractID$id <- as.numeric(tractID$id)

NY_layer <- sp:::spTransform(readOGR("D:/crime-data/nyc-taxi-data/nyct2010_15b/nyct2010.shp", layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))

# fortify as data.frame
NY_layer <- fortify(NY_layer, region = "BoroCT2010")
NY_layer$id <- as.numeric(NY_layer$id)
NY_layer <- left_join(NY_layer, tractID)
NY_layer$gid <- as.numeric(NY_layer$gid)


setnames(census, "GEO.id2", "ctlabel")

lmdata <- left_join(census, property_crime15)
lmdata$occ[is.na(lmdata$occ)] <- 0

lmdata[, c("V1","GEO.id","GEO.display.label"):=NULL]

lmdata <- lmdata[1:1000,]


test <- lm(occ ~ . -loc_gid, data = lmdata)

# criminal mischief
# trespass
# assault?
# OFFENSES AGAINST THE PERSON
# drugs?
# weapons?
# prostitution