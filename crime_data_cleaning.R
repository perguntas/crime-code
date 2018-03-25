#------------------------
# # JOIN ALL DATA SOURCES # #
#------------------------

# the variation where only crime in populated census tracts is counted!  Otherwise, use tractID as leading ID and
# not census$ctlabel

# Load Libraries --------------------------------------------------------------

library(RPostgreSQL)  # to access PSQL data
library(data.table)  # efficient memory management
library(lubridate)  # to access isoweek
library(dplyr)  # for data manipulation
library(dtplyr)  # to preserve data.table class in left_joins 
library(sp)  # to load shapefile
library(tidyr)  # to create dummy matrices
library(Matrix)  # for sparse matrices
library(rgdal)  # to read shapefiles
library(ggplot2)  # to manipulate shapefiles

library(beepr)  # used while loading the taxi data

# helper functions ------------------------------------------------------------
source("feature_helperfunctions.R")

# OBJECTIVE -------------------------------------------------------------------

# CREATE 2 FINAL VAR TABLES OF THE FOLLOWING FORM:

# id | y | week | Census | Taxi | POI | Twitter | County |
# ______________________________________________________________ --------|---| 22 | |------| |---------| |
# --------|---| 23 | |------| |---------| | ______________________________________________________________ where
# '---' denote weekly changing values and empty spaces denote constant values one table captures violent crime, the
# other property crime

# define analysis timeframe
start_day = as.Date("2015-06-01")
end_day = as.Date("2015-11-29")
time_window = unique(isoweek(seq(start_day, end_day, by = "days")))

# CENSUS ----------------------------------------------------------------------
census = fread("D:/crime-data/2010_NYC_Census_analysis.csv", stringsAsFactors = F)

final_frame = data.table(gid = rep(census$gid, length(time_window)), week = rep(time_window, each = nrow(census)))

# create lag_frame for taxi feature
lag_frame = data.table(gid = rep(census$gid, length(time_window)), week = rep(time_window - 1, each = nrow(census)))

# join census data
final_frame = left_join(final_frame, census)


# CRIME DATA ------------------------------------------------------------------ load data
con1 = dbConnect(dbDriver("PostgreSQL"), dbname = "crime-data", user = "postgres", host = "localhost")

crime_data = dbGetQuery(con1, "SELECT id, cmplnt_fr_dt, ofns_desc, ctlabel, loc_gid FROM crimes_sub")
setDT(crime_data)

# subset for 2015
crime_data[, `:=`(cmplnt_fr_dt, mdy(cmplnt_fr_dt))]
crime15 = subset(crime_data, format.Date(cmplnt_fr_dt, "%Y") == "2015")

# separate into offenses
property_crime = c("BURGLARY", "ARSON", "GRAND LARCENY", "PETIT LARCENY", "GRAND LARCENY OF MOTOR VEHICLE")
violent_crime = c("ROBBERY", "MURDER & NON-NEGL. MANSLAUGHTER", "FELONY ASSAULT")

# per week for Jun - Nov + one week earlier for lagged taxi feature
weekly_property_crime15 = weekly_crime_aggregation(data = crime15, weeks = c(time_window[1] - 1, time_window), crime = property_crime)
weekly_violent_crime15 = weekly_crime_aggregation(data = crime15, weeks = c(time_window[1] - 1, time_window), crime = violent_crime)

# Join crime data
final_frame_property = left_join(final_frame, weekly_property_crime15)
final_frame_violent = left_join(final_frame, weekly_violent_crime15)

# set lag_frame
lag_frame_property = left_join(lag_frame, weekly_property_crime15)
lag_frame_violent = left_join(lag_frame, weekly_violent_crime15)

# add zero crime
lag_frame_property[is.na(occ), `:=`(occ, 0)]
lag_frame_violent[is.na(occ), `:=`(occ, 0)]

final_frame_property[is.na(occ), `:=`(occ, 0)]
final_frame_violent[is.na(occ), `:=`(occ, 0)]

# set keys for week, gid
setkey(final_frame_property, week, gid)
setkey(final_frame_violent, week, gid)

rm(crime_data, crime15, property_crime, violent_crime, con1, final_frame)

# TAXI DATA -------------------------------------------------------------------

# there are three different settings, taxi(normalised by destination), taxi1 (normalised by source), taxi (not
# normalised) the final selection uses taxi, therefore only the initial code using 'tidy_sparse' is shown, not how
# to include it separately (which is the same as for 'taxi')

con2 = dbConnect(dbDriver("PostgreSQL"), dbname = "nyc-taxi-data", user = "root", host = "localhost")
# takes very long
taxi = dbReadTable(con2, "analysis_data")
beep(5)
setDT(taxi)
setkey(taxi, week, pickup_nyct2010_gid, dropoff_nyct2010_gid)

# exclude all trips which did not start or end in NYC
taxi = na.omit(taxi, cols = c("pickup_nyct2010_gid", "dropoff_nyct2010_gid"))

# exclude trips to areas without census data
taxi = taxi[(pickup_nyct2010_gid %in% census$gid) & (dropoff_nyct2010_gid %in% census$gid)]
# 70,266,249 valid trips

# save(taxi, file = 'taxi15.Rdata') load('taxi15.Rdata')

# create sparse contingency table for each week
taxi_list = lapply(time_window, function(x) xtabs(~taxi_factor(pickup_nyct2010_gid, census$gid) + taxi_factor(dropoff_nyct2010_gid, 
    census$gid), data = taxi[week == x], sparse = T))
# set ii = 0, normalise by ....
taxi_list = tidy_sparse(taxi_list, normal = "destination")
# taxi_list1 = tidy_sparse(taxi_list, normal ='source') taxi_list2 = tidy_sparse(taxi_list, normal ='counts')

# set names
names(taxi_list1) = time_window

# save(taxi_list, file = 'taxi_list15.Rdata') load('taxi_list15.Rdata')

# multiply flow matrix by lagged crime vector (use the other taxi lists for the other settings)
taxi_property_crime_list = sapply(time_window, function(x) taxi_list[[paste(x)]] %*% lag_frame_property[week == (x - 
    1), occ], USE.NAMES = T)
taxi_violent_crime_list = sapply(time_window, function(x) taxi_list[[paste(x)]] %*% lag_frame_violent[week == (x - 1), 
    occ], USE.NAMES = T)

names(taxi_property_crime_list) = time_window
names(taxi_violent_crime_list) = time_window

# Joining taxi data

for (k in time_window) {
    set(final_frame_property, i = which(final_frame_property[, week] == k), j = "taxi", value = taxi_property_crime_list[[paste(k)]][, 
        1])
}

for (k in time_window) {
    set(final_frame_violent, i = which(final_frame_violent[, week] == k), j = "taxi", value = taxi_violent_crime_list[[paste(k)]][, 
        1])
}

rm(con2, taxi, k, taxi_property_crime_list, taxi_violent_crime_list, taxi_list, census, lag_frame_property, lag_frame_violent)


# FOURSQUARE ------------------------------------------------------------------

# 1) match the coordinates of the foursquare venues to the NYC census tract shapefile 2) clean up data by counting
# how many venues per category in function 'foursquare_aggregation' 3) join to the final_frames 4) from those raw
# counts, normalise by total number of venues in the tract 5) calculate a heterogeneity measure of how many
# different measures are in the tract

# read foursquare data
f4s_frame = fread("D:/crime-data/Foursquare/f4s_data2.csv")

# 1) match foursquare coordinates to census tracts

# set spatial dataframe and ensure correct mapping
coordinates(f4s_frame) = c(3, 2)  # set coordinates on longitude, latitude
proj4string(f4s_frame) = CRS("+proj=longlat +datum=WGS84")

# read shapefile
tracts = sp:::spTransform(readOGR("D:/crime-data/nyc-taxi-data/nyct2010_15b/nyct2010.shp", layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))
tracts@data$gid = as.numeric(rownames(tracts@data)) + 1
names(tracts@data) = tolower(names(tracts@data))

# returns vector the same length as f4s_frame with census ID
f4s_frame$gid = over(f4s_frame, tracts[, "gid"])

# transform from spatial dataframe into data.table
foursquare = as.data.table(f4s_frame)

# save(foursquare, file = 'foursquare_complete.Rdata')

# 2) clean up data
foursquare = foursquare_aggregation(foursquare)

# 3) join to final_frames
final_frame_violent = left_join(final_frame_violent, foursquare)
final_frame_property = left_join(final_frame_property, foursquare)

# ensure that areas not present in foursquare data are set to zero in final_frame
for (i in names(final_frame_violent)) {
    final_frame_violent[is.na(get(i)), `:=`((i), 0)]
    final_frame_property[is.na(get(i)), `:=`((i), 0)]
}

# 4) copy the existing columns and append 'A' to their names
final_frame_violent[, `:=`(paste0(names(foursquare)[-1], "A"), final_frame_violent[, names(foursquare)[-1], with = F])]
final_frame_property[, `:=`(paste0(names(foursquare)[-1], "A"), final_frame_property[, names(foursquare)[-1], with = F])]

# set indices
cols = names(final_frame_property)[grepl("A", names(final_frame_property))]
cols0 = names(foursquare)[-1]

# divide by total number of venues in the census tract
final_frame_violent[, `:=`((cols), final_frame_violent[, cols, with = F]/rowSums(final_frame_violent[, cols, with = F]))]
final_frame_property[, `:=`((cols), final_frame_property[, cols, with = F]/rowSums(final_frame_property[, cols, with = F]))]

# set NaN (division by zero) to 0
for (i in names(final_frame_violent)) {
    final_frame_violent[is.na(get(i)), `:=`((i), 0)]
    final_frame_property[is.na(get(i)), `:=`((i), 0)]
}

# 5) calculate a heterogeneity measure sum how many venues per category divide the number of venues in each tract by
# the corresponding sum to obtain p_i calculate H = -sum(p_i * log(p_i))


final_frame_violent[, `:=`(H, apply(final_frame_violent[, cols, with = F], 1, function(x) -sum(x * log(x), na.rm = T)))]

final_frame_property[, `:=`(H, apply(final_frame_property[, cols, with = F], 1, function(x) -sum(x * log(x), na.rm = T)))]

rm(foursquare, tracts, f4s_frame, cols, cols0)

# TWITTER DATA ----------------------------------------------------------------

# 1) load twitter data and remove tweets not in the analysis timeframe create night variable by counting tweets sent
# between 22pm-6am 2) join with final_frames 3) create log(counts) variables 4) normalise tweet activity by dividing
# by NTA activity

# 1) load and manipulate
con3 = dbConnect(dbDriver("PostgreSQL"), dbname = "foursquare", user = "root", host = "localhost")
twitter = dbReadTable(con3, "twitter")
twitter = twitter_aggregation(twitter, time_window)
# save(twitter, file = 'aggregated_tweets15.RData') load('aggregated_tweets15.RData')

twitter_names = c("tweet_counts", "night_tweets")

# 2) join
final_frame_violent = left_join(final_frame_violent, twitter)
final_frame_property = left_join(final_frame_property, twitter)

# replace NA by 0
for (i in twitter_names) {
    final_frame_violent[is.na(get(i)), `:=`((i), 0)]
    final_frame_property[is.na(get(i)), `:=`((i), 0)]
}

# 3) take log
final_frame_property[, `:=`(c("log_tweets", "log_night"), lapply(.SD, log)), .SDcols = twitter_names]
final_frame_violent[, `:=`(c("log_tweets", "log_night"), lapply(.SD, log)), .SDcols = twitter_names]

# replace log(0) = -Inf by 0
invisible(lapply(c("log_tweets", "log_night"), function(.name) set(final_frame_violent, which(is.infinite(final_frame_violent[[.name]])), 
    j = .name, value = 0)))
invisible(lapply(c("log_tweets", "log_night"), function(.name) set(final_frame_property, which(is.infinite(final_frame_property[[.name]])), 
    j = .name, value = 0)))


# 4) normalise
final_frame_violent[, `:=`(c("NTA_counts", "NTA_night"), .SD/lapply(.SD, sum)), by = NTACode, .SDcols = twitter_names]
final_frame_property[, `:=`(c("NTA_counts", "NTA_night"), .SD/lapply(.SD, sum)), by = NTACode, .SDcols = twitter_names]

rm(twitter, con3, twitter_names)


# INCLUDE COUNTY DUMMY ---------------------------------------------------------

final_frame_property[, `:=`(county, substr(ctlabel, 1, 5))]
final_frame_property[, `:=`(county, recode(final_frame_property$county, `36005` = "bronx", `36047` = "kings", `36061` = "ny", 
    `36081` = "queens"))]
dummies = unique(final_frame_property[, county])
final_frame_property[, `:=`((dummies), lapply(dummies, function(x) county == x))]
final_frame_property[, `:=`((dummies), lapply(.SD, as.numeric)), .SDcols = dummies]

final_frame_violent[, `:=`(county, substr(ctlabel, 1, 5))]
final_frame_violent[, `:=`(county, recode(final_frame_violent$county, `36005` = "bronx", `36047` = "kings", `36061` = "ny", 
    `36081` = "queens"))]

final_frame_violent[, `:=`((dummies), lapply(dummies, function(x) county == x))]
final_frame_violent[, `:=`((dummies), lapply(.SD, as.numeric)), .SDcols = dummies]


final_frame_property[, `:=`(c("ctlabel", "county"), NULL)]
final_frame_violent[, `:=`(c("ctlabel", "county"), NULL)]

rm(dummies)

# FINAL ADJUSTMENTS -----------------------------------------------------------

setcolorder(final_frame_property, c("gid", "week", "occ", "population", "age", "male", "young_male", "white", "black", 
    "asian", "hispanic", "vacancy", "female_head", "institution", "tweet_counts", "night_tweets", "log_tweets", "log_night", 
    "NTA_counts", "NTA_night", "entertainment", "uni", "food", "professional", "nightlife", "outdoors", "shops", "travel", 
    "residential", "ny", "kings", "bronx", "queens", "taxi", "taxi1", "taxi2", "entertainmentA", "uniA", "foodA", "professionalA", 
    "nightlifeA", "outdoorsA", "shopsA", "travelA", "residentialA", "H", "NTACode"))
setcolorder(final_frame_violent, c("gid", "week", "occ", "population", "age", "male", "young_male", "white", "black", 
    "asian", "hispanic", "vacancy", "female_head", "institution", "tweet_counts", "night_tweets", "log_tweets", "log_night", 
    "NTA_counts", "NTA_night", "entertainment", "uni", "food", "professional", "nightlife", "outdoors", "shops", "travel", 
    "residential", "ny", "kings", "bronx", "queens", "taxi", "taxi1", "taxi2", "entertainmentA", "uniA", "foodA", "professionalA", 
    "nightlifeA", "outdoorsA", "shopsA", "travelA", "residentialA", "H", "NTACode"))

save(final_frame_property, file = "final_frame_property15.Rdata")
save(final_frame_violent, file = "final_frame_violent15.Rdata")


# load('final_frame_property15.Rdata') load('final_frame_violent15.Rdata')
