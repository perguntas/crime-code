#------------------------
#                       #
# JOIN ALL DATA SOURCES #
#                       #
#------------------------

# the variation where only crime in populated census tracts is counted! 
# Otherwise, use tractID as leading ID and not census$ctlabel

# Load Libraries --------------------------------------------------------------

library(RPostgreSQL) #
library(lubridate) #
library(data.table)#
library(magrittr)
library(dplyr) #
library(sp)
library(rgdal)
library(ggplot2)
library(MASS)
library(lubridate)
library(Matrix)
library(beepr)

# helper functions ------------------------------------------------------------
source("feature_helperfunctions.R")

# OBJECTIVE -------------------------------------------------------------------

# CREATE FINAL VAR TABLE OF THE FOLLOWING FORM:

# ctlabel | y | Week | Census | Taxi | POI | Twitter | Spatial |
# ______________________________________________________________
# --------|---|  22  |        |------|     |---------|         |
# --------|---|  23  |        |------|     |---------|         |
# ______________________________________________________________
# where "---" denote weekly changing values and empty spaces denote constant values

# load "ID translator" file
tractID <- read.csv("D:/crime-data/tractID.csv", stringsAsFactors = F)
tractID[] <- lapply(tractID, as.character)

# CENSUS ----------------------------------------------------------------------
census <- fread("D:/crime-data/2010_NYC_Census_analysis.csv", stringsAsFactors = F)
census <- left_join(census, tractID)

final_frame <- data.table(ctlabel = rep(census$ctlabel, length(time_window)),
                          loc_gid = rep(census$gid,     length(time_window)),
                          Week = rep(time_window, each = nrow(census)))

# CRIME DATA ------------------------------------------------------------------
# load data
con1 <- dbConnect(dbDriver("PostgreSQL"), dbname = "crime-data", user = "postgres", host = "localhost")

crime_data <- dbGetQuery(con1, "SELECT id, cmplnt_fr_dt, ofns_desc, ctlabel, loc_gid FROM crimes_sub")
setDT(crime_data)

# subset for 2015
crime_data$cmplnt_fr_dt <- mdy(crime_data$cmplnt_fr_dt)
crime15 <- subset(crime_data, format.Date(cmplnt_fr_dt, "%Y") == "2015")

# define analysis timeframe
start_day <- as.Date("2015-06-01")
end_day   <- as.Date("2015-11-29")
time_window <- unique(isoweek(seq(start_day, end_day, by = "days")))

# separate into offenses
violent_crime <- c("ROBBERY", "MURDER & NON-NEGL. MANSLAUGHTER", "FELONY ASSAULT")
property_crime <- c("BURGLARY", "ARSON", "GRAND LARCENY", "PETIT LARCENY", "GRAND LARCENY OF MOTOR VEHICLE")

# per week for Jun - Nov
weekly_violent_crime15  <- weekly_crime_aggregation(data = crime15, weeks = time_window, crime = violent_crime)
weekly_property_crime15 <- weekly_crime_aggregation(data = crime15, weeks = time_window, crime = property_crime)

# per year
violent_crime15  <- yearly_crime_aggregation(data = crime15, years = 2015, crime = violent_crime)
property_crime15 <- yearly_crime_aggregation(data = crime15, years = 2015, crime = property_crime)
setDT(weekly_violent_crime15)

# Join Data
final_frame <- merge(final_frame, weekly_violent_crime15, all.x = T, by = c("Week" = "Week", "ctlabel" = "ctlabel"), sort = T)

# add zero crime
final_frame[is.na(occ), occ := 0]


# TAXI DATA -------------------------------------------------------------------
con2 <- dbConnect(dbDriver("PostgreSQL"), dbname = "nyc-taxi-data", 
                  user = "postgres", host = "localhost")
taxi <- dbReadTable(con2, "analysis_data"); beep(5)
setDT(taxi)
# WARUM FUNKTIONIERT DAS NICHT?!

# exclude all trips which did not start or end in NYC
taxi <- data.table:::na.omit.data.table(taxi, cols = c("pickup_nyct2010_gid", "dropoff_nyct2010_gid"))

# exclude trips to areas without census data
taxi <- taxi[(pickup_nyct2010_gid %in% census$gid) & (dropoff_nyct2010_gid %in% census$gid)]


save(taxi, file = "taxi15.Rdata")

# create sparse contingency table for each week
taxi_list <- lapply(time_window, function(x) xtabs(~ taxi_factor(pickup_nyct2010_gid, census$gid)
                                                     + taxi_factor(dropoff_nyct2010_gid, census$gid),
                                                     data = taxi[week == x], sparse = T))
# set ii = 0, normalise by destination
taxi_list <- tidy_sparse(taxi_list, normal = "destination")

# name list
names(taxi_list) <- time_window

# subset to include only tracts in census NOT NEEDED RIGHT NOW
#taxi_list <- lapply(taxi_list, function(x) {x[rownames(x) %in% census$gid, colnames(x) %in% census$gid]; x})

# matrix multiplication
try <- taxi_list[["23"]] %*% final_frame$occ[order(final_frame$loc_gid)][final_frame$Week == 23]

test <- sapply(time_window, function(x) taxi_list[[paste(x)]] %*% final_frame$occ[order(final_frame$loc_gid)][final_frame$Week == x], 
               USE.NAMES = T)
# confirm values, not today


# JOINING THE  DATA -----------------------------------------------------------






final_frame[, flow := occ * ]


# langfristig? week = factor



# 2166 0.3408404


census$gid <- tractID$gid[match(census$ctlabel, tractID$ctlabel)]

data$vector1 <- key[match(data$id, key$mat), 'vetor']
census <- left_join(census, )
lmdata <- left_join(census, violent_crime0615)
lmdata$occ[is.na(lmdata$occ)] <- 0

# include loc_gid
lmdata <- left_join

test <- lm(occ ~ . - ctlabel, data = lmdata)

res <- glm.nb(occ ~ . + offset(log(HD01_S001)) -ctlabel, data = lmdata)


plotData <- left_join(NY_layer, census)

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = HD01_S001), color = "black", size = 0.25) +
  scale_fill_viridis(option = "magma", direction = -1, name = "Property Crime in 2015",
                     guide = guide_colorbar(direction = "horizontal",
                                            title.position = 'top',
                                            barheight = unit(4, units = "mm"),
                                            barwidth = unit(50, units = "mm"),
                                            draw.ulim = F,
                                            title.hjust = 0.6,
                                            label.hjust = 0.5)) +
  labs(x="", y="") +
  theme_map() +
  theme(legend.position = "bottom") +
  coord_map()

p # 2000 x 1360
