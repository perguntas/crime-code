library(data.table)  # for efficient data
library(RPostgreSQL) # to get data from PostgreSQL
library(rgdal)       # for spatial transformation
library(rgdal)
library(mapproj)     # necessary to ensure consistent coordinate mapping
library(ggplot2)
library(dplyr)
library(magrittr)
library(lubridate)
library(viridis)     # colour theme
library(scales)

source("helper_visualisation.R")

# setwd("")

# get crime data
con1 <- dbConnect(dbDriver("PostgreSQL"), dbname = "crime-data", user = "root", host = "localhost")
crime_data <- dbGetQuery(con1, "SELECT id, cmplnt_fr_dt, ofns_desc, ctlabel, loc_gid FROM crimes_sub"); setDT(crime_data)
crime_data$cmplnt_fr_dt <- mdy(crime_data$cmplnt_fr_dt)

tractID <- dbGetQuery(con1, "SELECT * FROM geo_mapping;")
colnames(tractID) <- c("id", "gid", "ctlabel")

crime15 <- subset(crime_data, format.Date(cmplnt_fr_dt, "%Y")=="2015")

property_crime <- c("BURGLARY", "ARSON", "GRAND LARCENY", "PETIT LARCENY", "GRAND LARCENY OF MOTOR VEHICLE")

property_crime15 <- 
  crime15 %>%
  filter(ofns_desc %in% property_crime) %>%
  mutate(ofns_desc = "property") %>%
  group_by(loc_gid, ctlabel) %>%
  summarise(occ=n())

### PLOT 1: Boxplot

box_property_crime15 <-
  crime15 %>%
  filter(ofns_desc %in% property_crime) %>%
  group_by(loc_gid, ofns_desc) %>%
  summarise(occ = n())

# make faster at some point
suppl_frame <- data.frame(gid = rep(unique(NY_layer$gid), length(property_crime)-1), 
                          ofns_desc = c(rep(property_crime[1], length(unique(NY_layer$gid))),
                                        rep(property_crime[2], length(unique(NY_layer$gid))),
                                        rep(property_crime[3], length(unique(NY_layer$gid))),
                                        rep(property_crime[5], length(unique(NY_layer$gid)))),
                          stringsAsFactors = F)

box_property_crime15 <- 
  suppl_frame %>%
  left_join(., box_property_crime15, by = c("gid" = "loc_gid", "ofns_desc" = "ofns_desc")) %>%
  mutate(occ = replace(occ, is.na(occ), 0))

ggplot(box_property_crime15, aes(x=factor(ofns_desc), y=occ)) + 
  geom_boxplot() + labs(x = "Property Crime", y = "Occurrence by CT in 2015") # 865, 599

### PLOT2: MAP OF NYC

# read shapefile
NY_layer <- sp:::spTransform(readOGR("D:/crime-data/nyc-taxi-data/nyct2010_15b/nyct2010.shp", layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))

# fortify as data.frame
NY_layer <- fortify(NY_layer, region = "BoroCT2010")
NY_layer <- left_join(NY_layer, tractID, by = c("id" = "boroct2010"))

# MAKE FUNCTION OR DPLYR CHAIN
plotData <- left_join(NY_layer, property_crime15, by = c("gid" = "loc_gid"))
plotData$occ[is.na(plotData$occ)] <- 0

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = occ), color = "black", size = 0.25) +
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

### PLOT 3: SEASONALITY IN MONTHS ## repeat for more years

crime15_season <- 
  crime15 %>%
  filter(ofns_desc %in% property_crime) %>%
  mutate(ofns_desc = "property",
         Month = factor(month(cmplnt_fr_dt), levels = 1:12),
         Week = factor(week(cmplnt_fr_dt), level = 1:53),
         Weekday = wday(cmplnt_fr_dt, label =T))

daily_crime15 <-
  crime15_season %>%
  group_by(cmplnt_fr_dt) %>%
  summarise(occ = n()) %>%
  arrange(cmplnt_fr_dt) %>%
  mutate(outlier = ifelse(is_outlier(occ), format(cmplnt_fr_dt, "%m/%d"), NA))

# weekday_crime15 <-
#   crime15_season %>%
#   mutate(KW = format(cmplnt_fr_dt, "%W")) %>%
#   group_by(KW) %>%
#   summarise(occ = n()) %>%
#   setorder(KW)

#lubridate::isoweek

ggplot(daily_crime15, aes(cmplnt_fr_dt, occ)) + 
  geom_line() +
  scale_x_date(date_labels="%b", date_breaks = "1 month", expand=c(0.02,0.02)) + 
  xlab("") + ylab("Property Crime Counts per Day") +
  geom_text(aes(label = outlier),na.rm=T)
