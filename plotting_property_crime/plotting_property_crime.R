# Visualising Property Crime --------------------------------------------------

# this file creates two plots
# 1) a boxplot of all the individuals crime in property crime
# 2) a "heatmap" of property crime in the analysis time frame


# Libraries -------------------------------------------------------------------

library(data.table)  # for efficient data
library(RPostgreSQL) # to get data from PostgreSQL
library(rgdal)       # for spatial transformation
library(mapproj)     # necessary to ensure consistent coordinate mapping
library(ggplot2)
library(dplyr)
library(magrittr)
library(lubridate)
library(viridis)     # colour theme
library(scales)

# load custom functions
source("helper_visualisation.R")
source("feature_helperfunctions.R")

# define analysis timeframe
start_day = as.Date("2015-06-01")
end_day   = as.Date("2015-11-29")
time_window = unique(isoweek(seq(start_day, end_day, by = "days")))

# Load and manipulate data ----------------------------------------------------

# load crime data
con1 = dbConnect(dbDriver("PostgreSQL"), 
                  dbname = "crime-data", 
                  user = "root", 
                  host = "localhost")

crime_data = dbGetQuery(con1, "SELECT id, cmplnt_fr_dt, 
                                ofns_desc, ctlabel, 
                                loc_gid FROM crimes_sub"); setDT(crime_data)

# parse date
crime_data$cmplnt_fr_dt = mdy(crime_data$cmplnt_fr_dt)

# filter crime data for 2015
crime15 = subset(crime_data, format.Date(cmplnt_fr_dt, "%Y")=="2015")

# set property crimes
property_crime = c("BURGLARY", "ARSON", "LARCENY", 
                    "PETIT LARCENY", "GRAND LARCENY OF MOTOR VEHICLE")

# read shapefile and match to analysis tracts

# requires both files, one for mapping, one for 
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

# Plot 1: Boxplot -------------------------------------------------------------

# remove tracts with empty values
census = census[rowSums(is.na(census)) == 0,]

# create a supplementary frame to collect zero crime observations
suppl_frame = data.table(id = rep(unique(census$gid), 
                                   times = length(property_crime)), 
                          ofns_desc = rep(property_crime, 
                                          each = length(unique(census$gid))))



# create boxplot dataframe
box_property_crime15 = 
  suppl_frame %>%
  left_join(., crime_aggregation(crime15, time_window, property_crime, aggr = F), 
            by = c("id" = "gid", "ofns_desc" = "ofns_desc")) %>%
  mutate(occ = replace(occ, is.na(occ), 0),
         occ = ifelse(id == 1262, NA, occ),
         outlier = ifelse(occ > 600, "Penn Station", NA))

# make plot
box_property = ggplot(box_property_crime15, aes(x=factor(ofns_desc), y=occ)) + 
  geom_boxplot() +
  ylab("\nOccurrence by census tract in Jun-Nov 2015\n") +
  plot_theme() +
  theme(axis.title.x=element_blank(),
        axis.text  = element_text(size=10),
        axis.text.x = element_text(size = 18)) +
  geom_text(aes(label = outlier), na.rm=T,
                family="Palatino Linotype", 
                size=4,
                vjust = 1.4) +
  scale_x_discrete(labels = c("Arson", "Burglary", "Theft", "Car Theft", "Petit Theft"))

# save
saveplot("Boxplot_Property", box_property)

# Plot 2: Map -----------------------------------------------------------------

# create map dataframe
map_property_crime15 = 
  NY_layer %>%
  left_join(., crime_aggregation(crime15, time_window, property_crime, aggr = T),
            by = c("id" = "gid")) %>%
  mutate(occ = replace(occ, is.na(occ), 0),
         occ = ifelse(id %in% suppl_frame$id, occ, NA),
         occ = ifelse(id == 1262, NA, occ),
         occ = ifelse(id == 900, NA, occ))

# create plot 
property_map = ggplot() +
  geom_polygon(data = map_property_crime15, aes(x = long, y = lat, group = group,
                                    fill = occ), color = "black", size = 0.25) +
  scale_fill_viridis(option = "inferno", direction = -1, name = "No. of incidents",
                     guide = guide_colorbar(direction = "horizontal",
                                            legend.position = "bottom",
                                            legend.title.align = 500,
                     title.position = 'top',
                     barheight = unit(4, units = "mm"),
                     barwidth = unit(50, units = "mm"),
                     draw.ulim = F,
                     title.hjust = 40,
                     label.hjust = 0.5)) +
  labs(x=NULL, y=NULL) +
  theme_map() +
  theme(legend.position = "bottom")+
  coord_map() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  

# save plot
savemap("Map_PropertyCrime2015", property_map)
