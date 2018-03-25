# Visualising Violent Crime ---------------------------------------------------

# this file creates two plots
# 1) a boxplot of all the individuals crime in violent crime
# 2) a "heatmap" of violent crime in the analysis time frame


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

# set violent crime vector
violent_crime = c("ROBBERY", "MURDER & NON-NEGL. MANSLAUGHTER", "FELONY ASSAULT")

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

# Plot 1: Boxplot -------------------------------------------------------------

# remove tracts with empty values
census = census[rowSums(is.na(census)) == 0,]

# create a supplementary frame to collect zero crime observations
suppl_frame = data.table(id = rep(unique(census$gid), 
                                   times = length(violent_crime)), 
                          ofns_desc = rep(violent_crime, 
                                          each = length(unique(census$gid))))

# create boxplot dataframe
box_violent_crime15 = 
  suppl_frame %>%
  left_join(., crime_aggregation(crime15, time_window, violent_crime, aggr = F), 
            by = c("id" = "gid", "ofns_desc" = "ofns_desc")) %>%
  mutate(occ = replace(occ, is.na(occ), 0),
         occ = ifelse(id == 1262, NA, occ))

# make plot
box_violent = ggplot(box_violent_crime15, aes(x=factor(ofns_desc), y=occ)) + 
  geom_boxplot() + 
  #stat_summary(fun.y = "mean", geom = "point", shape = 17, size = 3) +
  ylab("\nOccurrence by census tract in Jun-Nov 2015\n") +
  plot_theme() +
  theme(axis.title.x=element_blank(),
        axis.text  = element_text(size=10),
        axis.text.x = element_text(size = 18)) +
  scale_x_discrete(labels=c("Assault", "Murder & Manslaughter", "Robbery"))

# save
saveplot("Boxplot_Violent", box_violent)

# Plot 2: Map -----------------------------------------------------------------

# create map dataframe
map_violent_crime15 = 
  NY_layer %>%
  left_join(., crime_aggregation(crime15, time_window, violent_crime, aggr = T),
            by = c("id" = "gid")) %>%
  mutate(occ = replace(occ, is.na(occ), 0),
         occ = ifelse(id %in% suppl_frame$id, occ, NA),
         occ = ifelse(id == 1262, NA, occ))

# create plot
violent_map = ggplot() +
  geom_polygon(data = map_violent_crime15, aes(x = long, y = lat, group = group,
                                                fill = occ), color = "black", size = 0.25) +
  scale_fill_viridis(option = "inferno", direction = -1, name = "No. of incidents",
                     guide = guide_colorbar(direction = "horizontal",
                                            title.position = 'top',
                                            barheight = unit(4, units = "mm"),
                                            barwidth = unit(50, units = "mm"),
                                            draw.ulim = F,
                                            title.hjust = 0.6,
                                            label.hjust = 0.5)) +
  labs(x=NULL, y=NULL) +
  theme_map() +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  coord_map() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 

# save map
savemap("Map_ViolentCrime2015", violent_map)
