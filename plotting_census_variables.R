library(lubridate)
library(data.table)
library(magrittr)
library(dplyr)
library(sp)
library(rgdal)
library(ggplot2)
library(MASS)
library(viridis)
library(scales)
library(GGally)

source("helper_visualisation.R")
load("final_frame_property15.Rdata")
load("final_frame_violent15.Rdata")

final_frame_property[, violent := final_frame_violent[,occ]] 

# remove columns not needed for plotting
final_frame_property[, c("gid", "week", 
                         "young_male", "white", "institution",
                         "tweet_counts", "night_tweets", "log_tweets",
                         "NTA_counts", "NTA_night",
                         "ny", "kings", "bronx", "queens",
                         "taxi1", "taxi2",
                         "outdoorsA", "shopsA", "travelA", 
                         "residentialA", "entertainmentA", 
                         "uniA", "foodA", "professionalA",
                         "nightlifeA", "H", "NTACode") := NULL]

setcolorder(final_frame_property, c("occ", "violent", "log_night", "taxi", "population", "age",
                                 "male", "black", "asian", "hispanic", "vacancy", "female_head", 
                                 "entertainment", "uni","food", "professional", "nightlife", 
                                 "outdoors", "shops", "travel", "residential"))

setnames(final_frame_property, c("occ", "taxi"), c("property", "taxi_dest"))

p = ggpairs(final_frame_property, 
        columns = names(final_frame_property),#[-c(13:ncol(final_frame_property))],
        diag = list(continuous = wrap("barDiag")),
        upper = list(continuous = wrap("cor", size = 8, family = "Palatino Linotype",
                                       colour="black")),
        lower = list(continuous = wrap("points", color = "#595959"))) + 
  plot_theme()


saveggpairs("corrs_LALA", p, width = 2500, height = 1500)


# READ MAP DATA ------------------------------------------------

# read shapefile
tracts = sp:::spTransform(readOGR("D:/crime-data/nyc-taxi-data/nyct2010_15b/nyct2010.shp", layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))

# fortify as data.frame
tracts@data$id = as.numeric(rownames(tracts@data)) + 1

NY_layer = fortify(tracts, region = "id")
NY_layer$id = as.numeric(NY_layer$id)
NY_layer = inner_join(NY_layer, tracts@data, by = "id")
# match to ID_mapping
NY_layer = left_join(NY_layer, tractID, by = c("id" = "gid"))
rm(tracts)

pairs(census[, .(population, male, black, white, female_head)],  # not happy with col selection yet
      pch=".", # sets small values
      upper.panel = panel.cor, # print correlation
      diag.panel = panel.hist, # show histogram
      lower.panel = panel.smooth,
      col.smooth = "red",
      span = 0.3) # shows lines, not happy with that


# PLOT DATA -------------------------------------------------------
plotData = left_join(NY_layer, census, by = c("id" = "gid"))

# plot map of racial differences excl. staten island

plotData = plotData[plotData$BoroName != "Staten Island",]


p = ggplot() +
    geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                      fill = asian), color = "black", size = 0.25) +
    scale_fill_viridis(option = "magma", direction = -1, name = "Percentage of Asian Population",
                       labels = percent, limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1),
                       guide = guide_colorbar(direction = "horizontal",
                                              title.position = 'top',
                                              barheight = unit(4, units = "mm"),
                                              barwidth = unit(70, units = "mm"),
                                              draw.ulim = F,
                                              title.hjust = 0.6,
                                              label.hjust = 0.5)) +
    labs(x="", y="") +
    theme_map() +
    theme(legend.position = "bottom") +
    coord_map()
  
p

# This part creates the neighbourhood plot

# function converts nb object to a data.frame
nb_to_df = function(nb, coords){
  x = coords[, 1]
  y = coords[, 2]
  n = length(nb)
  
  cardnb = card(nb)
  i = rep(1:n, cardnb)
  j = unlist(nb)
  return(data.frame(x=x[i], xend=x[j],
                    y=y[i], yend=y[j]))
}

# load map data
census = read.csv("D:/crime-data/2010_NYC_Census_cleaned.csv", stringsAsFactors = F)

# read polygons to create neighbour matrix
tracts = sp:::spTransform(readOGR("D:/crime-data/nyc-taxi-data/nyct2010_15b/nyct2010.shp",
                                   layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))
tracts@data$id = as.numeric(rownames(tracts@data)) + 1
# exclude Farway Rock and City Island
tracts = subset(tracts, tracts@data$id %in% census$gid & !(tracts@data$id %in% c(1263, 2023, 2116)))

NY_layer = fortify(tracts, region = "id")

census = census[rowSums(is.na(census)) == 0,]
tracts = subset(tracts, tracts@data$id %in% census$gid)

NY_layer$census = ifelse(NY_layer$id %in% census$gid, 1, NA)

# create neighbours list
neighbours = poly2nb(tracts, row.names = tracts$id)

trueCentroids = as.data.frame(gCentroid(tracts, byid=TRUE))

# create distance-based neighbors
nb_df = nb_to_df(neighbours, trueCentroids)


# create plot 
linkmap = ggplot() +
  geom_polygon(data = NY_layer, aes(x = long, y = lat, fill = census, group = group),
               color = "black", size = 0.25) +
  coord_map() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_map() + 
  scale_fill_continuous(low="white", 
                        high="white",
                        na.value="grey50", guide=F) +
  geom_segment(aes(x=x, xend=xend, y=y, yend=yend),
                data = nb_df, colour = "black")

savemap("linkmap", linkmap)
  

  
