library(RPostgreSQL)
library(lubridate)
library(data.table)
library(magrittr)
library(dplyr)
library(sp)
library(rgdal)
library(ggplot2)
library(MASS)

# CRIME DATA ----------------------------------
# load data
crime_data <- dbGetQuery(con1, "SELECT id, cmplnt_fr_dt, ofns_desc, ctlabel, loc_gid FROM crimes_sub"); setDT(crime_data)
con1 <- dbConnect(dbDriver("PostgreSQL"), dbname = "crime-data", user = "postgres", host = "localhost")

# subset for 2015
crime_data$cmplnt_fr_dt <- mdy(crime_data$cmplnt_fr_dt)
crime15 <- subset(crime_data, format.Date(cmplnt_fr_dt, "%Y")=="2015")

# separate into offenses
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
  group_by(ctlabel) %>% # removing loc_gid as a test
  summarise(occ=n())

#lubridate::isoweek


# CENSUS -------------------------------
census <- fread("D:/crime-data/2010_NYC_Census_cleaned.csv", stringsAsFactors = F)

# tracts with population < 50 are set to NA
ind <- which(census$HD01_S001 < 50)
census[ind, c(names(census)[-1]) := as.list(NA)]

lmdata <- left_join(census, property_crime15)
lmdata$occ[is.na(lmdata$occ)] <- 0

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
