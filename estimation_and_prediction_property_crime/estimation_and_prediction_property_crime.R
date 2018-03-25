# ANALYSIS FILE ---------------------------------------------------------------

# This file loads the property crime data and runs regressions and save predictions

# necessary libraries
library(data.table)
library(maptools)    # to load shapefile
library(spdep)       # to set neighbour lists
library(rgdal)       # for spatial transformation
library(mapproj)     # necessary to ensure consistent coordinate mapping
library(matrixcalc)
library(MASS)
library(pscl)

# load model functions --------------------------------------------------------

source("CARlm.R")
source("SARlm.R")
source("Poisson_CAR.R")

# load data -------------------------------------------------------------------

load("final_frame_property15.Rdata")

# remove variables not needed
# (this selection was obtained with file "feature_combination_selection.R)
final_frame_property[, c("young_male", "white", "institution",
                        "tweet_counts", "night_tweets", "log_tweets",
                        "NTA_counts", "NTA_night",
                        "ny", "kings", "bronx", "queens",
                        "taxi1", "taxi2",
                        "outdoorsA", "shopsA", "travelA", 
                        "residentialA", "entertainmentA", 
                        "uniA", "foodA", "professionalA",
                        "nightlifeA", "H", "NTACode") := NULL]

# SPLIT INTO TEST AND TRAINING ------------------------------------------------

# train for 24 weeks, predict 2
train = final_frame_property[week %in% c(23:46)] 
test  = final_frame_property[week %in% c(47:48)]
rm(final_frame_property)

# MODELS ----------------------------------------------------------------------

# set model formulas
# census only
vars1 = paste(names(train)[c(4:11)], collapse = " + ")

# census + POI
vars2 = paste(names(train)[c(4:11, 13:21)], collapse = " + ")

# census + POI + Taxi
vars3 = paste(names(train)[c(4:11, 13:22)], collapse = " + ")

# census + POI + TWitter
vars4 = paste(names(train)[c(4:21)], collapse = " + ")

# census + Taxi + Twitter
vars5 = paste(names(train)[c(4:12, 22)], collapse = " + ")

# census + taxi
vars6 = paste(names(train)[c(4:11, 22)], collapse = " + ")

# census + twitter
vars7 = paste(names(train)[c(4:12)], collapse = " + ")

# full set
vars8 = paste(names(train)[-c(1:3)], collapse = " + ")

# PREPARATION -----------------------------------------------------------------

# read polygons to create neighbour matrix
NY_poly = readShapePoly("D:/crime-data/nyc-taxi-data/nyct2010_15b/nyct2010.shp")
NY_poly$region.id = as.character(as.numeric(row.names(NY_poly)) + 1)

# exclude Farway Rock and City Island
NY_poly = subset(NY_poly, NY_poly$region.id %in% train[,gid])

# create neighbours list
neighbours = poly2nb(NY_poly, row.names = NY_poly$region.id)

# create weights list
BList = nb2listw(neighbours, style = "B") 

rm(neighbours, NY_poly)

# CAR -------------------------------------------------------------------------

# loop through variables and set models

CAR_list = lapply(1:8, function(i) CARlm(as.formula(paste("occ ~ ", get(paste0("vars", i)))),
                                          data = train,
                                          listw = BList))

# Predictions
CAR_predictions = lapply(CAR_list, function(x) predict_CAR(object=x, 
                                                            newdata=test, 
                                                            listw=BList))

save(CAR_list, file = "Models/CAR_property.Rdata")
save(CAR_predictions, file = "Predictions/pred_CAR_property.Rdata")


# SAR -------------------------------------------------------------------------

# loop through variables and set models

SAR_list = lapply(1:8, function(i) SARlm(as.formula(paste("occ ~ ", get(paste0("vars", i)))),
                                          data = train,
                                          listw = BList))

SAR_predictions = lapply(SAR_list, function(x) predict_SAR(object = x, 
                                                            newdata = test,
                                                            listw = BList))

save(SAR_list, file = "Models/SAR_property.Rdata")
save(SAR_predictions, file = "Predictions/pred_SAR_property.Rdata")

# POISSON ---------------------------------------------------------------------

PCAR_list = lapply(1:8, function(i) CAR_poisson(as.formula(paste("occ ~ ", get(paste0("vars", i)))),
                                                 data = train,
                                                 listw = BList, verbose=T,
                                                 sigma_start = 1000))

PAR_predictions = lapply(PCAR_list, function(x) predict_Poisson(x, test))

save(PCAR_list, file = "Models/PAR_property.Rdata")
save(PAR_predictions, file = "Predictions/pred_PAR_property.Rdata")

# GLM -------------------------------------------------------------------------

GLM_list = lapply(1:8, function(i) glm(as.formula(paste("occ ~ ", get(paste0("vars", i)))),
                                    data = train,
                                    family = "poisson"))

GLM_predictions = lapply(GLM_list, function(x) predict(x, test))

save(GLM_list, file = "Models/GLM_poisson_property.Rdata")
save(GLM_predictions, file = "Predictions/pred_GLM_poisson_property.Rdata")


# LIR --------------------------------------------------------------------------

LIR_list = lapply(1:8, function(i) lm(as.formula(paste("occ ~ ", get(paste0("vars", i)))),
                                       data = train))

LIR_predictions = lapply(LIR_list, function(x) predict(x, test))

save(LIR_list, file = "Models/LIR_property.Rdata")
save(LIR_predictions, file = "Predictions/pred_LIR_property.Rdata")
