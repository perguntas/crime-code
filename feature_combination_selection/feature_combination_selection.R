# FEATURE COMBINATION SELECTION -----------------------------------------------

# This file considers all feature combinations of taxi, twitter, and POI features
# by running a CAR model on them
# and comparing the predictions

# Load Libraries --------------------------------------------------------------

library(data.table)
library(maptools)    # to load shapefile
library(spdep)       # to set neighbour lists
library(rgdal)       # for spatial transformation
library(mapproj)     # necessary to ensure consistent coordinate mapping
library(matrixcalc)
library(xtable)      # (optional: to print results for latex file)


# load model function
source("CARlm.R")
# load prediction evaluation function
source("evaluate_predictions.R")

# load property data (violent data is loaded later)
load("final_frame_property15.Rdata")

# split into training and validation data 
train = final_frame_property[week %in% c(23:44)]
valid = final_frame_property[week %in% c(45:46)]

# set all the taxi and twitter variables to loop through
vars = expand.grid(taxi=c(34:36), twitter=c(15:20))
eval_property = list()
eval_crime = list()

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

# RUNNING THE PROPERTY MODELS -------------------------------------------------

for (i in 1:18){
  # for count POI
  
  # paste variables
  variables = paste(names(final_frame_property)[c(4:6,9:13, vars$taxi[i],
                                                  21:29, vars$twitter[i])],
                     collapse = " + ")
  # run the model
  CAR = CARlm(as.formula(paste("occ ~ ", variables)), data = train, listw = BList,
                family = "CAR", verbose=F)
  # make predictions
  CAR_pred = predict_CAR(CAR, valid)
  # save evaluation
  eval_property[[i]] = evaluation(CAR_pred, valid[,"occ"])
}

for (i in 19:36){
  # for normalised POI
  
  # paste variables
  variables = paste(names(final_frame_property)[c(4:6,9:13, vars$taxi[i-18],
                                                  37:45, vars$twitter[i-18])],
                     collapse = " + ")
  # run the model
  CAR = CARlm(as.formula(paste("occ ~ ", variables)), data = train, listw = BList,
               family = "CAR", verbose=F)
  # make predictions
  CAR_pred = predict_CAR(CAR, valid)
  # save evaluation
  eval_property[[i]] = evaluation(CAR_pred, valid[,"occ"])
}
for (i in 37:54){
  # for entropy H
  
  # paste variables
  variables = paste(names(final_frame_property)[c(4:6, 9:13, vars$taxi[i-36],
                                                  46, vars$twitter[i-36])],
                     collapse = " + ")
  CAR = CARlm(as.formula(paste("occ ~ ", variables)), data = train, listw = BList,
               family = "CAR", verbose=F)
  # make predictions
  CAR_pred = predict_CAR(CAR, valid)
  # save evaluation
  eval_property[[i]] = evaluation(CAR_pred, valid[,"occ"])
}

# show results (optional)
# in console: do.call(rbind, eval_property)
# output latex: xtable(do.call(rbind, eval_property), digits=c(0, 4, 4))

# REPEAT FOR VIOLENT DATA -----------------------------------------------------
rm(train, valid)

# load violent data
load("final_frame_violent15.Rdata")

# split into training and validation data 
train = final_frame_property[week %in% c(23:44)]
valid = final_frame_property[week %in% c(45:46)]

eval_crime

for (i in 1:18){
  # for count POI
  
  # paste variables
  variables = paste(names(final_frame_property)[c(4:6,9:13, vars$taxi[i],
                                                   21:29, vars$twitter[i])],
                     collapse = " + ")
  # run the model
  CAR = CARlm(as.formula(paste("occ ~ ", variables)), data = train, listw = BList,
               family = "CAR", verbose=F)
  # make predictions
  CAR_pred = predict_CAR(CAR, valid)
  # save evaluation
  eval_crime[[i]] = evaluation(CAR_pred, valid[,"occ"])
}

for (i in 19:36){
  # for normalised POI
  
  # paste variables
  variables = paste(names(final_frame_property)[c(4:6,9:13, vars$taxi[i-18],
                                                   37:45, vars$twitter[i-18])],
                     collapse = " + ")
  # run the model
  CAR = CARlm(as.formula(paste("occ ~ ", variables)), data = train, listw = BList,
               family = "CAR", verbose=F)
  # make predictions
  CAR_pred = predict_CAR(CAR, valid)
  # save evaluation
  eval_crime[[i]] = evaluation(CAR_pred, valid[,"occ"])
}
for (i in 37:54){
  # for entropy H
  
  # paste variables
  variables = paste(names(final_frame_property)[c(4:6, 9:13, vars$taxi[i-36],
                                                   46, vars$twitter[i-36])],
                     collapse = " + ")
  CAR = CARlm(as.formula(paste("occ ~ ", variables)), data = train, listw = BList,
               family = "CAR", verbose=F)
  # make predictions
  CAR_pred = predict_CAR(CAR, valid)
  # save evaluation
  eval_crime[[i]] = evaluation(CAR_pred, valid[,"occ"])
}

# show results (optional)
# in console: do.call(rbind, eval_crime)
# output latex: xtable(do.call(rbind, eval_crime), digits=c(0, 4, 4))