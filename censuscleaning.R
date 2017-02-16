#--------------------------#
#                          #
# Cleaning the Census data #
#                          #
#--------------------------#

setwd("D:/crime-data")

census <- read.csv("2010_NYC_Census_Tracts.csv", stringsAsFactors = F)

# Remove all columns related to absolute numbers, excluding housing numbers and median age, excluding the first occurrence
census[1,] <- tolower(census[1,])
remove <- colnames(census)[(grepl("number", census[1,]) & !grepl("hous|median|relationship", census[1,]))][-1]
census <- census[, !colnames(census) %in% remove]

remove <- colnames(census)[(grepl("percent", census[1,]) & grepl("occupancy|household", census[1,]))]
census <- census[, !colnames(census) %in% remove]

# remove all tenure columns, keep HD02_S181, HD02_S184
remove <- colnames(census)[grepl("tenure", census[1,])]
remove <- remove[!(remove %in% c("HD02_S181", "HD02_S184"))]
census <- census[, !colnames(census) %in% remove]

# delete explanative first line
census <- census[-1,]

# Remove all columns with only 100 (percent) or " ( X ) " (ie without content)
census <- Filter(function(x) (length(unique(x)) > 2), census)

write.csv(census, "2010_NYC_Census_cleaned.csv")

# to do:
# - remove tracts not in NYC
# - remove lines with minimal variation
# - correlation analysis?