#--------------------------#
#                          #
# Cleaning the Census data #
#                          #
#--------------------------#

setwd("D:/crime-data")

census <- read.csv("2010_NYC_Census_Tracts.csv", stringsAsFactors = F)

census[1,] <- tolower(census[1,])

# Remove all columns without content (denoted by " ( X ) " in the census file)
remove <- colnames(census)[grepl("( X )", census[2,])]
census <- census[, !colnames(census) %in% remove]

# Remove all columns related to absolute number, excluding housing numbers and median age, excluding the first occurrence
remove <- colnames(census)[(grepl("number", census[1,]) & !grepl("hous|median", census[1,]))][-1]
census <- census[, !colnames(census) %in% remove]

