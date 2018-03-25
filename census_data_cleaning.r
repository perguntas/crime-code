#--------------------------#
#                          #
# Cleaning the Census data #
#                          #
#--------------------------#

library(RPostgreSQL)
library(dplyr)
library(magrittr)

path = "D:/crime-data/"
con1 = dbConnect(dbDriver("PostgreSQL"), dbname = "crime-data", user = "root", host = "localhost")

# get NYC ID matchfile
tractID = dbGetQuery(con1, "SELECT gid, ctlabel FROM nyct2010_centroids;")

# load census data
census = read.csv(paste0(path, "2010_NYC_Census_Tracts.csv"), stringsAsFactors = F)

census = 
  census %>% 
  # remove first row
  filter(row_number()!=1) %>%
  # subset to tracts in NYC
  #filter(GEO.id2 %in% tractID$ctlabel) %>%
  # make columns numeric
  mutate_each(funs(as.numeric), -GEO.id2) %>%
  # create new variable young_male for males between 15-29 and set column as character
  mutate(young_male = rowSums(cbind(HD02_S030, HD02_S031, HD02_S032), na.rm = T),
         GEO.id2 = as.character(GEO.id2)) %>%
  # select columns for analysis
  dplyr::select(ctlabel = GEO.id2,
         population = HD01_S001,
         age = HD01_S020,
         male = HD02_S026,
         young_male,
         white = HD02_S078,
         black = HD02_S079,
         # native = HD02_S080,
         asian = HD02_S081,
         # pacific = HD02_S089,
         hispanic = HD02_S107,
         vacancy = HD02_S171,
         female_head = HD02_S157,
         institution = HD02_S144) %>%
  # make numbers as percentages
  mutate_each(funs(./100), -ctlabel, -population, -age)

# tracts with population < 50 and Riker's Island are set to NA
ind = which(census$population < 50 | census$ctlabel == "36005000100")
census[ind, c(names(census)[-1])] = NA

# add gid column and subset to NY values only
census = left_join(tractID, census)

# subset to exclude Staten Island, Faraway Rock, and City Island
excl = c(grepl("36085", census$ctlabel) | 
                census$gid %in% c(763:766, 951, 973, 1052:1053, 1197, 1259, 
                                  1365:1366, 1394, 1420:1421, 1519, 1570:1573, 
                                  1709:1710, 1731, 2002:2003, 2008:2009, 2016,
                                  866))
census = subset(census, !excl)

# write cleaned file for data visualisation
write.csv(census, file = paste0(path, "2010_NYC_Census_cleaned.csv"), row.names = F)

# remove tracts with empty values
census = census[rowSums(is.na(census)) == 0,]

# write cleaned file for analysis
write.csv(census, file = paste0(path, "2010_NYC_Census_analysis.csv"), row.names = F)

