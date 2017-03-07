#--------------------------#
#                          #
# Cleaning the Census data #
#                          #
#--------------------------#

library(RPostgreSQL)
library(dplyr)

path <- "D:/crime-data/"
con1 <- dbConnect(dbDriver("PostgreSQL"), dbname = "crime-data", user = "postgres", host = "localhost")

# get NYC 
tractID <- dbGetQuery(con1, "SELECT * FROM geo_mapping;") # wait is this enough for visualisation

census <- read.csv(paste0(path, "2010_NYC_Census_Tracts.csv"), stringsAsFactors = F)

census <- 
  census %>% 
  # remove first row
  filter(row_number()!=1) %>%
  # subset to tracts in NYC
  filter(GEO.id2 %in% tractID$ctlabel) %>%
  # make columns numeric
  mutate_each(funs(as.numeric), -GEO.id2) %>%
  # create new variable young_male for males between 15-29 and set column as character
  mutate(young_male = rowSums(cbind(HD02_S030, HD02_S031, HD02_S032), na.rm = T),
         GEO.id2 = as.character(GEO.id2)) %>%
  # select columns for analysis
  select(ctlabel = GEO.id2,
         population = HD01_S001,
         age = HD01_S020,
         male = HD02_S026,
         young_male,
         white = HD02_S078,
         black = HD02_S079,
         native = HD02_S080,
         asian = HD02_S081,
         pacific = HD02_S089,
         hispanic = HD02_S107,
         vacancy = HD02_S171,
         female_head = HD02_S157,
         institution = HD02_S144) %>%
  # make numbers as percentages
  mutate_each(funs(./100), -ctlabel, -population, -age)

# tracts with population < 50 and Riker's Island are set to NA
ind <- which(census$population < 50 | census$ctlabel == "36005000100")
census[ind, c(names(census)[-1])] <- NA

#write cleaned file for data visualisation
write.csv(census, file = paste0(path, "2010_NYC_Census_cleaned.csv"), row.names = F)

# remove tracts with all empty values
census <- census[rowSums(apply(census, 2, is.na)) < (ncol(census) - 1),]

# write cleaned file for analysis
write.csv(census, file = paste0(path, "2010_NYC_Census_analysis.csv"), row.names = F)
