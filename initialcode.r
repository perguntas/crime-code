#-------------------------#
#                         #
# Cleaning the crime data #
#                         #
#-------------------------#

setwd("D:/crime-data")

crime16 <- read.csv("NYPD_Complaint_Map_Year_to_Date16.csv", stringsAsFactors = F)

head(crime16)

colnames(crime16) <- c("cmpl_num", "fromDate", "fromTime", "toDate", "toTime", "reportDate", "KY_CD", "offense", "PD_CD",
                      "PD_DESC", "CRM_ATPT_CPTD_CD", "Category", "Jurisdiction", "Boro", "ADDR_PCT_CD", 
                      "LocDesc", "Location", "PARKS_NM", "HADEVELOPT", "X_Coord", "Y_Coord", "Latitude", "Longitude", "Lat_Lon")

# formatting the date columns
crime16$fromDate   <- format(as.Date(crime16$fromDate, "%m/%d/%Y"), "%Y-%m-%d")
crime16$toDate     <- format(as.Date(crime16$toDate, "%m/%d/%Y"), "%Y-%m-%d")
crime16$reportDate <- format(as.Date(crime16$reportDate, "%m/%d/%Y"), "%Y-%m-%d")




