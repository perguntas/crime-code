#-------------------------#
#                         #
# Cleaning the crime data #
#                         #
#-------------------------#

setwd("D:/crime-data")

# For 2016
crime16 <- read.csv("NYPD_Complaint_Map_Year_to_Date16.csv", stringsAsFactors = F)

colnames(crime16) <- c("cmpl_num", "fromDate", "fromTime", "toDate", "toTime", "reportDate", "KY_CD", "offense", "PD_CD",
                      "PD_DESC", "CRM_ATPT_CPTD_CD", "Category", "Jurisdiction", "Boro", "ADDR_PCT_CD", 
                      "LocDesc", "Location", "PARKS_NM", "HADEVELOPT", "X_Coord", "Y_Coord", "Latitude", "Longitude", "Lat_Lon")

# formatting the date columns
crime16$fromDate   <- format(as.Date(crime16$fromDate, "%m/%d/%Y"), "%Y-%m-%d")
crime16$toDate     <- format(as.Date(crime16$toDate, "%m/%d/%Y"), "%Y-%m-%d")
crime16$reportDate <- format(as.Date(crime16$reportDate, "%m/%d/%Y"), "%Y-%m-%d")

# set public parks to binary
crime16$PARKS_NM <- ifelse(crime16$PARKS_NM == "", 0, 1)


# For 2006 - 2015
crime0615 <- read.csv("NYPD_Complaint_Map_Historic.csv", stringsAsFactors = F)
colnames(crime0615) <- colnames(crime16)

crime0615$fromDate   <- format(as.Date(crime0615$fromDate, "%m/%d/%Y"), "%Y-%m-%d")
crime0615$toDate     <- format(as.Date(crime0615$toDate, "%m/%d/%Y"), "%Y-%m-%d")
crime0615$reportDate <- format(as.Date(crime0615$reportDate, "%m/%d/%Y"), "%Y-%m-%d")

crime0615$PARKS_NM <- ifelse(crime0615$PARKS_NM == "", 0, 1)


# split data in half
firsthalfyears <- c(paste(seq(2006, 2009)))
crime0609 <- crime0615[grepl(paste(firsthalfyears, collapse = "|"), crime0615$fromDate),]
crime1015 <- crime0615[!grepl(paste(firsthalfyears, collapse = "|"), crime0615$fromDate),]

write.csv(crime0609, "NYPD_Complaints0609.csv")
write.csv(crime1015, "NYPD_Complaints1015.csv")
write.csv(crime16, "NYPD_Complaints16.csv")
