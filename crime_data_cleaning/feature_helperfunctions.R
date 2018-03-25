# Data Preparation Functions --------------------------------------------------


# TAXI DATA FUNCTIONS ---------------------------------------------------------

# Function to ensure correct labels for contingency matrix
taxi_factor = function(x, y) {
    factor(as.character(x), levels = sort(unique(y)))
}

# diagonal for sparse matrix
sparse_diagonal = function(x) {
    diag(x) = 0
    drop0(x)
}

# tidy diagonal and normalise by destination/source
tidy_sparse = function(lt, normal) {
    
    # tidy diagonal
    lt = lapply(lt, sparse_diagonal)
    
    # normalisation by destination
    if (normal == "destination") {
        lt = lapply(lt, function(y) sweep(y, 2, colSums(y, na.rm = T), FUN = "/"))
    } else if (normal == "source") {
        # normalisation by source
        lt = lapply(lt, function(y) sweep(y, 1, rowSums(y, na.rm = T), FUN = "/"))
    } else if (normal == "counts") {
        lt = lt
    } else {
        stop("invalid normalisation")
    }
    
    # set NaN values (possible due to division by 0) to sparse
    lt = lapply(lt, function(y) {
        y@x[is.na(y@x)] = 0
        y = drop0(y)
        y
    })
    return(lt)
}

# CRIME DATA FUNCTIONS --------------------------------------------------------

# aggregates case crime data to weekly data per census tracts for aggr=T, all violent/property crime cases are
# counted, for aggr=F, the crime types within are preserved

weekly_crime_aggregation = function(data, weeks, crime, aggr = T) {
    warning("This function aggregates based on week. Careful if data from multiple years are used")
    
    # Sanity checks
    cols = c("ofns_desc", "loc_gid", "cmplnt_fr_dt")
    
    if (any(!(cols %in% colnames(data)))) 
        stop("invalid data columns: ofns_desc, loc_gid, cmplt_fr_dt")
    if (is.numeric(weeks) == F) 
        stop("invalid weeks format: numeric vector")
    if ((is.vector(crime) & is.character(crime)) == F) 
        stop("invalid format for crime: character vector")
    
    # if aggregation, no distintion between crime type offenses is made (ie more aggregation)
    if (aggr == T) {
        result = data %>% # subset to cases in crime type
        filter(ofns_desc %in% crime) %>% # filter crimes in analysis weeks
        mutate(week = lubridate::isoweek(cmplnt_fr_dt)) %>% filter(week %in% weeks) %>% group_by(week, loc_gid) %>% 
            summarise(occ = n()) %>% rename(gid = loc_gid) %>% setorder(week, gid)
        setDT(result)
        
    } else if (aggr == F) {
        # for aggr == F, separate between offenses
        result = data %>% # subset to cases in crime type
        filter(ofns_desc %in% crime) %>% # filter crimes in analysis weeks
        mutate(week = lubridate::isoweek(cmplnt_fr_dt)) %>% filter(week %in% weeks) %>% group_by(week, loc_gid, ofns_desc) %>% 
            summarise(occ = n()) %>% rename(gid = loc_gid) %>% setorder(Week, gid)
        setDT(result)
    }
    result = result[complete.cases(result), ]
    return(result)
    
}

# same idea as in weekly_crime_aggregation, in this case, counts all crimes and does not count per week
crime_aggregation = function(data, weeks, crime, aggr = T) {
    warning("This function aggregates based on week. Careful if data from multiple years are used")
    # Sanity checks
    cols = c("ofns_desc", "loc_gid", "cmplnt_fr_dt")
    
    if (any(!(cols %in% colnames(data)))) 
        stop("invalid data columns: ofns_desc, loc_gid, cmplt_fr_dt")
    if (!(is.vector(crime) & is.character(crime))) 
        stop("invalid format for crime: character vector")
    
    if (aggr == T) {
        result = data %>% # subset to cases in crime type
        dplyr::filter(ofns_desc %in% crime) %>% # filter crimes in analysis weeks
        mutate(week = lubridate::isoweek(cmplnt_fr_dt)) %>% filter(week %in% weeks) %>% group_by(loc_gid) %>% summarise(occ = n()) %>% 
            rename(gid = loc_gid) %>% setorder(gid)
    } else if (aggr == F) {
        result = data %>% # subset to cases in crime type
        filter(ofns_desc %in% crime) %>% # filter crimes in analysis weeks
        mutate(week = lubridate::isoweek(cmplnt_fr_dt)) %>% filter(week %in% weeks) %>% group_by(loc_gid, ofns_desc) %>% 
            summarise(occ = n()) %>% dplyr::rename(gid = loc_gid) %>% setorder(gid)
    }
    result = result[complete.cases(result), ]
    return(result)
}


# aggregates dataframe with individuals tweets and census tract id to frame with counts and night counts per week
twitter_aggregation = function(data, weeks) {
    # Sanity checks
    cols = c("created_at", "gid")
    
    if (any((cols %in% colnames(data)) == F)) 
        stop("invalid data columns: created_at, gid")
    if (is.numeric(weeks) == F) 
        stop("invalid weeks format: numeric vector")
    
    # define if a tweet is sent at night
    nighttime = c(22, 23, 0, 1, 2, 3, 4, 5)
    
    result = twitter %>% # remove cases outside of census tracts (ie ferries)
    filter(complete.cases(gid)) %>% # set week and nighttime variable
    mutate(week = lubridate::isoweek(created_at), night = ifelse(hour(created_at) %in% nighttime, 1, 0)) %>% filter(week %in% 
        weeks) %>% group_by(week, gid) %>% summarise(tweet_counts = n(), night_tweets = sum(night))
    setDT(result)
    return(result)
}

# for foursquare table with venue data incl. categories and census tract id, aggregates to counts of each category
# per census tract
foursquare_aggregation = function(data) {
    stopifnot(c("category", "gid") %in% colnames(data))
    
    result = data %>% filter(complete.cases(.)) %>% # translate categories
    mutate(category = recode(category, `4d4b7104d754a06370d81259` = "entertainment", `4d4b7105d754a06372d81259` = "uni", 
        `4d4b7105d754a06374d81259` = "food", `4d4b7105d754a06376d81259` = "nightlife", `4d4b7105d754a06377d81259` = "outdoors", 
        `4d4b7105d754a06375d81259` = "professional", `4e67e38e036454776db1fb3a` = "residential", `4d4b7105d754a06378d81259` = "shops", 
        `4d4b7105d754a06379d81259` = "travel")) %>% # group by census tract
    group_by(gid, category) %>% dplyr::summarise(counts = n()) %>% # mutate to new columns
    tidyr::spread(category, counts) %>% # replace NA values
    mutate_each(funs(replace(., is.na(.), 0)), -gid)
    
    return(result)
}
