library(ROAuth)
library(jsonlite)
library(httr)

# create OAUTH, run once ------------------------------------------------------
requestURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "redacted"
consumerSecret = "redacted"

my_oauth = OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, requestURL = requestURL, accessURL = accessURL, 
    authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

save(my_oauth, file = "D:/crime-data/Twitter/my_oauth/oauth")


# LOAD FUNCTIONS---------------------------------------------------------------

getStatuses = function(ids = NULL, filename, my_oauth, verbose = TRUE, sleep = 1) {
    
    ## url to call
    url = "https://api.twitter.com/1.1/statuses/lookup.json"
    ids.left = ids
    
    limit = getLimitStatuses(my_oauth)
    
    ## while there's more data to download...
    while (length(ids.left) > 0) {
        ## making API call
        query = list(id = paste(ids.left[1:100], collapse = ","))
        url.data = GET_retry(url, query = query, httr::config(token = twitter_token))
        Sys.sleep(sleep)
        ## one API call less
        limit = limit - 1
        
        # parsing JSON
        json.data = httr::content(url.data)
        if (length(json.data$error) != 0) {
            message(url.data)
            stop("error downloading IDs! First ID not downloaded", ids[1])
        }
        
        # results to keep
        keep = c("created_at", "id_str", "text", "coordinates", "lang")
        data_list = lapply(json.data, function(x) subset(x, names(x) %in% keep))
        
        ## writing to disk
        conn = file(filename, "a")
        invisible(lapply(data_list, function(x) writeLines(jsonlite::toJSON(x, null = "null"), con = conn, useBytes = TRUE)))
        close(conn)
        
        # removing IDs done
        ids.left = ids.left[-(1:100)]
        
        while (limit == 0) {
            Sys.sleep(10)
            rate.limit = getLimitRate(my_oauth)
            if (rate.limit < 100) {
                Sys.sleep(300)
            }
            limit = getLimitStatuses(my_oauth)
            message(limit, " API call left\n")
        }
        
    }
}

getLimitRate = function(my_oauth) {
    url = "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params = list(resources = "followers,application")
    response = my_oauth$OAuthRequest(URL = url, params = params, method = "GET", cainfo = system.file("CurlSSL", "cacert.pem", 
        package = "RCurl"))
    return(unlist(rjson::fromJSON(response)$resources$application$`/application/rate_limit_status`[["remaining"]]))
}



getLimitStatuses = function(my_oauth) {
    url = "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params = list(resources = "statuses,application")
    response = my_oauth$OAuthRequest(URL = url, params = params, method = "GET", cainfo = system.file("CurlSSL", "cacert.pem", 
        package = "RCurl"))
    return(unlist(rjson::fromJSON(response)$resources$statuses$`/statuses/lookup`[["remaining"]]))
    
}

GET_retry = function(url, ..., times = 20) {
    res = suppressWarnings(httr::GET(url, ...))
    if (res$status_code > 226) {
        stat = 500
        while (stat > 226) {
            res = suppressWarnings(httr::GET(url, ...))
            stat = res$status_code
        }
    }
    return(res)
}


# Load OAUTH ------------------------------------------------------------------

oauth_folder = "D:/crime-data/Twitter/oauth/"

# load authorization
load(paste0(oauth_folder, list.files(oauth_folder)))

# preparing OAuth token for httr
options(httr_oauth_cache = FALSE)
app = httr::oauth_app("twitter", key = my_oauth$consumerKey, secret = my_oauth$consumerSecret)
credentials = list(oauth_token = my_oauth$oauthKey, oauth_token_secret = my_oauth$oauthSecret)
twitter_token = httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE), app = app, credentials = credentials)


# Prepare ID files ------------------------------------------------------------

# set timeframe
dates = seq(as.Date("2015-06-01"), as.Date("2015-11-30"), by = "days")

# set relevant area
ny_counties = c("36005", "36047", "36061", "36081", "36085")

# set input data
t_grid = expand.grid(dates = dates, counties = ny_counties)
t_grid$months = format(t_grid$dates, "%Y-%m")

# rehydrate tweets
for (i in 1:nrow(t_grid)) {
    path = paste0("D:/crime-data/Twitter/county_id_", t_grid$months[i], "/", t_grid$dates[i], "/", t_grid$dates[i], 
        "_", t_grid$counties[i], "_ID.txt")
    id_file = read.table(path)[, 1]
    getStatuses(ids = id_file, filename = paste0("tweets-", t_grid$months[i]), my_oauth = my_oauth)
}


library(jsonlite)
library(plyr)

json_parsing = function(json_file) {
    # set time to english to ensure proper weekday matching
    if (!grepl("English", Sys.getlocale("LC_TIME"))) {
        Sys.setlocale("LC_TIME", "English")
    }
    
    # read data
    data = jsonlite::fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse = ",")), flatten = T)
    
    # format columns
    data[5:6] = lapply(data[5:6], function(x) {
        x[sapply(x, is.null)] = NA
        x
    })
    data[1:4] = lapply(data[1:4], unlist)
    data[[6]] = do.call(rbind.fill.matrix, lapply(data[[6]], t))
    
    # combine columns
    data = do.call(cbind, data[c(1:4, 6)])
    colnames(data) = c("created_at", "id_str", "text", "language", "long", "lat")
    
    # remove cases without geo-location
    data = data[complete.cases(data), ]
    data = data[!duplicated(data[, 2]), ]
    # format timestamp
    data[, 1] = as.character(strptime(data[, 1], "%a %b %d %H:%M:%S %z %Y"))
    
    return(data)
}

# Read in json files and parse and format
t_files = format(seq(as.Date("2015-06-01"), as.Date("2015-11-30"), by = "month"), "%Y-%m")

for (i in 1:length(t_files)) {
    t_file = paste0("tweets-", t_files[i])
    assign(paste0("ids", i), json_parsing(t_file))
    
    # save full tweets
    write.csv(get(paste0("ids", i)), file = paste0("full_ids", i, ".csv"), row.names = F)
    
    # write truncated (without text) csv for activity analysis in psql
    write.csv(get(paste0("ids", i))[, -3], file = paste0("D:/crime-data/Foursquare/ids", i, ".csv"), row.names = F)
}
