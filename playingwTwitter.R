install.packages("RJSONIO")
library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)

# Declare Twitter API Credentials
api_key <- "vBtucIN1AHwlxWCWo6SzNL7Xz" # From dev.twitter.com
api_secret <- "rO5Cf6M9PKuE9kJJ8x72NSjHuhEjb4QOhHtwk6ocrAzPg0H5EU" # From dev.twitter.com
token <- "802556369022885888-8KqCI1mYQsViOj8WptUbrtK6cVkc3W6" # From dev.twitter.com
token_secret <- "fLRpk6QaGyN9nBpEsxtfOdzf7FfxmyNill1slGgXCkINt" # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

KaneTweets<-searchTwitter('Kane',geocode='51.603211,-0.066430,10mi',
                          since='2014-12-31',n=5000, retryOnRateLimit=1)
KaneTweetsData<-twListToDF(KaneTweets) # Turn into data frame

#middle: washington park: 40.691331, -73.975586 # 40 km radius

YankeeTweets<-searchTwitter('yankees',geocode='40.691331,-73.975586,25mi',
                          since='2014-12-31',n=5000, retryOnRateLimit=1)
yankeeTweetsData <- twListToDF(KaneTweets)
NYTweets <- searchTwitter("NY", geocode='40.691331,-73.975586,25mi',
                          since='2014-12-31',n=5000, retryOnRateLimit=1)
NYTweetsData <- twListToDF(NYTweets)

#lower left corner 40.491155, -74.256069
# top right corner 40.922536, -73.659435

