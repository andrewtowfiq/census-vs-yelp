library(httr)
library(jsonlite)
library(tidyverse)

# get api key from file, set as yelp.api.key <- 'YOUR KEY'
source('yelp_api_key.R')

# all zip codes in wa state
zip.codes <- read.csv('data/Zip Code .csv', stringsAsFactors = FALSE)

# counties in wa state
counties <- unique(zip.codes$County)

# search url
base <- 'https://api.yelp.com/v3/businesses/search'

# header to add
yelp.header <- paste("Bearer", yelp.api.key)

# returns a df of the top 50 restaurants based off yelp 
RestaurantSearch <- function(zip) {
   # query for specific zip code
   query <- list(
      location = zip,
      categories = "restaurants",
      limit = 50
   )
   # request
   search.response <- GET(url = base,
                          query = query,
                          add_headers(Authorization = yelp.header))
   # turn JSON object into data frame
   results <- fromJSON(content(search.response, 'text'))
   restaurants <- results$businesses
   return(restaurants)
}

# the first zip code
zip.code1 <- zip.codes[1, 1]
zip.restaurants <- RestaurantSearch(zip.code1)

flatten.coordinates <- zip.restaurants$coordinates
flatten.location <- zip.restaurants$location
categories <- zip.restaurants$categories
county <- (filter(zip.codes, Zip.Code == zip.code1) %>% 
   select(County))[1, 1]

# select certain columns and add more
zip.restaurants <- select(zip.restaurants, id, name, review_count, rating, 
   price) %>% 
   mutate(categories = categories,
          zip_code = zip.code1, 
          address = flatten.location$address1, 
          city = flatten.location$city, 
          county = county, 
          latitude = flatten.coordinates$latitude, 
          longitude = flatten.coordinates$longitude)

# loop to get the rest of the zipcodes
for (i in 2:length(zip.codes$Zip.Code)) {
   zip.code <- zip.codes[i, 1]
   yelp.data <- RestaurantSearch(zip.code)
   
   flatten.coordinates <- yelp.data$coordinates
   flatten.location <- yelp.data$location
   categories <- yelp.data$categories
   county <- (filter(zip.codes, Zip.Code == zip.code) %>% 
                 select(County))[1, 1]
   
   # change row names
   start.row <- length(zip.restaurants$name) + 1
   end.row <- start.row + length(yelp.data$name) - 1
   rownames(yelp.data) <- c(start.row:end.row)
   
   # select certain columns and add more
   yelp.data <- select(yelp.data, id, name, review_count, rating, price) %>%
      mutate(categories = categories,
             zip_code = flatten.location$zip_code, 
             address = flatten.location$address1, 
             city = flatten.location$city, 
             county = county, 
             latitude = flatten.coordinates$latitude, 
             longitude = flatten.coordinates$longitude)
   # combine data frames
   zip.restaurants <- rbind(zip.restaurants, yelp.data)
}
