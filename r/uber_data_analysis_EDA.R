#Let's analyze my Uber data
#Date Created: 01/15/2024
#Date Last Updated: 03/24/2024
#------------------------------

#----- Load libraries
message('Loading packages...')
suppressPackageStartupMessages({
  library(lubridate)
  library(dplyr)
  library(leaflet)
  library(stringr)
  library(ggplot2)
  library(tidyr)
  library(wordcloud)
  library(RColorBrewer)
  library(wordcloud2)
  library(tm)
  library(SemNetCleaner)
  library(padr)
  library(geosphere)
  library(jvamisc)
  
})

#----- Load data
path <- '~/uber_data_analysis/input/'
orders <- read.csv(paste0(path,'eats/eats_order_details.csv'))
restaurants <- read.csv(paste0(path,'eats/eats_restaurants.csv'))
rides <- read.csv(paste0(path,'rides/trips_data.csv'))

source('~/uber_data_analysis/r/catch_words.R')

#----- Clean up orders


#---- Dataset 1: Emailed to me from Uber
#---- Need to move time back 5 hours and date back one day if past midnight
orders_cleaned1 <- orders |>
  filter(Order.Time >= '2021-05-06') |>
  mutate(
    DateTime = as.POSIXct(Order.Time, format = "%Y-%m-%d %H:%M:%S"),
    Date = as.Date(DateTime),
    Time = format(DateTime, "%H:%M:%S")
  ) |>
  separate(Time, into = c('Hr','Min','Sec'),sep=':') |>
  mutate(
    Hr = as.numeric(Hr),
    Date = case_when(
      Hr <= 3 ~ (Date - days(1)),
      TRUE ~ Date
    ),
    Hr = case_when(
      Hr <= 3 ~ (Hr + 24 -5),
      TRUE ~ (Hr - 5)
    ),
    Time = paste0(Hr,":",Min,":",Sec),

  )


#----- Dataset 2: Painstaking manual entry
#----- Need to clean up order item names
orders_cleaned2 <- orders |>
  filter(Order.Time < '2021-05-06') |>
  mutate(
    Item.Remove.Prices = str_extract(Item.Name,".*(?=\\|)"),
    Order.Name = ifelse(is.na(Item.Remove.Prices),Item.Name,Item.Remove.Prices)
  ) |>
  select(-c('Item.Remove.Prices','Item.Name')) |>
  relocate(Order.Name,.before = Customizations) |>
  rename(Item.Name = Order.Name) |>
  mutate(
    DateTime = as.POSIXct(Order.Time, format = "%Y-%m-%d %H:%M:%S"),
    Date = as.Date(DateTime),
    Time = format(DateTime, "%H:%M:%S")
  ) |>
  separate(Time, into = c('Hr','Min','Sec'),sep=':') |>
  mutate(
    Hr = as.numeric(Hr),
    Time = paste0(Hr,":",Min,":",Sec)
  )
    
#----- Combine datasets
orders_cleaned <- rbind(
  orders_cleaned1,
  orders_cleaned2
)

#----- Group price spent per order by year
order_df <- orders_cleaned |>
  filter(Order.Status == "COMPLETED") |>
  select(Order.Time, Order.Price) |>
  distinct() |>
  mutate(year = lubridate::year(Order.Time)) |>
  filter(year < 2024) |>
  group_by(year) |>
  summarise(avg_spent_order = mean(Order.Price, na.rm=TRUE),
            total_spent_order = sum(Order.Price, na.rm=TRUE),
            num_order = n())

order_df
#----- Which restaurants have I ordered from the most?

restaurants <- restaurants |>
  mutate(
    Restaurant.Name = case_when(
      Restaurant.ID == "b907d314-d027-4044-9e90-843144b9d442" ~ 'Tropical Smoothie Cafe (2672 Avenir Place)',
      Restaurant.ID == "81f0df3d-c76e-4bab-a997-379eaea9b580" ~ 'The Little Beet',
      Restaurant.ID == "8da36ea8-bf39-4f68-9876-533a138d522e" ~ "Paisano's Pizza (1815 Wisconsin Ave NW)",
      Restaurant.ID == "c7ca32fd-d6d2-4e68-8279-a590ddd6181f" ~ "Focaccia Sandwiches (Tenleytown)",
      
      TRUE ~ Restaurant.Name
    )
  )

rest_df <- restaurants |>
  group_by(Restaurant.Name,Restaurant.ID) |>
  summarise(count = n()) |>
  arrange(desc(count))

rest_df
#----- Label the cuisine

rest_df <- rest_df |>
  mutate(
    cuisine_category = case_when(
      grepl(paste(italian, collapse = '|'), Restaurant.Name, ignore.case = TRUE) ~ "Italian",
      grepl(paste(indian, collapse = '|'), Restaurant.Name, ignore.case = TRUE) ~ "Indian",
      grepl(paste(thai, collapse = '|'), Restaurant.Name, ignore.case = TRUE) ~ "Thai",
      grepl(paste(hawaiian, collapse = '|'), Restaurant.Name, ignore.case = TRUE) ~ "Hawaiian",
      grepl(paste(greek, collapse = '|'), Restaurant.Name, ignore.case = TRUE) ~ "Greek",
      grepl(paste(mexican,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Mexican",
      grepl(paste(dessert,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Dessert",
      grepl(paste(american, collapse = '|'), Restaurant.Name, ignore.case = TRUE) ~ "American",
      grepl(paste(salad_healthy,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Salad/Healthy",
      grepl(paste(korean,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Korean",
      grepl(paste(ethiopian,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Ethiopian",
      grepl(paste(chinese,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Chinese",
      grepl(paste(japanese,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Japanese",
      grepl(paste(juice,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Juice",
      grepl(paste(vietnamese,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Vietnamese",
      grepl(paste(mediterranean,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Mediterranean",
      grepl(paste(salvadoran,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Salvadoran",
      grepl(paste(israeli,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Israeli",
      grepl(paste(peruvian,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Peruvian",
      grepl(paste(chinese_korean,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Chinese/Korean",
      grepl(paste(bangladeshi,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Bangladeshi",
      grepl(paste(south_african,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "South African",
      grepl(paste(bakery,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Bakery",
      grepl(paste(middle_eastern,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Middle Eastern",
      grepl(paste(afghan,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Afghanistani",
      grepl(paste(latin,collapse="|"), Restaurant.Name, ignore.case = TRUE) ~ "Latin",
      
      TRUE ~ "Other"
    )
  ) |>
  ungroup()  |>
  select(Restaurant.ID,cuisine_category) 

rest_df
#----- Create a word cloud of ordered items
orders_cleaned <- orders_cleaned |>
  mutate(cleaned_item_name = Item.Name)

#Step 1: Remove weird characters
orders_cleaned <- orders_cleaned |>
  mutate(
    cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\-', ' '),
    cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\/', ''),
    cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\~', ''),
    cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\&', ''),
    cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\.', ''),
    cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\_', ''),
    cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\*', ''),
    cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\+', ''),
    cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\|', ''),
    cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\$', '')
    
  )


#Step 2: Transform plural words into singular
orders_cleaned <- orders_cleaned |>
  mutate(
    cleaned_item_name = singularize(cleaned_item_name)
    )


text <- orders_cleaned$cleaned_item_name
# Create a corpus  
docs <- Corpus(VectorSource(text))

docs <- docs |>
  tm_map(removeNumbers) |>
  tm_map(removePunctuation) |>
  tm_map(stripWhitespace)

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))


dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(123) # for reproducibility 
wordcloud(
  words = df$word,
  freq = df$freq, 
  min.freq = 1,
  max.words=200, 
  random.order=FALSE, 
  rot.per=0.35,  
  colors=brewer.pal(8, "Dark2")
)


#----- Which cuisine did I order the most?
orders_with_cuisines <- inner_join(
  orders_cleaned,
  rest_df,
  by = 'Restaurant.ID',
  multiple = 'first'
)


cuisine_count <- orders_with_cuisines |>
  filter(Order.Status == "COMPLETED") |>
  select(Order.Time, cuisine_category) |>
  distinct() |>
  mutate(year = lubridate::year(Order.Time)) |>
  filter(year < 2024) |>
  group_by(cuisine_category) |>
  summarise(count = n()) |>
  arrange(desc(count))


top_cats <- unique(cuisine_count$cuisine_category)[1:5]

cuisine_count_year <- orders_with_cuisines |>
  filter(Order.Status == "COMPLETED") |>
  select(Order.Time, cuisine_category) |>
  distinct() |>
  mutate(year = lubridate::year(Order.Time)) |>
  filter(year < 2024) |>
  group_by(year, cuisine_category) |>
  summarise(count = n())

ggplot(data = cuisine_count_year |> filter(cuisine_category %in% top_cats), aes(x = year, y = count, color = cuisine_category)) +
  geom_line() + geom_point()


#----- Which items have I ordered from the most?

order_items_df <- orders |>
  group_by(Item.Name) |>
  summarise(count = n()) |>
  arrange(desc(count))


#----- When am I ordering the most?

hour_orders <- orders_cleaned |>
  group_by(Hr) |>
  summarise(count = n()) |>
  pad_int(
    by = 'Hr',
    start_val = 0,
    end_val = 23,
    step = 1
  ) |>
  mutate(
    count2 = ifelse(is.na(count),0,count)
  ) |>
  select(-c(count)) |>
  rename(count = count2)


hour_orders_per_year <- orders_cleaned |>
  mutate(year = as.factor(lubridate::year(Date))) |>
  group_by(year, Hr) |>
  summarise(count = n())|>
  pad_int(
    by = 'Hr',
    start_val = 0,
    end_val = 23,
    step = 1
  ) |>
  mutate(
    count2 = ifelse(is.na(count),0,count)
  ) |>
  select(-c(count)) |>
  rename(count = count2)


ggplot(data = hour_orders, aes(x = Hr, y = count)) +
  geom_line() + geom_point()

ggplot(data = hour_orders_per_year |> filter(year!=2024), aes(x = Hr, y = count, color = year)) +
  geom_line() + geom_point() +
  scale_color_brewer(palette = "RdYlBu")


#------------ Uber Rides
#----- Plot map of all pickup destinations
leaflet(rides |>filter(Trip.or.Order.Status == "COMPLETED")) |>
  addTiles() |>
  addCircleMarkers(
    lng = ~Begin.Trip.Lng, 
    lat = ~Begin.Trip.Lat, 
    popup = ~City
  )

#----- Plot map of all dropoff destinations
leaflet(rides |>filter(Trip.or.Order.Status == "COMPLETED")) |>
  addTiles() |>
  addCircleMarkers(
    lng = ~Dropoff.Lng, 
    lat = ~Dropoff.Lat, 
    popup = ~City,
    fillColor = 'purple',
    opacity = 1,
    fillOpacity = 0.5,
    stroke = FALSE,
    radius = ~Fare.Amount/10
  )


#----- Top addresses picked up from and dropped off at
ride_rank1 <- rides |>
  group_by(Dropoff.Address) |>
  summarise(count = n()) |>
  arrange(desc(count))

ride_rank2 <- rides |>
  group_by(Begin.Trip.Address) |>
  summarise(count = n()) |>
  arrange(desc(count))


#----- Correct times and dates for rides

#----- Make a time zone variable and adjust
#----- East Coast is all 5 hours ahead of truth
#----- West Coast is 8 hours ahead of truth

rides <- rides |>
  filter(Trip.or.Order.Status %in% c('COMPLETED','FARE_SPLIT')) |>
  mutate(
    time_zone = case_when(
      City %in% c('Asheville, NC','Baltimore-Maryland','Boston','Connecticut',
                  'New Jersey','New York City','Philadelphia','Pittsburgh',
                  'Richmond','Washington D.C.') ~ "EST",
      City %in% c('Chicago','Dallas','Louisville','St Louis') ~ 'CST',
      City %in% c('Denver') ~ 'MST',
      City %in% c('Los Angeles','Portland','San Francisco') ~'PST',
      TRUE ~ 'Not US TZ'
    ),
    #----- Pickup Dates/Times
    DateTime_Pickup = as.POSIXct(Begin.Trip.Time, format = "%Y-%m-%d %H:%M:%S"),
    Date_Pickup = as.Date(DateTime_Pickup),
    Time_Pickup = format(DateTime_Pickup, "%H:%M:%S"),
    #----- Dropoff Dates/Times
    DateTime_Dropoff = as.POSIXct(Dropoff.Time, format = "%Y-%m-%d %H:%M:%S"),
    Date_Dropoff = as.Date(DateTime_Dropoff),
    Time_Dropoff = format(DateTime_Dropoff, "%H:%M:%S")
  ) |>
  separate(Time_Pickup, into = c('Hr_Pickup','Min_Pickup','Sec_Pickup'),sep=':') |>
  separate(Time_Dropoff, into = c('Hr_Dropoff','Min_Dropoff','Sec_Dropoff'),sep=':') |>
  mutate(
    Hr_Dropoff = as.numeric(Hr_Dropoff),
    Hr_Pickup = as.numeric(Hr_Pickup),
    Hr_Dropoff = case_when(
      time_zone == "EST" ~ Hr_Dropoff - 5,
      time_zone == "PST" ~ Hr_Dropoff - 8,
      time_zone == "CST" ~ Hr_Dropoff - 5,
      time_zone == "MST" ~ Hr_Dropoff - 8,
      
      
      TRUE ~ Hr_Dropoff
    ),
    Hr_Pickup = case_when(
      time_zone == "EST" ~ Hr_Pickup - 5,
      time_zone == "PST" ~ Hr_Pickup - 8,
      time_zone == "CST" ~ Hr_Pickup - 5,
      time_zone == "MST" ~ Hr_Pickup - 8,
      
      
      TRUE ~ Hr_Pickup
    ),
    Date_Pickup = case_when(
      Hr_Pickup <= 0 ~ Date_Pickup - days(1),
      TRUE ~ Date_Pickup
    ),
    Date_Dropoff = case_when(
      Hr_Dropoff <= 0 ~ Date_Dropoff - days(1),
      TRUE ~ Date_Dropoff
    ),
    Hr_Dropoff = case_when(
      Hr_Dropoff < 0 ~ Hr_Dropoff + 22,
      TRUE ~ Hr_Dropoff
    ),
    Hr_Pickup = case_when(
      Hr_Pickup < 0 ~ Hr_Pickup + 22,
      TRUE ~ Hr_Pickup
    ),
  
  )
  

#----- Create coordinates datasets
rides_pickup <- rides |>
  select(Begin.Trip.Lng,Begin.Trip.Lat) |>
  mutate(
    Begin.Trip.Lat = case_when(
      is.na(Begin.Trip.Lat) ~ 0,
      TRUE ~ Begin.Trip.Lat
    ),
    Begin.Trip.Lng = case_when(
      is.na(Begin.Trip.Lng) ~ 0,
      TRUE ~ Begin.Trip.Lng
    ),
  )

rides_dropoff <- rides |>
  select(Dropoff.Lng, Dropoff.Lat) |>
  mutate(
    Dropoff.Lat = case_when(
      is.na(Dropoff.Lat) ~ 0,
      TRUE ~ Dropoff.Lat
    ),
    Dropoff.Lng = case_when(
      is.na(Dropoff.Lng) ~ 0,
      TRUE ~ Dropoff.Lng
    ),
  )


rides <- rides |>
  mutate(
    state_pickup = latlong2(rides_pickup, to = 'state'),
    state_dropoff = latlong2(rides_dropoff, to = 'state'),
    interstate_trip = case_when(
      state_pickup != state_dropoff ~ 'Interstate',
      TRUE ~ 'Intrastate'
    )
  )

state_pickup_totals <- rides |>
  group_by(state_pickup) |>
  summarise(count = n())|>
  arrange(desc(count))

state_dropoff_totals <- rides |>
  group_by(state_dropoff) |>
  summarise(count = n()) |>
  arrange(desc(count))

ggplot(state_pickup_totals, aes(x = state_pickup, y= count)) +
  geom_bar(stat = 'identity')


#----- Which counties do I visit the most?
#install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")
#install.packages("remotes")
#remotes::install_github("JVAdams/jvamisc")

rides <- rides |>
  mutate(
    county_pickup = latlong2(rides_pickup, to="county"),
    county_dropoff = latlong2(rides_dropoff, to="county")
  ) |>
  mutate(
    county_pickup = str_to_title(
      paste0(county_pickup[,2], ' County '), locale = 'en'
      ),
    county_dropoff = str_to_title(
      paste0(county_dropoff[,2], ' County '), locale = 'en'
    )
  )

#----- Average Distance Between Pickup and Dropoff
rides <- rides |>
  mutate(
    dist_miles = geosphere::distHaversine(rides_pickup, rides_dropoff)/1609,
    dist_cat = case_when(
      dist_miles <= 5 ~ '0-5 miles',
      dist_miles > 5 & dist_miles <= 10 ~ '6-10 miles',
      dist_miles > 10 & dist_miles <= 15 ~ '11-15 miles',
      dist_miles > 15 & dist_miles <= 20 ~ '16-20 miles',
      dist_miles > 20 ~ '20+ miles'
    ),
    dist_diff = Distance..miles. - dist_miles
  )

#----- Hour of Rides

plot_rides <- rides |>
  filter(
    lubridate::year(Date_Pickup)>=2015 
  ) |>
  mutate(
    year = lubridate::year(Date_Pickup)
  ) |>
  group_by(Hr_Pickup) |>
  summarise(count = n())

ggplot(plot_rides, aes(x = Hr_Pickup, y = count)) +
  geom_point() + geom_line()

#----- Length of Rides
rides <- rides |>
  mutate(
    TripMin = as.integer((DateTime_Dropoff - DateTime_Pickup))/60
  )|>
  filter(TripMin>=0) |>
  mutate(
    TripMin_Cat = case_when(
      TripMin <= 10 ~ '0-10 min',
      TripMin > 10 & TripMin <= 20 ~ '10-20 min',
      TripMin > 20 & TripMin <= 30 ~ '20-30 min',
      TripMin > 30 & TripMin <= 40 ~ '30-40 min',
      TripMin > 40 & TripMin <= 50 ~ '40-50 min',
      TripMin > 50 & TripMin <= 60 ~ '50-60 min',
      TripMin > 60 ~ '60 min'
    )
  )

table(rides$TripMin_Cat)
