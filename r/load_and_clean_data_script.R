
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
message('Loading Uber Eats and Rides data...')

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


message('Uber Eats data cleaned')




#------ Cleaning the restaurant data


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


rest_smaller_df <- restaurants[,c('Restaurant.ID','Restaurant.Name')] |>
  distinct()

eats_df <- inner_join(
  orders_cleaned,
  rest_smaller_df,
  by = 'Restaurant.ID'
)

message('Restaurant data cleaned...')


message('Start cleaning rides data...')


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
    ),
    year = lubridate::year(Date_Pickup)
  )


message('Rides data cleaned.')