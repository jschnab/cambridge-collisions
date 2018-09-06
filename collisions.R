#script to analyse car collisions data in Cambridge, UK
library(tidyverse)

collisions <- read_csv("Cambs_RTC.csv")

#import data on number of collisions per geographic coordinates ("hotspots" of collisions)
number_collisions <- read_csv("number_collisions.csv")

#---------------------------------------------------------------------#
#--- what types of vehicles are involved in accidents most often ? ---#
#---------------------------------------------------------------------#
type_vehicles <- collisions %>%
  count(Vehicle_1_Type, Vehicle_2_Type) %>%
  arrange(desc(n))

#--------------------------------------------#
#--- where do accidents frequently occur? ---#
#--------------------------------------------#
collisions %>%
  count(Location) %>%
  arrange(desc(n))
#this may not be very useful, as "Location" may have different names for the same place

#junction between Park road and Burghley road
#7 accidents, is there a common pattern in them?
collisions %>%
  filter(Location=="PARK RD JUNCTION BURGHLEY RD PETERBOROUGH") %>%
  select(Date, Severity, Light, Vehicle_1_Type, Vehicle_1_Manouvres, Vehicle_2_Type, Vehicle_2_Manouvres)
#most of them involve a car and a cycle, some of them in dark conditions
#it is possible the cycle was not visible enough for the car
#most accidents happened in 2012 and 2013, so road condition may have improved
#let's have a look at what the vehicles did
park_burghley <- collisions %>%
  filter(Location=="PARK RD JUNCTION BURGHLEY RD PETERBOROUGH") %>%
  select(Time, Date, Light, Vehicle_1_Type, Vehicle_1_Manouvres, Vehicle_2_Type, Vehicle_2_Manouvres,
         Vehicle_1_Location, Vehicle_2_Location, Vehicle_1_From_Direction, Vehicle_1_To_Direction,
         Vehicle_2_From_Direction, Vehicle_2_To_Direction, Easting, Northing)
#when a cycle was involved, the cycle was either in the middle of or leaving the roundabout
#consistent with the car driver not seeing the cycling
#however, 2 times out of 4 the accident happened during the day
#one accident happened at 7:59am on 22nd April 2015, so the sun could have been low, impairing visibility
#however, the geometry of the roundabout makes this unlikely, as the car was going North (only very
#slightly East)
#from SW to NE (and vice versa) the road is straight, possibly encouraging drivers to cut the roundabout
#4 times out of 7 a vehicle was going from/to NE/SW
#with extended filter based on Easting and Northing, I find 18 accidents for this junction
park_burghley <- collisions %>%
  mutate(loc_east=Easting %/% 10, loc_north=Northing %/% 10) %>%
  filter(loc_east >= 51936 & loc_east <= 51938 & loc_north >= 29944 & loc_north <= 29947 ) %>%
  select(Time, Date, Location, `Junction Detail`, Light, Vehicle_1_Type, Vehicle_1_Manouvres, 
         Vehicle_2_Type, Vehicle_2_Manouvres,
         Vehicle_1_Location, Vehicle_2_Location, Vehicle_1_From_Direction, Vehicle_1_To_Direction,
         Vehicle_2_From_Direction, Vehicle_2_To_Direction, Easting, Northing)

collisions %>%
  mutate(loc_east=Easting %/% 10, loc_north=Northing %/% 10) %>% #round Easting and Northing to 10 meters
  select(Date, Road_Class, Main_rd_no, `Junction Detail`, Speed_limit, Location, loc_east, loc_north) %>%
  count(loc_east, loc_north) %>%
  arrange(desc(n))
#actually this gives less counts than count(Location)

#--------------------------------------------------------------------#
#--- automate counting of number of accidents over Cambridgeshire ---#
#--------------------------------------------------------------------#
#determine grid boundaries
min_east <- min(collisions$Easting)
max_east <- max(collisions$Easting)
min_north <- min(collisions$Northing)
max_north <- max(collisions$Northing)

#set arrays to store data
#next two lines for the whole grid, two subsequent lines for subset
#easting_cells <- (max_east - min_east) / 50 #number of easting slices used for counting accidents
#northing_cells <- (max_north - min_north) / 50 #northing slices for counting accidents
easting_cells <- (553000 - 537000) / 50 #cambridge only
northing_cells <- (265000 - 252000) / 50 #cambridge only
m <- easting_cells * northing_cells #number of rows in "coordinates" and "n_collisions" array
n_collisions <- array(0, c(m + 1, 3)) #array to store number of accidents at a given pair of coordinates

#loop through coordinates and count the number of accidents at each location
counter = 1 #counter to increment arrays coordinates
for(i in seq(537000, 553000, 50)){
  for(j in seq(252000, 265000, 50)){
    #record coordinates
    n_collisions[counter, 1] <- i + 25
    n_collisions[counter, 2] <- j + 25
    #record number of accidents
    n_collisions[counter, 3] <- collisions %>%
      filter(Easting >= i & Easting < i + 50 & Northing >= j & Northing < j + 50) %>%
      count() %>%
      .$n
    counter = counter + 1
  }
}

#convert array to tibble and remove coordinate pairs without recorded accidents
#and sort coordinate pairs in descending order for number of accidents
number_collisions <- as_tibble(n_collisions) %>%
  filter(V3 != 0) %>%
  arrange(desc(V3)) %>%
  rename(Easting=V1, Northing=V2, n=V3)

#save data as csv file
write_csv(number_collisions, "number_collisions.csv")

#---------------------------------------------------------------#
# --- hotspot of collisions : Royal Cambridge Hotel junction ---#
#---------------------------------------------------------------#
east <- number_collisions$Easting[[3]]
north <- number_collisions$Northing[[3]]
hotspot_3 <- collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
  select(Date,
         Year,
         Location,
         Vehicle_1_Type,
         Vehicle_1_Location,
         Vehicle_2_Type,
         Vehicle_2_Location,
         Vehicle_1_Manouvres,
         Vehicle_1_From_Direction,
         Vehicle_1_To_Direction,
         Vehicle_2_Manouvres,
         Vehicle_2_From_Direction,
         Vehicle_2_To_Direction)

#can we see a pattern in the data?
#the collisions in RCH junction actually occur at two different but close locations
#one is the junction between Trumpington st and Lensfield rd
#the other one is the junction between Trumpington st and Fen Causeway
collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
  ggplot() +
  geom_histogram(aes(Northing), binwidth=5)

#for Trumpington - Fen Cway
Trumpington_Fen <- collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25,
         Northing < 257620) %>%
  select(Date,
         Year,
         Location,
         Vehicle_1_Type,
         Vehicle_1_Location,
         Vehicle_2_Type,
         Vehicle_2_Location,
         Vehicle_1_Manouvres,
         Vehicle_1_From_Direction,
         Vehicle_1_To_Direction,
         Vehicle_2_Manouvres,
         Vehicle_2_From_Direction,
         Vehicle_2_To_Direction)

#for Trumpington - Lensfield
Trumpington_Lensfield <- collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25,
         Northing > 257620) %>%
  select(Date,
         Year,
         `Junction Detail`,
         Location,
         Vehicle_1_Type,
         Vehicle_1_Location,
         Vehicle_2_Type,
         Vehicle_2_Location,
         Vehicle_1_Manouvres,
         Vehicle_1_From_Direction,
         Vehicle_1_To_Direction,
         Vehicle_2_Manouvres,
         Vehicle_2_From_Direction,
         Vehicle_2_To_Direction)

#what are the main types of vehicles involved?
#most collisions (17/19) occur between a motorized vehicle and a cycle
collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
  count(Vehicle_1_Type, Vehicle_2_Type)

#what are the locations of the vehicles when the collision occured?
#most accidents (14/19) occur when a car is entering the roundabout while a cycle is in the middle of 
#or leaving it
location <- collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
  count(Vehicle_1_Type, Vehicle_2_Type, Vehicle_1_Location, Vehicle_2_Location)

#--------------------------------------------------------------------#
# --- hotspot of collisions : Barnwell rd - Newmarket rd junction ---#
#--------------------------------------------------------------------#
east <- number_collisions$Easting[[1]]
north <- number_collisions$Northing[[1]]
hotspot_1 <- collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
  select(Date,
         Year,
         `Junction Detail`,
         Location,
         Vehicle_1_Type,
         Vehicle_1_Location,
         Vehicle_2_Type,
         Vehicle_2_Location,
         Vehicle_1_Manouvres,
         Vehicle_1_From_Direction,
         Vehicle_1_To_Direction,
         Vehicle_2_Manouvres,
         Vehicle_2_From_Direction,
         Vehicle_2_To_Direction)

#what are the main types of vehicles involved?
collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
  count(Vehicle_1_Type, Vehicle_2_Type) %>%
  arrange(desc(n))

#what are the locations of the vehicles when the collision occured?
location <- collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
  count(Vehicle_1_Type, Vehicle_2_Type, Vehicle_1_Location, Vehicle_2_Location) %>%
  arrange(desc(n))

#----------------------------------------------------------------------------#
# --- hotspot of collisions : Park rd - Burghley rd rdbout (Peterborough) ---#
#----------------------------------------------------------------------------#
east <- number_collisions$Easting[[2]]
north <- number_collisions$Northing[[2]]
hotspot_2 <- collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
  select(Date,
         Year,
         `Junction Detail`,
         Location,
         Vehicle_1_Type,
         Vehicle_1_Location,
         Vehicle_2_Type,
         Vehicle_2_Location,
         Vehicle_1_Manouvres,
         Vehicle_1_From_Direction,
         Vehicle_1_To_Direction,
         Vehicle_2_Manouvres,
         Vehicle_2_From_Direction,
         Vehicle_2_To_Direction)

#-----------------------------------------------------------------------------#
# --- hotspot of collisions : Queens - Madingley - Northampton rds rdabout ---#
#-----------------------------------------------------------------------------#
east <- number_collisions$Easting[[4]]
north <- number_collisions$Northing[[4]]
hotspot_4 <- collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
  select(Date,
         Year,
         `Junction Detail`,
         Location,
         Vehicle_1_Type,
         Vehicle_1_Location,
         Vehicle_2_Type,
         Vehicle_2_Location,
         Vehicle_1_Manouvres,
         Vehicle_1_From_Direction,
         Vehicle_1_To_Direction,
         Vehicle_2_Manouvres,
         Vehicle_2_From_Direction,
         Vehicle_2_To_Direction)

#-------------------------------------------------------------------#
# --- hotspot of collisions : A1139 - A15 rdabout (Peterborough) ---#
#-------------------------------------------------------------------#
east <- number_collisions$Easting[[5]]
north <- number_collisions$Northing[[5]]
hotspot_5 <- collisions %>%
  filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
  select(Date,
         Year,
         `Junction Detail`,
         Location,
         Vehicle_1_Type,
         Vehicle_1_Location,
         Vehicle_2_Type,
         Vehicle_2_Location,
         Vehicle_1_Manouvres,
         Vehicle_1_From_Direction,
         Vehicle_1_To_Direction,
         Vehicle_2_Manouvres,
         Vehicle_2_From_Direction,
         Vehicle_2_To_Direction)
