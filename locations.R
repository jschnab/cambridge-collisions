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