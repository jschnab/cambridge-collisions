#script to analyse car collisions data in Cambridge, UK
library(tidyverse)

collisions <- read_csv("Cambs_RTC.csv")

#import data on number of collisions per geographic coordinates ("hotspots" of collisions)
number_collisions <- read_csv("number_collisions.csv")

collisions_updated <- read_csv("collisions_updated.csv")

#------------------------------------------------------#
#--- what is the distribution of accident severity? ---#
#------------------------------------------------------#
collisions %>%
  group_by(Severity) %>% 
  summarise(prop_severity=n() / 12108)
#the proportion of fatal accident is 0.0154, serious 0.155, slight 0.830

#are bicycles more vulnerable? are severity proportions different for bicycles?
collisions %>%
  filter(Vehicle_1_Type == "Pedal cycle" | Vehicle_2_Type == "Pedal cycle" | Vehicle_3_Type =="Pedal cycle") %>%
  group_by(Severity) %>% 
  summarise(prop_severity=n() / 2665)
#fatal: 0.00375, serious: 0.16, slight: 0.836

collisions %>%
  filter(Vehicle_1_Type != "Pedal cycle" | is.na(Vehicle_1_Type)) %>% 
  filter(Vehicle_2_Type != "Pedal cycle" | is.na(Vehicle_2_Type)) %>% 
  filter(Vehicle_3_Type != "Pedal cycle" | is.na(Vehicle_3_Type)) %>% 
  group_by(Severity) %>% 
  summarise(prop_severity=n() / 9443)
#fatal: 0.0186, serious: 0.153, slight: 0.828

#is the difference in severity classes between cycles and non cycles significant?
severity_cycles <- collisions %>%
  filter(Vehicle_1_Type == "Pedal cycle" | Vehicle_2_Type == "Pedal cycle" | Vehicle_3_Type =="Pedal cycle") %>%
  count(Severity) %>% 
  rename("cycles"=n)

severity_noncycles <- collisions %>%
  filter(Vehicle_1_Type != "Pedal cycle" | is.na(Vehicle_1_Type)) %>% 
  filter(Vehicle_2_Type != "Pedal cycle" | is.na(Vehicle_2_Type)) %>% 
  filter(Vehicle_3_Type != "Pedal cycle" | is.na(Vehicle_3_Type)) %>% 
  count(Severity) %>% 
  rename("non-cycles"=n)

#merge data into one tibble
severity <- merge(severity_cycles, severity_noncycles)
rownames(severity) <- severity$Severity
severity <- severity %>%
  select(-c(Severity))

#perform Chi-sq test
chisq.test(severity)
#we can reject hypothesis that collision severity is idependant of the involvment of a cycle in the accident

#what is the ranking of accident locations for number of serious and fatal accidents?
#are there locations where severity is higher than average?
n_serious <- c() #create empty vector to be filled with number of serious/fatal collisions
location <- c() #create empty vector to be filled with names of locations of collisions
for(i in 1:dim(number_collisions)[1]){
  #get coordinates
  east <- number_collisions[i,]$Easting
  north <- number_collisions[i,]$Northing
  #get number of serious accidents
  n_serious[i] <- collisions %>% 
    filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
    filter(Severity != "Slight") %>% 
    count() %>% 
    .$n
  #get name of junction, selects most frequent denomination as the same junction has different names
  junc_names <- collisions %>% 
    filter(Easting >= east -25 & Easting < east +25 & Northing >= north -25 & Northing < north +25) %>%
    select(Location) %>% 
    table()
  location[i] <- names(junc_names[max(junc_names)])[1]
}
number_collisions$n_serious <- n_serious #add number of serious/fatal collisions to number_collisions tibble
number_collisions$locations <- location #add name of junction

#need to merge rows with same location name
#save data
write_csv(number_collisions, "collisions_updated.csv")

#get most dangerous locations
dangerous_loc <- number_collisions %>% 
  arrange(desc(n_serious)) %>% 
  .[1:20,]

#---------------------------------------------------------------------#
#--- what types of vehicles are involved in accidents most often ? ---#
#---------------------------------------------------------------------#
type_vehicles <- collisions %>% 
  count(Vehicle_1_Type, Vehicle_2_Type, Vehicle_3_Type) %>%
  arrange(desc(n))
#car & cycle 3rd most frequent vehicle couple (2046 accidents, 23 % of accidents involving two or three vehicles)
#car & car is first, car alone third

#total number of accidents is 12108
#how many accidents involve a cycle?
collisions %>%
  filter(Vehicle_1_Type == "Pedal cycle" | Vehicle_2_Type == "Pedal cycle" | Vehicle_3_Type =="Pedal cycle") %>% 
  count()
#2665 i.e. 22 % of accidents

test <- collisions %>%
  slice(1:20) %>% 
  filter(Vehicle_1_Type != "Pedal cycle" | Vehicle_2_Type != "Pedal cycle" | Vehicle_3_Type !="Pedal cycle") %>% 
  select(Vehicle_1_Type, Vehicle_3_Type, Vehicle_2_Type)

#---------------------------------------------------------------------#
#--- is there a link between speed limit and severity of accidents ---#
#---------------------------------------------------------------------#
collisions %>%
  count(Speed_limit) %>% 
  arrange(desc(n))
#hard to say because there is no information of frequentation of differents roads with different speed limits

#-----------------------------------------------------------------------------#
#--- is there a pattern of vehicle position and/or movement in accidents ? ---#
#-----------------------------------------------------------------------------#
#what are the most dangerous types of junctions?
collisions %>%
  count(`Junction Detail`) %>% 
  arrange(desc(n))
#1st is "not at junction"
#2nd is "T/staggered junction"
#3rd is "roundabout"

#what is happening most frequently when accidents occur away from a junction
collisions %>%
  filter(`Junction Detail` == "Not at junction") %>% 
  group_by(Vehicle_1_Manouvres, Vehicle_2_Manouvres) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
#1st is vehicle alone going ahead
#2nd is both vehicles going ahead
#3rd is going ahead VS slow or stopping -> short inter-vehicle distance?
#4th and 5th is single vehicle in turn
#6th is going ahead VS parked

collisions %>%
  filter(`Junction Detail` == "Not at junction",
         Vehicle_1_Manouvres == "Going ahead",
         Vehicle_2_Manouvres == "Going ahead") %>% 
  group_by(Vehicle_1_From_Direction, Vehicle_1_To_Direction, Vehicle_2_From_Direction, Vehicle_2_To_Direction) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
#difficult to know more about what happened from vehicle direction or location

#what is happening most frequently when accidents occur at a T/staggered junction?
collisions %>%
  filter(`Junction Detail` == "'T'/staggered junctn") %>% 
  group_by(Vehicle_1_Manouvres, Vehicle_2_Manouvres) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
#1st is turning right VS going ahead
#2nd is single vehicle going ahead
#3rd is turning left VS going ahead

collisions %>% 
  filter(`Junction Detail` == "'T'/staggered junctn", 
         Vehicle_1_Manouvres == "Turning right",
         Vehicle_2_Manouvres == "Going ahead") %>% 
  group_by(Vehicle_1_From_Direction, Vehicle_1_To_Direction, Vehicle_2_From_Direction, Vehicle_2_To_Direction) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

#what is the most dangerous type of junction when a cyclist is involved?
#-----------------------------------------------------------------------
collisions %>% 
  filter(Vehicle_1_Type == "Pedal cycle" | Vehicle_2_Type == "Pedal cycle" | Vehicle_3_Type =="Pedal cycle") %>% 
  count(`Junction Detail`) %>% 
  arrange(desc(n))
#1st is T/staggered junction
#2nd is not a junction
#3rd is roundabout

collisions %>% 
  filter(Vehicle_1_Type == "Pedal cycle" | Vehicle_2_Type == "Pedal cycle") %>% 
  filter(`Junction Detail` == "'T'/staggered junctn") %>% 
  count(Vehicle_1_Manouvres, Vehicle_2_Manouvres) %>% 
  arrange(desc(n))
#1st and 2nd is turning left or right VS going ahead
#3rd is moving off VS going ahead

collisions %>% 
  filter(Vehicle_1_Type == "Pedal cycle" | Vehicle_2_Type == "Pedal cycle") %>% 
  filter(`Junction Detail` == "'T'/staggered junctn") %>% 
  filter(Vehicle_1_Manouvres == "Moving off", Vehicle_2_Manouvres == "Going ahead") %>% 
  count(Vehicle_1_From_Direction, Vehicle_1_To_Direction, Vehicle_2_From_Direction, Vehicle_2_To_Direction) %>% 
  arrange(desc(n))
#in most manouvres vehicles do not go in the same direction but cross their path

collisions %>% 
  filter(Vehicle_1_Type == "Pedal cycle" | Vehicle_2_Type == "Pedal cycle") %>% 
  filter(`Junction Detail` == "Not at junction") %>% 
  count(Vehicle_1_Manouvres, Vehicle_2_Manouvres) %>% 
  arrange(desc(n))
#1st is both going ahead
#2nd is one overtaking the other going ahead
#3rd is one overtaking the other parked

collisions %>% 
  filter(Vehicle_1_Type == "Pedal cycle" | Vehicle_2_Type == "Pedal cycle") %>% 
  filter(`Junction Detail` == "Not at junction") %>% 
  filter(Vehicle_1_Manouvres == "Going ahead", Vehicle_2_Manouvres == "Going ahead") %>% 
  count(Vehicle_1_From_Direction, Vehicle_1_To_Direction, Vehicle_2_From_Direction, Vehicle_2_To_Direction) %>% 
  arrange(desc(n))
#1st is both vehicles parked
#many other seems to happen when both vehicles go in the same direction

collisions %>% 
  filter(Vehicle_1_Type == "Pedal cycle" | Vehicle_2_Type == "Pedal cycle") %>% 
  filter(`Junction Detail` == "Not at junction") %>% 
  filter(Vehicle_1_Manouvres == "Going ahead", Vehicle_2_Manouvres == "Going ahead") %>% 
  filter(Vehicle_1_From_Direction == Vehicle_2_From_Direction & Vehicle_2_To_Direction == Vehicle_1_To_Direction) %>% 
  count()
# = 108 / 190 -> 57 % but about same frequency when exluding bicycles from analysis


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
         Severity,
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
