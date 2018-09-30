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