#In this script, the Snuffelfiets data is aggregated using a 1km x 1km grid of the province of Utrecht. 
#In the temporal dimension, data is aggregated per six hours (morning, afternoon, evening, night data is deleted)

#Create 1km x 1km grid of gemeente_shapefile 
grid = st_bbox(shp) %>%     
  st_as_stars(dx = 1000)  %>%           #Stars is used to create grid 
  st_set_crs(crs) %>%                   #Crs = 28992 --> RdNew / Amersfoort
  st_crop(shp)                          #Bounding box is cropped on province boundaries to remove out of scope datapoints


#Aggregation function, the data is filtered on hours and day first. Afterward, spatial aggregation is performed on the remaining data
Aggregate_space_time = function(data,shp = gemeente_shapefile,hours = c(0:24), days = c(1:31), crs = crs){
  filtered_data = data %>% filter(day %in% days & hour %in% hours)
  filtered_data = st_intersection(shp, filtered_data)        #All data outside the province is deleted
  #print(length((filtered_data)))

  #Aggregation function, after aggregating by gridcell the centroid is used to store data in Point format
  #Cells with NA value are thrown out by the filter function
  aggregated = aggregate(filtered_data['pm2_5'],by = grid, as_points = TRUE,FUN = mean, na.rm = F) %>% st_as_sf(as_points = TRUE, merge = F) %>% st_centroid() %>% filter(pm2_5 > 0.5)
  
  return(aggregated)
}

#Test aggregation function
test101 = Aggregate_space_time(test_sf,gemeente_shapefile, hours = c(12:17), days = c(1))

#This loop is used to call the aggregation function in for different aggregations over time
#It loops over the days in the data set, this is 1:2 februari and 6-31 Januari
#After trying different time windows, the current research chooses three 6-hour windows (morning, afternoon, evening), therefore the loop is called 3 times
#When aggregating morning data, the days included are 1:3 and 7:31, since 12am is the cutoff point of the weekly data
for (i in c(1:2,6:31)) {
  aggregated = Aggregate_space_time(test_sf,gemeente_shapefile, hours = c(18:23), days = c(i))
  st_write(aggregated, paste('Six_hour_aggr_18_24_',i,'.shp'))        #The aggregated results are stored to disk
  print(paste('done with',i))
}                                                                                             
