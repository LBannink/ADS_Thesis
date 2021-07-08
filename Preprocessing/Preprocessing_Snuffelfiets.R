preprocessing_pm_speed <- function(data, transform_zero = T)
{
  
  #First, throw away values with pm_2_5 more than 150
  filtered_data = data %>% filter(pm2_5 < 150)
  
  #Second, values below 1 pm2_5 are transformed to one (or deleted based on transform condition)
  # 0.5 is used as condition since pm2_5 values are integers
  
  if (transform_zero == T)
  {
    filtered_data = filtered_data %>% mutate(pm2_5 = ifelse(pm2_5 < 0.5, 1, pm2_5))
  }
  else
  {
    filtered_data = filtered_data %>% filter(pm2_5 > 0.5)
  }
  #Calculate speed to filter non moving observations
  #Order the data
  ordered_data = filtered_data %>% arrange(sensor,trip_sequence)
  
  #Create Variable to check if it is the same trip
  ordered_data$same_trip = (ordered_data$sensor == lag(ordered_data$sensor, default = 'first') & ordered_data$trip_sequence == lag(ordered_data$trip_sequence, default = 0)) + 0
  
  #If same trip, calculate distance and time difference between observations
  ordered_data$distance_seq = geodist(data.frame(ordered_data$lon,ordered_data$lat),sequential = T,pad = T)
  ordered_data$time_diff = difftime(lag(ordered_data$recording_time),ordered_data$recording_time,units = 'secs')
  ordered_data$distance_seq = ifelse(ordered_data$same_trip == 0,0,ordered_data$distance_seq)
  
  #calculate m/sec, transform to km/hour
  ordered_data$km_hour = (ordered_data$distance_seq/as.integer(ordered_data$time_diff)) * 3.6
  
  #Filter speed that is too low or too high or not recorded in same trip
  ordered_data = ordered_data %>% filter((km_hour < 45 & km_hour > 5) | (same_trip == 0))
  
  return(ordered_data)
}

