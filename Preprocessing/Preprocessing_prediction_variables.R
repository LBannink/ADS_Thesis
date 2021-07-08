#CREATE PREDICT VARIABLES

#In this file, all additional spatial and temporal predictors for the regression component are preproccessed

#First, the observed and predicted (OK) are joined in a df, this df is used for hybrid kriging, where OK is used as a predictor
all_ok_hybrid = st_join(int_val_ok_df,all_regr)
all_ok_hybrid = all_ok_hybrid %>% filter(tmdntfr == timeslotidentifier)

#Weather variables collected by KNMI at de Bilt, raw data is added in the data folder. (Unused variables and days are already deleted)
weather_vars_fl <- read_delim("C:/Users/lucas/Desktop/2020-2021/Master-ADS/Thesis_ADS/regression vars/Wind_d_s_Humi.csv", 
                            ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)
colnames(weather_vars_fl) = c('hour','wind_direction','avg_wind_speed','humidity')

#Delete night hours
weather_vars_fil = weather_vars_fl %>% filter(hour %in% c(6:23))

#Summarise per six hours
weather_vars_fil$six_hour_index <- c(0, rep(1:(nrow(weather_vars_fil)-1)%/%6))+1
six_hour_weather_vars = group_by(weather_vars_fil, six_hour_index) %>% 
  summarise(six_hour_weather_vars,wind_direction = median(wind_direction),wind_speed = mean(avg_wind_speed),humid = median(humidity),hour_ind = median(hour))
new_weather = weather_vars_fil %>% group_by(six_hour_index) %>% summarise(med_wind_dir = median(wind_direction),mean_wind_speed = mean(avg_wind_speed),humid = median(humidity),hour_ind = median(hour))
wvars = new_weather[1:4]
colnames(wvars) = c('timeslot_id','wind_direction','avg_wind_speed','humidity')

#Categorise wind direction by degrees 
wvars$wind_direction_cat = ifelse(wvars$wind_direction %in% c(22:67),'NO',
                                  ifelse(wvars$wind_direction %in% c(68:112),'O',
                                         ifelse(wvars$wind_direction %in% c(113:157),'ZO',
                                                ifelse(wvars$wind_direction %in% c(158:202),'Z',
                                                       ifelse(wvars$wind_direction %in% c(203:247),'ZW',
                                                              ifelse(wvars$wind_direction %in% c(248:292),'W',
                                                                     ifelse(wvars$wind_direction %in% c(293:337),'NW','N')))))))
                                  
table(wvars$wind_direction_cat)

#Join weather variables with observations based on timestep, in both monitoring stations and snuffelfiets data
all_regr = merge(all_df,wvars,by.x = 'timeslotidentifier',by.y = 'timeslot_id')
full_external = merge(full_external,wvars,by.x = 'timeslotidentifier.x',by.y = 'timeslot_id')

#Spatial Variables:
#Import highways from OSM
highways <- 
  opq(st_bbox(grid %>% st_transform(crs = 4326))) %>% 
  add_osm_feature("highway", "motorway") %>% 
  osmdata_sf()
#Tranform to right crs
highways_u = highways$osm_lines$geometry %>% st_transform(crs = crs) %>% st_combine()

#Calculate distance to nearest highway
dist_2_highway = as.numeric(c(st_distance(all_regr,highways_u)))
all_regr$dist2hw = dist_2_highway

#Plot distance to check validity 
ggplot()  + geom_sf(data = highways_u) + geom_sf(data = all_regr, mapping = aes(col = dist2hw))

#Address density is calculated using QGIS, in the data folder, the shapefile is available. 
oad_stats =st_read("C:/Users/lucas/Desktop/2020-2021/Master-ADS/Thesis_ADS/regression vars/oad_stats.shp",crs = crs)

#Join all predictors with observations
all_regr = st_join(all_regr, oad_stats)  

#Add variable for time of daya and time of week 
weekend_dates_jan2020 = c(1,2,11,12,18,19,25,26)
all_regr$weekend = ifelse(all_regr$date %in% weekend_dates_jan2020,1,0)
all_regr$timeofday = ifelse(all_regr$hour == 9,'mon',ifelse(all_regr$hour == 15,'aft','eve'))


#Repeat all (spatial) steps for the monitoring station data and locations
external_predict = full_external %>% select(c(2:9,14,16,19:21,38:41))
external_predict$dist2hw = as.numeric(c(st_distance(external_predict$geometry,highways_u)))

mon_oad = st_read('C:/Users/lucas/Desktop/2020-2021/Master-ADS/Thesis_ADS/regression vars/monitoring_OAD.shp',crs = crs)
external_predict = st_join(external_predict,mon_oad)

external_predict = external_predict %>% rename(OAD_mean = OAD_mean_m) 

external_predict$weekend = ifelse(full_external$daytime.x %in% weekend_dates_jan2020,1,0)
external_predict$timeofday = ifelse(external_predict$hour == 6,'mon',ifelse(external_predict$hour == 12,'aft','eve'))

st_write(external_predict,'ext_full.shp')