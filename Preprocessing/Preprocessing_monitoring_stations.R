setwd('C:/Users/lucas/Desktop/2020-2021/Master-ADS/Thesis_ADS/Monitoring_stations')
getwd()

#This script is used to preprocess the monitoring station data
#The raw data is a csv file that includes all monitoring stations and all measurements in 2020
#This file is available in the raw data, and uploaded: 

monitoring_df <- read_delim("2020_PM25.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)

# codes Utrecht station 'NL10636	NL10641	NL10643	NL10644'
# Locations Utrecht stations (52.105031,5.124464)	(52.20153,4.987444)	(52.101308,5.128183)	(51.974489,4.923301)
# Station names Utrecht-Kardinaal de Jongweg	Breukelen-Snelweg	Utrecht-Griftpark	Cabauw-Wielsekade

codes_U = c('NL10636','NL10641','NL10643','NL10644')

#Filter on right 4-week window
monitoring_df$Begindatumtijd = as.POSIXct(monitoring_df$Begindatumtijd)
monitoring_df$Einddatumtijd = as.POSIXct(monitoring_df$Einddatumtijd)
monitoring_df = monitoring_df %>% filter(Einddatumtijd < as.POSIXct('2020-02-03 13:30:00'))
monitoring_df = monitoring_df %>% filter(Einddatumtijd > as.POSIXct('2020-01-06 13:30:00'))

#Filter on right stations
monitoring_df_selection = monitoring_df %>% select(Einddatumtijd,Begindatumtijd,codes_U)
#Change names to understandable ones
colnames(monitoring_df_selection) = c('Endtime','Starttime','pm2_5_Kard_Jongweg','pm2_5_Breukelen','pm2_5_Griftpark','pm2_5_CWkade')
#Create hour variable
monitoring_df_selection$Hourtime = hour(test_monitoring_df_selection$Starttime)

#Delete observations in the night, as these are deleted in the snuffelfiets data as well 
m_df = monitoring_df_selection %>% filter(Hourtime %in% c(6:23))

# The coordinates of the monitoring stations are to be added, as an example how it is done CW kade is used. The other coords are found in the earlier comment
m_df_CWK = m_df %>% select(Starttime,Endtime,pm2_5_CWkade)
m_df_CWK$lat = 51.974489
m_df_CWK$lon = 4.923301
#crs = 28992
m_df_CWK = st_as_sf(m_df_CWK, coords = c('lon','lat'), crs = 4326)
m_df_CWK_28 = st_transform(m_df_CWK, crs = crs)

#Assign names
names_geo = c('Starttime','Endtime','pm2_5','geometry')
colnames(m_df_CWK_28) = names_geo

#Bind 4 stations 
monitoring_geo_full = rbind(m_df_Breuk_28,m_df_CWK_28,m_df_Grift_28,m_df_Kard_28)
m_df_CWK_28$daytime = day(m_df_CWK_28$Starttime)

# Test plot to check if locations are valid
ggplot()   + geom_sf(data = provincie_shp)+ geom_sf(data = monitoring_geo_full, mapping = aes(col = pm2_5))

#Aggregate observations over six hours, these are the same timeslots as aggregated in Snuffelfiets
#Again, CWK is used as example
six_hours = hours(6)
CWK = m_df_CWK_28 %>% group_by(daytime) %>% summarise_by_time(.date_var = Starttime,.by = six_hours, m_pm2_5 = mean(pm2_5,na.rm = T))
testtime = summarise_by_time(m_df_Grift_28,.date_var = Starttime, time,.by = six_hours, pm2_5 = mean(pm2_5))

#Bind 4 observations, and timeslot ID similar to snuffelfiets
all_monitoring_84 = rbind(Kard,Breuk,Grift,CWK)
all_monitoring_84$timeslotidentifier = as.factor(all_monitoring_84$Starttime) %>% as.numeric()

#Create seperate df with just the locations of mon. stations
monitoring_locations = st_as_sfc(unique(all_monitoring_84$geometry))
monitoring_locations = st_as_sf(monitoring_locations, crs = crs)
monitoring_locations$name = c('Kard','Breuk','Griftpark','CBW')
colnames(monitoring_locations) = c('geometry','station_name')
st_geometry(monitoring_locations) <- "geometry"

#Calculate Statistics of between predicted and observec
warnings()
table(all_predicted_monitoring$tmdntfr)

#These timeslots have invalid predictions from ordinary
false_identifiers = c(2,66,67,71)   

#Join predicted values from OK and actual values  to test validation
all_monitoring_full = all_monitoring_84 %>% filter(!(timeslotidentifier %in% false_identifiers))
joined = st_join(all_monitoring_full,all_predicted_monitoring, by = c('timeslotidentifier' = 'tmdntfr'))
joined = joined %>% filter(timeslotidentifier == tmdntfr)

OK_pred_actual_val = joined
#Example performance metric
rmse(OK_pred_actual_val$m_pm2_5,OK_pred_actual_val$vr1_prd)
