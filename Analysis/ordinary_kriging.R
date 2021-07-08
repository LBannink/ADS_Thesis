#This script is used to perform ordinary kriging
#The input that is needed is the merged dataframes by the aggregation script
#This can be done by for example: 

dfs = sapply(.GlobalEnv, is.data.frame)
all_df = do.call(rbind, mget(names(dfs)[dfs]))

#The other required input is the gemeente shapefile as the kriging grid, and the monitoring stations data for the external validation 

# Add timeslot id for every time step, this way the timeslot id will be in chronological order
all_df = all_df %>% mutate(timeslotidentifier = datetime %>% as.factor() %>% as.numeric())
table(all_df$timeslotidentifier)

#This loop performs kriging for every timestep
#Kriging is performed in three different manners: on the research grid, leave-one-out cross-validation and/or external validation

for (i in c(1:84)){
  #Filter on time id to only include data in current timestep 
  data_included = all_df %>% filter(timeslotidentifier == i)
  #Fit variogram on current timestep observations
  v = variogram(pm2_5 ~ 1,data_included)
  vm = fit.variogram(v, vgm(1, "Exp", 7000, 1)) #Max range is set to 7000 meter, nugget to 1
  
  #Note that performing all three kriging options is very time consuming
  #1. KRIGING ON GRID
  o_kriged_map = krige(pm2_5~1, data_included, grid, vm) #This results in a stars object
  map = st_as_sf(o_kriged_map)                           #To save results to disk, stars needs to be transformed to shapefile
  st_write(map, paste('Ordinary_kriged_',i,'.shp'))
  
  #2. KRIGING ON MONITORING LOCATIONS
  o_kriged_points = krige(pm2_5~1, data_included, monitoring_locations, vm) #Kriging prediction on locations of monitoring stations
  o_kriged_points$station_name = c('Kard','Breuk','Griftpark','CBW')        #The order needs to be similar to the ordet in 'monitoring locations'
  o_kriged_points$timeidentifier = i                                        #Add time ID
  st_write(o_kriged_points, paste('External_validation_OK_',i,'.shp',sep = '')) #Save results to disk 
  
  
  #3. Leave-one-out cross validation
  x = krige.cv(formula = pm2_5~1,locations = data_included, vm, nfold = nrows) #LOOCV is performed by the built-in function in gstat (krige.cv) 
  x$timeidentifier = i
  st_write(x, paste('Loocv_validation_df_ok_',i,'.shp',sep = ''))              #Save results to disk
  
  print(paste('done with',i))
}

#LOAD LOOCV FILES
for (i in c(1:84)){
  assign(paste('validated_',i,sep = ''),st_read(paste('Loocv_validation_df_ok_',i,'.shp',sep = ''), crs = crs)) 
}

#COMBINE ALL LOOCV IN ONE FILE
list_xts[3] <- lapply(ls(pattern="validated"), function(x) get(x))
all_validated <-do.call(rbind,list_xts[3:86])
all_validated = all_validated %>% filter(vr1_prd > 0.001)
int_val_ok_df = all_validated
table(all_validated$tmdntfr)

#LOAD PREDICTED EXTERNAL VALIDATION FILES
for (i in c(1:84)){
  assign(paste('Monitoring_predicted_',i,sep = ''),st_read(paste('External_validation_OK_',i,'.shp',sep = ''), crs = crs)) 
}

list_mon <- lapply(ls(pattern="Monitoring"), function(x) get(x))
all_predicted_monitoring <-do.call(rbind,list_mon)
all_predicted_monitoring = all_predicted_monitoring %>% filter(vr1_prd > 0.001)

#The results of this prediction are stored and passed on to 'results.R'

