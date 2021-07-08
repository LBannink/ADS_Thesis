#SPATIO-TEMPORAL KRIGING STEP 1: CREATE STARS OBJECT
#Requirements: All_df with aggregated observations, research grid (only spatial), monitoring stations data

#In order to perform space-time kriging, a stars object with two dimensions is created of the observations
#Space dimension: Point geometry; Time dimension: time id (1:84)
#Time dim:
time_dim = unique(all_df$datetime) %>% as.factor()
#Space dim: 
xy = st_coordinates(unique(all_df))
all_df = all_df[order(xy[,"X"], xy[,"Y"]),]
space_dim = st_sfc(unique(all_df$geometry), crs = crs)

#Create dimensions object
dims = st_dimensions(space = space_dim,time = time_dim)

#Cast data according to created dimensions: 
casted = dcast(all_df, as.character(geometry) ~ datetime,value.var = 'pm2_5', fun.aggregate = max)
#Transform to matrix
c_matrix = as.matrix(casted)
#Transform to stars, assign right dimensions
stars_df = st_as_stars('pm2_5' = c_matrix, dimensions = dims)

#SPATIAL TEMPORAL KRIGING STEP 2: FIND VARIOGRAM
#The newly created stars df is used to fit a spatial-temporal variogram
v_st = variogramST(pm2_5~1,stars_df,timelags = 6:00) #First, a sample is drawn to set the parameters, timelags = 6 hour in the data 

plot(v_st)        #Plot is used to visually inspect the sample

prodSumModel <- vgmST("productSum",                   #Product sum model according to pebesma et al. (2019)
                      space=vgm( 7.5,"Exp", 10000,0), #The parameters are altered towards the sample 
                      time= vgm( 120,"Sph",   180,0),
                      k=2)
StAni = estiStAni(v_st, c(0,200000))
fitProdSumModel <- fit.StVariogram(v_st, prodSumModel, fit.method = 7,   #Fitting of the spatio-temporal variogram
                                    stAni = StAni, method = "L-BFGS-B",
                                    control = list(parscale = c(1,10,1,1,0.1,1,10)),
                                    lower = rep(0.0001, 7))
#Parameters of the resulting model
fitProdSumModel

#Two plots are used to compare the sample to the fitted variogram, these have to be similar
plot(v_st, fitProdSumModel, wireframe=FALSE, all=TRUE, scales=list(arrows=FALSE), zlim=c(0,350)) 
plot(v_st_res, model=fitprodsummodel_res, wireframe=TRUE, all=TRUE, scales=list(arrows=FALSE), zlim=c(0,185)) #THIS PLOT IS FOUND IN THE THESIS 



# EXTERNAL VAL
#A stars object is created to predict the values at the monitoring locations
#Time dimension is the same as the training data, space dimension are the point locations of the monitoring stations
st_as_stars(list(pts = matrix(monitoring_locations$station_name, length(time_dim), 4))) %>%
  st_set_dimensions(names = c("time", "station")) %>%
  st_set_dimensions("time", time_dim) %>%
  st_set_dimensions("station", monitoring_locations$geometry) -> new_pt

#krigeST (gstat) performs spatio-temporal kriging, max 50 closest neighbours are used to reduce computing time
st_kriged_stations = krigeST(pm2_5~1,data = stars_df['pm2_5'],newdata = new_pt,nmax = 50,stAni = StAni ,modelList = fitProdSumModel)

#Check example of results
new_pt[,,1]
st_kriged_stations
plot(as.xts(st_kriged_stations[2]))

#Transform results back to dataframe
dtf = data.frame(st_kriged_stations) %>% st_as_sf(crs = crs)
dtf$tid = dtf$time %>% as.POSIXct() %>% as.factor() %>% as.numeric()
dtf = dtf %>% select(-pts)

#Join with observed values
st_ext_val = st_join(all_monitoring_84,dtf) %>% filter(tid == timeslotidentifier)
st_ext_val$residual = st_ext_val$m_pm2_5 - st_ext_val$var1.pred
#rmse(st_ext_val$var1.pred,st_ext_val$m_pm2_5)
#rmse()


#INTERNAL VAL          #NOTE: THIS ANALYSIS IS COMPUTATIONALLY INTENSIVE
#Create empty stars object with same dimensions to store results in
st_as_stars(list(pts = matrix(1, 1255, length(t)))) %>%
  st_set_dimensions(names = c("geo_point", "time")) %>%
  st_set_dimensions("geo_point", geom_sort)%>% 
  st_set_dimensions("time", t)  -> internal_point

#Leave one location out loop; this loop deletes observations at 'loc' and predicts afterwards
#Note that this loop predicts a value for every possible space-time combination, while there is not an available observation at every location
for (loc in 1:1255){
  st_kriged_internal_example = krigeST(pm2_5~1,data = stars_df['pm2_5',-loc],
                              newdata = internal_point['pts',loc],nmax = 50,stAni = StAni,
                              modelList = fitProdSumModel)
  #Results of prediction are stored in regular matrix
  emp[loc,] = st_kriged_internal_example$var1.pred
}
#
lolocv_mat = emp

#Observed minus predicted to calculate residuals; NA's in res_matrix are space-time locations without observed value
res_matrix = matr - lolocv_mat
mean(res_matrix,na.rm = T)

#transform to vector to store easily in df
resized_matr = as.vector(matr)
resized_residuals = as.vector(res_matrix)
resized_pred = as.vector(lolocv_mat)

#Results matrix, all space-time locations without available observations are thrown out
int_val_st_df = data.frame(resized_matr,resized_pred,resized_residuals) %>% filter(!is.nan(resized_residuals))

mae(int_val_st_df$resized_matr,int_val_st_df$resized_pred)
rmse(all_validated_cc$observed,all_validated_cc$var1_pred)


#GRID KRIGE #COMPUTATIONALLY INTENSIVE
#Krige on the research grid using stkrige, useful to map predictions

#Create stars object with added temporal dimension
d = dim(grid)
st_as_stars(pts = array(1, c(d[1], d[2], time=length(dayt)))) %>%
  st_set_dimensions("time", dayt) %>%
  st_set_dimensions("x", st_get_dimension_values(grid, "x")) %>%
  st_set_dimensions("y", st_get_dimension_values(grid, "y")) %>%
  st_set_crs(crs) -> grid_st

st_kriged_grid = krigeST(pm2_5~1,data = stars_df['pm2_5'],newdata = grid_st,nmax = 50,stAni = StAni ,modelList = fitProdSumModel)

#ggplot() + geom_stars(data = st_kriged_grid['var1.pred'])
#st_kriged_grid_exp = st_kriged_grid[1:58,1:50,44]



