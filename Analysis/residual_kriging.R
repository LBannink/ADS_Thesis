#This script is used for residual st-kriging
#It is very similar to the ST_kriging script, only with the residuals from the trend prediction as input
#More thorough explanation of every step is found in ST_kriging script

#Cast residuals in right dimensions
cast_res = dcast(all_regr, as.character(test) ~ datetime,value.var = 'residuals', fun.aggregate = max)
mat_res = as.matrix(cast_res[,-1])
mat_res[mat_res == -Inf] = NA
res_stars = st_as_stars(res = mat_res,dim = dims)

#Sample variogram
v_st_res = variogramST(res~1,res_stars,timelags = 6:00)
plot(v_st_res)

#Set parameters variogram
prodsummodel_res <- vgmST("productSum",
                          space=vgm( 7.5,"Exp", 10000,0),
                          time= vgm( 50,"Sph",   180,0),
                          k=2)
StAni = estiStAni(v_st_res, c(0,200000))
#Estimate variogram
fitprodsummodel_res <- fit.StVariogram(v_st_res, prodsummodel_res, fit.method = 7,
                                       stAni = StAni, method = "L-BFGS-B",
                                       control = list(parscale = c(1,10,1,1,0.1,1,10)),
                                       lower = rep(0.0001, 7))
fitprodsummodel_res
#Plot to check similarity between variogram and sample
plot(v_st_res, fitprodsummodel_res, wireframe=FALSE, all=TRUE, scales=list(arrows=FALSE), zlim=c(0,350))

plot(v_st_res, model=fitprodsummodel_res, wireframe=TRUE, all=TRUE, scales=list(arrows=FALSE), zlim=c(0,185))

st_kriged_stations_res = krigeST(res~1,data = res_stars['res'],newdata = new_pt,nmax = 50,stAni = StAni ,modelList = fitprodsummodel_res)

#Create empty matrix for results
empty_residual = matrix(1,1255,84)

#STKrige residuals, using leave one location out validations
for (loc in 1:1255){
  st_kriged_internal_example_res = krigeST(res~1,data = res_stars['res',-loc],
                                       newdata = internal_point['pts',loc],nmax = 50,stAni = StAni,
                                       modelList = fitprodsummodel_res)
  empty_residual[loc,] = st_kriged_internal_example_res$var1.pred
}

#Test example location
st_kriged_internal_example_res = krigeST(res~1,data = res_stars['res'],
                                         newdata = grid_st,nmax = 50,stAni = StAni,
                                         modelList = fitprodsummodel_res)

st_kriged_internal_example_res

#Calculate differences between observed and estimated residuals
res_matrix_fitres = mat_res - empty_residual
mean(res_matrix,na.rm = T)

#Back to vectors for dataframe
resized_matr = as.vector(mat_res)
resized_residuals = as.vector(res_matrix_fitres)
resized_pred = as.vector(empty_residual)

res_res = data.frame(resized_matr,resized_residuals,resized_pred) %>% filter(!is.na(resized_residuals))

#Use residual kriging estimation also for the space_time locations of the monitoring stations
st_kriged_stations_res = krigeST(res~1,data = res_stars['res'],newdata = new_pt,nmax = 50,stAni = StAni ,modelList = fitprodsummodel_res)

dtf_res = data.frame(st_kriged_stations_res) %>% st_as_sf(crs = crs)
dtf_res$tid = dtf_res$time %>% as.POSIXct() %>% as.factor() %>% as.numeric()
dtf_res = dtf_res %>% select(-pts)

#Join with observed values
external_predict = st_join(external_predict,dtf_res) %>% filter(tid == timeslotidentifier)

