#TREND ANALYSIS

#NEEDED: all_regr # all data (enriched) on snuffelfiets; external_predict is used for predicting at monitoring sites
#In this script the regression component of the regression ST kriging and hybrid model are calculated

#First linear model: This one is without OK prediction and used for spatio-temporal kriging
trend_mdl = pm2_5 ~ humidity + avg_wind_speed + wind_direction_cat + weekend + timeofday + dist2hw + sqrt(OAD_mean) 
trend_result = lm(trend_mdl, data = all_regr)
summary(trend_result)

#Check collinearity
vif_tr = vif(trend_result)

#The residuals are added to the dataframe which is passed on to the residual kriging script
all_regr$residuals = trend_result$residuals
all_regr$trend_fitted_val = trend_result$fitted.values

#Trend part when only including sp. variables
spt_trend = lm(pm2_5 ~ + dist2hw + log(OAD_mean), data = all_regr)
summary(spt_trend)

#Predicting pm2_5 at monitoring stations using the trend model
external_predict$predicted = predict(trend_result, newdata = external_predict)
external_predict$predicted_res = external_predict$observed_new - external_predict$predicted

#Trend for monitoring stations if the coefficients are being re-estimated
trend_mdl_ext = observed_new ~ humidity + avg_wind_speed + wind_direction_cat + timeofday + weekend + dist2hw + sqrt(OAD_mean_m)
trend_result_ext = lm(trend_mdl_ext, data = external_predict)
external_predict$trend_residual = trend_result_ext$residuals
external_predict$trend_fitted_val = trend_result_ext$fitted.values
summary(trend_result_ext)

#This is part of the hybrid model: First, only estimating PM2_5 by using the OK prediction
trend_pred_ext = observed_new ~ ordinary_kriging_predicted 
trend_result_pred = lm(trend_pred_ext, data = all_regr)
summary(trend_result_pred)

#Next, the full hybrid model including the other predictors
trend_hybrid = observed_new ~ ordinary_kriging_predicted + humidity + avg_wind_speed + wind_direction_cat + timeofday + weekend + dist2hw + sqrt(OAD_mean_m)
trend_result_pred = lm(trend_hybrid, data = all_regr)
external_predict$hybrid_fitted = trend_result_pred$fitted.values
external_predict$hybrid_res = trend_result_pred$residuals
summary(trend_result_pred)
vif(trend_result_pred)
#Use the hybrid model to predict at external locations
external_predict$pred_ext = predict(trend_result_pred,newdata = external_predict)

#Again, the coefficients are also re-estimated using the 
trend_hybrid_ok_only = observed_new ~ ordinary_kriging_predicted
trend_hybrid_ok_only_res = lm(trend_hybrid_ok_only, data = external_predict)
trend_hybrid_reestimated = lm(trend_hybrid, data = external_predict)

trend_hybrid_ok_only_int = observd ~ vr1_prd + humidity + avg_wind_speed + wind_direction_cat + timeofday + weekend + dist2hw + sqrt(OAD_mean)
trend_hybrid_ok_only_int_res = lm(trend_hybrid_ok_only_int, data = all_ok_hybrid)
all_ok_hybrid$fitted_hybrid_LUR = trend_hybrid_ok_only_int_res$fitted.values
all_ok_hybrid$res_hybrid_LUR = trend_hybrid_ok_only_int_res$residuals

#Calculate residuals of hybrid model
external_predict$res_hybrid = external_predict$observed_new - external_predict$pred_ext

#rmse(external_predict$predicted_res,external_predict$res_res_kriging)

#mean(external_predict$res_hybrid)

#This loop was originally used to fully externally predict the monitoring stations, 
#however, the difference between the two results is so slight it is not worth the wait
for (i in 1:26928){
  trend_hybrid_ok_only_int_res_loocv = lm(trend_hybrid_ok_only_int, data = all_ok_hybrid[-i,])
  all_ok_hybrid$prd_loocv[i] = predict(trend_result,newdata = all_ok_hybrid[i,])
  print(paste('done with',i))
}
