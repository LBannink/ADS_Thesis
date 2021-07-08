#THESE ARE ALL THE RESULTS PRESENTED IN THE THESIS
#The preprocessing and analysis is done in earlier scripts, this one is just the final results

#Ordinary kriging results Loocv: 
mean(all_validated_cc$residual)      #Mean Error
mae(all_validated_cc$observed,all_validated_cc$var1_pred)   #Mean Absolute Error
rmse(all_validated_cc$observed,all_validated_cc$var1_pred)  #Root mean squared error 

#Spatio-Temporal Kriging results Loocv: 
mean(int_val_st_df$resized_residuals)
mae(int_val_st_df$resized_matr,int_val_st_df$resized_pred)
rmse(int_val_st_df$resized_matr,int_val_st_df$resized_pred)

#Hybrid LUR kriging results loocv
mean(all_ok_hybrid$residuals)
mae(all_ok_hybrid$fitted_hybrid_LUR,all_ok_hybrid$observd)
rmse(all_ok_hybrid$fitted_hybrid_LUR,all_ok_hybrid$observd)

#Spatio-Temporal Regression Kriging results Loocv: 
mean(res_res$resized_residuals)
mae(res_res$resized_matr,res_res$resized_pred)
rmse(res_res$resized_matr,res_res$resized_pred)

#Ordinary kriging results External: 
mean(full_external$v_residual_ok)
mae(full_external$observed_new,full_external$ordinary_kriging_predicted)
rmse(full_external$observed_new,full_external$ordinary_kriging_predicted)

#Spatio-Temporal kriging results External: 
mean(full_external$v_residual_stk)
mae(full_external$observed_new,full_external$st_krige_pred)
rmse(full_external$observed_new,full_external$st_krige_pred)

#Hybrid LUR kriging results External:
mean(external_predict$res_hybrid)
mae(external_predict$pred_ext,external_predict$observed_new)
rmse(external_predict$pred_ext,external_predict$observed_new)

#Spatio-Temporal Regression Kriging results External: 
mean(external_predict$res_res_kriging)
mae(external_predict$var1.pred,external_predict$predicted_res)
rmse(external_predict$var1.pred,external_predict$predicted_res)

#Results hybrid when re-estimating coefficients
mean(external_predict$hybrid_res)
mae(external_predict$hybrid_fitted,external_predict$observed_new)
rmse(external_predict$hybrid_fitted,external_predict$observed_new)

#This last part was used to plot the research grid on an example day, this can be found in the thesis
st_kriged_grid_plot_day = st_kriged_grid[,,,50]
st_kriged_grid_plot_day = st_transform(st_kriged_grid_plot_day,crs = crs) %>% st_crop(gemeente_shapefile)
o_kriged_map_plot = krige(pm2_5~1, plot_day, grid, vm)
o_kriged_map_plot = o_kriged_map_plot %>% st_crop(st_kriged_grid_plot_day)
o_kriged_map_plot_test = o_kriged_map_plot_test %>% st_crop(gemeente_shapefile)

OK_plot = ggplot() + geom_stars(data = o_kriged_map_plot_test, aes(fill = var1.pred),sf = T) + geom_sf(data = st_cast(gemeente_shapefile, "MULTILINESTRING")) + geom_sf(data = plot_day, alpha = 0.7, size = 0.4) +
  ggtitle('Ordinary Kriging Map Evening January 22')  + theme_minimal()+labs(fill="Predicted PM2.5") + scale_fill_gradient(low='white',high = 'red', limits = c(14,90))
OK_plot  

STK_plot = ggplot() + geom_stars(data = st_kriged_grid_plot_day, aes(fill = var1.pred)) + geom_sf(data = st_cast(gemeente_shapefile, "MULTILINESTRING")) + 
  geom_sf(data = plot_day, alpha = 0.7, size = 0.4) + ggtitle('ST Kriging Map Evening January 22') + scale_fill_gradient(low='white',high = 'red', limits = c(14,90)) + theme_minimal() +labs(fill="Predicted PM2.5")
STK_plot


