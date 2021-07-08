# Getting Started
# In this script, the required packages and the raw data loaded  
# The first packages are the most used ones, latter packages are only marginally used. 
# LOAD PACKAGES
library(tidyverse)
library(sf)
library(gstat)
library(stars)
library(sp)
library(lubridate)
library(timetk)
library(grid)
library(string)
library(Metrics)
library(spacetime)
library(xts)
library(maditr)
library(Matrix)
library(patchwork)
library(car)
library(egg)
library(gtable)
library(geodist)
library(Metrics)
library(rlist)
#setwd('C:/Users/lucas/Desktop/2020-2021/Master-ADS/Thesis_ADS/new_git')
#getwd()

#Raw snuffelfiets data is available at https://dataplatform.nl/#/data/fb526029-3700-460f-9848-68cfe79b0e5d, and also available in the raw data tab of this repository
#LOAD snuffelfiets RAW DATA
raw_week1 = read.csv('resource_2020_01_06_2020_01_13.csv')
raw_week2 = read.csv('resource_2020_01_13_2020_01_20.csv')
raw_week3 = read.csv('resource_2020_01_20_2020_01_27.csv')
raw_week4 = read.csv('resource_2020_01_27_2020_02_03.csv')

#Bind 4 weeks of interest in one df
raw_all = rbind(raw_week1,raw_week2,raw_week3,raw_week4)

#Preprocessing function is outlined in preprocessing_first_steps file
preprocessed_all = preprocessing_pm_speed(raw_all)

#This gemeente_shapefile is a shapefile containing the borders of the province of Utrecht, this is used to create the kriging/aggregation grid
#The data is found using the open source WMS plugin of the dutch government, and also available in the raw data tab of this repository
gemeente_shapefile = st_transform(st_as_sf(st_read('Utrecht_gemeentes.shp',crs = 4326),coords = geometry),crs = crs)


