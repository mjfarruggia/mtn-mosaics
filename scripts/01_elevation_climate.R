############################################################
## 01_elevation_climate.R
## Authors: Adrianne Smits, Janice Brahney, Angela Strecker, MJ Farruggia
## Last Updated: Feb 2026 by MJF
############################################################

source("scripts/00_setup.R")

#load hypothetical gradient in landcover - Rock vs Forest--------------------------------------------------------
BarrenVeg<-read.csv('data/original/BarrenVeg.csv', header=TRUE) #mean + stdev of landcover vs elevation from EPA NLS 
MixedVeg<-read.csv('data/original/MixedVeg.csv', header=TRUE) #mean and stdev on veg type vs elevation from EPA NLS

## Constants ------------------------------------------------------------------------------------------------------------
elevation <-seq(100,4000,100)
#Carbon input constants per vegetation type. These are coarse approimations from the literature. Very few studies to draw from. 
Cf<- 15.52 #kg/m2/yr, water soluble carbon and carbohydrate input rate to soils from forests Plant Soil (2010) 337:151-165
Cg<- 11.24 #kg/m2/yr, water soluble carbon and carbohydrate input rate to soils from grasslands Plant Soil (2010) 337:151-165
Cs<- 3.86  # Shrub Peri et al 2018 Sustainabilitt
Cw<- 75   # Wetlamd (forest x 5), from Fissore et al 2009
Cr<- 0    # rock

## Equations with elevation-----------------------------------------------------------------------------------------------
#Basic Climate
Precip = 0.232*elevation + 598.7 #model derived from ClimateWNA elevation gradients (mm or L/m2), average of 7 latitude starting points
MAT = -0.0039*elevation +12.55 #model derived from ClimateWNA elevation gradients, average of 7 latitude starting points

## Hargreaves evaportransporation ----------------------------------------------------------------------------------
#Samani et al 2000
R = ((0.0008*elevation + 15.396)*2.45) #Solar radiation in mm/day
TD = -0.0033*elevation + 28.26 #temp difference, as above from ClimateWNA
KT = (0.00185*TD^2) - (0.0433*TD) + 0.4023 #Samani et al
ET = 0.0135*KT*R*(TD^0.5)*(MAT+17.8)
Runoff=Precip-ET
