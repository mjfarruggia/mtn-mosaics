
########### Mountain Lakes Code Combined ############################################################################
## Authors: Adrianne Smits, Janice Brahney, Angela Strecker
## Combined January 2022
## Figures modified Oct 2022 with code provide by Bella Oleksy
####################################################################################################################


####################################################################################################################
#### DOC MODEL
#### Author: Janice Brahney
#### April 2018
#### This script simulates DOC runoff generation based on the temperature, precipitation (Null), 
####variation in the amount of vegetation cover, and the variation in the amount and type of vegetation
##################################################################################################################################################


##-------------
#modified by MJ Farruggia Feb 2026
##-------------

install.packages("viridis")
library(ggplot2)
library(gridExtra)
library(egg)
library(cowplot)
library(truncnorm)
library(broom)
library(viridis)
rm(list = ls())

#load hypothetical gradient in landcover - Rock vs Forest
BarrenVeg<-read.csv('data/BarrenVeg.csv', header=TRUE) #mean + stdev of landcover vs elevation from EPA NLS 
MixedVeg<-read.csv('data/MixedVeg.csv', header=TRUE) #mean and stdev on veg type vs elevation from EPA NLS

#### Constants 
elevation <-seq(100,4000,100)
#Carbon input constants per vegetation type. These are coarse approimations from the literature. Very few studies to draw from. 
Cf<- 15.52 #kg/m2/yr, water soluble carbon and carbohydrate input rate to soils from forests Plant Soil (2010) 337:151-165
Cg<- 11.24 #kg/m2/yr, water soluble carbon and carbohydrate input rate to soils from grasslands Plant Soil (2010) 337:151-165
Cs<- 3.86  # Shrub Peri et al 2018 Sustainabilitt
Cw<- 75   # Wetlamd (forest x 5), from Fissore et al 2009
Cr<- 0    # rock

#### Equations with elevation
#Basic Climate
Precip = 0.232*elevation + 598.7 #model derived from ClimateWNA elevation gradients (mm or L/m2), average of 7 latitude starting points
MAT = -0.0039*elevation +12.55 #model derived from ClimateWNA elevation gradients, average of 7 latitude starting points

#Hargreaves evaportransporation #Samani et al 2000
R = ((0.0008*elevation + 15.396)*2.45) #Solar radiation in mm/day
TD = -0.0033*elevation + 28.26 #temp difference, as above from ClimateWNA
KT = (0.00185*TD^2) - (0.0433*TD) + 0.4023 #Samani et al
ET = 0.0135*KT*R*(TD^0.5)*(MAT+17.8)
Runoff=Precip-ET

#########################################
############# NULL MODEL ################
#########################################

#Soil Carbon storage and potential export as a function of temperature and precipitation only
#CarbonIn is set to the average of forest and grassland (13.38 Kg/m2) and is held constant

CarbonIn = 13.38 #kg/m2 (hold constant for sake of simplicity)
TurnT = 138*exp(-0.11*MAT) #Weighted carbon turnover time from Trumbore et al 1996 in yr
SoilC = CarbonIn*TurnT #Soil carbon stocks in kg/m2
IceFree = 22.7*MAT+70 #Ice/Snow free days from ClimateWNA ********
DOCProd = (3.3134*MAT+12.937)*IceFree #ug/kg/year DOC Production based on the number of ice/snow free days #Christ and David 1996

DOCRunoff_Null = SoilC*DOCProd/Runoff/1000 #runoff in mg/L
DOCExpNull<-as.data.frame(cbind(elevation, DOCRunoff_Null))
colnames(DOCExpNull) = c("elevation", "DOC")


#### NULL PLOTS
## regular biplots
par(mfrow = c(2, 3))
plot(elevation, DOCRunoff_Null)

##Fancy ggplot
library(ggplot2)
library(gridExtra)
library(ggthemes)

# Customize a theme with large enough text  -------------------------------

#I'm guessing 10 would be the minimum but you can play around here

theme_MS <- function () { 
  theme_base(base_size=10) %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, linewidth=1.0),
      plot.title = element_text(face="bold", hjust=0.5, size=10, vjust=2),
      plot.subtitle = element_text(color="dimgrey", hjust=0.5, size=10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      #aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text.y = element_text(size=10, angle=270),
      strip.text.x = element_text(size=10),
      panel.spacing=grid::unit(0,"lines"),
      axis.ticks.length = unit(0.1, "cm")
    )
}

theme_set(theme_MS())

#annotate panel letters inside plot
panelLetter.normal <- data.frame(
  xpos = c(-Inf),
  ypos =  c(Inf),
  hjustvar = c(-0.2) ,
  vjustvar = c(1.5))


###Plot Null Model

plot_DOCnull= ggplot(DOCExpNull, aes(x=elevation, y=DOC)) +
  geom_line(aes(y=DOC))+
  
  labs(y="DOC (mg/L)")+
  labs(x="Elevation (m)")+
  #labs(title="Null") +
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500)) +

  #geom_text(data=panelLetter.normal,
            #aes(x=xpos,
               # y=ypos,
               # hjust=hjustvar,
                #vjust=vjustvar,
                #label="D: Null", #this is the only thing you have to change
                #fontface="bold"))+
  
  annotate("text", x=350, y =50, label= "D: Null", fontface="bold")+  #hard to get this to align, seems to readjust with the 'combined' function, 350 is from the left of the whole plot when all plotted together.
  
  #xlim(10, 31)+ #expanding the limit here slightly so that the axis text doesn't get cut off
  theme(plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(3, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())


plot(plot_DOCnull)


#################################################
######### MODEL 1 Vegetation Changes ############
#################################################

#Soil Carbon storage and potential export as a function of temperature and precipitation 
#CarbonIn is set to the average of forest and grassland (13.38 Kg/m2) but the proportion of vegetation cover
#is allowed to vary according to that observed in the EPA NLA database. Vegetation cover cannot be greater than 1 or less than 0

#Function to Generate random variables with a mean and std
rnorm2<-function(n,mean,sd){mean+sd*scale(rnorm(n))}

#produce 100 random vegetation cover proportions for each 100 meter increase in elevation
#Values based on mean and std of EPA NLS data for all veg cover types
VegProp = matrix(nrow=40, ncol=100)

Arraylength1<-dim(BarrenVeg)
for (i in 1:Arraylength1[1])
{
  VegProp[i,]<-rnorm2(100,BarrenVeg[i,2],BarrenVeg[i,4])
}
VegProp[VegProp<0]<-0 #remove negative values
#VegProp[VegProp>1]<-1 #remove
VegProp<-t(VegProp)

#Predict DOC runoff for each of the 100 proportions at each elevation
Carbon1Array = matrix(nrow=40, ncol=100)
SoilC1Array = matrix(nrow=40, ncol=100)
DOCRun1Array = matrix(nrow=40, ncol=100)

Carbon1Array <- VegProp*CarbonIn
SoilC1Array = t(Carbon1Array)*TurnT #Soil carbon stocks in kg/m2
DOCRun1Array = SoilC1Array*DOCProd/Runoff/1000 #runoff in mg/L
DOCRun1Min = apply(DOCRun1Array, 1, FUN = min)
DOCRun1Max = apply(DOCRun1Array, 1, FUN = max)
DOCRun1Mean = as.matrix(rowMeans(DOCRun1Array))
DOCRun1Std = as.matrix(apply(DOCRun1Array,1,sd, na.rm=T))
DOCRunMod1 = as.data.frame(cbind(elevation,DOCRun1Mean,DOCRun1Std))
colnames(DOCRunMod1)= c("elevation", "DOCRunmean","DOCRunstd")

write.csv(DOCRun1Array, file="data/DOCRun.csv")
write.csv(VegProp, file="data/VegProp.csv")

plot_DOC_M1 = ggplot(DOCRunMod1, aes(x=elevation, y=DOCRunmean)) +
  geom_line(aes(y=DOCRunmean))+
  geom_ribbon(data=DOCRunMod1, aes(x=elevation, ymin=DOCRun1Min, ymax=DOCRun1Max), alpha=0.2) +
  
  labs(y="DOC (mg/L)")+
  #labs(title="Vegetation Cover") +
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  
  #geom_text(data=panelLetter.normal,
   #         aes(x=xpos,
      #          y=ypos,
      #          hjust=hjustvar,
        #        vjust=vjustvar,
        #        label="E: Vegetation Cover", #this is the only thing you have to change
        #        fontface="bold"))+
  annotate("text", x=1350, y =50, label= "E: Vegetation Cover", fontface="bold")+  
  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())
  
plot(plot_DOC_M1)

grid.arrange(plot_DOCnull,plot_DOC_M1, ncol=2)



###################################################
############ MODEL 2 Veg Type Changes ########
###################################################

#Soil Carbon storage and potential export as a function of temperature and precipitation 
#The type of vegetation and therefore carbon is allowed to vary with elevation based on the
#EPA NLS catchment data.

#Function to Generate random variables with a mean and std
rnorm2<-function(n,mean,sd){mean+sd*scale(rnorm(n)) }

##create distributions based of mean and std that sum to 1

VP=matrix(nrow=40,ncol=5)
CarbonArray2 = matrix(nrow=40, ncol=100)

for (ii in 1:100){
for (i in 1:40){
  barren<-abs(rnorm2(2,MixedVeg[i,1],MixedVeg[i,6]))
  forest<-abs(rnorm2(2,MixedVeg[i,2],MixedVeg[i,7]))
  shrub<-abs(rnorm2(2,MixedVeg[i,3],MixedVeg[i,8]))
  grass<-abs(rnorm2(2,MixedVeg[i,4],MixedVeg[i,9]))
  wetland<-abs(rnorm2(2,MixedVeg[i,5],MixedVeg[i,10]))
  sumall=barren[1]+forest[1]+shrub[1]+grass[1]+wetland[1]
  VP[i,]<-cbind((barren[1]/sumall),(forest[1]/sumall),(shrub[1]/sumall),(grass[1]/sumall),(wetland[1]/sumall))
      
  CarbonArray2[i,ii] = VP[i,1]*Cr + VP[i,2]*Cf + VP[i,3]*Cs + VP[i,4]*Cg + VP[i,5]*Cw
}
}

DOCRun2Array = CarbonArray2*TurnT*DOCProd/Runoff/1000
DOCRun2Mean = as.matrix(rowMeans(DOCRun2Array))
DOCRun2Min = apply(DOCRun2Array, 1, FUN = min)
DOCRun2Max = apply(DOCRun2Array, 1, FUN = max)
DOCRun2Std = as.matrix(apply(DOCRun2Array,1,sd, na.rm=T))
DOCRunMod2 = as.data.frame(cbind(elevation,DOCRun2Mean,DOCRun2Std))
colnames(DOCRunMod2)= c("elevation", "DOCRunmean","DOCRunstd")

## 2 for 95% and 3 for 99.7%, but talked to Susan Durham and agreed not a true confidence interfal. but can use the precentile.
plot_DOC_M2 = ggplot(DOCRunMod2, aes(x=elevation, y=DOCRunmean)) +
  geom_line(aes(y=DOCRunmean))+
  geom_ribbon(data=DOCRunMod2, aes(x=elevation, ymin=DOCRun2Min, ymax=DOCRun2Max), alpha=0.2) +
  
  #Aesthetics
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  labs(title="Vegetation Type + Cover")

  
plot(plot_DOC_M2)

grid.arrange(plot_DOCnull,plot_DOC_M1, plot_DOC_M2,ncol=3, heights=c(2,2))



###################################################
############ validation plot  ########
###################################################
# try adding validation data to plot (a)

ValData<-read.csv('data/ValData.csv', header=TRUE) #validation data from mnt ranges
ValData<-as.data.frame(ValData)
colnames(ValData)= c("Region", "Elevation","Eofbase","DOC")
library(viridis)


### Add validation data


plot_Dval = ggplot()+
  #Models
  geom_line(data=DOCRunMod2, aes(x=elevation,y=DOCRunmean), size=1.5)+
  geom_ribbon(data=DOCRunMod2, aes(x=elevation, ymin=DOCRun2Min, ymax=DOCRun2Max), alpha=0.2) +
  
  #validation
  geom_point(data=ValData, aes(x=Elevation, y=DOC,color=Region, shape=Region, fill =Region), color="white",size=1)+
  scale_shape_manual(values=c(21,22,23,24,21,22,23,24,21,22,23,24))+
  #scale_fill_viridis(discrete = TRUE) +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  scale_color_manual(values = c("#440154FF",  "#482677FF", "#404788FF", "#39568CFF","#2D708EFF",  "#238A8DFF",  "#1F968Bff","#29AF7FFF","#55C667FF"))+
  scale_fill_manual(values = c("#440154FF",  "#482677FF", "#404788FF", "#39568CFF","#2D708EFF",  "#238A8DFF",  "#1F968Bff","#29AF7FFF","#55C667FF"))+
  geom_smooth(data=ValData, aes(x=Elevation, y=DOC, color = Region, fill = Region), method = "lm", se=F, size=0.75) + 
  
  #Aesthetics
  labs(x="Elevation (m)")+
  #labs(title="Veg. Cover + Type; Synoptic Data") +
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  #theme(legend.position = c(0.77,0.77))+
  theme(legend.key.size = unit(0.25, 'cm'))+

  #geom_text(data=panelLetter.normal,
    #        aes(x=xpos,
     #           y=ypos,
      #          hjust=hjustvar,
       #         vjust=vjustvar,
        #        label="F: Veg. Cover + Type; Synoptic Data", #this is the only thing you have to change
         #       fontface="bold"))+
  
  annotate("text", x=1450, y =47, label= stringr::str_wrap("F: Veg. Cover + Type; Synoptic Data", width = 25), fontface="bold")+  
  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())


plot(plot_Dval)

grid.arrange(plot_DOCnull,plot_DOC_M1, plot_Dval, ncol=3, heights=c(4.5,3))

#grid.arrange(plot_DOCnull,plot_DOC_M1, plot_DOC_M2, plot_Dval, ncol=4)




###################################################
############ functions ########
###################################################


#Random vegetation proportion
Randomf = function(x){
  x=runif(5,min=0,max=1)
  x=x/sum(x)
  return(x)
}



###############################################################################################################################################################################
##Author: Adrianne Smits
##Date: 4/24/2018
##Mountain Lakes Group
###################################################################################################################################################################################
##Mountain gradients and mosaics: Lake temperature mosaic model 3
##This script simulates lake temperature changes in response to solar radiation, snowmelt input, ice-off date
##for a model small mountain lake (e.g. Emerald Lake in the Sierra Nevada). The snowmelt inputs and ice-off dates, and solar radiation term
#are allowed to vary
##with elevation. The lake is assumed to be at steady state (no volume change), and we are ignoring stratification plus a bajillion other things.
#In this version of the model, we will vary both lake surface area AND the fraction of solar exposure that reaches the lake surface, as a consequence of watershed topography (slope and aspect)
#The depth of the mixed layer are constant, assume stratification....

#Note: For this model, watershed topography only affects incoming solar radiation to the lake, not snow accumulation or melt rate. This is probably unrealistic

##################################################################################################################################################################################

##Load packages:
#Sirad to compute solar radiation:
library('sirad')
#load Geosphere package to calculate daylength from latitude and doy
library('geosphere')

###############################################################################################################################################################################
##Load air temperature time series (emerald lake weather station 2016)
load('data/02_AirTdaily.Rdata')
#plot(AirTdaily$doy, AirTdaily$AirTemp_C_Avg)

##################################################################################################################################################################################
###Note: no longer using!!!
##Load linear regression model between average watershed slope and
##average solar exposure ratio (ratio between obstructed: unobstructed lake radiation inputs)
# load('02_Slope_Solar_lm.Rdata')
# int <- coef(meanslope.lm)[1]
# slope <-coef(meanslope.lm)[2]

###################################################################################################################################################################################
##Input parameters
#make a matrix of elevations and radiation fractions/lake areas to loop through. For each elevation, have 50 combinations of area+radiation
#vector of elevations
elevation <-seq(1000,4000,200)
#elevation <- runif(100, 1000,3200)

#dataframes of lake areas and radiation fractions for all elevation bands:
lkSA <- data.frame(matrix(ncol = 50, nrow = length(elevation)))
rownames(lkSA) <- elevation
for (i in 1:length(elevation)){
  lkSA[i,] <- runif(50, 10000,200000) #lake areas
}
lkSA

radfracs <- data.frame(matrix(ncol = 50, nrow = length(elevation)))
rownames(radfracs) <- elevation
for (i in 1:length(elevation)){
  radfracs[i,] <- runif(50, 0.7,1) #radiation fraction
}
radfracs


#create vector of column names for results
SAcols <- rep(NA,50)
for (i in 1:length(radfracs )){
  SAcols[i] <- paste('combo',i) 
}

#empty dataframe to hold results
column.names <- c('Elevation','SWE','IceOutdoy',SAcols)
results <- data.frame(matrix(ncol = length(column.names), nrow = length(elevation)))
colnames(results) <- column.names
results$Elevation <- elevation
results

#Static lake and watershed properties
lat<- 36.59694444#decimal degrees
lon <- -118.675957#decimal degrees
EML_elev <- 2810 #elevation of Emerald lake
Watershed_area <- 121#hectares
#lkSA <- 2.5*10000 #m^2
#MeanDepth = lkSA/4000 #in m; keep lake depth:SA ratio constant; Note that we did not use ratio from EML b/c it resulted in absurdly deep lakes when SA is increased
MeanDepth = 5# Depth of upper mixed layer... play with this!!! Could model this as a function of lake surface area (relationship in Fee paper), but maybe then we don't want to change the lossfrac term

lkStartT <- 3 #degrees C
#Model constants
snowmeltT <- 1 #assume for now that snowmelt temperature stays constant throughout melt period
HeatCap <- 4.184*1000000#Heat capacity of water in Joules per cubic meters *degrees C...change units?
AirT_lapse <- -0.0065 #degrees C per meter (from Brubacker 1996); air temperature lapse rate


#Loop over a bunch of elevations
for (j in 1: length(elevation)){
  #Lake elevation
  elev <- elevation[j] #meters
  
  #Snowmelt terms
  #Note: We are using a simple temperature index model (degree day) to melt the snow,
  #     but eventually we may add in a radiation component, as in Brubacker et al. 1996
  #SWE <- 1100 #mm; avg across watershed
  SWE = 0.232*elev + 598.7 #model derived from ClimateWNA elevation gradients (mm or L/m2), average of 7 latitude starting points
  results$SWE[j] <- SWE
  TotalSWEVol <- (SWE/1000)*Watershed_area*1000
  
  #Time span of model run based on SWE/ice off relationship
  #IceOut <- round(121.40785 + 0.030458*SWE)#this will vary with SWE based on regression relationship in Sadro et al.2018 LOL paper
  IceOut <- round(120 + 0.04*SWE)
  results$IceOutdoy[j] <- IceOut
  startDOY <-IceOut
  endDOY <- 230 #this does not vary (date of max temp achieved, take an average value from Emerald long-term data)
  days <- seq(startDOY,endDOY, by=1) #sequence of days over which to run model
  
  #Loop to create time series of daily snowmelt
  airTsnowmelt <- AirTdaily$pred# vector of daily EML air temp in 2016 (daily average) 
  airTsnowmelt <- airTsnowmelt+ AirT_lapse*(elev - EML_elev)#corrected for elevation by lapse rate
  airTsnowmelt[airTsnowmelt<0]=0 #set any air temp below zero to zero so that no snow melts below freezing
  dailyMeltRate_dd <- 2.74 *(airTsnowmelt)#a function of air temp (Snowmelt chapter in USDA national engineering handbook; mm/day)
  dailyMeltVol <-Watershed_area*1000*dailyMeltRate_dd/1000 #m^3
  SWEVolleft <- rep(0,length(dailyMeltVol))
  SWEVolleft[1:days[1]] = TotalSWEVol#Snowmelt is assumed zero before ice-off
  for(i in days[1]:days[length(days)]){ #subtract from the snowpack until it's gone
    SWEVolleft[i]=SWEVolleft[i-1]-dailyMeltVol[i]
  }
  SWEVolleft[SWEVolleft<0]=0 #no negative SWE allowed!
  endMeltDOY <- length(which(SWEVolleft>0))#day when snow is gone
  #plot(SWEVolleft,type='l')#check plot
  #abline(v=endDOY)#day when lake heating ends
  dailyMeltIn <- c(dailyMeltVol[startDOY:endMeltDOY],rep(0,(endDOY-endMeltDOY)))#volume of snowmelt entering lake (m^3)
  length(dailyMeltIn)
  #plot(days,dailyMeltIn,type='l')
  #advectic heat into lake (joules)
  advecIn <- dailyMeltIn*snowmeltT*HeatCap#advectic heat into lake (joules)
  length(advecIn)
  
  
  #Radiation terms (convert ultimately to units of Joules, Watts= Joules/second)
  #Angstrom Prescott model for solar radiation, using coefficients that vary by elevation
  #as proposed in Neuwirth 1980 Solar Energy (Table 2; based on summer values)
  daylengths <- daylength(lat=lat,doy=AirTdaily$doy)#vector of daylight lengths (in hours)
  A0 =0.205
  A1 =0.023
  A2 =0.007
  A= A0 + (elev/1000)*A1 + A2*(elev/1000)^2
  B0=0.497
  B1=-0.069
  B2=0.019
  B= B0 + (elev/1000)*B1 + B2*(elev/1000)^2
  
  SSD = daylengths*0.5 #length of the day that sunlight actually reaches the ground/lake surface
  #solar radiation in in megajoules per m^2 for the entire year
  radMJoules_AP <- ap(days=seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by="days"), 
                      lat=lat, lon=lon, extraT=NULL, A=A, B=B, SSD=SSD)
  
  #Now loop over a bunch of radiation fractions + lake areas
  for (k in 1:length(lkSA)){
    
    RadiationIn <- radMJoules_AP*lkSA[j,k]*radfracs[j,k]  #megajoules into lake surface, multiplied by fraction based on how topography (watershed slope and aspect) blocks solar radiation
    RadiationOut <- 0.1*RadiationIn#some fraction of RadiationIn, albedo term in Wetzel and Likens chapter 4
    netRad <- (RadiationIn - RadiationOut)*1000000 #joules
    netRad <- netRad[days]#only radiation for lake ice off period
    length(netRad)
    
    #Heat loss fraction due to LWnet, latent heat flux, and sensible heat flux:
    lossfrac <-0.6 +lkSA[j,k]*(0.000001)#Change this to scale with lake surface area (e.g. big lakes have higher wind-driven fluxes?)
    
    ##Run heat budget model##
    
    ##empty vectors to hold total heat and lake temperature time series
    lkVol = lkSA[j,k]*MeanDepth #m^3
    advecOut <- rep(0,length(days))
    Qloss <- rep(0,length(days)) #heat loss attributed to latent, sensible, and LWnet (based on EML heat budget for 2016)
    Qtot <- rep(0,length(days))#Daily heat fluxes in or out
    Qs <- rep(0,length(days)) #Heat storage in the lake
    LakeT <- rep(0,length(days)) #lake temperature
    
    ##Heat budget model
    for (i in 1:length(days)){
      if (i==1) {
        #Outlet advection heat loss
        advecOut[i]= dailyMeltIn[i]*lkStartT*HeatCap
        #Heat loss from sensible, latent, and lw radation:
        Qloss[i] = lossfrac*netRad[i]
        ##Total energy input in units of Joules
        Qtot[i] = netRad[i] - Qloss[i] + advecIn[i] - advecOut[i] 
        ##Heat storage term:
        Qs[i]= lkStartT*lkVol*HeatCap + Qtot[i]
        ##Lake temp 
        LakeT[i] = Qs[i]/(lkVol*HeatCap)
        
      } else {
        #Outlet advection heat loss, assume water: volume in = water volume out
        advecOut[i]= dailyMeltIn[i]*LakeT[i-1]*HeatCap
        #Heat loss from sensible, latent, and lw radation:
        Qloss[i] = lossfrac*netRad[i]
        ##Total energy input in units of Joules
        Qtot[i] = netRad[i] - Qloss[i] + advecIn[i] - advecOut[i]
        ##Heat storage term:
        Qs[i]= Qs[i-1] + Qtot[i]
        ##Lake temp 
        LakeT[i] = Qs[i]/(lkVol*HeatCap)
      }
      
    }#end of heat budget loop
    
    #fill in final dataframe
    results[j,(k+3)] <- max(LakeT) 
    
  }#end of lake SA/radfrac loop
  
}#end of elevation loop

results
#################################################################################################################################################################################################################################

##Save results
results_lkarea_topo <- results

save(results_lkarea_topo,file='data/04_mglm_output_lkarea_topo.Rdata')

#Plot maximum summer lake temperature (upper mixed layer) versus elevation   
png('output/03_mglm_mosaic3_lkarea_topo_temp.png',height=4, width=6,units='in',res=600)
par(mfrow=c(1,1),mar=c(3,3,1,1),oma=c(1,1,1,1))
plot(results$Elevation,results[,8], xlim=c(900,3300),ylim=c(10,28),pch='', ann=F,axes=F,yaxs='i',xaxs='i')
axis(side=1, at=seq(0,4000,by=500),cex.axis=1.2,lwd=2)#x-axis
mtext(side=1,line=2.5,'Elevation (m)',cex=1.3)#x axis label
axis(side=2,at=seq(0,35,by=4),cex.axis=1.2,lwd=2)
mtext(side=2,line=2.5,expression(Max~Epi~T~(degree~C)),cex=1.3)#y-axis label
box(lwd=2)
palette <- colorRampPalette(c('lightskyblue','slateblue'))
cols <- palette(length(column.names))

for (i in 4:length(column.names)){
  points(results$Elevation,results[,i], pch=19, col='black')
  #lines(results$Elevation,results[,i], col=cols[i],lwd=2)
}

dev.off()



  #Ae###################################################################################################################################################################################
##Mountain gradients and mosaics: Figures for gradient and mosaic models of lake temperature
##################################################################################################################################################################################

#set working directory
# wd <-'C:/Users/asmits/Dropbox/Todd_share/Sierra Postdoc/Mountain Lakes Group/Lake Temp Model R Scripts'
# setwd(wd)

##################################################################################################################################################################################
#load libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(viridis)
options(ggplot2.continuous.fill="viridis")

##################################################################################################################################################################################
##Load output from the elevation gradient and mosaic models for temperature:

#null model (elevation only)
load('data/03_mglm_output_nullmodel.Rdata')

#variable lake surface area model (elevation + SA): Mosaic 1
load('data/03_mglm_output_lkarea.Rdata')

#variable SA and topography (elevation + SA + topography): Mosaic 2
load('data/03_mglm_output_lkarea_topo.Rdata')

##################################################################################################################################################################################
##Load data from real lakes for validation

real.data <- read.csv('data/01_Temp_model_validation_data.csv',header=TRUE)
head(real.data)
#exclude lake from the alps
real.data <- real.data[-which(real.data$Mt_range=='Alps'),]
real.means <- data.frame(summarise(group_by(real.data,Mt_range,Lake_name, Elevation),
                                   
                                   Max_T_mean =mean(Max_epi_T, na.rm=TRUE),
                                   Max_T_sd=sd(Max_epi_T, na.rm=TRUE)))

##################################################################################################################################################################################
##Calculate mins, max, means and standard deviationsof of max epilimnion temperature for lakes at each elevation band (model output)

#elevation+ SA
lkarea_sd <- apply(results_lkarea[,-(1:3)], MARGIN=1, FUN='sd')
lkarea_max <- apply(results_lkarea[,-(1:3)], MARGIN=1, FUN='max')
lkarea_min <- apply(results_lkarea[,-(1:3)], MARGIN=1, FUN='min')
lkarea_mean <- apply(results_lkarea[,-(1:3)], MARGIN=1, FUN='mean')
results_lkarea <- cbind(results_lkarea,lkarea_mean)
results_lkarea <- cbind(results_lkarea,lkarea_sd)
results_lkarea <- cbind(results_lkarea,lkarea_max)
results_lkarea <- cbind(results_lkarea,lkarea_min)


#elevation+ SA + radiation fraction
lkareatopo_sd <- apply(results_lkarea_topo[,-(1:3)], MARGIN=1, FUN='sd')
lkareatopo_max <- apply(results_lkarea_topo[,-(1:3)], MARGIN=1, FUN='max')
lkareatopo_min <- apply(results_lkarea_topo[,-(1:3)], MARGIN=1, FUN='min')
lkareatopo_mean <- apply(results_lkarea_topo[,-(1:3)], MARGIN=1, FUN='mean')
results_lkarea_topo <- cbind(results_lkarea_topo,lkareatopo_mean)
results_lkarea_topo <- cbind(results_lkarea_topo,lkareatopo_sd)
results_lkarea_topo <- cbind(results_lkarea_topo,lkareatopo_max)
results_lkarea_topo <- cbind(results_lkarea_topo,lkareatopo_min)

##################################################################################################################################################################################
##Multi-panel figure using gglplot, to match output figures from DOC and zoop diversity models

#null model
plot_Exnull= ggplot(results_null, aes(x=elevation, y=LakeT)) +
  geom_line(aes(y=LakeT))+labs(x="Elevation (m)")+labs(y=expression(Epilimnion~Temperature~(degree~C))) +
  #aesthetics
  labs(y="Temperature (?C)")+
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(5,30),breaks=seq(5,30,5)) +
  scale_x_continuous(limits=c(000,4000),breaks=seq(000,4000,500))+
  #labs(title="Null") +

  #geom_text(data=panelLetter.normal,
          #aes(x=xpos,
              #y=ypos,
              #hjust=hjustvar,
             # vjust=vjustvar,
              #label="A: Null", #this is the only thing you have to change
             # fontface="bold"))+
 
  annotate("text", x=350, y =30, label= "A: Null", fontface="bold")+  
  
  #xlim(10, 31)+ #expanding the limit here slightly so that the axis text doesn't get cut off
 
   theme(plot.margin=unit(c(0.5,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(3, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())

plot(plot_Exnull)

#Mosaic 1: Lake Area
plot_Ex_M1 = ggplot(results_lkarea, aes(x=Elevation, y=lkarea_mean)) +
  geom_line(aes(y=lkarea_mean))+
  geom_ribbon(data=results_lkarea, aes(x=Elevation, ymin=lkarea_min, ymax=lkarea_max), alpha=0.2) +
  
  #Aesthetics
  #labs(title="Lake Area") +
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(5,30),breaks=seq(5,30,5)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+

#geom_text(data=panelLetter.normal,
          #aes(x=xpos,
           #   y=ypos,
            #  hjust=hjustvar,
             # vjust=vjustvar,
             # label="B: Lake Area", #this is the only thing you have to change
             # fontface="bold"))+
  
  annotate("text", x=800, y =30, label= "B: Lake Area", fontface="bold")+  
  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0.5,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())
plot(plot_Ex_M1)

#grid.arrange(plot_Exnull,plot_Ex_M1, ncol=2)

#Mosaic 2: Lake Area + Topography
plot_Ex_M2 = ggplot(results_lkarea_topo, aes(x=Elevation, y=lkareatopo_mean)) +
  geom_line(aes(y=lkareatopo_mean))+theme_bw()+labs(x="Elevation (m)")+labs(y=expression(Epilimnion~Temperature~(degree~C))) +
  geom_ribbon(data=results_lkarea_topo, aes(x=Elevation, ymin=lkareatopo_min, ymax=lkareatopo_max), alpha=0.2) +
  geom_text(data=panelLetter.normal,
            aes(x=xpos,
                y=ypos,
                hjust=hjustvar,
                vjust=vjustvar,
                label="B", #this is the only thing you have to change
                fontface="bold"))+
  #Aesthetics
  labs(title="Lake Area + Topography") +
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(5,30),breaks=seq(5,30,5)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))

plot(plot_Ex_M2)

#All three models together in three panels
panels = grid.arrange(plot_Exnull,plot_Ex_M1, plot_Ex_M2,ncol=3)

#Real data panel
range_average <-real.data %>% group_by(Mt_range)%>% summarise(Max_T_mean=mean(Max_epi_T,na.rm=TRUE),
                                                              Elevation=median(Elevation,na.rm=TRUE))
Plot_Ex_realdata <- 
  ggplot(real.means, aes(x=Elevation,y=Max_T_mean)) +
  geom_point(mapping = aes(x=Elevation,y=Max_T_mean,fill=Mt_range),size=2,shape=21) +
  geom_smooth(aes(x=Elevation,y=Max_T_mean,col=Mt_range,fill=NA),method='lm')+
  geom_errorbar(aes(ymin=Max_T_mean-Max_T_sd, ymax=Max_T_mean+Max_T_sd,color=Mt_range), width=.2)+
  theme_bw()+
  scale_color_viridis_d(begin = 0, end = .8)+
  scale_fill_viridis_d(begin = 0, end = .8)+
  theme(legend.position=c(0.25,0.2),
        legend.background = element_rect(fill = "white", color = "black"))+
  theme(legend.position='none')+
  ggrepel::geom_label_repel(aes(x=Elevation,y=Max_T_mean,label=Mt_range,color=Mt_range),data=range_average,
                            alpha=0.8,nudge_y=3.5,segment.color=NA,size=2)+
  labs(x="Elevation (m)")+
  labs(y=expression(Epilimnion~Temperature~(degree~C)))+
  labs(title="Data") +
  theme(axis.title.y=element_blank())+
  coord_cartesian(xlim=c(1000,3800),ylim = c(5, 28))



plot(Plot_Ex_realdata)

############Plot similar to Richness##########
#Estimate = as.data.frame(cbind(results_null[,1],results_null[,4]))
#colnames(Estimate)= c("elevation","LakeT")

colnames(real.means)= c("Region", "Lake_name","Elevation", "Max_T_mean", "Max_T_sd")


plot_Tval = ggplot()+

  geom_line(data=results_lkarea_topo,  aes(x=Elevation, y=lkareatopo_mean), size=1.5)+
  geom_ribbon(data=results_lkarea_topo, aes(x=Elevation, ymin=lkareatopo_min, ymax=lkareatopo_max), alpha=0.2) +

  geom_point(data=real.means, aes(x=Elevation, y=Max_T_mean,color=Region, shape=Region, fill =Region), color="white",size=1)+
  scale_shape_manual(values=c(21,22,23,24,21))+
  
  scale_color_manual(values = c("#440154FF",  "#453781FF", "#33638DFF", "#238A8DFF",  "#29AF7FFF"))+
  scale_fill_manual(values = c("#440154FF",  "#453781FF", "#33638DFF", "#238A8DFF",  "#29AF7FFF"))+
  geom_smooth(data=real.means,aes(x=Elevation, y=Max_T_mean, color = Region),formula = 'y ~ x', method = "lm", se=F, size=0.75) + 
  #geom_errorbar(data=real.means,aes(x=Elevation, y=Max_T_mean, ymin=Max_T_mean-Max_T_sd, ymax=Max_T_mean+Max_T_sd,color=Region), width=.2)+

  labs(x="Elevation (m)")+
  #labs(title="Lake Area + Topo.; Synoptic Data") +
  scale_y_continuous(limits=c(5,30),breaks=seq(5,30,5)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+

  #theme(legend.position = c(0.8,0.85))+
  #theme(legend.title = element_text(color="black", size=10, face="bold"))+
  theme(legend.key.size = unit(0.25, 'cm'))+
  
  #geom_text(data=panelLetter.normal,
    #        aes(x=xpos,
     #           y=ypos,
      #          hjust=hjustvar,
      #          vjust=vjustvar,
       #         label="C: Lake Area + Topo.; Synoptic Data", #this is the only thing you have to change
        #        fontface="bold"))+

  annotate("text", x=1450, y =28.5, label= stringr::str_wrap("C: Lake Area + Topo.; Synoptic Data", width = 25), fontface="bold")+  
  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0.5,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())


plot(plot_Tval)


panels = grid.arrange(plot_Exnull,plot_Ex_M1, plot_Tval,ncol=3, heights=c(4.5,3))


#All three models plus real data together in four panels
#panels = grid.arrange(plot_Exnull,plot_Ex_M1, plot_Ex_M2,Plot_Ex_realdata,ncol=4,
                      # left='Epilimnion Temperature (Celsius)')

#save figure
ggsave("output/03_mlgm_lake_temp_4panels_ggplot.png",plot=panels,width=9, height=3.5, units='in')

####################################################################################################################
#### Zooplankton Model
#### Author: Angela Strecker
####################################################################################################################

#setwd("G:/My Drive/collaborations/mountain lakes/working group/zooplankton diversity/model and database") unshared file
#setwd("/Volumes/GoogleDrive/My Drive/Powell Center Mtn Lakes Reno Meeting/Conceptual models/Biodiversity model/model simulations")  #laptop
#setwd("G:/My Drive/Powell Center Mtn Lakes Reno Meeting/Conceptual models/Biodiversity model/model simulations")

library(ggplot2)
library(gridExtra)
library(egg)
library(cowplot)
library(truncnorm)
library(broom)
library(viridis)
full.data<-read.csv("data/zoop_env_lake_surfaceonly_1.20.22.csv",header=T)
#subset for Cascade lakes (MORA,NOCA,GPNF)
data<-subset(full.data,region=="MORA"|region=="NOCA"|region=="GP NF"|region=="MH NF"|region=="DES NF"|region=="WILL NF")
val.data<-subset(full.data,region !="MORA" & region !="NOCA" & region !="GP NF" & region !="MH NF" & region !="DES NF" & region !="WILL NF" & region !="BC EAST" & region !="BC INTERIOR"& region !="BC WEST" & region !="FOOT" & region !="GLAC" & region !="KANA" & region !="MASS" & region !="UMQ NF" & region !="WIN NF")

hist(data$temperature) #normal
hist(data$area)

max(data$area)
min(data$area)
area.mean=mean(data$area)
area.sd=sd(data$area)
quantile(data$area,0.99) #37.574
quantile(data$area,0.01) #0.02728
rtruncnorm(100, a=0, b=37.574, mean = area.mean, sd = area.sd)

log.area=log(data$area)
hist(log.area) #mostly normal
log.area.mean=mean(log.area)
log.area.sd=sd(log.area)
max(log.area)
min(log.area)
hist(rtruncnorm(100, a=-Inf, b=10, mean = log.area.mean, sd = log.area.sd))  #use this


#derive equations
model1<-lm(richness~elevation,data=data); summary(model1) #significant
model2<-lm(richness~elevation+temperature,data=data);summary(model2) #all significant
model3<-lm(richness~elevation+temperature+log(area),data=data);summary(model3)   #area not significant
model4<-lm(richness~elevation+temperature+fish_PA_bin,data=data);summary(model4)   #fish not significant
model5<-lm(richness~elevation+log(area),data=data);summary(model5) #area not significant
model6<-lm(richness~elevation+log(area)+temperature,data=data);summary(model6)  #area not significant
model7<-lm(richness~log(area),data=data);summary(model7)

m2.elev.coef<-summary(model2)$coefficients[2,1]
m2.temp.coef<-summary(model2)$coefficients[3,1]
m2.intercept<-summary(model2)$coefficients[1,1]
m3.elev.coef<-summary(model3)$coefficients[2,1]
m3.temp.coef<-summary(model3)$coefficients[3,1]
m3.area.coef<-summary(model3)$coefficients[4,1]
m3.intercept<-summary(model3)$coefficients[1,1]


##### simulations
#### null model (Model1)
elev <-seq(400,2500,100) #n=22
pred.rich<-predict(model1,list(elevation=elev))
rich.null<-as.data.frame(cbind(elev, pred.rich))

plot_null= ggplot(rich.null, aes(x=elev, y=pred.rich)) +
  geom_line(aes(y=pred.rich))+labs(y="Zooplankton richness") +
  
  labs(y="Zooplankton Richness")+
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(0,15),breaks=seq(0,15,2)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  #labs(title="Null") +
  
  #geom_text(data=panelLetter.normal,
            #aes(x=xpos,
            #    y=ypos,
            #    hjust=hjustvar,
            #    vjust=vjustvar,
             #   label="G: Null", #this is the only thing you have to change
             #   fontface="bold"))+
  
  annotate("text", x=350, y =15, label= "G: Null", fontface="bold")+  
  
  #xlim(10, 31)+ #expanding the limit here slightly so that the axis text doesn't get cut off
  theme(plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_text(angle=45, hjust=1))

plot(plot_null)


##### model with temp (Model2)

rich.model2<- matrix(nrow=22, ncol=550)
for (i in 1:22)
  for(j in 1:550)
  {
    temp<-rnorm(550,mean=15.165, sd=4.479) #mean, SD from Loewen et al. dataset
    rich.model2[i,j]=(m2.elev.coef*elev[i])+(m2.temp.coef*temp[j])+m2.intercept  #from model2 above
  }

rich.model2.mean = as.matrix(rowMeans(rich.model2))
rich.model2.std = as.matrix(apply(rich.model2,1,sd, na.rm=T))
rich.model2 = as.data.frame(cbind(elev,rich.model2.mean,rich.model2.std))
colnames(rich.model2)= c("elevation", "rich.model2.mean","rich.model2.std")

#showing full data range
plot_model2 = ggplot(rich.model2, aes(x=elevation, y=rich.model2.mean)) +
  geom_line(aes(y=rich.model2.mean))+
  geom_ribbon(data=rich.model2, aes(x=elevation, ymin=rich.model2.mean-rich.model2.std, ymax=rich.model2.mean+rich.model2.std), alpha=0.2) +
  
  #Aesthetics
  #labs(title="Temperature") +
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(0,15),breaks=seq(0,15,2)) +
  scale_x_continuous(limits=c(00,4000),breaks=seq(000,4000,500))+

  #geom_text(data=panelLetter.normal,
   #         aes(x=xpos,
    #            y=ypos,
     #           hjust=hjustvar,
     #           vjust=vjustvar,
      #          label="H: Temperature", #this is the only thing you have to change
      #          fontface="bold"))+
  annotate("text", x=1050, y =15, label= "H: Temperature", fontface="bold")+  

  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_text(angle=45, hjust=1))

plot(plot_model2)

#95% confidence interval
plot_model2 = ggplot(rich.model2, aes(x=elevation, y=rich.model2.mean)) +
geom_line(aes(y=rich.model2.mean))+theme_bw()+labs(x="Elevation (m)")+labs(y="Zooplankton richness") +
  geom_ribbon(data=rich.model2, aes(x=elevation, ymin=rich.model2.mean-rich.model2.std*1.96, ymax=rich.model2.mean+rich.model2.std*1.96), alpha=0.2) +
  theme(axis.title.y=element_blank(), axis.text.y=element_text(size=12),
        axis.text.x = element_text(size=12),axis.title.x = element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"))+
  scale_y_continuous(limits=c(2.5,8.5),breaks=seq(3,8,1))

(plot_model2)

grid.arrange(plot_null,plot_model2, ncol=2)


##### model with temp + fish (Model3)
rich.model3<- array(dim=c(22,25,25))

for (i in 1:22) 
  for(j in 1:25) 
    for(k in 1:25)
    {
      temp<-rnorm(25,mean=15.165, sd=4.479) #mean, SD from Loewen et al. dataset
      area<-rtruncnorm(25, a=-Inf, b=Inf, mean = log.area.mean, sd = log.area.sd) 
      rich.model3[i,j,k]=(m3.elev.coef*elev[i])+(m3.temp.coef*temp[j])+(m3.area.coef*area[k])+m3.intercept #from model4 above
    }


rich.model3.mean = as.matrix(rowMeans(rich.model3))
rich.model3.std = as.matrix(apply(rich.model3,1,sd, na.rm=T))
rich.model3 = as.data.frame(cbind(elev,rich.model3.mean,rich.model3.std))
colnames(rich.model3)= c("elevation", "rich.model3.mean","rich.model3.std")

#showing full data range
plot_model3 = ggplot(rich.model3, aes(x=elevation, y=rich.model3.mean)) +
  geom_line(aes(y=rich.model3.mean))+
  geom_ribbon(data=rich.model3, aes(ymin=rich.model3.mean-rich.model3.std, ymax=rich.model3.mean+rich.model3.std), alpha=0.2) +
  
  #Aesthetics
  labs(title="Temperature + Area") +
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(0,15),breaks=seq(0,15,2)) +
  scale_x_continuous(limits=c(000,4000),breaks=seq(000,4000,500))+
  theme_classic() +


geom_text(data=panelLetter.normal,
          aes(x=xpos,
              y=ypos,
              hjust=hjustvar,
              vjust=vjustvar,
              label="I", #this is the only thing you have to change
              fontface="bold"))+
  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0.5,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"))

plot(plot_model3)

#95% confidence interval
#plot_model3 = ggplot(rich.model3, aes(x=elevation, y=rich.model3.mean)) +
geom_line(aes(y=rich.model3.mean))+theme_bw()+labs(y="Zooplankton richness") +
  geom_ribbon(data=rich.model3, aes(ymin=rich.model3.mean-rich.model3.std*1.96, ymax=rich.model3.mean+rich.model3.std*1.96), alpha=0.2) +
  theme(axis.title.y=element_blank(), axis.text.y=element_text(size=12), axis.text.x = element_text(size=12), axis.title.x=element_blank())+
  scale_y_continuous(limits=c(2.5,8.5),breaks=seq(3,8,1)) 

#plot(plot_model3)

grid.arrange(plot_null,plot_model2,plot_model3, ncol=3)

panel<-plot_grid(plot_null,plot_model2,plot_model3,label_size=12,hjust=-3,vjust=2,align="hv",nrow=1,labels=c("(a)","(b)","(c)"))
panel

#grid.arrange(plot_null,plot_model2,plot_model3,ncol=3)
#ggarrange(plot_null,plot_model2,plot_model3,ncol=3) #equal width panels from package 'egg'

write.csv(rich.model2,"data/rich.model2.11.17.21.csv")
write.csv(rich.model3,"data/rich.model3.11.17.21.csv")
write.csv(rich.null,"data/rich.null.11.17.21.csv")

# try adding validation data to plot (a)

val.data$region<-as.factor(val.data$region)
levels(val.data$region)
val.data$region<-factor(val.data$region, levels=c("YUK NORTH", "YUK SOUTH","BC NORTH", "JASP", "BANF", "YOHO", "REVE", "KOOT", "WATE", "UMQ NF", "RR-SIS NF", "LASSEN"),labels=c("N. Yukon", "S. Yukon","N. BC", "Jasper", "Banff", "Yoho", "Revelstoke", "Kootenay", "Waterton", "Umpqua", "Rogue River-Siskiyou", "Lassen"))

pdf(file="output/validation_12.17.21.pdf", width=8, height=6)

###Orignal Graph
plot_Rval = ggplot()+
  geom_line(data=rich.null, aes(x=elev,y=pred.rich), size=1.5)+
  geom_point(data=val.data, aes(x=elevation, y=richness,color=region, fill=region, shape=region), color="white",size=3)+
  scale_shape_manual(values=c(21,22,23,24,21,22,23,24,21,22,23,24))+
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  geom_smooth(data=val.data, aes(x=elevation, y=richness, color = region, fill = region), method = "lm", se=F, size=0.75) + 
  theme_bw()+
  labs(y="Zooplankton richness", x="Elevation (m)") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),title = element_text(size=14,face="bold"))+
  scale_y_continuous(limits=c(0,25),breaks=seq(0,25,5)) + theme(legend.position = c(0.85,0.65))
dev.off()
plot(plot_Rval)
panel<-plot_grid(plot_null,plot_model2,plot_model3,plot_Rval, label_size=12,hjust=-3,vjust=2,align="hv",nrow=1,labels=c("(a)","(b)","(c)", "(d)"))
panel
#scale_color_viridis(discrete = TRUE, option = "D")+
# scale_shape_manual(values=c(15,16,17,18,15,16,17,18,15,16,17,18))+


#### make everyone the same
colnames(val.data)= c("Lake.ID", "lake.name","Region", "temperature","fish_PA","stocking","elevation", "max_depth","area", "richness", "fish_PA_bin")

plot_Rval = ggplot()+
  #Models
  geom_line(data=rich.model3,  aes(x=elevation, y=rich.model3.mean), size=1.5)+
  geom_ribbon(data=rich.model3, aes(x=elevation, ymin=rich.model3.mean-rich.model3.std, ymax=rich.model3.mean+rich.model3.std), alpha=0.2) +
  
  #validation
  geom_point(data=val.data, aes(x=elevation, y=richness,color=Region, fill=Region, shape=Region), color="white",size=1)+
  scale_shape_manual(values=c(21,22,23,24,21,22,23,24,21,22,23,24))+
  #scale_fill_viridis(discrete = TRUE) +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  scale_color_manual(values = c("#440154FF",  "#482677FF", "#404788FF", "#39568CFF","#33638DFF",  "#287D8EFF", "#238A8DFF",  "20A387FF","#29AF7FFF","#3CBB75FF", "#55C667FF", "#73D055FF"))+
  scale_fill_manual(values = c("#440154FF",  "#482677FF", "#404788FF", "#39568CFF","#33638DFF",  "#287D8EFF", "#238A8DFF", "20A387FF" ,"#29AF7FFF","#3CBB75FF" ,"#55C667FF", "#73D055FF"))+
  geom_smooth(data=val.data, aes(x=elevation, y=richness, color = Region, fill = Region), method = "lm", se=F, size=0.5) + 
  
  #Aesthetics
  labs(x="Elevation (m)")+
  #labs(title="Temp., Temp. + Area; Synoptic Data") +
  scale_y_continuous(limits=c(0,15),breaks=seq(0,15,2)) +
  scale_x_continuous(limits=c(00,4000),breaks=seq(000,4000,500))+

  #theme(legend.position = c(0.77,0.73))+
  #theme(legend.title = element_text(color="black", size=10, face="bold"))+
  theme(legend.key.size = unit(0.25, 'cm')) +
  
 # geom_text(data=panelLetter.normal,
    #        aes(x=xpos,
   #             y=ypos,
     #           hjust=hjustvar,
      #          vjust=vjustvar,
       #         label="I: Temp., Temp. + Area; Synoptic Data", #this is the only thing you have to change
        #        fontface="bold"))+
  
  annotate("text", x=1600, y =14, label= stringr::str_wrap("I: Temp., Temp. + Area; Synoptic Data", width = 25), fontface="bold") +  
  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_text(angle=45, hjust=1))


dev.off()
plot(plot_Rval)


grid.arrange(plot_null,plot_model2,plot_Rval, ncol=3,heights=c(4.5,3))


#MergeFigure
library(patchwork)
library(tidyverse)

combined<-(plot_Exnull+plot_Ex_M1+plot_Tval) / #first row
  (plot_DOCnull+plot_DOC_M1+plot_Dval) / # second row, duplicating just for demonstration of 3x3
  (plot_null+plot_model2+plot_Rval) # third row
combined


ggsave("output/AllModelsUpdate100722g.png", combined, width=7, height=7,units="in", dpi=600)



