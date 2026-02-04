############################################################
## 03_lake_temperature_model.R
## Lake epilimnion temperature model
## Authors: Adrianne Smits, Janice Brahney
## Last Updated: Feb 2026 by MJ Farruggia
############################################################
source("scripts/00_setup.R")
source("scripts/01_elevation_climate.R")
# dev.off()

# Load air temperature time series (emerald lake weather station 2016)-------------------------------------------
load('data/original/02_AirTdaily.Rdata')
# plot(AirTdaily$doy, AirTdaily$AirTemp_C_Avg)



# Load linear regression model btwn avg. watershed slope and avg. solar exposure ratio (ratio between obstructed: unobstructed lake radiation inputs) -----
###Note: no longer using!!!

# load('02_Slope_Solar_lm.Rdata')
# int <- coef(meanslope.lm)[1]
# slope <-coef(meanslope.lm)[2]



# Input parameters ---------------------------------------------------------------------------------------------
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

#Static lake and watershed properties-------------------------------------------------------------------------------------
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


#Loop over a bunch of elevations -----------------------------------------------------------------------------------
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



##Save results-------------------------------------------------------------------------------------
results_lkarea_topo <- results

save(results_lkarea_topo,file='data/processed/04_mglm_output_lkarea_topo.Rdata')

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

# dev.off()



# Plot lake temp models -----------------------------------------------------------------------------

##Load output from the elevation gradient and mosaic models for temperature:

#null model (elevation only)
load('data/original/03_mglm_output_nullmodel.Rdata')

#variable lake surface area model (elevation + SA): Mosaic 1
load('data/original/03_mglm_output_lkarea.Rdata')

#variable SA and topography (elevation + SA + topography): Mosaic 2
load('data/original/03_mglm_output_lkarea_topo.Rdata')

##Load data from real lakes for validation

real.data <- read.csv('data/original/01_Temp_model_validation_data.csv',header=TRUE)
head(real.data)
#exclude lake from the alps
real.data <- real.data[-which(real.data$Mt_range=='Alps'),]
real.means <- data.frame(summarise(group_by(real.data,Mt_range,Lake_name, Elevation),
                                   
                                   Max_T_mean =mean(Max_epi_T, na.rm=TRUE),
                                   Max_T_sd=sd(Max_epi_T, na.rm=TRUE)))

## Calculate stats for max epi temp -------------------------------------------------------
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


## Plot null model----------------------------------------------------
plot_Exnull <- ggplot(results_null, aes(x=elevation, y=LakeT)) +
  geom_line(aes(y=LakeT))+labs(x="Elevation (m)")+labs(y=expression(Epilimnion~Temperature~(degree~C))) +
  #aesthetics
  labs(y="Temperature (°C)")+
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
  
  annotate("text", x=350, y =28.5, label= "A: Null", fontface="bold")+  
  
  #xlim(10, 31)+ #expanding the limit here slightly so that the axis text doesn't get cut off
  
  theme(plot.margin=unit(c(0.5,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(3, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())
plot_Exnull


## Plot Mosaic 1: Lake Area --------------------------------------------------------------
plot_Ex_M1 <- ggplot(results_lkarea, aes(x=Elevation, y=lkarea_mean)) +
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
  
  annotate("text", x=800, y =28.5, label= "B: Lake Area", fontface="bold")+  
  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0.5,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())
plot_Ex_M1

#grid.arrange(plot_Exnull,plot_Ex_M1, ncol=2)

## Plot Mosaic 2: Lake Area + Topography------------------------------------------------------
plot_Ex_M2 <- ggplot(results_lkarea_topo, aes(x=Elevation, y=lkareatopo_mean)) +
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

plot_Ex_M2

#All three models together in three panels
# dev.off()
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



Plot_Ex_realdata

## Plot all together -------------------------------------------------
#plot similar to richness
#Estimate = as.data.frame(cbind(results_null[,1],results_null[,4]))
#colnames(Estimate)= c("elevation","LakeT")

colnames(real.means)= c( "Region", "Lake_name","Elevation", "Max_T_mean", "Max_T_sd")


plot_Tval <- ggplot()+
  
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


plot_Tval


panels = grid.arrange(plot_Exnull,plot_Ex_M1, plot_Tval,ncol=3, heights=c(4.5,3))


#All three models plus real data together in four panels
#panels = grid.arrange(plot_Exnull,plot_Ex_M1, plot_Ex_M2,Plot_Ex_realdata,ncol=4,
# left='Epilimnion Temperature (Celsius)')

#save figure
ggsave("output/03_mlgm_lake_temp_4panels_ggplot.png",plot=panels,width=9, height=3.5, units='in')
