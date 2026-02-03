############################################################
# 02_DOC_model.R
# DOC export model along elevation gradients
# Authors: Janice Brahney
# Last Updated: Feb 2026 by MJ Farruggia
############################################################

source("scripts/00_setup.R")
source("scripts/01_elevation_climate.R")


# Null model ----------------------------------------------------

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


## Null model plots ---------------------------------------------------
# regular biplots
par(mfrow = c(2, 3))
plot(elevation, DOCRunoff_Null)


#Plot Null Model

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
  
  annotate("text", x=350, y =47, label= "D: Null", fontface="bold")+  #hard to get this to align, seems to readjust with the 'combined' function, 350 is from the left of the whole plot when all plotted together.
  
  #xlim(10, 31)+ #expanding the limit here slightly so that the axis text doesn't get cut off
  theme(plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(3, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())


plot(plot_DOCnull)

# Model 1 - Vegetation Changes ---------------------------------------------------

#Soil Carbon storage and potential export as a function of temperature and precipitation 
#CarbonIn is set to the average of forest and grassland (13.38 Kg/m2) but the proportion of vegetation cover
#is allowed to vary according to that observed in the EPA NLA database. Vegetation cover cannot be greater than 1 or less than 0

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
Carbon1Array <- matrix(nrow=40, ncol=100)
SoilC1Array <- matrix(nrow=40, ncol=100)
DOCRun1Array <- matrix(nrow=40, ncol=100)

Carbon1Array <- VegProp*CarbonIn
SoilC1Array <- t(Carbon1Array)*TurnT #Soil carbon stocks in kg/m2
DOCRun1Array <- SoilC1Array*DOCProd/Runoff/1000 #runoff in mg/L
DOCRun1Min <- apply(DOCRun1Array, 1, FUN = min)
DOCRun1Max <- apply(DOCRun1Array, 1, FUN = max)
DOCRun1Mean <- as.matrix(rowMeans(DOCRun1Array))
DOCRun1Std <- as.matrix(apply(DOCRun1Array,1,sd, na.rm=T))
DOCRunMod1 <- as.data.frame(cbind(elevation,DOCRun1Mean,DOCRun1Std))
colnames(DOCRunMod1) <-  c("elevation", "DOCRunmean","DOCRunstd")

write.csv(DOCRun1Array, file="data/DOCRun.csv")
write.csv(VegProp, file="data/VegProp.csv")

## Model 1 plots  ---------------------------------------------------------------
plot_DOC_M1 <- ggplot(DOCRunMod1, aes(x=elevation, y=DOCRunmean)) +
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
  annotate("text", x=1350, y =47, label= "E: Vegetation Cover", fontface="bold")+  
  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())

plot_DOC_M1

grid.arrange(plot_DOCnull,plot_DOC_M1, ncol=2)


# Model 2 - Veg type changes ---------------------------------------
#Soil Carbon storage and potential export as a function of temperature and precipitation 
#The type of vegetation and therefore carbon is allowed to vary with elevation based on the
#EPA NLS catchment data.


##create distributions based of mean and std that sum to 1

VP <- matrix(nrow=40,ncol=5)
CarbonArray2 <- matrix(nrow=40, ncol=100)

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

DOCRun2Array <- CarbonArray2*TurnT*DOCProd/Runoff/1000
DOCRun2Mean <- as.matrix(rowMeans(DOCRun2Array))
DOCRun2Min <- apply(DOCRun2Array, 1, FUN = min)
DOCRun2Max <- apply(DOCRun2Array, 1, FUN = max)
DOCRun2Std <- as.matrix(apply(DOCRun2Array,1,sd, na.rm=T))
DOCRunMod2 <- as.data.frame(cbind(elevation,DOCRun2Mean,DOCRun2Std))
colnames(DOCRunMod2) <-  c("elevation", "DOCRunmean","DOCRunstd")

## Model 2 plots -----------------------------------------------------------------------------

## 2 for 95% and 3 for 99.7%, but talked to Susan Durham and agreed not a true confidence interfal. but can use the precentile.
plot_DOC_M2 <- ggplot(DOCRunMod2, aes(x=elevation, y=DOCRunmean)) +
  geom_line(aes(y=DOCRunmean))+
  geom_ribbon(data=DOCRunMod2, aes(x=elevation, ymin=DOCRun2Min, ymax=DOCRun2Max), alpha=0.2) +
  
  #Aesthetics
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  labs(title="Vegetation Type + Cover")


plot_DOC_M2

grid.arrange(plot_DOCnull,plot_DOC_M1, plot_DOC_M2,ncol=3, heights=c(2,2))


# Validation plot -----------------------------------------------------------------

# try adding validation data to plot (a)

ValData<-read.csv('data/ValData.csv', header=TRUE) #validation data from mnt ranges
ValData<-as.data.frame(ValData)
colnames(ValData)= c("Region", "Elevation","Eofbase","DOC")

### Add validation data


plot_Dval <- ggplot()+
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


plot_Dval

grid.arrange(plot_DOCnull,plot_DOC_M1, plot_Dval, ncol=3, heights=c(4.5,3))

#grid.arrange(plot_DOCnull,plot_DOC_M1, plot_DOC_M2, plot_Dval, ncol=4)

