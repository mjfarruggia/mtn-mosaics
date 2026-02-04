############################################################
## 04_zooplankton_model.R
## Zooplankton richness along elevation gradients
## Authors: Angela Strecker, Janice Brahney
## Last Updated: Feb 2026 by MJ Farruggia
############################################################
source("scripts/00_setup.R")
source("scripts/01_elevation_climate.R")


full.data<-read.csv("data/original/zoop_env_lake_surfaceonly_1.20.22.csv",header=T)
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
#### null model (Model1)---------------------------------------------------------
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
  
  annotate("text", x=350, y =14, label= "G: Null", fontface="bold")+  
  
  #xlim(10, 31)+ #expanding the limit here slightly so that the axis text doesn't get cut off
  theme(plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_text(angle=45, hjust=1))

plot(plot_null)


##### model with temp (Model2)---------------------------------------------------------

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
  annotate("text", x=1050, y =14, label= "H: Temperature", fontface="bold")+  
  
  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_text(angle=45, hjust=1))

plot(plot_model2)

#95% confidence interval---------------------------------------------------------
plot_model2_CI = ggplot(rich.model2, aes(x=elevation, y=rich.model2.mean)) +
geom_line(aes(y=rich.model2.mean))+theme_bw()+labs(x="Elevation (m)")+labs(y="Zooplankton richness") +
  geom_ribbon(data=rich.model2, aes(x=elevation, ymin=rich.model2.mean-rich.model2.std*1.96, ymax=rich.model2.mean+rich.model2.std*1.96), alpha=0.2) +
  theme(axis.title.y=element_blank(), axis.text.y=element_text(size=12),
        axis.text.x = element_text(size=12),axis.title.x = element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"))+
  scale_y_continuous(limits=c(2.5,8.5),breaks=seq(3,8,1))

plot_model2_CI
#(plot_model2)

grid.arrange(plot_null,plot_model2, ncol=2)


##### model with temp + fish (Model3)---------------------------------------------------------
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

#95% confidence interval---------------------------------------------------------
plot_model3 = ggplot(rich.model3, aes(x=elevation, y=rich.model3.mean)) +
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

write.csv(rich.model2,"data/processed/rich.model2.11.17.21.csv")
write.csv(rich.model3,"data/processed/rich.model3.11.17.21.csv")
write.csv(rich.null,"data/processed/rich.null.11.17.21.csv")

# Add validation data to plot (a)---------------------------------------------------------

val.data$region<-as.factor(val.data$region)
levels(val.data$region)
val.data$region<-factor(val.data$region, levels=c("YUK NORTH", "YUK SOUTH","BC NORTH", "JASP", "BANF", "YOHO", "REVE", "KOOT", "WATE", "UMQ NF", "RR-SIS NF", "LASSEN"),labels=c("N. Yukon", "S. Yukon","N. BC", "Jasper", "Banff", "Yoho", "Revelstoke", "Kootenay", "Waterton", "Umpqua", "Rogue River-Siskiyou", "Lassen"))

pdf(val.data, file="validation_12.17.21.pdf", width=8, height=6)

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


# dev.off()
plot(plot_Rval)


grid.arrange(plot_null,plot_model2,plot_Rval, ncol=3,heights=c(4.5,3))






