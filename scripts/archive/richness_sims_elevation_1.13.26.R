# setwd("G:/My Drive/collaborations/mountain lakes/working group/zooplankton diversity/model and database")

data<-read.csv("data/zoop_env_lake_surfaceonly_1.20.22.csv",header=T)
library(ggplot2)
library(gridExtra)
library(egg)
library(cowplot)

#derive equations
model5<-lm(richness~elevation,data=data); summary(model5)
model6<-lm(richness~elevation+temperature,data=data);summary(model6)
model7<-lm(richness~elevation+temperature+log(area),data=data);summary(model7)

####simulations
## null model 
elev <-seq(400,2500,100)
pred.rich<-predict(model5,list(elevation=elev))
rich.null<-as.data.frame(cbind(elev, pred.rich))

plot_null= ggplot(rich.null, aes(x=elev, y=pred.rich)) +
  geom_line(aes(y=pred.rich))+theme_bw()+labs(y="Zooplankton richness") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),title = element_text(size=14,face="bold"))+
  theme(axis.title.x=element_blank())+
  scale_y_continuous(limits=c(2.5,8.5),breaks=seq(3,8,1))

plot(plot_null)

## model with temp (Model1)
hist(data$temperature)  #yes, normal

rich.model1<- matrix(nrow=22, ncol=100) #test w n=100 for now
for (i in 1:22)
  for(j in 1:100)
  {
    temp<-rnorm(100,4.5,26.2) #absolute max and min from full data set)
    rich.model1[i,j]=(-0.0012392*elev[i])+(0.138707*temp[j])+4.8218508 # manual entry from model6
  }

rich.model1.mean = as.matrix(rowMeans(rich.model1))
rich.model1.std = as.matrix(apply(rich.model1,1,sd, na.rm=T))
rich.model1 = as.data.frame(cbind(elev,rich.model1.mean,rich.model1.std))
colnames(rich.model1)= c("elevation", "rich.model1.mean","rich.model1.std")

plot_model1 = ggplot(rich.model1, aes(x=elevation, y=rich.model1.mean)) +
  geom_line(aes(y=rich.model1.mean))+theme_bw()+labs(x="elevation (m)")+labs(y="Zooplankton richness") +
  geom_ribbon(data=rich.model1, aes(x=elevation, ymin=rich.model1.mean-rich.model1.std*2.576, ymax=rich.model1.mean+rich.model1.std*2.576), alpha=0.2) + #99% confidence interval
  theme(axis.title.y=element_blank(), axis.text.y=element_text(size=12),
        axis.text.x = element_text(size=12),axis.title.x = element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"))+
  scale_y_continuous(limits=c(-10,16),breaks=seq(-10,16,2))

plot(plot_model1)

grid.arrange(plot_null,plot_model1, ncol=2)


## model with temp + surface area (Model2)
rich.model2<- array(dim=c(22,25,25))

for (i in 1:22) 
  for(j in 1:25) 
    for(k in 1:25)
    {
      temp<-rnorm(25,4.5,26.2) #absolute max and min from full data set)
      area<-runif(25,min=0.2,max=1393.5) #5th and 95th percentiles, normal dist was skewed (from full data set)
      area=log(area) #area logged to be consistent with GLM
      rich.model2[i,j,k]=(-0.0009319*elev[i])+(0.155*temp[j])+(0.105*area[k])+3.943 # manual entry from model7
    }

rich.model2.mean = as.matrix(rowMeans(rich.model2))
rich.model2.std = as.matrix(apply(rich.model2,1,sd, na.rm=T))
rich.model2 = as.data.frame(cbind(elev,rich.model2.mean,rich.model2.std))
colnames(rich.model2)= c("elevation", "rich.model2.mean","rich.model2.std")

plot_model2 = ggplot(rich.model2, aes(x=elevation, y=rich.model2.mean)) +
  geom_line(aes(y=rich.model2.mean))+theme_bw()+
  geom_ribbon(data=rich.model2, aes(x=elevation, ymin=rich.model2.mean-rich.model2.std*2.576, ymax=rich.model2.mean+rich.model2.std*2.576), alpha=0.2) + #99% confidence interval
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.y=element_text(size=12),
        title = element_text(size=14,face="bold"))+
  scale_y_continuous(limits=c(-10,16),breaks=seq(-10,16,2))

plot(plot_model2)


ggarrange(plot_null,plot_model1,plot_model2,ncol=3) #equal width panels from package 'egg'

panel<-plot_grid(plot_null,plot_model1,plot_model2,label_size=12,hjust=-3,vjust=2,align="hv",nrow=1,labels=c("(a)","(b)","(c)"))
panel


#### repeat figs w 95% CI
plot_model1 = ggplot(rich.model1, aes(x=elevation, y=rich.model1.mean)) +
  geom_line(aes(y=rich.model1.mean))+theme_bw()+labs(x="elevation (m)")+labs(y="Zooplankton richness") +
  geom_ribbon(data=rich.model1, aes(x=elevation, ymin=rich.model1.mean-rich.model1.std*1.96, ymax=rich.model1.mean+rich.model1.std*1.96), alpha=0.2) + #95% confidence interval
  theme(axis.title.y=element_blank(), axis.text.y=element_text(size=12),
        axis.text.x = element_text(size=12),axis.title.x = element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"))+
  scale_y_continuous(limits=c(-10,16),breaks=seq(-10,16,2))

plot(plot_model1)


plot_model2 = ggplot(rich.model2, aes(x=elevation, y=rich.model2.mean)) +
  geom_line(aes(y=rich.model2.mean))+theme_bw()+
  geom_ribbon(data=rich.model2, aes(x=elevation, ymin=rich.model2.mean-rich.model2.std*1.96, ymax=rich.model2.mean+rich.model2.std*1.96), alpha=0.2) + #95% confidence interval
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.y=element_text(size=12),
        title = element_text(size=14,face="bold"))+
  scale_y_continuous(limits=c(-10,16),breaks=seq(-10,16,2))

plot(plot_model2)


ggarrange(plot_null,plot_model1,plot_model2,ncol=3) #equal width panels from package 'egg'

panel<-plot_grid(plot_null,plot_model1,plot_model2,label_size=12,hjust=-3,vjust=2,align="hv",nrow=1,labels=c("(a)","(b)","(c)"))
panel
