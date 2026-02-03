############################################################
## 05_figures.R
## All figures: DOC, temperature, zooplankton, synthesis
## Authors: Adrianne Smits, Janice Brahney, Angela Strecker, MJ Farruggia
## Last Updated: Feb 2026 by MJ Farruggia
############################################################

source("scripts/00_setup.R", local = FALSE)
source("scripts/01_elevation_climate.R", local = FALSE)
source("scripts/02_DOC_model.R", local = FALSE)
source("scripts/03_lake_temperature_model.R", local = FALSE, echo=T)
source("scripts/04_zooplankton_model.R", local = FALSE)


## Original figures---------------------------------------------------------------------------------------------------------
combined<-(plot_Exnull+plot_Ex_M1+plot_Tval) / #first row
  (plot_DOCnull+plot_DOC_M1+plot_Dval) / # second row, duplicating just for demonstration of 3x3
  (plot_null+plot_model2+plot_Rval) # third row
combined

# ggsave("AllModelsUpdate100722g.png", combined, width=7, height=7,units="in", dpi=600)

## Model data only----------------------------------------------------------------------------------------------------------

###temp-----
    #null model still works: plot_Exnull
    #lake area model still works: plot_Ex_M1
    #need to edit Lake Area + Topo so it doesn't include synoptic data
plot_Tval2 <- ggplot()+
  geom_line(data=results_lkarea_topo,  aes(x=Elevation, y=lkareatopo_mean), size=1.5)+
  geom_ribbon(data=results_lkarea_topo, aes(x=Elevation, ymin=lkareatopo_min, ymax=lkareatopo_max), alpha=0.2) +
  #synoptic data - remove  
    # geom_point(data=real.means, aes(x=Elevation, y=Max_T_mean,color=Region, shape=Region, fill =Region), color="white",size=1)+
      # scale_shape_manual(values=c(21,22,23,24,21))+
      # scale_color_manual(values = c("#440154FF",  "#453781FF", "#33638DFF", "#238A8DFF",  "#29AF7FFF"))+
      # scale_fill_manual(values = c("#440154FF",  "#453781FF", "#33638DFF", "#238A8DFF",  "#29AF7FFF"))+
      # geom_smooth(data=real.means,aes(x=Elevation, y=Max_T_mean, color = Region),formula = 'y ~ x', method = "lm", se=F, size=0.75) + 
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(5,30),breaks=seq(5,30,5)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  theme(legend.key.size = unit(0.25, 'cm'))+
  annotate("text", x=1450, y =28.5, label= stringr::str_wrap("C: Lake Area + Topo", width = 25), fontface="bold")+  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0.5,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())
plot_Tval2


###DOC-----
  #null model still works: plot_DOc_null
  #Veg cover model still works: plot_DOC_M1
  #veg cover + type needs editing so doesn't include synoptic data
plot_Dval2 <- ggplot()+
  #Models
  geom_line(data=DOCRunMod2, aes(x=elevation,y=DOCRunmean), size=1.5)+
  geom_ribbon(data=DOCRunMod2, aes(x=elevation, ymin=DOCRun2Min, ymax=DOCRun2Max), alpha=0.2) +
  
  #synoptic data - remove
    # geom_point(data=ValData, aes(x=Elevation, y=DOC,color=Region, shape=Region, fill =Region), color="white",size=1)+
    # scale_shape_manual(values=c(21,22,23,24,21,22,23,24,21,22,23,24))+
    # #scale_fill_viridis(discrete = TRUE) +
    # #scale_color_viridis(discrete = TRUE, option = "D")+
    # scale_color_manual(values = c("#440154FF",  "#482677FF", "#404788FF", "#39568CFF","#2D708EFF",  "#238A8DFF",  "#1F968Bff","#29AF7FFF","#55C667FF"))+
    # scale_fill_manual(values = c("#440154FF",  "#482677FF", "#404788FF", "#39568CFF","#2D708EFF",  "#238A8DFF",  "#1F968Bff","#29AF7FFF","#55C667FF"))+
    # geom_smooth(data=ValData, aes(x=Elevation, y=DOC, color = Region, fill = Region), method = "lm", se=F, size=0.75) + 
    #
  #Aesthetics
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  theme(legend.key.size = unit(0.25, 'cm'))+
  annotate("text", x=1450, y =47, label= stringr::str_wrap("F: Veg. Cover + Type", width = 25), fontface="bold")+  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())
plot_Dval2

#zooplankton -----
  #null model still works: plot_null
  #temp model still works: plot_model2
  # temp, temp+area model needs editing so doesn't include synoptic data

plot_Rval2 <- ggplot()+
  #Models
  geom_line(data=rich.model3,  aes(x=elevation, y=rich.model3.mean), size=1.5)+
  geom_ribbon(data=rich.model3, aes(x=elevation, ymin=rich.model3.mean-rich.model3.std, ymax=rich.model3.mean+rich.model3.std), alpha=0.2) +
  
  #synoptic data -remove
  # geom_point(data=val.data, aes(x=elevation, y=richness,color=Region, fill=Region, shape=Region), color="white",size=1)+
  # scale_shape_manual(values=c(21,22,23,24,21,22,23,24,21,22,23,24))+
  # #scale_fill_viridis(discrete = TRUE) +
  # #scale_color_viridis(discrete = TRUE, option = "D")+
  # scale_color_manual(values = c("#440154FF",  "#482677FF", "#404788FF", "#39568CFF","#33638DFF",  "#287D8EFF", "#238A8DFF",  "20A387FF","#29AF7FFF","#3CBB75FF", "#55C667FF", "#73D055FF"))+
  # scale_fill_manual(values = c("#440154FF",  "#482677FF", "#404788FF", "#39568CFF","#33638DFF",  "#287D8EFF", "#238A8DFF", "20A387FF" ,"#29AF7FFF","#3CBB75FF" ,"#55C667FF", "#73D055FF"))+
  # geom_smooth(data=val.data, aes(x=elevation, y=richness, color = Region, fill = Region), method = "lm", se=F, size=0.5) + 
  # 
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
  
  annotate("text", x=1600, y =14, label= stringr::str_wrap("I: Temp., Temp. + Area", width = 25), fontface="bold") +  
  
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_text(angle=45, hjust=1))

plot_Rval2


combined_modelsonly<-(plot_Exnull+plot_Ex_M1+plot_Tval2) / #first row
  (plot_DOCnull+plot_DOC_M1+plot_Dval2) / # second row, duplicating just for demonstration of 3x3
  (plot_null+plot_model2+plot_Rval2) # third row
combined_modelsonly

#save
ggsave("output/models_only_gridplot_02032026.png",plot=combined_modelsonly,width=9, height=8, units='in')



## Empirical data only -----------------------------------------------------------------------------------------------------


###temp-----

plot_Tval_synoptic <- ggplot()+

  #synoptic data 
  geom_point(data=real.means, aes(x=Elevation, y=Max_T_mean,color=Region, shape=Region, fill =Region), color="white",size=1)+
  scale_shape_manual(values=c(21,22,23,24,21))+
  scale_color_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"))+
  scale_fill_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"))+
  # guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(shape = 21, size = 3, fill = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"))),
  #        fill = "none", shape = "none") +
  geom_smooth(data=real.means,aes(x=Elevation, y=Max_T_mean, color = Region),formula = 'y ~ x', method = "lm", se=F, size=0.75) +
  #models- remove or make lighter and in background
    geom_line(data=results_lkarea_topo,  aes(x=Elevation, y=lkareatopo_mean), size=1, alpha=0.3)+
    geom_ribbon(data=results_lkarea_topo, aes(x=Elevation, ymin=lkareatopo_min, ymax=lkareatopo_max), alpha=0.1) +
  #aesthetics 
  labs(x="Elevation (m)", y="Temperature (°C)")+
  scale_y_continuous(limits=c(5,30),breaks=seq(5,30,5)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  theme(legend.key.size = unit(0.25, 'cm'))+
  # annotate("text", x=1450, y =28.5, label= stringr::str_wrap("C: Lake Area + Topo", width = 25), fontface="bold")+  
  theme(legend.position = "right",
        guide_legend(nrow=3, byrow=TRUE)  ,     
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),
        # axis.title.y=element_blank(),
        plot.margin=unit(c(0.5,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"))
plot_Tval_synoptic


###DOC-----

plot_Dval_synoptic <- ggplot()+

  # 
  #synoptic data - plot
  geom_point(data=ValData, aes(x=Elevation, y=DOC,color=Region, shape=Region, fill =Region), color="white",size=1)+
  scale_shape_manual(values=c(21,22,23,24,21,22,23,24,21,22,23,24))+
  #scale_fill_viridis(discrete = TRUE) +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  scale_color_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF","#8491B4FF", "#91D1C2FF", "#DC0000FF", "#7E6148FF"))+
  scale_fill_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF","#8491B4FF", "#91D1C2FF", "#DC0000FF", "#7E6148FF"))+
  geom_smooth(data=ValData, aes(x=Elevation, y=DOC, color = Region, fill = Region), method = "lm", se=F, size=0.75) +
  #
  #Models - remove or make lighter and in background
  geom_line(data=DOCRunMod2, aes(x=elevation,y=DOCRunmean), size=1, alpha=0.3)+
  geom_ribbon(data=DOCRunMod2, aes(x=elevation, ymin=DOCRun2Min, ymax=DOCRun2Max), alpha=0.1) +
  #Aesthetics
  labs(x="Elevation (m)", y="DOC (mg/L)")+
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  theme(legend.key.size = unit(0.25, 'cm'))+
  # guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(shape = 21, size = 3, fill = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF","#8491B4FF", "#91D1C2FF", "#DC0000FF", "#7E6148FF"))),
  #        fill = "none", shape = "none") +
  
    theme(legend.position = "right",
        guide_legend(nrow=3, byrow=TRUE)  ,      
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),
        # axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"))
      
plot_Dval_synoptic

#zooplankton -----
 
#use new df from A. Strecker - testing_lakes.csv 
#update zoop data regional groupings based on "super_region" column in testing_lakes.csv (Feb 2026 update; groupings provided by A. Strecker)

val.data.new <- read.csv("data/testing_lakes.csv", header=T)

plot_Rval_synoptic <- ggplot()+

  #synoptic data -plot
  geom_point(data=val.data.new, aes(x=ELEV_MASL, y=richness,color=super_region, fill=super_region, shape=super_region), color="white",size=1)+
   scale_shape_manual(values=c(21,22,23,24,21,22,23,24,21,22,23,24))+
  #scale_fill_viridis(discrete = TRUE) +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  scale_color_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"),  guide = guide_legend(nrow = 3, byrow = TRUE))+
  scale_fill_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"),  guide = guide_legend(nrow = 3, byrow = TRUE))+
  geom_smooth(data=val.data.new, aes(x=ELEV_MASL, y=richness, color = super_region, fill = super_region), method = "lm", se=F, size=0.75) +
  # 
  #Models - remove or make lighter and in background
  geom_line(data=rich.model3,  aes(x=elevation, y=rich.model3.mean), size=1, alpha=0.3)+
  geom_ribbon(data=rich.model3, aes(x=elevation, ymin=rich.model3.mean-rich.model3.std, ymax=rich.model3.mean+rich.model3.std), alpha=0.1) +
  
  #Aesthetics
  labs(x="Elevation (m)", y="Zooplankton Richness", color="Region", fill="Region", shape="Region")+
  #labs(title="Temp., Temp. + Area; Synoptic Data") +
  scale_y_continuous(limits=c(0,15),breaks=seq(0,15,2)) +
  scale_x_continuous(limits=c(00,4000),breaks=seq(000,4000,500))+
    # guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(shape = 21, size = 3, fill = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"))),
    # fill = "none", shape = "none") +
  theme(legend.key.size = unit(0.25, 'cm'),
        legend.position = "right",
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),
        # axis.title.y=element_blank(),
        plot.margin=unit(c(0,0,0.5,0), "lines"), #TOP, RIGHT, BOTTOM, LEFT
        axis.ticks.length.y = unit(0, "pt"),
        axis.text.x=element_text(angle=45, hjust=1))
plot_Rval_synoptic



#1 row
combined_synopticonly<-(plot_Tval_synoptic + plot_Dval_synoptic + plot_Rval_synoptic) 
combined_synopticonly
#save
ggsave("output/synoptic_with_background_models_horizontal_02032026.png",plot=combined_synopticonly,width=15, height=6, units='in')


#3 rows
combined_synopticonly<-
  (plot_Tval_synoptic)/
  (plot_Dval_synoptic)/
  (plot_Rval_synoptic) 
combined_synopticonly
#save
ggsave("output/synoptic_with_background_models_vertical_02032026.png",plot=combined_synopticonly,width=8, height=12, units='in')

## fig S2: water temp CoV x elevation, colored by mtn range-----------------------------------------------------------------



## fig S3: DOC CoV x elevation, colored by mtn range------------------------------------------------------------------------