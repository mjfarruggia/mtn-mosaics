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
    #null model:
plot_Exnull <- ggplot(results_null, aes(x=elevation, y=LakeT)) +
  geom_line(linewidth=2,aes(y=LakeT))+labs(x="Elevation (m)")+labs(y=expression(Epilimnion~Temperature~(degree~C))) +
  #aesthetics
  labs(y="Temperature (°C)")+
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(5,30),breaks=seq(5,30,5)) +
  scale_x_continuous(limits=c(000,4000),breaks=seq(000,4000,500))+
  labs(title="Null Gradient Model") +
  annotate("text", x=100, y =28.5, label= "A", fontface="bold")+  
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot_Exnull



    #lake area model:
plot_Ex_M1 <- ggplot(results_lkarea, aes(x=Elevation, y=lkarea_mean)) +
  geom_line(linewidth=2,aes(y=lkarea_mean))+
  geom_ribbon(data=results_lkarea, aes(x=Elevation, ymin=lkarea_min, ymax=lkarea_max), alpha=0.2) +
  
  #Aesthetics
  #labs(title="Lake Area") +
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(5,30),breaks=seq(5,30,5)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  labs(title="1 Mosaic Feature") +
  
  annotate("text", x=100, y =28.5, label= "B", fontface="bold")+  
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_Ex_M1
    

#need to edit Lake Area + Topo so it doesn't include synoptic data
plot_Tval2 <- ggplot()+
  geom_line(linewidth=2,data=results_lkarea_topo,  aes(x=Elevation, y=lkareatopo_mean), size=1.5)+
  geom_ribbon(data=results_lkarea_topo, aes(x=Elevation, ymin=lkareatopo_min, ymax=lkareatopo_max), alpha=0.2) +
  labs(title="2 Mosaic Features") +
  
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
  annotate("text", x=100, y =28.5, label= stringr::str_wrap("C", width = 25), fontface="bold")+  
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot_Tval2


###DOC-----
  ####null model ----
plot_DOCnull= ggplot(DOCExpNull, aes(x=elevation, y=DOC)) +
  geom_line(linewidth=2,aes(y=DOC))+
  
  labs(y="DOC (mg/L)")+
  # labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500)) +
  annotate("text", x=100, y =47, label= "D", fontface="bold")+  #hard to get this to align, seems to readjust with the 'combined' function, 350 is from the left of the whole plot when all plotted together.
  
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(plot_DOCnull)


  ####Veg cover model----
plot_DOC_M1 <- ggplot(DOCRunMod1, aes(x=elevation, y=DOCRunmean)) +
  geom_line(linewidth=2, aes(y=DOCRunmean))+
  geom_ribbon(data=DOCRunMod1, aes(x=elevation, ymin=DOCRun1Min, ymax=DOCRun1Max), alpha=0.2) +
  
  labs(y="DOC (mg/L)")+
  #labs(title="Vegetation Cover") +
  # labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+

  annotate("text", x=100, y =47, label= "E", fontface="bold")+  
  
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_blank(),       
        axis.title.x=element_blank(),

        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_DOC_M1


  ####Veg cover + type ---- 
      # needs editing so doesn't include synoptic data
plot_Dval2 <- ggplot()+
  #Models
  geom_line(linewidth=2, data=DOCRunMod2, aes(x=elevation,y=DOCRunmean), size=1.5)+
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
  annotate("text", x=100, y =47, label= stringr::str_wrap("F", width = 25), fontface="bold")+  
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot_Dval2

##zooplankton -----
  ####null model -----
plot_null= ggplot(rich.null, aes(x=elev, y=pred.rich)) +
  geom_line(linewidth=2,aes(y=pred.rich))+labs(y="Zooplankton richness") +
  
  labs(y="Zooplankton Richness")+
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(0,8),breaks=seq(0,8,2)) +
  scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  annotate("text", x=100, y =7, label= "G", fontface="bold")+  
  theme_bw(base_size = 16) +
  theme(
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
  
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot(plot_null)

  ####temp model ----
plot_model2 = ggplot(rich.model2, aes(x=elevation, y=rich.model2.mean)) +
  geom_line(linewidth=2, aes(y=rich.model2.mean))+
  geom_ribbon(data=rich.model2, aes(x=elevation, ymin=rich.model2.mean-rich.model2.std, ymax=rich.model2.mean+rich.model2.std), alpha=0.2) +
  #Aesthetics
  labs(x="Elevation (m)")+
  scale_y_continuous(limits=c(0,8),breaks=seq(0,8,2)) +
  scale_x_continuous(limits=c(00,4000),breaks=seq(000,4000,500))+
    annotate("text", x=100, y =7, label= "H", fontface="bold")+  
  theme_bw(base_size = 16) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
  
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(plot_model2)

  #### temp, temp+area model ----
#needs editing so doesn't include synoptic data
plot_Rval2 <- ggplot()+
  #Models
  geom_line(linewidth=2, data=rich.model3,  aes(x=elevation, y=rich.model3.mean))+
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
  scale_y_continuous(limits=c(0,8),breaks=seq(0,8,2)) +
  scale_x_continuous(limits=c(00,4000),breaks=seq(000,4000,500))+
    theme(legend.key.size = unit(0.25, 'cm')) +
  annotate("text", x=100, y =7, label= stringr::str_wrap("I", width = 25), fontface="bold") +  
  theme_bw(base_size = 16) +
  theme(
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
  
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot_Rval2


# Fig 2 - Combine model plots into a grid ------------
combined_modelsonly<-(plot_Exnull+plot_Ex_M1+plot_Tval2) / #first row
  (plot_DOCnull+plot_DOC_M1+plot_Dval2) / # second row, duplicating just for demonstration of 3x3
  (plot_null+plot_model2+plot_Rval2) # third row
combined_modelsonly

#save
  ggsave("output/models_only_gridplot_02192026.png",plot=combined_modelsonly,width=10, height=8, units='in')



## Empirical data only -----------------------------------------------------------------------------------------------------

mtn_colors <- c(     #this is an NCEAS scicomm colorblind friendly recommended palette https://www.nceas.ucsb.edu/sites/default/files/2022-06/Colorblind%20Safe%20Color%20Schemes.pdf

  "Yukon" = "#332288",
  "Northern Rockies" = "#117733",
  "Columbia" = "#44AA99",
  "Olympic" = "#88CCEE",
  "Cascades" = "#DDCC77",
  "Siskiyou" = "#AA4499",
  "Southern Rockies" = "#CC6677",
  "Sierra Nevada" = "#882255",
  "Alps and Pyrenees" = "#999933"
)
      #color ramp arranged roughly North --> South??

###temp-----
#reclassify mountain ranges to a common grouping (see colors above)
real.means <- real.means %>%
  mutate(Region = stringr::str_trim(Region))

real.means <- real.means %>%
  mutate(Region = case_when(
    Region == "Canadian Rockies"   ~ "Northern Rockies",
    Region == "Rocky Mountains"    ~ "Southern Rockies",
    TRUE                           ~ Region  
  ))

real.means$Region <- factor(real.means$Region, levels = c("Yukon", "Northern Rockies", "Columbia", 
                                       "Olympic", "Cascades", "Siskiyou", "Southern Rockies", "Sierra Nevada", "Alps and Pyrenees"))

plot_Tval_synoptic <- ggplot()+
  #synoptic data 
  geom_point(data=real.means, aes(x=Elevation, y=Max_T_mean,color=Region, shape=Region, fill =Region), color="white",size=2)+
  scale_shape_manual(values=c(21,21,21,21,21))+
  scale_color_manual(values = mtn_colors)+
  scale_fill_manual(values = mtn_colors)+
  geom_smooth(data=real.means,aes(x=Elevation, y=Max_T_mean, color = Region, show.legend=F),formula = 'y ~ x', method = "lm", se=F, size=2) +
  #models- remove or make lighter and in background
    geom_line(data=results_lkarea_topo,  aes(x=Elevation, y=lkareatopo_mean), size=2, alpha=0.2)+
    geom_ribbon(data=results_lkarea_topo, aes(x=Elevation, ymin=lkareatopo_min, ymax=lkareatopo_max), alpha=0.05) +
  #aesthetics 
  labs(x=" ", y="Temperature (°C)")+
  scale_y_continuous(limits=c(5,30),breaks=seq(5,30,5)) +
  # scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  scale_x_continuous(limits = c(500, 3750), breaks = seq(500, 3750, by = 250)) +
  theme(legend.key.size = unit(0.25, 'cm'))+
  # annotate("text", x=1450, y =28.5, label= stringr::str_wrap("C: Lake Area + Topo", width = 25), fontface="bold")+  
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(shape = 21, size = 3)),
         fill = "none", 
         shape = "none")
plot_Tval_synoptic


###DOC-----
#reclassify mountain ranges to a common grouping (see colors above)
ValData <- ValData %>%
  mutate(Region = stringr::str_trim(Region))

ValData <- ValData %>%
  mutate(Region = case_when(
    Region == "Canadian Rockies"  ~ "Northern Rockies",
    Region == "N. Rocky"          ~ "Northern Rockies",
    Region == "Rocky Mountains"   ~ "Southern Rockies",
    Region == "S. Rocky"          ~ "Southern Rockies",
    Region == "Uintas"            ~ "Southern Rockies",
    Region == "Wind River"        ~ "Southern Rockies",
    Region == "Yellowstone"       ~ "Southern Rockies",
    Region == "Cascades"          ~ "Cascades",
    Region == "Columbia"          ~ "Columbia",
    Region == "Sierra Nevada"     ~ "Sierra Nevada",
    Region == "Alps and Pyrenees" ~ "Alps and Pyrenees",
    TRUE                          ~ Region  
  ))

ValData$Region <- factor(ValData$Region, levels = c("Yukon", "Northern Rockies", "Columbia", 
                                                          "Olympic", "Cascades", "Siskiyou", "Southern Rockies", "Sierra Nevada", "Alps and Pyrenees"))

plot_Dval_synoptic <- ggplot()+
  # 
  #synoptic data - plot
  geom_point(data=ValData, aes(x=Elevation, y=DOC,color=Region, shape=Region, fill =Region), color="white",size=2)+
  scale_shape_manual(values=c(21,21,21,21,21,21))+
  #scale_fill_viridis(discrete = TRUE) +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  scale_color_manual(values = mtn_colors)+
  scale_fill_manual(values = mtn_colors)+
  geom_smooth(data=ValData, aes(x=Elevation, y=DOC, color = Region, fill = Region), method = "lm", se=F, size=2) +
  #
  #Models - remove or make lighter and in background
  geom_line(data=DOCRunMod2, aes(x=elevation,y=DOCRunmean), size=2, alpha=0.2)+
  geom_ribbon(data=DOCRunMod2, aes(x=elevation, ymin=DOCRun2Min, ymax=DOCRun2Max), alpha=0.05) +
  #Aesthetics
  labs(x= " ",y="DOC (mg/L)")+
  scale_y_continuous(limits=c(0,25),breaks=seq(0,25,5)) +
  # scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))+
  scale_x_continuous(limits = c(500, 3750), breaks = seq(500, 3750, by = 250)) +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  
  guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(shape = 21, size = 3)),
         fill = "none", 
         shape = "none")
      
plot_Dval_synoptic

#zooplankton -----
 
#use new df from A. Strecker - testing_lakes.csv 
#update zoop data regional groupings based on "super_region" column in testing_lakes.csv (Feb 2026 update; groupings provided by A. Strecker)

val.data.new <- read.csv("data/original/testing_lakes.csv", header=T)
val.data.new <- val.data.new %>% rename("Region" = "super_region")
val.data.new$Region <- factor(val.data.new$Region, levels = names(mtn_colors))

val.data.new$Region <- factor(val.data.new$Region, levels = c("Yukon", "Northern Rockies", "Columbia", 
                                                    "Olympic", "Cascades", "Siskiyou", "Southern Rockies", "Sierra Nevada", "Alps and Pyrenees"))


plot_Rval_synoptic <- ggplot()+
  #synoptic data -plot
  geom_point(data=val.data.new, aes(x=ELEV_MASL, y=richness,color=Region, fill=Region, shape=Region), color="white",size=2)+
   scale_shape_manual(values=c(21,21,21,21,21))+
  #scale_fill_viridis(discrete = TRUE) +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  scale_color_manual(values=mtn_colors)+
  scale_fill_manual(values=mtn_colors)+
  geom_smooth(data=val.data.new, aes(x=ELEV_MASL, y=richness, color = Region, fill = Region, show.legend=F), method = "lm", se=F, size=2) +
  # 
  #Models - remove or make lighter and in background
  geom_line(data=rich.model3,  aes(x=elevation, y=rich.model3.mean), size=2, alpha=0.2)+
  geom_ribbon(data=rich.model3, aes(x=elevation, ymin=rich.model3.mean-rich.model3.std, ymax=rich.model3.mean+rich.model3.std), alpha=0.05) +
  
  #Aesthetics
  labs(x="Elevation (m)", y="Zooplankton Richness", color="Region", fill="Region", shape="Region")+
  #labs(title="Temp., Temp. + Area; Synoptic Data") +
  scale_y_continuous(limits=c(0,15),breaks=seq(0,15,2)) +
  # scale_x_continuous(limits=c(00,4000),breaks=seq(000,4000,500))+
  scale_x_continuous(limits = c(500, 3750), breaks = seq(500, 3750, by = 250)) +
  theme_bw(base_size = 16) +
  theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(shape = 21, size = 3)),
         fill = "none", 
         shape = "none") 
plot_Rval_synoptic



#1 row (horizontal)
# combined_synopticonly<-(plot_Tval_synoptic + plot_Dval_synoptic + plot_Rval_synoptic) 
# combined_synopticonly
# #save
# ggsave("output/synoptic_with_background_models_horizontal_02032026.png",plot=combined_synopticonly,width=15, height=6, units='in')


#3 rows (vertical)
combined_synopticonly<-
  (plot_Tval_synoptic)/
  (plot_Dval_synoptic)/
  (plot_Rval_synoptic) 
combined_synopticonly
#save
# ggsave("output/synoptic_with_background_models_vertical_02042026_v2.png",plot=combined_synopticonly,width=5, height=12, units='in')


#legend with all the mtns
legend_data <- data.frame(
  Region = factor(names(mtn_colors), levels = names(mtn_colors)),
  x = 1, y = 1)

legend_only_plot <- ggplot(legend_data, aes(x = x, y = y, color = Region)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = mtn_colors) +
  theme_void(base_size = 15) + 
  guides(color = guide_legend(
    nrow = 2, byrow = T, 
    title.position = "top", title.hjust = 0.5,
    override.aes = list(alpha = 1, linetype = 1, shape = NA))) +
  theme(legend.position = "bottom", 
    legend.title = element_text(face = "bold", size = 16),
    legend.text  = element_text(size = 16),
    legend.key.width = unit(0.5, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = -20, r = 0, b =10, l = 0) )
legend_only_plot

plot_Tval_no_legend <- plot_Tval_synoptic + theme(legend.position = "none",
                                                  plot.margin = margin(t = 5, r = 20, b = 3, l = 5))

plot_Dval_no_legend <- plot_Dval_synoptic + theme(legend.position = "none",
                                                  plot.margin = margin(t = 3, r = 20, b = 3, l = 5))

plot_Rval_no_legend <- plot_Rval_synoptic + theme(legend.position = "none",
                                                  plot.margin = margin(t = 3, r = 20, b = 2, l = 5))


plots_col <- plot_grid(plot_Tval_no_legend, plot_Dval_no_legend,plot_Rval_no_legend,ncol = 1, align = "v")

combined_with_legend <- plot_grid(plots_col,legend_only_plot, ncol = 1, rel_heights = c(1, 0.14))
combined_with_legend
#save
# ggsave("output/synoptic_with_background_models_single_legend_02042026_v2.png",plot=combined_with_legend,width=5, height=12, units='in')


#Supplemental figs --------------------
## fig S2: water temp CoV x elevation, colored by mtn range-----------------------------------------------------------------

temp_cv <- real.means %>%
  mutate(binned_elev = elev_bin(Elevation)) %>% #elev bin is a function in the 00_setup script
  group_by(Region, binned_elev) %>%
  summarise(cv = sd(Max_T_mean, na.rm = TRUE) / mean(Max_T_mean, na.rm = TRUE),
    n_lakes = n(),
    .groups = "drop")%>%
  filter(n_lakes >= 4)


temp_cv_plot <- ggplot(temp_cv, aes(binned_elev, cv, color = Region, group = Region)) +
  geom_point() +
  geom_smooth(data = temp_cv %>% 
                group_by(Region) %>% 
                filter(n() > 2),se=F, method="loess", span=1) +
  scale_color_manual(values = mtn_colors) +
  scale_x_continuous(limits = c(500, 3750), 
                     breaks = seq(500, 3750, by = 250)) +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title= element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  labs(x = " ",
    y = "CoV - Temperature")
temp_cv_plot

## fig S3: DOC CoV x elevation, colored by mtn range------------------------------------------------------------------------
doc_means <- ValData %>%
  group_by(Region) %>%
  summarise(Elevation = mean(Elevation, na.rm = TRUE),
    DOC_mean  = mean(DOC, na.rm = TRUE),
    .groups = "drop")

doc_cv <- ValData %>%
  mutate(binned_elev = elev_bin(Elevation)) %>%
    group_by(Region, binned_elev) %>%
  summarise(cv = sd(DOC, na.rm = TRUE) / mean(DOC, na.rm = TRUE),
    n_lakes = n(),
    .groups = "drop")%>%
  filter(n_lakes >= 4)

doc_cv_plot <- ggplot(doc_cv, aes(binned_elev, cv, color = Region, group = Region)) +
  geom_point() +
  geom_smooth(data = doc_cv %>% 
                group_by(Region) %>% 
                filter(n() > 2),se=F, method="loess", span=1) +
  scale_color_manual(values = mtn_colors) +
  scale_x_continuous(limits = c(500, 3750), 
                     breaks = seq(500, 3750, by = 250)) +
  scale_y_continuous(limits = c(0, 1.5), 
                     breaks = seq(0, 1.5, length.out = 4)) +  
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_blank(),
        axis.text    = element_text(size = 16),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = " ",
       y = "CoV - DOC")
doc_cv_plot

## fig S4: richness CoV x elevation, colored by mtn range-------------------------------------------------------------------
rich_cv <- val.data.new %>%
  mutate(binned_elev = elev_bin(ELEV_MASL)) %>%
  group_by(Region, binned_elev) %>%
  summarise(
    cv = sd(richness, na.rm = TRUE) / mean(richness, na.rm = TRUE),
    n_lakes = n(),
    .groups = "drop")%>%
  filter(n_lakes >=4) 


rich_cv_plot <- ggplot(rich_cv, aes(binned_elev, cv, color = Region, group = Region)) +
  geom_point() +
  geom_smooth(data = rich_cv %>% 
      group_by(Region) %>% 
      filter(n() > 2),
      se = FALSE, method = "loess", span = 1) +
  scale_color_manual(values = mtn_colors) +
  scale_x_continuous(limits = c(500, 3750), 
                     breaks = seq(500, 3750, by = 250)) +
    theme_bw(base_size = 16) +
  theme(axis.title   = element_text(size = 18),
        axis.text    = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Lower elevation bin (250m intervals)",
       y = "CoV - Richness")
rich_cv_plot

#save
# ggsave("output/temp_cv_02042026.png",plot=temp_cv_plot,width=8, height=6, units='in')
# 
# ggsave("output/doc_cv_02042026.png",plot=doc_cv_plot,width=8, height=6, units='in')
# 
# ggsave("output/zoop_cv_02042026.png",plot=rich_cv_plot,width=8, height=6, units='in')





#stack empirical data and cv data plots into a grid----
temp_cv_no_legend <- temp_cv_plot + theme(legend.position = "none",
                                                  plot.margin = margin(t = 5, r = 20, b = 3, l = 5))

doc_cv_no_legend <- doc_cv_plot + theme(legend.position = "none",
                                                  plot.margin = margin(t = 3, r = 19, b = 3, l = 5))

rich_cv_no_legend <- rich_cv_plot + theme(legend.position = "none",
                                                  plot.margin = margin(t = 3, r = 20, b = 2, l = 5))

cowplot::plot_grid(plot_Tval_no_legend ,temp_cv_no_legend, plot_Dval_no_legend,doc_cv_no_legend, plot_Rval_no_legend,rich_cv_no_legend,  ncol=2)

#add legend below all of them
labels <- c("A", "D",
            "B", "E",
            "C", "F")
labels <- matrix(labels, nrow = 3, ncol = 2)
labels <- as.vector(labels)

empirical_cv_plots <- cowplot::plot_grid(plot_Tval_no_legend, temp_cv_no_legend, plot_Dval_no_legend, doc_cv_no_legend, plot_Rval_no_legend, rich_cv_no_legend, ncol = 2, labels = labels)

empirical_cv_with_legend <- cowplot::plot_grid(empirical_cv_plots, legend_only_plot, ncol = 1, rel_heights = c(1, 0.08))
empirical_cv_with_legend


 ggsave("output/empirical_and_cv_02192026.png",plot=empirical_cv_with_legend,width=11, height=13, units='in') 

