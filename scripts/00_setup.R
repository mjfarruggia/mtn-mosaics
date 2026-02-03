############################################################
## 00_setup.R
## setup for downstream scripts: libraries, helper functions, themes
## Authors: Adrianne Smits, Janice Brahney, Angela Strecker, MJ Farruggia
## Last Updated: Feb 2026 by MJF
############################################################

## Load packages ---------------------------------------------------------
if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(
  tidyverse,
  gridExtra,
  egg,
  cowplot,
  truncnorm,
  broom,
  geosphere, #to calculate daylength from latitude and doy
  sirad, #to compute solar radiation
  ggrepel,
  viridis,
  patchwork,
  ggthemes
)

options(ggplot2.continuous.fill="viridis")


## Functions---------------------------------------------------------

#Function to Generate random variables with a mean and std
rnorm2<-function(n,mean,sd){mean+sd*scale(rnorm(n))}

#Random vegetation proportion
Randomf = function(x){
  x=runif(5,min=0,max=1)
  x=x/sum(x)
  return(x)
}



# Customized plotting theme with large enough text -------------------------------
#I'm guessing 10 would be the minimum but you can play around here
  #mjf - i changed to 16
theme_MS <- function () {
  theme_base(base_size=16) %+replace%
    theme(
      panel.background = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, linewidth=1.0),
      plot.title = element_text(face="bold", hjust=0.5, size=16, vjust=2),
      plot.subtitle = element_text(color="dimgrey", hjust=0.5, size=16),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      #aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text.y = element_text(size=16, angle=270),
      strip.text.x = element_text(size=16),
      panel.spacing=grid::unit(0,"lines"),
      axis.ticks.length = unit(0.1, "cm")
    )
}

theme_set(theme_MS())

#annotate panel letters inside plot
panelLetter.normal <- data.frame(
  xpos = c(-Inf),
  ypos = c(Inf),
  hjustvar = c(-0.2),
  vjustvar = c(1.5)
)
