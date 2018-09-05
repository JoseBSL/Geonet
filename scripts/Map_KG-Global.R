###########################################################################################
##
## R source code to read and visualize Köppen-Geiger fields (Version of 29 August 2017)                                                                                    
##
## Climate classification after Kottek et al. (2006), downscaling after Rubel et al. (2017)
##
## Kottek, M., J. Grieser, C. Beck, B. Rudolf, and F. Rubel, 2006: World Map of the  
## Köppen-Geiger climate classification updated. Meteorol. Z., 15, 259-263.
##
## Rubel, F., K. Brugger, K. Haslinger, and I. Auer, 2017: The climate of the 
## European Alps: Shift of very high resolution Köppen-Geiger climate zones 1800-2100. 
## Meteorol. Z., DOI 10.1127/metz/2016/0816.
##
## (C) Climate Change & Infectious Diseases Group, Institute for Veterinary Public Health
##     Vetmeduni Vienna, Austria
##
###########################################################################################

# required packages 
library(raster); library(rasterVis); library(rworldxtra); data(countriesHigh)
library(rgdal)
library(ggplot2)
library(plyr)
library(dplyr)

# Read raster files
period='1986-2010'
r <- raster(paste("data/Map_KG-Global/KG_", period, '.grd', sep=''))
raster.points <- rasterToPoints(r)
raster.points <- data.frame(raster.points)
colnames(raster.points) <-c('x','y','layer')

#make climate zones dataframe
clim.zones <- data.frame(layer = c(1:32), 
                   climate =  c('Af', 'Am', 'As', 'Aw', 'BSh', 'BSk', 'BWh', 'BWk', 'Cfa', 'Cfb','Cfc', 'Csa', 'Csb', 'Csc', 'Cwa','Cwb', 'Cwc', 'Dfa', 'Dfb', 'Dfc','Dfd', 'Dsa', 'Dsb', 'Dsc', 'Dsd','Dwa', 'Dwb', 'Dwc', 'Dwd', 'EF','ET', 'Ocean'))
clim.zones$zone <- left(clim.zones$climate,1)
clim.zones$zone2[clim.zones$zone %in% c("A")] <- "Tropical"
clim.zones$zone2[clim.zones$zone %in% c("B")] <- "Arid"
clim.zones$zone2[clim.zones$zone %in% c("C")] <- "Temperate"
clim.zones$zone2[clim.zones$zone %in% c("D")] <- "Cold continental"
clim.zones$zone2[clim.zones$zone %in% c("E")] <- "Polar"

#merge raster and climate zone dfs
points.zones <- merge(raster.points,clim.zones, by="layer")
z <- c("A","B","C","D","E")
points.zones <- dplyr::filter(points.zones, zone %in% z) %>% droplevels()
points.zones$zone2 <- factor(points.zones$zone2, levels = c("Tropical", "Arid", "Temperate", "Cold continental", "Polar"))

#plot the map
mp <- ggplot()
mp <- mp + geom_raster(data=points.zones, 
                     aes(y=y, x=x, fill=zone2), 
                     alpha=0.4) 
mp <- mp + geom_point(data=g.sub,
                     aes(x=Longitude, y=Latitude),
                     colour='black', alpha=0.4, stroke=0,size=0.05/g.sub$prop_links)
mp <- mp + facet_wrap(~PolOrder,ncol = 3)
mp <- mp + theme(axis.text.y=element_blank(),
                 axis.text.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.ticks=element_blank(), 
                 panel.background = element_rect(fill='white'),
                 plot.background = element_rect(fill='white'),
                 panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 panel.border = element_rect(color = "black", fill = NA, size = 0.5))
mp <- mp + coord_cartesian(ylim = c(-50, 79))
mp <- mp + theme(strip.background = element_rect(colour="NA", fill=NA),
            strip.text = element_text(size=12))
mp <- mp + scale_fill_manual(values=c("#4DAF4A","#E41A1C","#984EA3","#FF7F00","#377EB8"),drop=FALSE)
mp