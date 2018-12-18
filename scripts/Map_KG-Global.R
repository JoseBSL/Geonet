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
library(maptools)
library(tidybayes)

# Get Natural Earth shapefiles
download.file(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip", "ne_110m_admin_0_countries.zip", "auto")
unzip("ne_110m_admin_0_countries.zip")
file.remove("ne_110m_admin_0_countries.zip")

# Load and fortify regular data
world <- readOGR(".", "ne_110m_admin_0_countries")
continents.regular <- fortify(world, region="CONTINENT")

# Read raster files
period='1986-2010'
r <- raster(paste("data/Map_KG-Global/KG_", period, '.grd', sep=''))
raster.points <- rasterToPoints(r)
raster.points <- data.frame(raster.points)
colnames(raster.points) <-c('x','y','layer')

#make climate zones dataframe
clim.zones <- data.frame(layer = c(1:32), 
                   climate =  c('Af', 'Am', 'As', 'Aw', 'BSh', 'BSk', 'BWh', 'BWk', 
                                'Cfa', 'Cfb','Cfc', 'Csa', 'Csb', 'Csc', 'Cwa','Cwb', 
                                'Cwc', 'Dfa', 'Dfb', 'Dfc','Dfd', 'Dsa', 'Dsb', 'Dsc',
                                'Dsd','Dwa', 'Dwb', 'Dwc', 'Dwd', 'EF','ET', 'Ocean'))
clim.zones$zone <- left(clim.zones$climate,1)
clim.zones$zone2[clim.zones$zone %in% c("A")] <- "Tropical"
clim.zones$zone2[clim.zones$zone %in% c("B")] <- "Arid"
clim.zones$zone2[clim.zones$zone %in% c("C")] <- "Temperate"
clim.zones$zone2[clim.zones$zone %in% c("D")] <- "Continental" ##Can we just call this continental? or Cold-Temperate
clim.zones$zone2[clim.zones$zone %in% c("E")] <- "Polar"

#merge raster and climate zone dfs
points.zones <- merge(raster.points,clim.zones, by="layer")
z <- c("A","B","C","D","E")
points.zones <- dplyr::filter(points.zones, zone %in% z) %>% droplevels()
points.zones$zone2 <- factor(points.zones$zone2, 
                levels = c("Tropical", "Arid", "Temperate", "Continental", "Polar"))
colnames(points.zones)[6] <- "Climate_zone"
#plot the map
map <- ggplot()
map <- map + geom_map(data=continents.regular,map=continents.regular, aes(map_id=id), 
                      colour="black", fill="white", size=0.4) + 
  expand_limits(x=continents.regular$long, y=continents.regular$lat) + 
  coord_equal()
map <- map + geom_raster(data=points.zones, 
                     aes(y=y, x=x, fill=zone2), 
                     alpha=0.4) 
map <- map + geom_point(data=g.sub,
                     aes(x=Longitude, y=Latitude),
                     colour='black', alpha=0.4,size=0.5+(0.05/g.sub$prop_links))
map <- map + geom_point(data=g.sub,
                        aes(x=Longitude, y=Latitude),
                        colour='black', pch=1,size=0.5+(0.05/g.sub$prop_links))
map <- map + facet_wrap(~PolOrder,ncol = 3)
map <- map + theme(axis.line.x = element_line(size=0, colour = "black"),
                   axis.line.y = element_line(size=0, colour = "black"),
                   panel.grid.major = element_line(colour = "#d3d3d3"),
                   panel.grid.minor = element_blank(), 
                   panel.background = element_blank()) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =10),
        axis.title.x=element_text(size=16, vjust = 1),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =10),
        axis.title.y=element_text(size=16, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(axis.ticks.length = unit(2, "mm"))+
  theme(strip.background = element_rect(colour="NA", fill=NA),
        strip.text = element_text(size=12))
map <- map + scale_fill_manual(values=c("#4DAF4A","#E41A1C","#984EA3","#FF7F00","#377EB8"),
                               drop=FALSE,name="Climate zone")
map
ggsave("clim_prop_map.pdf",plot=map,width=15,height=5,units="in")

####################
###SPECIALISATION###
####################

map <- ggplot()
map <- map + xlab("Longitude") + ylab("Latitude")
map <- map + geom_map(data=continents.regular,map=continents.regular, 
                      aes(map_id=id), colour="black", fill="white", size=0.4) + 
  expand_limits(x=continents.regular$long, y=continents.regular$lat) + 
  coord_equal()
map <- map + geom_raster(data=points.zones, 
                         aes(y=y, x=x, fill=zone2), 
                         alpha=0.4) 
map <- map + geom_point(data=spec.graph.agg,aes(x=Longitude, y=Latitude),
                        colour="black",alpha=0.5,size=1+spec.graph.agg$.value)
map <- map + geom_point(data=spec.graph.agg,aes(x=Longitude, y=Latitude),
                        colour="black",pch=1,size=1+spec.graph.agg$.value)
map <- map + facet_wrap(~PolOrder,ncol = 3)
map <- map + theme(axis.line.x = element_line(size=0, colour = "black"),
                   axis.line.y = element_line(size=0, colour = "black"),
                   panel.grid.major = element_line(colour = "#d3d3d3"),
                   panel.grid.minor = element_blank(), 
                   panel.background = element_blank()) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =10),
        axis.title.x=element_text(size=16, vjust = 1),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =10),
        axis.title.y=element_text(size=16, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(axis.ticks.length = unit(2, "mm"))+
  theme(strip.background = element_rect(colour="NA", fill=NA),
        strip.text = element_text(size=12))
map <- map + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
map <- map + scale_colour_brewer(palette="Dark2")
map <- map + theme(legend.position="none",panel.border = element_rect(color = "black",
                   fill = NA, size = 1))
map <- map + scale_fill_manual(values=c("#4DAF4A","#E41A1C","#984EA3","#FF7F00","#377EB8"),
                               drop=FALSE,name="Climate zone")
ggsave("specmap.pdf",plot=map,width=15,height=5,units="in")

