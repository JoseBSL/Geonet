# required packages 
library(raster)
library(rasterVis)
library(rworldxtra)
data(countriesHigh)
library(rgdal)
library(ggplot2)
library(plyr)
library(dplyr)
library(maptools)
library(tidybayes)

#load predicted generalisation values for each taxa group in each network
gen_predict <- sp.links.melt.5[!duplicated(sp.links.melt.5[,c("Network","animal.order")]),]
gen.predict.df <- cbind(gen_predict,predict(spec_ord_mod_1,newdata=gen_predict))

#gen_predict=read.csv("data/outputs/spec_graph.csv")

# Get Natural Earth shapefiles
#download.file(url="http://www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
#              "ne_110m_admin_0_countries.zip",
#              "auto")
#unzip("ne_110m_admin_0_countries.zip")
#file.remove("ne_110m_admin_0_countries.zip")

# Load and fortify regular data
world <- readOGR(dsn="data/maps/110m_cultural/")
continents.regular <- fortify(world, region="CONTINENT")

# Read raster files
period='1986-2010'
r <- raster(paste("data/maps/Map_KG-Global/KG_", period, '.grd', sep=''))
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
clim.zones$zone2[clim.zones$zone %in% c("D")] <- "Continental"
clim.zones$zone2[clim.zones$zone %in% c("E")] <- "Polar"

#merge raster and climate zone dfs
points.zones <- merge(raster.points,clim.zones, by="layer")
z <- c("A","B","C","D","E")
points.zones <- dplyr::filter(points.zones, zone %in% z) %>% droplevels()
points.zones$zone2 <- factor(points.zones$zone2, 
                             levels = c("Tropical",
                                        "Arid",
                                        "Temperate",
                                        "Continental",
                                        "Polar"))
colnames(points.zones)[6] <- "Climate_zone"

# Load and fortify regular data
#world <- readOGR(".", "ne_110m_admin_0_countries")
#continents.regular <- fortify(world, region="CONTINENT")
group.colors <- c(Tropical = "#1b9e77",
                  Arid = "#d95f02",
                  Temperate ="#66a61e",
                  Continental = "#e7298a",
                  Polar = "#7570b3")

#make the graph
map <- ggplot()
map <- map + xlab(NULL) + ylab(NULL)
map <- map + geom_map(data=continents.regular,
                      map=continents.regular,
                      aes(map_id=id),
                      colour="white",
                      fill="white",
                      size=0.0001) + 
  expand_limits(x=continents.regular$long,
                y=continents.regular$lat) + 
  coord_equal()
map <- map + geom_raster(data=points.zones, 
                         aes(y=y,
                             x=x,
                             fill=Climate_zone), 
                         alpha=1)
map <- map + scale_fill_manual(values = group.colors)
map <- map + geom_point(data=gen.predict.df,
                        aes(x=Long, y=Lat),
                        pch=1, size=0.5+(gen.predict.df$Estimate))
map <- map + ylim(-54, 83)
map <- map + facet_wrap(~animal.order,ncol = 3)
map <- map + theme(axis.line.x = element_blank(),
                   axis.line.y = element_blank(),
                   panel.grid.major = element_line(size=.4, colour = "#d3d3d3"),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank()) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =10),
        axis.title.x=element_text(size=16, vjust = 1),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =10),
        axis.title.y=element_text(size=16, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(axis.ticks.length = unit(2, "mm"),
        axis.ticks = element_line(colour = 'black', size = 0.4))+
  theme(strip.background = element_rect(colour="NA", fill=NA),
        strip.text = element_text(size=12))
map <- map + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
map <- map + theme(panel.border = element_rect(color = "black",
                                               fill = NA, size = 0.4))+
  labs(colour="Climate zone",fill="Climate zone")
map <- map + ggtitle("B) Pollinator generalism")

ggsave(map,file="plots/fig 2B - generalism.jpg",
       device="jpg",
       width=15,
       height=5,
       units="in")
