library(dplyr)
library(ggplot2)
library(rgdal)
library(maptools)

# Get Natural Earth shapefiles
download.file(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip", "ne_110m_admin_0_countries.zip", "auto")
unzip("ne_110m_admin_0_countries.zip")
file.remove("ne_110m_admin_0_countries.zip")

# Load and fortify regular data
world <- readOGR(".", "ne_110m_admin_0_countries")
continents.regular <- fortify(world, region="CONTINENT")
continents.regular %>% filter(id != "Antarctica") -> continents.regular
str(continents.regular)
map <- ggplot()
map <- map + xlab("Longitude") + ylab("Latitude")
map <- map + geom_map(data=continents.regular,map=continents.regular, aes(map_id=id), colour="black", fill="white", size=0.4) + 
             expand_limits(x=continents.regular$long, y=continents.regular$lat) + 
             coord_equal()
#map <- map + geom_point(data=filter(links.clim, Order == "Coleoptera"),
map <- map + geom_point(data=g.sub,
                        aes(x=Longitude, y=Latitude, colour=PolOrder),alpha=0.5,size=0.05/g.sub$prop_links)
map <- map + facet_wrap(~PolOrder,ncol = 3)
map <- map + theme(axis.line.x = element_line(size=0, colour = "black"),
                   axis.line.y = element_line(size=0, colour = "black"),
                   panel.grid.major = element_line(colour = "#d3d3d3"),
                   panel.grid.minor = element_blank(), panel.background = element_blank()) +
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
map <- map + theme(legend.position="none",panel.border = element_rect(color = "black", fill = NA, size = 1))
map


