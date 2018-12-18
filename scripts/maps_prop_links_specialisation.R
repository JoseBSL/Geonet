library(plyr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(maptools)
library(tidybayes)

# Get Natural Earth shapefiles
download.file(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip", "ne_110m_admin_0_countries.zip", "auto")
unzip("ne_110m_admin_0_countries.zip")
file.remove("ne_110m_admin_0_countries.zip")

# Load and fortify regular data
world <- readOGR(".", "ne_110m_admin_0_countries")
continents.regular <- fortify(world, region="CONTINENT")
group.colors <- c(Tropical = "#1b9e77", Arid = "#d95f02", Temperate ="#66a61e", Continental = "#e7298a", Polar = "#7570b3")
map <- ggplot()
map <- map + xlab(NULL) + ylab(NULL)
map <- map + geom_map(data=continents.regular,map=continents.regular, aes(map_id=id), colour="white", fill="white", size=0.0001) + 
             expand_limits(x=continents.regular$long, y=continents.regular$lat) + 
             coord_equal()
map <- map + geom_raster(data=points.zones, 
                         aes(y=y, x=x, fill=Climate_zone), 
                         alpha=1)
map <- map + scale_fill_manual(values = group.colors)
map <- map + geom_point(data=g.sub,
                        aes(x=Longitude, y=Latitude), pch=1, size=0.6+(g.sub$prop_links*8))
map <- map + ylim(-54, 83)
map <- map + facet_wrap(~PolOrder,ncol = 3)
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
map <- map + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.4))
ggsave("graphs/links_map_V4.pdf",plot=map,width=15,height=5,units="in")



###Specialisation

# Load and fortify regular data
levels(links.full.sub$PolOrder)
str(spec.graph)
spec.graph=links.full.sub%>%
  add_fitted_draws(sp3,n=100,re_formula=NULL)

spec.graph.agg=aggregate(fitted~Network+PolOrder+Latitude+Longitude,data=sp.links.melt.sub, FUN="mean")
range(spec.graph.agg$fitted)
map <- ggplot()
map <- map + xlab("Longitude") + ylab("Latitude")
map <- map + geom_map(data=continents.regular,map=continents.regular, aes(map_id=id), colour="black", fill="white", size=0.4) + 
  expand_limits(x=continents.regular$long, y=continents.regular$lat) + 
  coord_equal()
map <- map + geom_raster(data=points.zones, 
                       aes(y=y, x=x, fill=zone2), 
                       alpha=0.4) 
map <- map + geom_point(data=spec.graph.agg,
                        aes(x=Longitude, y=Latitude), 
                        colour="black",alpha=0.5,size=1+spec.graph.agg$fitted)
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
ggsave("graphs/specmap.pdf",plot=map,width=15,height=5,units="in")

