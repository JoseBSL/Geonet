
#################################################
#correct number of links for sampling effort with Bascompte null model
#################################################
library(brms)
library(reshape2)
library(vegan)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(glmmTMB)
library(emmeans)

#cast long format dataframe
geonet$bin <- geonet$Int[geonet$Int > 0] <- 1
sp.links.melt <- geonet %>%
  group_by(Network, Pollinator) %>%
  summarise(value=sum(bin))
#M_PL_062_00 has very large number of links for all species...456 - seems suspicious 

#add order and family to dataframe
geo.uni <- unique(geonet[c("Pollinator", "PolFamily", "PolOrder")])
sp.links.order <- merge(sp.links.melt,geo.uni, by="Pollinator")
sp.links.order$PolOrder <- as.factor(sp.links.order$PolOrder)
table(sp.links.order$PolOrder)

#filter dataframe to retain orders of interest
ord <- c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera")
links.full.sub <- dplyr::filter(sp.links.order, PolOrder %in% ord)
#add climate data
#clim.dat <- unique(g4[c("Network","Latitude","Longitude","Reference","clim","ele")])
#clim.dat$ClimateZ[26] = "B"# not working, need to check thi
links.full.sub <- merge(links.full.sub,unique(g4[c("Network","Latitude","Longitude","Reference","clim","ele","Size")]), by="Network")
links.full.sub$PolOrder <- as.factor(links.full.sub$PolOrder) %>% droplevels()
str(links.full.sub)

#bayes gamma +1
gammaprior=prior(normal(0,2),class="b")+
  prior(normal(0,2),class="Intercept")+
  prior(normal(0,1),class="sd")

is.integer(links.full.sub$value)

sp3=brm(value ~ PolOrder*clim + (1|Reference/Network),
        family=poisson(link="log"),
        data=links.full.sub,cores=4,inits=0)

pp_check(sp3,nsamples=100,ylim=10)
check_all_diagnostics(sp3)

##GRAPHING

str(links.full.sub)
hist(links.full.sub$value)
str(links.full.sub$PolOrder)

bayes_R2(sp3)

##PLOT tidy bayes
spec.plot1=links.full.sub%>%
  add_fitted_draws(sp3,n=100)%>%
  ggplot(aes(x=value,y=as.factor(PolOrder)))+
  stat_intervalh(aes(x=.value))+
  facet_grid(~clim, scale = "free_y")+
  scale_color_brewer()+
  stat_pointintervalh(aes(x=value),pch=15,.width=c(0))+
  theme_bw()+
  xlab("Specialisation")+
  ylab("Pollinator order")
spec.plot1

spec.plot2=links.full.sub%>%
  add_fitted_draws(sp3,n=100)%>%
  ggplot(aes(x=value,y=as.factor(clim)))+
  stat_intervalh(aes(x=.value))+
  facet_grid(~PolOrder, scale = "free_y")+
  scale_color_brewer()+
  theme_bw()+
  xlab("Specialisation")+
  ylab("Pollinator order")
spec.plot2

#plot generalisation on world map
#need to chnage this to plot model estimates and climate zone
WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

map <- ggplot()
map <- map + xlab("Longitude") + ylab("Latitude")
map <- map + geom_map(data=WorldData, map=WorldData,
                      aes(x=long, y=lat, group=group, map_id=region))
map <- map + geom_point(data=links.clim.net,
                        aes(x=Longitude, y=Latitude, colour=value),size=1.5)
map <- map + scale_colour_gradient(low="green",high="red")
map <- map + facet_wrap(Order~clim,ncol = 5)
map <- map + theme(axis.line.x = element_line(size=.5, colour = "black"),
                   axis.line.y = element_line(size=.5, colour = "black"),
                   panel.grid.major = element_line(colour = "#d3d3d3"),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =10),
        axis.title.x=element_text(size=24, vjust = 1),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =10),
        axis.title.y=element_text(size=24, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(axis.ticks.length = unit(2, "mm"))+
  theme(strip.background = element_rect(colour="NA", fill=NA),
        strip.text = element_text(size=8))
map <- map + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
map

#################################################
#END
#################################################