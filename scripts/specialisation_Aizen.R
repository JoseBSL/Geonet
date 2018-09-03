



sum.links <- geonet %>%
              group_by(Network, Pollinator) %>%
                summarise(tot=sum(Int))

min.max <- sum.links %>%
      group_by(Network) %>%
          summarise(min=min(tot), 
                    max=max(tot))

comb.links <- merge(min.max,sum.links)

comb.links$std = (comb.links$tot - comb.links$min) / (comb.links$max - comb.links$min)

#add order and family to dataframe
geo.uni <- unique(geonet[c("Pollinator", "Family", "Order")])
links.order.sd <- merge(comb.links,geo.uni, by="Pollinator")
links.order.sd <- links.order.sd %>% mutate_if(is.factor,as.character)
links.order.sd[links.order.sd$Family%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("Order")]="Bee"
links.order.sd[links.order.sd$Family%in%c("Syrphidae"),c("Order")]="Syrphidae"

full.spec <- merge(links.order.sd,sp.links.order)

#filter dataframe to retain orders of interest
ord <- c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera")
links.order.sd <- filter(links.order.sd, Order %in% ord)
#add climate data
clim.dat <- unique(g4[c("Network","Latitude","Longitude","ClimateZ")])
clim.dat$ClimateZ[26] = "B"
links.order.sd <- merge(links.order.sd,clim.dat, by="Network")
links.order.sd$clim <- as.factor(left(links.order.sd$ClimateZ,1))
links.order.sd$Order <- as.factor(links.order.sd$Order)
links.order.sd$log <- log(links.order.sd$std+1)

mx <- glmmTMB(std ~ Order*clim + (1|Network),
              family=gaussian,
              data=links.order.sd)

mxx <- glmmTMB(log(std+1) ~ Order*clim + (1|Network),
              family=gaussian,
              data=links.order.sd)

mxxx <- glm(log(std+1) ~ Network*Order,
            family=gaussian,
            data=links.order.sd)

mxxxx <- glmmTMB(log ~ Order*clim + Order*abs(Latitude) + (1|Network),
              family=gaussian,
              data=links.order.sd)

library(MuMIn)
MuMIn::dredge(mxxxx)


net.compx <- emmeans(mxxx, pairwise ~ Order|Network, by = "Network", level=0.95, adj="fdr")

#generate letters for groups
net.comp.CLDx <- CLD(net.compx, by = "Network", which = 1, Letters = letters, level = .95, adjust = "fdr")
net.comp.CLDx$value <- exp(net.comp.CLDx$emmean)
net.comp.CLDx$value <- net.comp.CLDx$value-1

#add climate data
links.clim.netx <- merge(net.comp.CLDx,clim.dat, by="Network")
links.clim.netx$clim <- as.factor(left(links.clim.netx$ClimateZ,1))
links.clim.netx <- na.omit(links.clim.netx)

#plot generalisation on world map
WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

map <- ggplot()
map <- map + xlab("Longitude") + ylab("Latitude")
map <- map + geom_map(data=WorldData, map=WorldData,
                      aes(x=long, y=lat, group=group, map_id=region))
#map <- map + geom_point(data=filter(links.clim, Order == "Coleoptera"),
map <- map + geom_point(data=links.clim.netx,
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


library(DHARMa)
res = simulateResiduals(mxx)
plot(res, rank = T)

resid <- resid(mxx, type = "pearson")
fitted <- fitted(mxx)
df <- data.frame(resid, fitted)
ggplot(df, aes(fitted, resid)) + geom_point() + ylab("Pearsons residuals") + xlab("Fitted values")

d <- density(log(xy$std+1)) # returns the density data 
plot(d) # plots the results

comb.links <- comb.links[comb.links$Network!="M_PL_062",]
