library(brms)
library(plyr)
library(dplyr)



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
geo.uni <- unique(geonet[c("Pollinator", "PolFamily", "PolOrder")])
links.order.sd <- merge(comb.links,geo.uni, by="Pollinator")

full.spec <- merge(links.order.sd,sp.links.order)

#filter dataframe to retain orders of interest
ord <- c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera")
links.order.sd <- filter(links.order.sd, PolOrder %in% ord)
#add climate data
clim.dat <- unique(g4[c("Network","Latitude","Longitude","ClimateZ")])
clim.dat$ClimateZ[26] = "B"
links.order.sd <- merge(links.order.sd,clim.dat, by="Network")
links.order.sd$clim <- as.factor(left(links.order.sd$ClimateZ,1))
links.order.sd$PolOrder <- as.factor(links.order.sd$PolOrder) %>% droplevels()
links.order.sd$log <- log(links.order.sd$std+1)
links.order.sd=links.order.sd[!is.na(links.order.sd$PolOrder),]

links.order.sd[links.order.sd$std==1,c("std")]=0.9999

mx2 <- brm(std ~ PolOrder*clim + (1|Network),
          family=zero_inflated_beta(),
          data=links.order.sd,cores=4)
pp_check(mx2)
range(links.order.sd$std)
links.order.sd[is.na(links.order.sd$clim),]
links.order.sd[links.order.sd$std==0.9999,c("std")]=1
mx3 <- brm(std ~ PolOrder*clim + (1|Network),
           family=zero_one_inflated_beta(),
           data=links.order.sd,cores=4)
mx3
pp_check(mx3,nsamples=100)
bayes_R2(mx3)
bayes_factor()
library(tidybayes)
spec.aiz.plot1=links.order.sd%>% droplevels() %>%
  add_fitted_draws(mx3,n=100)%>%
  ggplot(aes(x=std,y=as.factor(PolOrder)))+
  stat_intervalh(aes(x=.value))+
  facet_grid(~clim, scale = "free_y")+
  scale_color_brewer()+
  stat_pointintervalh(aes(x=std),pch=15,.width=c(0))+
  theme_bw()+
  xlab("Specialisation")+
  ylab("Pollinator order")

spec.aiz.plot1

