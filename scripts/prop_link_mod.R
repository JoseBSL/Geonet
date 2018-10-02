library(rstan)
library(bayesplot)
library(brms)
library(plyr)
library(dplyr)

geonet=read.csv("data/processed/geonet.csv")
geonet$clim=as.factor(geonet$clim)
str(geonet)
g=geonet%>%
  group_by(Network, PolOrder) %>%
  summarise(order_links=sum(Int))

g2=g%>%
  group_by(Network) %>%
  summarise(int_tot=sum(order_links))

g3=merge(g,g2)

g3$prop_links=g3$order_links/g3$int_tot

g4=merge(g3,reference,by="Network")

#subset data by insect order
g.sub <- subset(g4, PolOrder %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))

str(g.sub)
g.sub$clim=factor(g.sub$clim,ordered = FALSE)
propprior <- prior(normal(0,2), class = b) + prior(normal(0,0.4), class = sd)

str(g.sub)
##FULL MODEL - THREE WAY INTERACTION
prop1=brm(prop_links~PolOrder*clim*scale(ele)+(1|Reference/Network),prior=propprior,
              family=Beta(link = "logit"),
              data=g.sub,cores = 4)

sjPlot::plot_model(prop1)
pp_check(prop1,nsamples=100) # GOOD

ppc_violin_grouped(g.sub$prop_links, 
                   yrep = posterior_predict(prop1, nsamples = 100), group = g.sub$clim)

ppc_violin_grouped(g.sub$prop_links, 
                   yrep = posterior_predict(prop1, nsamples = 100), group = g.sub$PolOrder)

check_all_diagnostics(prop1$fit)

bayes_R2(prop1)

##PLOT
library(tidybayes)
prop.plot1=g.sub%>% droplevels() %>%
  add_fitted_draws(prop1,n=100)%>%
  ggplot(aes(x=value,y=as.factor(PolOrder)))+
  stat_intervalh(aes(x=.value))+
  facet_grid(~clim, scale = "free_y")+
  scale_color_brewer()+
  theme_bw()+
  xlab("Prop. network links")+
  ylab("Pollinator order")
prop.plot1


##remove ants from hymenoptera

