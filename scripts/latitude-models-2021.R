###latitude models

library(bayesplot)
library(brms)
library(plyr)
library(dplyr)
library(emmeans)
library(ggplot2)

options(brms.backend="cmdstanr")

##latitude variables
g.sub$abs.lat<-abs(g.sub$Lat)
g.sub$scale.abs.lat<-scale(g.sub$abs.lat)

sp.links.melt.5$abs.lat<-abs(sp.links.melt.5$Lat)
sp.links.melt.5$scale.abs.lat<-scale(sp.links.melt.5$abs.lat)


#Prop links model
prop_lat_mod<-brm(prop_links~animal.order*scale.abs.lat+(1|Reference/Network),
                  family=Beta(link = "logit"),
                  inits=0,
                  iter=2000,
                  prior=prop_ord_prior,
                  data=g.sub,cores=4)

#generalism model
spec_lat_mod=brm(value2 ~ animal.order * scale.abs.lat + 
                     offset(log(int_tot)) + 
                     (1|Reference/Network),
                   family=negbinomial(link="log"),
                   prior=spec_ord_prior,
                   cores=4,
                   data=sp.links.melt.5)

prop_lat_mod<-add_criterion(prop_lat_mod,
                            criterion="loo")
prop_ord_mod1<-add_criterion(prop_ord_mod1,
                             criterion="loo")

spec_lat_mod<-add_criterion(spec_lat_mod,
                            criterion="loo")

spec_ord_mod_1<-add_criterion(spec_ord_mod_1,
                              criterion="loo")

loo_compare(prop_lat_mod,
            prop_ord_mod1)

loo_compare(spec_lat_mod,
            spec_ord_mod_1)

###PLOT THIS SKIT

prop.lat.effects <- plot(conditional_effects(prop_lat_mod))
spec.lat.effects <- plot(conditional_effects(spec_lat_mod),
                         conditions = list(int_tot=100))

prop.lat.plot <- prop.lat.effects[[3]]$data
spec.lat.plot <- spec.lat.effects[[3]]$data

spec.lat.plot[,10:13] <- spec.lat.plot[,10:13]+1

colnames(prop.lat.plot)[1:2] <- c("Latitude","Pollinator taxa")
colnames(spec.lat.plot)[1:2] <- c("Latitude","Pollinator taxa")

prop.lat.plot$`Pollinator taxa` <- revalue(prop.lat.plot$`Pollinator taxa`,
                                       c("Bee" = "Bee",
                                         "Coleoptera" = "Coleoptera",
                                         "Lepidoptera" = "Lepidoptera",
                                         "Hymenoptera" = "Non-bee Hymenoptera",
                                         "Diptera" = "Non-syrphid Diptera",
                                         "Syrphidae" = "Syrphidae"))

prop.lat.plot$`Pollinator taxa` <- factor(prop.lat.plot$`Pollinator taxa`,
                                      levels=c("Bee","Coleoptera","Lepidoptera",
                                               "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

spec.lat.plot$`Pollinator taxa` <- revalue(spec.lat.plot$`Pollinator taxa`,
                                       c("Bee" = "Bee",
                                         "Coleoptera" = "Coleoptera",
                                         "Lepidoptera" = "Lepidoptera",
                                         "Hymenoptera" = "Non-bee Hymenoptera",
                                         "Diptera" = "Non-syrphid Diptera",
                                         "Syrphidae" = "Syrphidae"))

spec.lat.plot$`Pollinator taxa` <- factor(spec.lat.plot$`Pollinator taxa`,
                                      levels=c("Bee","Coleoptera","Lepidoptera",
                                               "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

g.sub2 <- g.sub
sp.links.melt.6 <-  sp.links.melt.5

rbind.prop.lat.plot <- rbind.fill(prop.lat.plot,g.sub2)%>%droplevels()
rbind.spec.lat.plot <- rbind.fill(spec.lat.plot,sp.links.melt.6)%>%droplevels()

rbind.prop.lat.plot$animal.order <- revalue(rbind.prop.lat.plot$animal.order,c("Bee" = "A",
                                                                       "Coleoptera" = "B",
                                                                       "Lepidoptera" = "C",
                                                                       "Hymenoptera" = "D",
                                                                       "Diptera" = "E",
                                                                       "Syrphidae"  =      "F"))

rbind.prop.lat.plot$animal.order <-  factor(rbind.prop.lat.plot$animal.order,levels=c("A","B","C","D","E","F"))
rbind.prop.lat.plot <- droplevels(rbind.prop.lat.plot)
rbind.prop.lat.plot$prop_links <- as.numeric(rbind.prop.lat.plot$prop_links)

###here

rbind.spec.lat.plot$animal.order <- revalue(rbind.spec.lat.plot$animal.order,c("Bee" = "A",
                                                                               "Coleoptera" = "B",
                                                                               "Lepidoptera" = "C",
                                                                               "Hymenoptera" = "D",
                                                                               "Diptera" = "E",
                                                                               "Syrphidae"  =      "F"))

rbind.spec.lat.plot$animal.order <-  factor(rbind.spec.lat.plot$animal.order,levels=c("A","B","C","D","E","F"))
rbind.spec.lat.plot <- droplevels(rbind.spec.lat.plot)

#COLOURS AND BREAKS
plot_cols=c("A"="#a6cee3",
            "B"="#b2df8a",
            "C"="#fb9a99",
            "D"="#fdbf6f",
            "E"="#cab2d6",
            "F"="#e5c494",
            "Bee"="#1f78b4",
            "Coleoptera"="#33a02c",
            "Lepidoptera"="#e31a1c",
            "Non-bee Hymenoptera"="#ff7f00",
            "Non-syrphid Diptera"="#6a3d9a",
            "Syrphidae"="#b15928")

list_spp=c("Bee","Coleoptera","Lepidoptera","Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae")

rbind.prop.lat.plot$abs.latitude <- rbind.prop.lat.plot$Latitude*
                                    attr(g.sub2$scale.abs.lat, 'scaled:scale') + 
                                    attr(g.sub2$scale.abs.lat, 'scaled:center')

##PLOT
prop.lat.gg <- ggplot(rbind.prop.lat.plot,
               aes(x=abs.latitude,
                   y=estimate__))+
  geom_point(aes(y=prop_links,
                 x=abs.lat,
                 col=animal.order),
             show.legend = F,
             size=3,
             position=position_jitterdodge(dodge.width=0,
                                           jitter.width = 0.7),
             alpha=0.5)+
  geom_ribbon(aes(ymin=lower__,
                    ymax=upper__,
                    fill=`Pollinator taxa`),
                alpha=0.4,
                show.legend = F)+
    geom_line(aes(col=`Pollinator taxa`),
              size=2,show.legend = F)+
  theme_bw()+
  ylab("Proportion of links")+
  xlab("Latitude")+
  scale_fill_manual(breaks=list_spp,
                       values=plot_cols,
                       name="Pollinator taxa")+
  scale_color_manual(breaks=list_spp,
                     values=plot_cols,
                     name="Pollinator taxa")+
  theme(strip.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.text = element_text(face = "bold",size=14),
        panel.spacing = unit(0.5,"lines"),
        #axis.text.x=element_blank(),
        aspect.ratio = 1,
        axis.ticks.x = element_blank())

#prop.lat.gg

rbind.spec.lat.plot$abs.latitude <- rbind.spec.lat.plot$Latitude*
  attr(sp.links.melt.6$scale.abs.lat, 'scaled:scale') + 
  attr(sp.links.melt.6$scale.abs.lat, 'scaled:center')

spec.lat.gg <- ggplot(rbind.spec.lat.plot,
                      aes(x=abs.latitude,
                          y=estimate__))+
  geom_point(aes(y=value,
                 x=abs.lat,
                 col=animal.order),
             show.legend = F,
             size=3,
             position=position_jitterdodge(dodge.width=0,
                                           jitter.width = 0.7),
             alpha=0.5)+
  scale_y_log10()+
  geom_ribbon(aes(ymin=lower__,
                  ymax=upper__,
                  fill=`Pollinator taxa`),
              alpha=0.4,
              show.legend = T)+
  geom_line(aes(col=`Pollinator taxa`),
            size=2,show.legend = T)+
  theme_bw()+
  ylab("Normalised generalism")+
  xlab("Latitude")+
  scale_fill_manual(breaks=list_spp,
                    values=plot_cols,
                    name="Pollinator taxa")+
  scale_color_manual(breaks=list_spp,
                     values=plot_cols,
                     name="Pollinator taxa")+
  theme(strip.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.text = element_text(face = "bold",size=14),
        panel.spacing = unit(0.5,"lines"),
       # axis.text.x=element_blank(),
        aspect.ratio = 1,
        axis.ticks.x = element_blank())

spec.lat.gg

prop.lat.grob <- ggplotGrob(prop.lat.gg)
spec.lat.grob <- ggplotGrob(spec.lat.gg)

ggsave(cbind(prop.lat.grob,spec.lat.grob),
       file="plots and tables/Figure S1 - Latitude.jpg",
       device="jpg",
       width=8,
       height=4)

ggsave(cbind(prop.lat.grob,spec.lat.grob),
       file="plots and tables/Figure S1 - Latitude.pdf",
       device="pdf",
       width=8,
       height=4)
