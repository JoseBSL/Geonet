#Prop links order model

library(bayesplot)
library(brms)
library(plyr)
library(dplyr)
library(emmeans)
library(ggplot2)

options(brms.backend="cmdstanr")

##Priors
prop_ord_prior <- prior(normal(0,5), class = Intercept) +  
  prior(normal(0,2), class = b) + 
  prior(normal(0,0.4), class = sd)

g.sub%>%
  filter(animal.order%in%c("Hymenoptera",
                           "Coleoptera",
                           "Lepidoptera"))%>%
  summarise(sum(order_links))

3326/sum(g.sub$order_links)

#Model
prop_ord_mod1=brm(prop_links~animal.order*clim+(1|Reference/Network),
                  family=Beta(link = "logit"),
                  inits=0,
                  iter=2000,
                  prior=prop_ord_prior,
                  data=g.sub,cores=4)

pp_check(prop_ord_mod1,nsamples=100)
#conditional_effects(prop_ord_mod1)
performance::r2(prop_ord_mod1)
#Cond. 43, marginal 42

###################
#####PAIRWISE#####
#################

pairwise.prop.links <- emmeans(prop_ord_mod1,~animal.order:clim,
                               type="response")

pairwise.prop.contrasts <- as.data.frame(contrast(emmeans(prop_ord_mod1,
                                                          ~animal.order:clim),
                                                  "pairwise"))

######################
#####PLOT TYPE 2#####
####################

prop.plot=as.data.frame(pairwise.prop.links)

colnames(prop.plot)[1:2] <- c("Pollinator taxa","Climate zone")

prop.plot$`Pollinator taxa` <- revalue(prop.plot$`Pollinator taxa`,
                                            c("Bee" = "Bee",
                                              "Coleoptera" = "Coleoptera",
                                              "Lepidoptera" = "Lepidoptera",
                                              "Hymenoptera" = "Non-bee Hymenoptera",
                                              "Diptera" = "Non-syrphid Diptera",
                                              "Syrphidae" = "Syrphidae"))

prop.plot$`Pollinator taxa` <- factor(prop.plot$`Pollinator taxa`,
                                           levels=c("Bee","Coleoptera","Lepidoptera",
                                                    "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

g.sub2 <- g.sub

colnames(g.sub2)[9]="Climate zone"

g.sub2$`Pollinator taxa` <- g.sub2$animal.order

g.sub2$`Pollinator taxa` <- revalue(g.sub2$`Pollinator taxa`,
                                   c("Bee" = "Bee",
                                     "Coleoptera" = "Coleoptera",
                                     "Lepidoptera" = "Lepidoptera",
                                     "Hymenoptera" = "Non-bee Hymenoptera",
                                     "Diptera" = "Non-syrphid Diptera",
                                     "Syrphidae" = "Syrphidae"))

rbind.prop.plot <- rbind.fill(prop.plot,g.sub2)

rbind.prop.plot$`Climate zone` <- factor(rbind.prop.plot$`Climate zone`,levels=c("A","B","C","D","E"))

rbind.prop.plot$`Climate zone` <- revalue(rbind.prop.plot$`Climate zone`,c("A" = "Tropical",
                                                                           "B" = "Arid",
                                                                           "C" = "Temperate",
                                                                           "D" = "Continental",
                                                                           "E" = "Polar"))

rbind.prop.plot$animal.order <- revalue(rbind.prop.plot$animal.order,c("Bee" = "A",
                                                               "Coleoptera" = "B",
                                                               "Lepidoptera" = "C",
                                                               "Hymenoptera" = "D",
                                                               "Diptera" = "E",
                                                               "Syrphidae"  =      "F"))

rbind.prop.plot$animal.order <-  factor(rbind.prop.plot$animal.order,levels=c("A","B","C","D","E","F"))
rbind.prop.plot <- droplevels(rbind.prop.plot)
rbind.prop.plot$prop_links <- as.numeric(rbind.prop.plot$prop_links)

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


##PLOT
prop.gg=ggplot(rbind.prop.plot,
               aes(x=`Pollinator taxa`,
                   y=response,
                   col=`Pollinator taxa`))+
  geom_point(aes(y=prop_links,
                 col=animal.order),
             show.legend = F,
             size=0.5,
             position=position_jitterdodge(dodge.width=0,
                                           jitter.width = 0.7),
             alpha=0.5)+
  geom_point(aes(col=`Pollinator taxa`),
             size=2,show.legend = F)+
  geom_errorbar(aes(ymin=lower.HPD,
                    ymax=upper.HPD,
                    col=`Pollinator taxa`),
                width=0.4,
                show.legend = F)+
  facet_wrap(~`Climate zone`,ncol=5)+
  theme_bw()+
  ylab("Proportion of links")+
  xlab(NULL)+
  scale_color_manual(breaks=list_spp,
                     values=plot_cols,
                     name="Pollinator taxa")+
  theme(strip.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.text = element_text(face = "bold",size=14),
        panel.spacing = unit(0.5,"lines"),
        axis.text.x=element_blank(),
        aspect.ratio = 1,
        axis.ticks.x = element_blank())

prop.gg

##save models
save(prop_ord_mod1,file="data/models/prop_ord_mod1.RData",compress="xz")
