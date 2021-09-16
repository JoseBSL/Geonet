####
##Specialisation model
###

library(brms)
library(emmeans)
library(plyr)
library(dplyr)
library(tidybayes)
library(ggplot2)
library(performance)

options(brms.backend="cmdstanr")

#variables
range(sp.links.melt.5$int_tot)
sp.links.melt.5$scale_tot <- scale(sp.links.melt.5$int_tot)

range(sp.links.melt.5$scale_tot)
plot(sp.links.melt.5$int_tot~sp.links.melt.5$scale_tot)
#minus 1 so account for truncation in count model
sp.links.melt.5$value2 <- sp.links.melt.5$value-1

###Sum with offset
spec_ord_prior=prior(normal(0,2),class="b")+
  prior(normal(0,5),class="Intercept")+
  prior(normal(0,2),class="sd")

spec_ord_mod_1=brm(value2 ~ animal.order * clim + 
               offset(log(int_tot)) + 
               (1|Reference/Network),
               family=negbinomial(link="log"),
               prior=spec_ord_prior,
               cores=4,
               data=sp.links.melt.5)

spec_ord_mod_2=brm(value2 ~ animal.order * clim + 
                     scale_tot + 
                     (1|Reference/Network),
                   family=negbinomial(link="log"),
                   prior=spec_ord_prior,
                   cores=4,
                   data=sp.links.melt.5)

conditional_effects(spec_ord_mod_2)

r2(spec_ord_mod_1)
r2(spec_ord_mod_2)
#Marg. r2: 35,

brms::pp_check(spec_ord_mod_1)+xlim(0,20)

###################
#####PAIRWISE#####
#################
pairwise.spec <- emmeans(spec_ord_mod_2,~animal.order:clim,
                               type="response")

pairwise.spec.contrasts <- as.data.frame(contrast(emmeans(spec_ord_mod_2,~animal.order:clim),
                                                  "pairwise"))

###############
#####PLOT#####
#############

spec.plot=as.data.frame(pairwise.spec)
spec.plot[,3:5] <- spec.plot[,3:5]+1

colnames(spec.plot)[1:2] <- c("Pollinator taxa","Climate zone")

spec.plot$`Pollinator taxa` <- revalue(spec.plot$`Pollinator taxa`,
                                            c("Bee" = "Bee",
                                              "Coleoptera" = "Coleoptera",
                                              "Lepidoptera" = "Lepidoptera",
                                              "Hymenoptera" = "Non-bee Hymenoptera",
                                              "Diptera" = "Non-syrphid Diptera",
                                              "Syrphidae" = "Syrphidae"))

spec.plot$`Pollinator taxa` <- factor(spec.plot$`Pollinator taxa`,
                                           levels=c("Bee","Coleoptera","Lepidoptera",
                                                    "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

sp.links.melt.6 <- sp.links.melt.5
colnames(sp.links.melt.6)[9]="Climate zone"

sp.links.melt.6$`Pollinator taxa` <- sp.links.melt.6$animal.order

sp.links.melt.6$`Pollinator taxa` <- revalue(sp.links.melt.6$`Pollinator taxa`,
                                    c("Bee" = "Bee",
                                      "Coleoptera" = "Coleoptera",
                                      "Lepidoptera" = "Lepidoptera",
                                      "Hymenoptera" = "Non-bee Hymenoptera",
                                      "Diptera" = "Non-syrphid Diptera",
                                      "Syrphidae" = "Syrphidae"))

rbind.spec.plot <- rbind.fill(spec.plot,sp.links.melt.6)

rbind.spec.plot$`Climate zone` <- factor(rbind.spec.plot$`Climate zone`,levels=c("A","B","C","D","E"))

rbind.spec.plot$`Climate zone` <- revalue(rbind.spec.plot$`Climate zone`,c("A" = "Tropical",
                                                                           "B" = "Arid",
                                                                           "C" = "Temperate",
                                                                           "D" = "Continental",
                                                                           "E" = "Polar"))

rbind.spec.plot$animal.order <- revalue(rbind.spec.plot$animal.order,c("Bee" = "A",
                                                               "Coleoptera" = "B",
                                                               "Lepidoptera" = "C",
                                                               "Hymenoptera" = "D",
                                                               "Diptera" = "E",
                                                               "Syrphidae"  =      "F"))

rbind.spec.plot$animal.order <-  factor(rbind.spec.plot$animal.order,levels=c("A","B","C","D","E","F"))
rbind.spec.plot <- droplevels(rbind.spec.plot)
#rbind.spec.plot$value <- as.numeric(rbind.spec.plot$value)

spec.gg <- ggplot(rbind.spec.plot,aes(x=`Pollinator taxa`,
                                   y=prob,col=`Pollinator taxa`))+
  geom_point(aes(y=value,col=animal.order),show.legend = F,size=0.5,
             position=position_jitterdodge(dodge.width=0,
                                           jitter.width = 0.7,
                                           jitter.height = 0.05),
             alpha=0.5)+
  geom_point(aes(col=`Pollinator taxa`),
             size=2,show.legend = F)+
  geom_errorbar(aes(ymin=lower.HPD,
                    ymax=upper.HPD,
                    col=`Pollinator taxa`),
                width=0.4,show.legend =F)+
  facet_wrap(~`Climate zone`,ncol=5)+
  theme_bw()+
  ylab("Normalised generalism")+
  xlab(NULL)+
  scale_y_log10()+
  #scale_y_continuous(breaks=c(1,4,7,10,14),limits=c(1,14))+
  scale_color_manual(breaks=list_spp,
                     values=plot_cols,name="Pollinator taxa")+
  theme(strip.text.x = element_blank(),
        panel.spacing = unit(0.5,"lines"),
        strip.background =element_rect(fill="white"),
        #axis.text.x=element_blank(),
        axis.text.x=element_text(angle = 45, vjust = 1, hjust=1),
        aspect.ratio = 1,axis.ticks.x = element_blank())

spec.gg


##save models
save(spec_ord_mod_2,file="data/models/spec_ord_mod_2.RData",compress="xz")
