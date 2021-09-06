####
##Specialisation model
###


library(brms)
library(emmeans)
library(plotrix)
library(plyr)
library(rstan)
library(dplyr)
library(tidybayes)
library(tidyverse)
library(DescTools)
library(ggplot2)

sp.links.melt.5$clim <- as.factor(sp.links.melt.5$clim)
sp.links.melt.5$PolOrder <- as.factor(sp.links.melt.5$PolOrder)
sp.links.melt.5$Latitude <- abs(sp.links.melt.5$Latitude)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


###Sum with offset
spec_ord_prior=prior(normal(0,2),class="b")+
  prior(normal(0,5),class="Intercept")+
  prior(normal(0,2),class="sd")

spec_ord_mod_1=brm(value-1 ~ PolOrder * clim + 
               offset(log(int_tot)) +
               (1|Reference/Network),
               family=poisson(link="log"),
               control=list(adapt_delta=0.9,max_treedepth=15),
               prior=spec_ord_prior,
               cores=4,
               data=sp.links.melt.5)

spec_ord_mod_2=brm(value-1 ~ PolOrder * clim + 
               offset(log(int_tot)) + 
               (1|Reference/Network),
               family=negbinomial(link="log"),
               prior=spec_ord_prior,
               cores=4,
               data=sp.links.melt.5)


spec_ord_mod_3=brm(value-1 ~ PolOrder * clim + 
                     offset(log(int_tot)) + 
                     (1|Reference/Network),
                   family=zero_inflated_poisson(link="log"),
                   prior=spec_ord_prior,
                   control=list(adapt_delta=0.9,max_treedepth=15),
                   cores=4,
                   data=sp.links.melt.5)

spec_ord_mod_4=brm(value-1 ~ PolOrder * clim + 
                     offset(log(int_tot)) + 
                     (1|Reference/Network),
                   family=zero_inflated_negbinomial(link="log"),
                   prior=spec_ord_prior,
                   control=list(adapt_delta=0.9,max_treedepth=15),
                   cores=4,
                   data=sp.links.melt.5)

spec_ord_mod_5=brm(value-1 ~ PolOrder * Latitude + 
                     offset(log(int_tot)) + 
                     (1|Reference/Network),
                   family=negbinomial(link="log"),
                   prior=spec_ord_prior,
                   cores=4,
                   data=sp.links.melt.5)

spec_ord_prior2 = prior(normal(0,5),class="Intercept")+
  prior(normal(0,2),class="sd")

spec_ord_mod_6=brm(value-1 ~ 1 + 
                     (1|Reference/Network),
                   family=negbinomial(link="log"),
                   prior=spec_ord_prior2,
                   cores=4,
                   data=sp.links.melt.5)

#8:46

spec_ord_mod_1=add_ic(spec_ord_mod_1,ic=c("waic"))
spec_ord_mod_2=add_ic(spec_ord_mod_2,ic=c("waic"))
spec_ord_mod_3=add_ic(spec_ord_mod_3,ic=c("waic"))
spec_ord_mod_4=add_ic(spec_ord_mod_4,ic=c("waic"))
spec_ord_mod_5=add_ic(spec_ord_mod_5,ic=c("waic"))
spec_ord_mod_6=add_ic(spec_ord_mod_6,ic=c("waic"))

compare_ic(spec_ord_mod_1,spec_ord_mod_2,spec_ord_mod_4,
           spec_ord_mod_5,spec_ord_mod_6,ic=c("waic"))

compare_ic(spec_ord_mod_2,spec_ord_mod_5,
           spec_ord_mod_6,ic=c("waic"))

bayes_R2(spec_ord_mod_2)

pp_check(spec_ord_mod_2,type="violin_grouped",group=c("clim"))
pp_check(spec_ord_mod_2,type="violin_grouped",group=c("PolOrder"))

pp_check(spec_ord_mod_2,type="ecdf_overlay",nsamples=100)

pp_check(spec_ord_mod_2,type="freqpoly_grouped",group=c("clim"))
pp_check(spec_ord_mod_2,type="freqpoly_grouped",group=c("PolOrder"))

pp_check(spec_ord_mod_2,nsamples=100)+ 
  coord_cartesian(xlim = c(0, 50))



###################
#####PAIRWISE#####
#################

spec_ord_mod_emm <- spec_ord_mod_2 %>%
  emmeans( ~ PolOrder|clim) %>% 
  gather_emmeans_draws() %>% 
  mutate(intera = paste(PolOrder,clim , sep = ".")) 

##compact letter differences
spec_ord_cld<-spec_ord_mod_emm %>% 
  ungroup() %>% ## to get rid of unneeded columns
  select(.value, intera, .draw) %>% 
  spread(intera, .value) %>% 
  select(-.draw) %>% ## we need to get rid of all columns not containing draws
  cld_pmatrix()


###Compute HDI intervals  - check against CLD
spec_ord_hdi<-spec_ord_mod_emm %>% 
  ungroup %>% 
  compare_levels(.value, by = intera) %>% 
  mode_hdi()

spec_ord_hdi$group1=word(spec_ord_hdi$intera,1)
spec_ord_hdi$group2=word(spec_ord_hdi$intera,3)


write.csv(spec_ord_hdi,"data/outputs/spec_ord_hdi.csv")
write.csv(spec_ord_cld,"data/outputs/spec_ord_cld.csv")


###############
#####PLOT#####
#############

spec.plot=plot(marginal_effects(spec_ord_mod_2,
                                conditions = data.frame(int_tot = 100)))

spec.plot.data <-  spec.plot[[3]]$data



colnames(spec.plot.data)[1:2] <- c("Pollinator taxa","Climate zone")

spec.plot.data$`Pollinator taxa` <- revalue(spec.plot.data$`Pollinator taxa`,
                                            c("Bee" = "Bee",
                                              "Coleoptera" = "Coleoptera",
                                              "Lepidoptera" = "Lepidoptera",
                                              "Hymenoptera" = "Non-bee Hymenoptera",
                                              "Diptera" = "Non-syrphid Diptera",
                                              "Syrphidae" = "Syrphidae"))

spec.plot.data$`Pollinator taxa` <- factor(spec.plot.data$`Pollinator taxa`,
                                           levels=c("Bee","Coleoptera","Lepidoptera",
                                                    "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

sp.links.melt.6 <- sp.links.melt.5
colnames(sp.links.melt.6)[6]
colnames(sp.links.melt.6)[18]="Climate zone"

sp.links.melt.6$`Pollinator taxa` <- sp.links.melt.6$PolOrder

sp.links.melt.6$`Pollinator taxa` <- revalue(sp.links.melt.6$`Pollinator taxa`,
                                    c("Bee" = "Bee",
                                      "Coleoptera" = "Coleoptera",
                                      "Lepidoptera" = "Lepidoptera",
                                      "Hymenoptera" = "Non-bee Hymenoptera",
                                      "Diptera" = "Non-syrphid Diptera",
                                      "Syrphidae" = "Syrphidae"))
spec.plot.data$value=c("")
rbind.spec.plot <- rbind.fill(spec.plot.data,sp.links.melt.6)

rbind.spec.plot$`Climate zone` <- factor(rbind.spec.plot$`Climate zone`,levels=c("A","B","C","D","E"))

rbind.spec.plot$`Climate zone` <- revalue(rbind.spec.plot$`Climate zone`,c("A" = "Tropical",
                                                                           "B" = "Arid",
                                                                           "C" = "Temperate",
                                                                           "D" = "Continental",
                                                                           "E" = "Polar"))

rbind.spec.plot[rbind.spec.plot$`Pollinator taxa`%in%"Non-bee Hymenoptera",]
rbind.spec.plot$PolOrder <- revalue(rbind.spec.plot$PolOrder,c("Bee" = "A",
                                                               "Coleoptera" = "B",
                                                               "Lepidoptera" = "C",
                                                               "Hymenoptera" = "D",
                                                               "Diptera" = "E",
                                                               "Syrphidae"  =      "F"))

rbind.spec.plot$PolOrder <-  factor(rbind.spec.plot$PolOrder,levels=c("A","B","C","D","E","F"))
rbind.spec.plot <- droplevels(rbind.spec.plot)
rbind.spec.plot$value <- as.numeric(rbind.spec.plot$value)

spec.gg=ggplot(rbind.spec.plot,aes(x=`Pollinator taxa`,y=estimate__+1,col=`Pollinator taxa`))+
  geom_point(aes(y=value,col=PolOrder),show.legend = F,size=0.5,
             position=position_jitterdodge(dodge.width=0,jitter.width = 0.7,jitter.height = 0.2),
             alpha=0.5)+
  geom_point(aes(col=`Pollinator taxa`),
             size=2,show.legend = F)+
  geom_errorbar(aes(ymin=lower__+1,ymax=upper__+1,col=`Pollinator taxa`),
                width=0.4,show.legend =F)+
  facet_wrap(~`Climate zone`,ncol=5)+
  theme_bw()+
  ylab("Normalised generalism")+
  xlab(NULL)+
  scale_y_continuous(breaks=c(1,4,7,10,14),limits=c(1,14))+
  scale_color_manual(breaks=list_spp,values=plot_cols,name="Pollinator taxa")+
  theme(
    strip.text.x = element_blank(),
        panel.spacing = unit(0.5,"lines"),strip.background =element_rect(fill="white"),axis.text.x=element_blank(),aspect.ratio = 1,axis.ticks.x = element_blank())
spec.gg
ggsave(spec.gg,file="generalism plot.pdf",device = "pdf",dpi=320,width=15,height=5,units = c("in"))

