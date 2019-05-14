#Prop links order model

library(rstan)
library(bayesplot)
library(brms)
library(plyr)
library(dplyr)

g.sub$Reference=as.factor(g.sub$Reference)
g.sub$Network=as.factor(g.sub$Network)
g.sub$PolOrder=as.factor(g.sub$PolOrder)
g.sub$clim=as.factor(g.sub$clim)
#g.sub$scale_ele=scale(g.sub$ele)
g.sub$abs_lat=abs(g.sub$Latitude)

##ord MOD

##ord prior
prop_ordint_prior <- prior(normal(0,5), class = Intercept) + 
  prior(normal(0,0.4), class = sd)

prop_ord_mod=brm(prop_links~1+(1|Reference/Network),
                 family=Beta(link = "logit"),inits=0,iter=2000,
                 prior=prop_ordint_prior,
                 control=list(max_treedepth=15),
                 data=g.sub,cores=4)

##ord prior
prop_ord_prior <- prior(normal(0,5), class = Intercept) +  
  prior(normal(0,2), class = b) + 
  prior(normal(0,10), class = phi) + 
  prior(normal(0,0.4), class = sd)

prop_ord_mod1=brm(prop_links~PolOrder*clim+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,
                  prior=prop_ord_prior,
                  control=list(adapt_delta=0.99),
                  data=g.sub,cores=4)

#marginal_effects(prop_ord_mod1,method=c("fitted"))

check_all_diagnostics(prop_ord_mod1$fit)
#plot(prop_ord_mod1)
np_prop_ord_mod1 <- nuts_params(prop_ord_mod1)
mcmc_nuts_energy(np_prop_ord_mod1,binwidth=1)

prop_ord_mod2=brm(prop_links~clim+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,
                  prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)

check_all_diagnostics(prop_ord_mod2$fit)
#plot(prop_ord_mod2)
np_prop_ord_mod2 <- nuts_params(prop_ord_mod2)
mcmc_nuts_energy(np_prop_ord_mod2,binwidth=1)

prop_ord_mod3=brm(prop_links~PolOrder+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,
                  prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)

check_all_diagnostics(prop_ord_mod3$fit)
#plot(prop_ord_mod3)
np_prop_ord_mod3 <- nuts_params(prop_ord_mod3)
mcmc_nuts_energy(np_prop_ord_mod3,binwidth=1)


prop_ord_mod4=brm(prop_links~PolOrder+clim+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,
                  prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)

check_all_diagnostics(prop_ord_mod4$fit)
#plot(prop_ord_mod4)
np_prop_ord_mod4 <- nuts_params(prop_ord_mod4)
mcmc_nuts_energy(np_prop_ord_mod4,binwidth=1)

prop_ord_mod5=brm(prop_links~PolOrder*abs_lat+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,
                  prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)


prop_ord_mod6=brm(prop_links~PolOrder+abs_lat+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,
                  prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)

prop_ord_mod7=brm(prop_links~PolOrder*abs_lat+I(abs_lat^2)+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,
                  prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)

prop_ord_mod8=brm(prop_links~PolOrder*abs_lat+I(abs_lat^2)+I(abs_lat^3)+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,
                  prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)

emmeans(prop_ord_mod6,"PolOrder")

prop_ord_mod=add_ic(prop_ord_mod,ic=c("loo","waic"))
prop_ord_mod1=add_ic(prop_ord_mod1,ic=c("loo","waic"))
prop_ord_mod2=add_ic(prop_ord_mod2,ic=c("loo","waic"))
prop_ord_mod3=add_ic(prop_ord_mod3,ic=c("loo","waic"))
prop_ord_mod4=add_ic(prop_ord_mod4,ic=c("loo","waic"))
prop_ord_mod5=add_ic(prop_ord_mod5,ic=c("loo","waic"))
prop_ord_mod6=add_ic(prop_ord_mod6,ic=c("loo","waic"))


compare_ic(prop_ord_mod,prop_ord_mod1,prop_ord_mod2,prop_ord_mod3,
           prop_ord_mod4,prop_ord_mod5,prop_ord_mod6,ic=c("waic"))
compare_ic(prop_ord_mod,prop_ord_mod1,prop_ord_mod2,prop_ord_mod3,
           prop_ord_mod4,prop_ord_mod5,prop_ord_mod6,ic=c("loo"))

compare_ic(prop_ord_mod1,prop_ord_mod5,ic=c("waic"))
###WEIGHTINGS
model_weights(prop_ord_mod,prop_ord_mod1,prop_ord_mod2,prop_ord_mod3,
              prop_ord_mod4,prop_ord_mod5,prop_ord_mod6,weights=c("waic"))

bayes_R2(prop_ord_mod) # Int only model
bayes_R2(prop_ord_mod1) # Clim * family model
bayes_R2(prop_ord_mod2) # Clim only
bayes_R2(prop_ord_mod3) # family only
bayes_R2(prop_ord_mod4) # Clim + family
bayes_R2(prop_ord_mod5)
bayes_R2(prop_ord_mod6)

pp_check(prop_ord_mod,nsamples=100)
pp_check(prop_ord_mod1,nsamples=100)
pp_check(prop_ord_mod2,nsamples=100)
pp_check(prop_ord_mod3,nsamples=100)
pp_check(prop_ord_mod4,nsamples=100)
pp_check(prop_ord_mod5,nsamples=100)
pp_check(prop_ord_mod6,nsamples=100)

###################
#####PAIRWISE#####
#################
prop_ord_emm <- prop_ord_mod1 %>%
  emmeans( ~ PolOrder|clim) %>% 
  gather_emmeans_draws() %>% 
  mutate(intera = paste(PolOrder,clim , sep = ".")) 

##compact letter differences
prop_ord_cld<-prop_ord_emm %>% 
  ungroup() %>% ## to get rid of unneeded columns
  select(.value, intera, .draw) %>% 
  spread(intera, .value) %>% 
  select(-.draw) %>% ## we need to get rid of all columns not containing draws
  cld_pmatrix()

prop_ord_cld

###Compute HDI intervals  - check against CLD
prop_ord_hdi<-prop_ord_emm %>% 
  ungroup %>% 
  compare_levels(.value, by = intera) %>% 
  mode_hdi()

write.csv(prop_ord_hdi,"data/outputs/prop_ord_hdi.csv")
write.csv(prop_ord_cld,"data/outputs/prop_ord_cld.csv")


######################
#####PLOT TYPE 2#####
####################

prop.plot=plot(marginal_effects(prop_ord_mod1))

prop.plot.data <-  prop.plot[[3]]$data



colnames(prop.plot.data)[1:2] <- c("Pollinator taxa","Climate zone")

prop.plot.data$`Pollinator taxa` <- revalue(prop.plot.data$`Pollinator taxa`,
                                            c("Bee" = "Bee",
                                              "Coleoptera" = "Coleoptera",
                                              "Lepidoptera" = "Lepidoptera",
                                              "Hymenoptera" = "Non-bee Hymenoptera",
                                              "Diptera" = "Non-syrphid Diptera",
                                              "Syrphidae" = "Syrphidae"))

prop.plot.data$`Pollinator taxa` <- factor(prop.plot.data$`Pollinator taxa`,
                                           levels=c("Bee","Coleoptera","Lepidoptera",
                                                    "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

g.sub2 <- g.sub
colnames(g.sub2)[6]
colnames(g.sub2)[18]="Climate zone"

g.sub2$`Pollinator taxa` <- g.sub2$PolOrder

g.sub2$`Pollinator taxa` <- revalue(g.sub2$`Pollinator taxa`,
                                   c("Bee" = "Bee",
                                     "Coleoptera" = "Coleoptera",
                                     "Lepidoptera" = "Lepidoptera",
                                     "Hymenoptera" = "Non-bee Hymenoptera",
                                     "Diptera" = "Non-syrphid Diptera",
                                     "Syrphidae" = "Syrphidae"))
prop.plot.data$prop_links=c("")
rbind.prop.plot <- rbind.fill(prop.plot.data,g.sub2)

rbind.prop.plot$`Climate zone` <- factor(rbind.prop.plot$`Climate zone`,levels=c("A","B","C","D","E"))

rbind.prop.plot$`Climate zone` <- revalue(rbind.prop.plot$`Climate zone`,c("A" = "Tropical",
                                                                           "B" = "Arid",
                                                                           "C" = "Temperate",
                                                                           "D" = "Continental",
                                                                           "E" = "Polar"))

rbind.prop.plot[rbind.prop.plot$`Pollinator taxa`%in%"Non-bee Hymenoptera",]
rbind.prop.plot$PolOrder <- revalue(rbind.prop.plot$PolOrder,c("Bee" = "A",
                                                               "Coleoptera" = "B",
                                                               "Lepidoptera" = "C",
                                                               "Hymenoptera" = "D",
                                                               "Diptera" = "E",
                                                               "Syrphidae"  =      "F"))

rbind.prop.plot$PolOrder <-  factor(rbind.prop.plot$PolOrder,levels=c("A","B","C","D","E","F"))
rbind.prop.plot <- droplevels(rbind.prop.plot)
rbind.prop.plot$prop_links <- as.numeric(rbind.prop.plot$prop_links)

prop.gg=ggplot(rbind.prop.plot,aes(x=`Pollinator taxa`,y=estimate__,col=`Pollinator taxa`))+
  geom_point(aes(y=prop_links,col=PolOrder),show.legend = F,size=0.5,
             position=position_jitterdodge(dodge.width=0,jitter.width = 0.7),
             alpha=0.5)+
  geom_point(aes(col=`Pollinator taxa`),
             size=2,show.legend = F)+
  geom_errorbar(aes(ymin=lower__,ymax=upper__,col=`Pollinator taxa`),
                width=0.4,show.legend = F)+
  facet_wrap(~`Climate zone`,ncol=5)+
  theme_bw()+
  ylab("Proportion of links")+
  xlab(NULL)+
  scale_color_manual(breaks=list_spp,values=plot_cols,name="Pollinator taxa")+
  theme(
        strip.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.text = element_text(face = "bold",size=14),
        panel.spacing = unit(0.5,"lines"),
        axis.text.x=element_blank(),
        aspect.ratio = 1,
        axis.ticks.x = element_blank())
prop.gg
ggsave(prop.gg,file="prop links plot.pdf",device = "pdf",dpi=320,width=15,height=5,units = c("in"))


##save models

save(prop_ord_mod,file="prop_ord_mod.RData",compress="xz")
save(prop_ord_mod1,file="prop_ord_mod1.RData",compress="xz")


waic(prop_ord_mod,prop_ord_mod1)
