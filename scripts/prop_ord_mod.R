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
group.colors.ord <- c("#1b9e77","#d95f02","#66a61e", "#e7298a", "#7570b3","#e6ab02")
#1b9e77
#d95f02
#66a61e
#e7298a
#7570b3
#e6ab02
plt_ord <- plot(marginal_effects(prop_ord_mod1))

proppy=plt_ord[[3]]$data

proppy$clim=revalue(proppy$clim,
                           c("A"="Tropical", 
                             "B"="Arid",
                             "C" = "Temperate",
                             "D" = "Continental",
                             "E" = "Polar"))

proppy$PolOrder=revalue(proppy$PolOrder,
                    c("Diptera" = "Non-syrphid Diptera",
                      "Hymenoptera" = "Non-bee Hymenoptera"))

pd <- position_dodge(width=0.4)
proppy_plot=ggplot(proppy,aes(x=clim,y=estimate__,col=PolOrder))+
  geom_point(position = pd,size=2)+
  geom_errorbar(aes(ymin=lower__,ymax=upper__),position = pd,width=0.4)+
  scale_color_manual(values=group.colors.ord)+
  xlab(NULL)+
  ylab("Prop. of links")+
  labs(color='Pollinator taxa')+
  theme_bw()

proppy_plot

prop_plot=plt_ord[[3]]+theme_bw()+theme()+
  xlab("Taxa")+
  ylab("Prop. of links")+
  ggtitle("A) Network links")+
  scale_color_manual(values=group.colors.ord)

plt_ord2 <- plot(marginal_effects(prop_ord_mod5))

prop_plot2=plt_ord2[[3]]+theme_bw()+theme()+
  facet_wrap(~PolOrder)+
  xlab("Latitude")+
  ylab("Prop. of links")+
  ggtitle("A) Proportion of network links")+
  scale_fill_brewer(palette="Dark2")+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position="none")

prop_plot2

##save models

save(prop_ord_mod,file="prop_ord_mod.RData",compress="xz")
save(prop_ord_mod1,file="prop_ord_mod1.RData",compress="xz")


waic(prop_ord_mod,prop_ord_mod1)
