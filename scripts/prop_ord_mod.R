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
                 family=Beta(link = "logit"),inits=0,iter=2000,prior=prop_ordint_prior,
                 data=g.sub,cores=4)

##ord prior
prop_ord_prior <- prior(normal(0,5), class = Intercept) +  
  prior(normal(0,2), class = b) + 
  prior(normal(0,10), class = phi) + 
  prior(normal(0,0.4), class = sd)

prop_ord_mod1=brm(prop_links~PolOrder*clim+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)

#marginal_effects(prop_ord_mod1,method=c("fitted"))

check_all_diagnostics(prop_ord_mod1$fit)
#plot(prop_ord_mod1)
np_prop_ord_mod1 <- nuts_params(prop_ord_mod1)
mcmc_nuts_energy(np_prop_ord_mod1,binwidth=1)

prop_ord_mod2=brm(prop_links~clim+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)

check_all_diagnostics(prop_ord_mod2$fit)
#plot(prop_ord_mod2)
np_prop_ord_mod2 <- nuts_params(prop_ord_mod2)
mcmc_nuts_energy(np_prop_ord_mod2,binwidth=1)

prop_ord_mod3=brm(prop_links~PolOrder+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)

check_all_diagnostics(prop_ord_mod3$fit)
#plot(prop_ord_mod3)
np_prop_ord_mod3 <- nuts_params(prop_ord_mod3)
mcmc_nuts_energy(np_prop_ord_mod3,binwidth=1)


prop_ord_mod4=brm(prop_links~PolOrder+clim+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)

check_all_diagnostics(prop_ord_mod4$fit)
#plot(prop_ord_mod4)
np_prop_ord_mod4 <- nuts_params(prop_ord_mod4)
mcmc_nuts_energy(np_prop_ord_mod4,binwidth=1)

prop_ord_mod5=brm(prop_links~PolOrder*abs_lat+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,prior=prop_ord_prior,
                  control=list(adapt_delta=0.9),
                  data=g.sub,cores=4)


prop_ord_mod6=brm(prop_links~PolOrder+abs_lat+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=2000,prior=prop_ord_prior,
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

#############
####PLOT####
###########
library(tidybayes)

prop.plot1=g.sub%>% droplevels() %>%
  add_fitted_draws(prop_ord_mod1,n=100)%>%
  ggplot(aes(x=value,y=PolOrder))+
  stat_intervalh(aes(x=.value*100))+
  stat_pointintervalh(aes(x = .value*100), .width = c(.0),pch=15,col="red")+
  facet_grid(~clim, scale = "free_y")+
  coord_flip()+
  scale_color_brewer()+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))+
  xlab("Network links (%)")+
  ylab("Order")
prop.plot1

######################
#####PLOT TYPE 2#####
####################

plt_ord <- plot(marginal_effects(prop_ord_mod1))

plt_ord[[3]]+theme_bw()+theme(axis.text.x = element_text(angle=90))+
  xlab("Order")+
  ylab("Prop. of links")
