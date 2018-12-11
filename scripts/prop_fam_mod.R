#Prop links model

library(rstan)
library(bayesplot)
library(brms)
library(plyr)
library(dplyr)

#g.sub$Reference=as.factor(g.sub$Reference)
#g.sub$Network=as.factor(g.sub$Network)
#g.sub$PolOrder=as.factor(g.sub$PolOrder)
#g.sub$clim=as.factor(g.sub$clim)
#g.sub$scale_ele=scale(g.sub$ele)
#g.sub$abs_lat=abs(g.sub$Latitude)

g_fam_sub_6$Reference=as.factor(g_fam_sub_6$Reference)
g_fam_sub_6$Network=as.factor(g_fam_sub_6$Network)
g_fam_sub_6$PolOrder=as.factor(g_fam_sub_6$PolOrder)
g_fam_sub_6$PolFamily=as.factor(g_fam_sub_6$PolFamily)
g_fam_sub_6$clim=as.factor(g_fam_sub_6$clim)
g_fam_sub_6$scale_ele=scale(g_fam_sub_6$ele)
g_fam_sub_6$abs_lat=abs(g_fam_sub_6$Latitude)

##prior
prop_prior <- prior(normal(0,5), class = Intercept) + 
             prior(normal(0,25), class = phi) + 
             prior(normal(0,0.4), class = sd)

##FAM MOD
prop_fam_mod=brm(prop_links~1+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=4000,prior=prop_prior,
                  data=g_fam_sub_6,cores=4)

##fam prior
propprior <- prior(normal(0,5), class = Intercept) +  
  prior(normal(0,25), class = phi) + 
  prior(normal(0,2), class = b) + 
  prior(normal(0,0.4), class = sd)

prop_fam_mod1=brm(prop_links~PolFamily*clim+(1|Reference/Network),
              family=Beta(link = "logit"),inits=0,iter=4000,prior=propprior,
              data=g_fam_sub_6,cores=4)
#marginal_effects(prop_fam_mod1,method=c("fitted"))

check_all_diagnostics(prop_fam_mod1$fit)
#plot(prop_fam_mod1)
np_prop_fam_mod1 <- nuts_params(prop_fam_mod1)
mcmc_nuts_energy(np_prop_fam_mod1,binwidth=1)

prop_fam_mod2=brm(prop_links~clim+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=4000,prior=propprior,
                  data=g_fam_sub_6,cores=4)

check_all_diagnostics(prop_fam_mod2$fit)
#plot(prop_fam_mod2)
np_prop_fam_mod2 <- nuts_params(prop_fam_mod2)
mcmc_nuts_energy(np_prop_fam_mod2,binwidth=1)

prop_fam_mod3=brm(prop_links~PolFamily+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=4000,prior=propprior,
                  data=g_fam_sub_6,cores=4)

check_all_diagnostics(prop_fam_mod3$fit)
#plot(prop_fam_mod3)
np_prop_fam_mod3 <- nuts_params(prop_fam_mod3)
mcmc_nuts_energy(np_prop_fam_mod3,binwidth=1)

prop_fam_mod4=brm(prop_links~PolFamily+clim+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=4000,prior=propprior,
                  data=g_fam_sub_6,cores=4)

check_all_diagnostics(prop_fam_mod4$fit)
#plot(prop_fam_mod4)
np_prop_fam_mod4 <- nuts_params(prop_fam_mod4)
mcmc_nuts_energy(np_prop_fam_mod4,binwidth=1)

prop_fam_mod5=brm(prop_links~PolFamily*abs_lat+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=4000,prior=propprior,
                  data=g_fam_sub_6,cores=4)


prop_fam_mod6=brm(prop_links~PolFamily+abs_lat+(1|Reference/Network),
                  family=Beta(link = "logit"),inits=0,iter=4000,prior=propprior,
                  data=g_fam_sub_6,cores=4)

prop_fam_mod=add_ic(prop_fam_mod,ic=c("loo","waic"))
prop_fam_mod1=add_ic(prop_fam_mod1,ic=c("loo","waic"))
prop_fam_mod2=add_ic(prop_fam_mod2,ic=c("loo","waic"))
prop_fam_mod3=add_ic(prop_fam_mod3,ic=c("loo","waic"))
prop_fam_mod4=add_ic(prop_fam_mod4,ic=c("loo","waic"))
prop_fam_mod5=add_ic(prop_fam_mod5,ic=c("loo","waic"))
prop_fam_mod6=add_ic(prop_fam_mod6,ic=c("loo","waic"))


compare_ic(prop_fam_mod,prop_fam_mod1,prop_fam_mod2,prop_fam_mod3,
           prop_fam_mod4,prop_fam_mod5,prop_fam_mod6,ic=c("waic"))
compare_ic(prop_fam_mod,prop_fam_mod1,prop_fam_mod2,prop_fam_mod3,
           prop_fam_mod4,prop_fam_mod5,prop_fam_mod6,ic=c("loo"))


###WEIGHTINGS
model_weights(prop_fam_mod,prop_fam_mod1,prop_fam_mod2,prop_fam_mod3,
              prop_fam_mod4,prop_fam_mod5,prop_fam_mod6,weights=c("waic"))

bayes_R2(prop_fam_mod) # Int only model
bayes_R2(prop_fam_mod1) # Clim * Family model
bayes_R2(prop_fam_mod2) # Clim only
bayes_R2(prop_fam_mod3) # Family only
bayes_R2(prop_fam_mod4) # Clim + Family
bayes_R2(prop_fam_mod5) # Lat * Family
bayes_R2(prop_fam_mod6) # Lat + Family

marginal_effects(prop_fam_mod1)


pp_check(prop_fam_mod1,nsamples=100)
pp_check(prop_fam_mod2,nsamples=100)
pp_check(prop_fam_mod3,nsamples=100)
pp_check(prop_fam_mod4,nsamples=100)
pp_check(prop_fam_mod5,nsamples=100)
pp_check(prop_fam_mod6,nsamples=100)

plot(prop_fam_mod1)

###WEIGHTINGS

##PLOT
library(tidybayes)
prop.fam.plot1=g_fam_sub_6%>% droplevels() %>%
  add_fitted_draws(prop_fam_mod1,n=100)%>%
  ggplot(aes(x=value,y=as.factor(PolFamily)))+
  stat_intervalh(aes(x=.value*100))+
  facet_wrap(~clim, scale = "free_y",nrow=5)+
  scale_color_brewer()+
  coord_flip()+
  theme_bw()+
  xlab("Network links (%)")+
  ylab("Order")
prop.fam.plot1

###################
#####PAIRWISE#####
#################
prop_fam_emm <- prop_fam_mod1 %>%
  emmeans( ~ PolFamily|clim) %>% 
  gather_emmeans_draws() %>% 
  mutate(intera = paste(PolFamily,clim , sep = ".")) 

##compact letter differences
prop_fam_cld<-prop_fam_emm %>% 
  ungroup() %>% ## to get rid of unneeded columns
  select(.value, intera, .draw) %>% 
  spread(intera, .value) %>% 
  select(-.draw) %>% ## we need to get rid of all columns not containing draws
  cld_pmatrix()

prop_fam_cld

###Compute HDI intervals  - check against CLD
prop_fam_hdi<-prop_fam_emm %>% 
  ungroup %>% 
  compare_levels(.value, by = intera) %>% 
  mode_hdi()


prop_fam_hdi$group1=word(prop_fam_hdi$intera,1)
prop_fam_hdi$group2=word(prop_fam_hdi$intera,3)


write.csv(prop_fam_hdi,"data/outputs/prop_fam_hdi.csv")
write.csv(prop_fam_cld,"data/outputs/prop_fam_cld.csv")

str(prop_fam_hdi)

word(prop_fam_hdi$intera,3)[1]


############
###PLOT####
##########

#FOR ORDERING X-AXIS
fam_groups=g_fam_sub_6[!duplicated(g_fam_sub_6$PolFamily),c("PolOrder","PolFamily")]
fam_groups[fam_groups$PolOrder%in%"Syrphidae","PolOrder"]="Diptera"

fam_order=arrange(fam_groups,PolOrder,PolFamily)


plt <- plot(marginal_effects(prop_fam_mod1))


plt[[3]]+theme_bw()+theme(axis.text.x = element_text(angle=90))+
xlab("Family")+
  ylab("Prop. of links")+
  scale_x_discrete(limits=as.character(fam_order$PolFamily))





