###JACCARD MODELS - BEES

library(rstan)
library(bayesplot)
library(brms)
library(plyr)
library(dplyr)
library(modelr)
library(plotrix)
library(tidybayes)

##JUST BEES
jaccard_sub_bee=jaccard_sub[jaccard_sub$Order1=="Bee",] %>% 
  droplevels()
jaccard_sub_bee=jaccard_sub_bee[jaccard_sub_bee$Order2=="Bee",] %>% 
  droplevels()


jaccard_bee_mean=aggregate(jac~Reference+Network+clim+ele+Size+
                               Latitude+jac.fam+jac.ord,data=jaccard_sub_bee,
                             FUN = mean)


##ADD STANDARD ERROR
jaccard_bee_sei=aggregate(jac~Reference+Network+clim+ele+Size+
                              Latitude+jac.fam+jac.ord,data=jaccard_sub_bee,
                            FUN = function(x) c(mean = mean(x), se = std.error(x)))[9]

jaccard_bee_mean$jaccard=jaccard_bee_sei$jac[,1]
jaccard_bee_mean$se=jaccard_bee_sei$jac[,2]

jaccard_bee_length=aggregate(jac~Reference+Network+clim+ele+Size+
                                 Latitude+jac.fam+jac.ord,
                               data=jaccard_sub_bee,
                               FUN = length)

jaccard_bee_mean$net_length=jaccard_bee_length$jac


##########################
###BEE FAMILY MODEL 1####
########################

jac.bee.prior<- prior(normal(0,100), class = Intercept) +  
  prior(normal(0,20), class = b) + 
  prior(normal(0,20), class = sd)

jac.bee.fam.sei=brm(exp(jac)|trunc(ub=exp(1))~clim*jac.fam+(1|Reference/Network),
                family=student(),
                data=jaccard_bee_mean,
                prior=jac.bee.prior,
                chains=4,cores=4)

jac.bee.fam.sei.1=brm(exp(jac)|trunc(ub=exp(1))|se(sei,sigma=TRUE)~clim*jac.fam+(1|Reference/Network),
                    family=gaussian(),
                    data=jaccard_bee_mean,
                    prior=jac.bee.prior,
                    chains=4,cores=4)

jac.bee.fam.sei
pp_check(jac.bee.fam.sei,nsamples=100)

