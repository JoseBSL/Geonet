###JACCARD MODELS

library(rstan)
library(bayesplot)
library(brms)
library(plyr)
library(dplyr)
library(modelr)
library(plotrix)
library(tidybayes)

options(mc.cores = parallel::detectCores())
##DATAFRAME
jaccard=read.csv("data/processed/geonet_jaccard.csv",stringsAsFactors = TRUE,row.names=c("X"))
str(jaccard)

jaccard_sub=jaccard %>% 
  group_by(jac.fam)          %>% 
  filter(all(c("A", "B","C","D","E") %in% clim)) %>% 
  droplevels()




##ORDER LEVEL

##REMOVE NAs from DF
jaccard_order=jaccard_sub[!is.na(jaccard_sub$Order1) == TRUE,]
jaccard_order=jaccard_order[!is.na(jaccard_order$Order2) == TRUE,]

#jaccard_order$jac[jaccard_order$jac==0]=0.0001
#jaccard_order$jac[jaccard_order$jac==1]=0.9999




jaccard_order_mean=aggregate(jac~Reference+clim+ele+Size+
                             Latitude+jac.fam+jac.ord,data=jaccard_order,
                  FUN = mean)


##ADD STANDARD ERROR
jaccard_order_sei=aggregate(jac~Reference+clim+ele+Size+
                            Latitude+jac.fam+jac.ord,data=jaccard_order,
                          FUN = function(x) c(mean = mean(x), se = std.error(x)))[8]

jaccard_order_mean$jaccard=jaccard_order_sei$jac[,1]
jaccard_order_mean$se=jaccard_order_sei$jac[,2]

jaccard_order_length=aggregate(jac~Reference+clim+ele+Size+
                               Latitude+jac.fam+jac.ord,
                                       data=jaccard_order,
                            FUN = length)

jaccard_order_mean$net_length=jaccard_order_length$jac

#jaccard_order_mean=jaccard_order_mean[!is.na(jaccard_order_mean$sei)==TRUE,]

##PRIORS
#NULL FOR NOW

##FURTHER SUBSETTING
jaccard_fam_mean=jaccard_order_mean %>% 
  group_by(jac.fam)  %>% 
 filter(n() >= 10) 

jaccard_fam_mean=jaccard_order_mean %>% 
  group_by(jac.fam)          %>% 
  filter(all(c("A", "B","C","D","E") %in% clim)) %>% 
  droplevels()

str(jaccard_fam_mean)
levels(as.factor(jaccard_fam_mean$jac.ord))

##FIRST MODEL
jac.ord.sei=brm(exp(jac)|trunc(lb=exp(0),ub=exp(1))~clim*jac.ord+(1|Reference),
                family=gaussian,
                data=jaccard_fam_mean,
                chains=4,cores=4)

jac.ord.sei.0=brm(exp(jac)|trunc(ub=exp(1))~clim*jac.ord+(1|Reference),
                family=gaussian,
                data=jaccard_fam_mean,
                chains=4,cores=4)

jac.ord.sei.1=brm(exp(jac)|trunc(lb=exp(0),ub=exp(1))~clim*jac.ord+
                  offset(log(net_length))+(1|Reference),
                family=gaussian,
                data=jaccard_fam_mean,
                chains=4,cores=4)

pp_check(jac.brm.sei,nsamples=1000)

jac.fam.sei=brm(exp(jac)|trunc(lb=exp(0),ub=exp(1))~clim*jac.fam+(1|Reference),
                family=gaussian,
                data=jaccard_fam_mean,
                chains=4,cores=4)

jac.fam.sei.1=brm(exp(jac)|trunc(lb=exp(0),ub=exp(1))~clim*jac.fam+
                    offset(log(net_length))+(1|Reference),
                  family=gaussian,
                  data=jaccard_fam_mean,
                  chains=4,cores=4)

jac.ord.sei=add_ic(jac.ord.sei,ic=c("loo","waic"))
jac.ord.sei.1=add_ic(jac.ord.sei.1,ic=c("loo","waic"))

jac.fam.sei=add_ic(jac.fam.sei,ic=c("loo","waic"))
jac.fam.sei.1=add_ic(jac.fam.sei.1,ic=c("loo","waic"))


