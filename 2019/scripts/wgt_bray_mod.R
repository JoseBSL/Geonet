
library(magrittr)
library(plyr)
library(dplyr)
library(bayesplot)
library(forcats)
library(tidyr)
library(modelr)
library(tidybayes)
library(ggplot2)
library(ggstance)
library(ggridges)
library(cowplot)
library(rstan)
library(brms)
library(ggrepel)
library(plotrix)

options(stringsAsFactors = TRUE)
bray=read.csv("data/processed/bray.all.sub1.csv")
str(bray)

#clean up duplicate groupings
bray[bray$bray.ord%in%"Syrphidae_Bee",c("bray.ord")]="Bee_Syrphidae"
bray[bray$bray.ord%in%"Diptera_Coleoptera",c("bray.ord")]="Coleoptera_Diptera"
bray[bray$bray.ord%in%"Lepidoptera_Coleoptera",c("bray.ord")]="Coleoptera_Lepidoptera"
bray[bray$bray.ord%in%"Hymenoptera_Coleoptera",c("bray.ord")]="Coleoptera_Hymenoptera"
bray[bray$bray.ord%in%"Hymenoptera_Diptera",c("bray.ord")]="Diptera_Hymenoptera"
bray[bray$bray.ord%in%"Lepidoptera_Diptera",c("bray.ord")]="Diptera_Lepidoptera"
bray[bray$bray.ord%in%"Lepidoptera_Hymenoptera",c("bray.ord")]="Hymenoptera_Lepidoptera" 

############between within orders
bray$bw=c()
bray[bray$bray.ord%in%"Syrphidae_Syrphidae",c("bw")]="w"
bray[bray$bray.ord%in%"Syrphidae_Lepidoptera",c("bw")]="b"
bray[bray$bray.ord%in%"Syrphidae_Hymenoptera",c("bw")]="b"
bray[bray$bray.ord%in%"Syrphidae_Diptera",c("bw")]="w"
bray[bray$bray.ord%in%"Syrphidae_Coleoptera",c("bw")]="b"
bray[bray$bray.ord%in%"Lepidoptera_Lepidoptera",c("bw")]="w"
bray[bray$bray.ord%in%"Hymenoptera_Lepidoptera",c("bw")]="b"
bray[bray$bray.ord%in%"Hymenoptera_Hymenoptera",c("bw")]="w"
bray[bray$bray.ord%in%"Diptera_Lepidoptera",c("bw")]="b"
bray[bray$bray.ord%in%"Diptera_Hymenoptera",c("bw")]="b"
bray[bray$bray.ord%in%"Diptera_Diptera",c("bw")]="w"
bray[bray$bray.ord%in%"Coleoptera_Lepidoptera",c("bw")]="b"
bray[bray$bray.ord%in%"Coleoptera_Hymenoptera",c("bw")]="b"
bray[bray$bray.ord%in%"Coleoptera_Diptera",c("bw")]="b"
bray[bray$bray.ord%in%"Coleoptera_Coleoptera",c("bw")]="w"
bray[bray$bray.ord%in%"Bee_Syrphidae",c("bw")]="b"
bray[bray$bray.ord%in%"Bee_Lepidoptera",c("bw")]="b"
bray[bray$bray.ord%in%"Bee_Hymenoptera",c("bw")]="w"
bray[bray$bray.ord%in%"Bee_Diptera",c("bw")]="b"
bray[bray$bray.ord%in%"Bee_Coleoptera",c("bw")]="b"
bray[bray$bray.ord%in%"Bee_Bee",c("bw")]="w"

##################

bray_bee=bray[bray$bray.ord%in%"Bee_Bee",]

library(plotrix)
str(bray)
#bray=merge(bray,reference[,c("Reference","Network")],by="Network")

mean_se=aggregate(std.bray~Network+Reference+clim+ele+bray.ord+bw,data=bray,
                  FUN = function(x) c(mean = mean(x), se = std.error(x)))[7]
str(mean_se)
bray_se$z=mean_se$std.bray[,1]
bray_se$sei=mean_se$std.bray[,2]

write.csv(bray_se,"data/processed/bray_se.csv")

bray_se=read.csv("data/processed/bray_se.csv",stringsAsFactors = TRUE)

levels(bray_se$bray.ord)
##WITH STANDARD ERROR

bray_sei=bray_se[!is.na(bray_se$sei) == TRUE,]
bray_sei=bray_sei[!bray_sei$sei == 0,]
bray_sei=bray_sei[!is.na(bray_sei$z) == TRUE,]
levels(bray_sei$bray.ord)
range(bray_sei$z)

boxplot(z~clim,bray_sei)

bray.se.prior <- prior(normal(0,1), class = b) + prior(normal(0,0.4), class = sd)+
  prior(normal(0,0.4), class = sigma)

bray.brm.sei=brm(z|se(sei, sigma = TRUE)~clim*bray.ord+(1|Reference/Network),
                       prior=bray.se.prior,
                       data=bray_sei,
                       chains=4,cores=4,
                       control = list(adapt_delta = 0.9))
  
  
library(lme4)  
bray.lme=lmer(std.bray~clim*bray.fam+(1|Reference/Network),bray_bee)
library(lmerTest)
summary(bray.lme)

library(emmeans)
Em=emmeans(bray.lme, ~bray.fam|clim)
test(Em)

str(bray)
library(ggplot2)
ggplot(bray_bee,aes(y=std.bray,x=bray.fam))+
                    geom_boxplot()+
                    facet_wrap(~clim)+
                    geom_hline(yintercept=1.96, linetype="dashed", color = "red")+
                    geom_hline(yintercept=-1.96, linetype="dashed", color = "red")+
                    theme_bw()
  

  
plot(bray.brm.sei) 
bray.brm.sei2=brm(z|se(sei, sigma = TRUE)~clim*bray.ord*ele+(1|Reference/Network),
                   prior=bray.se.prior,
                   data=bray_sei,
                   chains=4,cores=4,
                   control = list(adapt_delta = 0.9))
  
bray.brm.sei3=brm(z|se(sei, sigma = TRUE)~clim*bw*ele+(1|Reference/Network),
                        prior=bray.se.prior,
                        data=bray_sei,
                        chains=4,cores=4,
                        control = list(adapt_delta = 0.9))

pp_check(bray.brm.sei,nsamples=100) # GOOD

ppc_violin_grouped(bray_sei$z, 
                   yrep = posterior_predict(bray.brm.sei, nsamples = 100), 
                   group = bray_sei$clim)

ppc_violin_grouped(bray_sei$z, 
                   yrep = posterior_predict(bray,brm.sei2, nsamples = 100), 
                   group = bray_sei$bray.ord)

check_all_diagnostics(bray.brm.sei$fit)

bayes_R2(bray.brm.sei)

##PLOT
library(tidybayes)

niche.plot1=bray_sei%>% droplevels() %>%
  add_fitted_draws(bray.brm.sei,n=100)%>%
  ggplot(aes(x=sei,y=bray.ord))+
  xlab("Z-score")+
  ylab("Order niche overlap")+
  stat_intervalh(aes(x=.value))+
  facet_grid(~clim, scale = "free_y")+
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_color_brewer()+
  stat_pointintervalh(aes(x=sei),pch=15,.width=c(0))+
  theme_bw()
niche.plot1

niche.plot2=bray_sei%>% droplevels() %>%
  add_fitted_draws(null.geo.brm.sei2,n=100)%>%
  ggplot(aes(x=z,y=bw))+
  xlab("Z-score")+
  ylab("Order overlap")+
  stat_intervalh(aes(x=.value))+
  facet_grid(~clim, scale = "free_y")+
  stat_pointintervalh(aes(x=z),pch=15,.width=c(0))+
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_color_brewer()+
  theme_bw()
niche.plot2



##BEE FAMILY CASE STUDY
str(bray_bee)

bee_mean_se=aggregate(std.bray~Network+Reference+clim+ele+bray.fam,data=bray_bee,
                  FUN = function(x) c(mean = mean(x), se = std.error(x)))
str(bee_mean_se)
bee_mean_se$z=bee_mean_se$std.bray[,1]
bee_mean_se$sei=bee_mean_se$std.bray[,2]

bray.bee.brm=brm(z|se(sei, sigma = TRUE)~clim*bray.fam+(1|Reference/Network),
                  prior=bray.se.prior,
                  data=bee_mean_se,
                  chains=4,cores=4)

str(bray_sei)

ggplot(bray_sei,aes(x=z,y=bray.ord))+geom_point()+facet_wrap(~clim)+
  geom_vline(xintercept = -1.96, linetype = "longdash") +
  geom_vline(xintercept = 1.96, linetype = "longdash")

bee.niche.plot=bee_mean_se%>% droplevels() %>%
  add_fitted_draws(model=bray.bee.brm,n=100,re_formula = NA)%>%
  ggplot(aes(x=z,y=bray.fam))
  xlab("Z-score")+
  ylab("Order overlap")
  stat_intervalh(aes(x=.value))+
  facet_grid(~clim, scale = "free_y")+
  stat_pointintervalh(aes(x=z),pch=15,.width=c(0))+
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_color_brewer()+
  theme_bw()
bee.niche.plot
