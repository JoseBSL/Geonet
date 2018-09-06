
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

nullg=read.csv("data/processed/null_niche.csv")

#clean up duplicate groupings
nullg[nullg$jac.ord%in%"Syrphidae_Bee",c("jac.ord")]="Bee_Syrphidae"
nullg[nullg$jac.ord%in%"Diptera_Coleoptera",c("jac.ord")]="Coleoptera_Diptera"
nullg[nullg$jac.ord%in%"Lepidoptera_Coleoptera",c("jac.ord")]="Coleoptera_Lepidoptera"
nullg[nullg$jac.ord%in%"Hymenoptera_Coleoptera",c("jac.ord")]="Coleoptera_Hymenoptera"
nullg[nullg$jac.ord%in%"Hymenoptera_Diptera",c("jac.ord")]="Diptera_Hymenoptera"
nullg[nullg$jac.ord%in%"Lepidoptera_Diptera",c("jac.ord")]="Diptera_Lepidoptera"
nullg[nullg$jac.ord%in%"Lepidoptera_Hymenoptera",c("jac.ord")]="Hymenoptera_Lepidoptera" 

############between within orders
nullg$bw=c()
nullg[nullg$jac.ord%in%"Syrphidae_Syrphidae",c("bw")]="w"
nullg[nullg$jac.ord%in%"Syrphidae_Lepidoptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Syrphidae_Hymenoptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Syrphidae_Diptera",c("bw")]="w"
nullg[nullg$jac.ord%in%"Syrphidae_Coleoptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Lepidoptera_Lepidoptera",c("bw")]="w"
nullg[nullg$jac.ord%in%"Hymenoptera_Lepidoptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Hymenoptera_Hymenoptera",c("bw")]="w"
nullg[nullg$jac.ord%in%"Diptera_Lepidoptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Diptera_Hymenoptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Diptera_Diptera",c("bw")]="w"
nullg[nullg$jac.ord%in%"Coleoptera_Lepidoptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Coleoptera_Hymenoptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Coleoptera_Diptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Coleoptera_Coleoptera",c("bw")]="w"
nullg[nullg$jac.ord%in%"Bee_Syrphidae",c("bw")]="b"
nullg[nullg$jac.ord%in%"Bee_Lepidoptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Bee_Hymenoptera",c("bw")]="w"
nullg[nullg$jac.ord%in%"Bee_Diptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Bee_Coleoptera",c("bw")]="b"
nullg[nullg$jac.ord%in%"Bee_Bee",c("bw")]="w"

##################



nullgeo=aggregate(z~Network+clim+ele+jac.ord+bw,data=nullg,FUN="mean")%>% droplevels()

mean_se=aggregate(z~Network+clim+ele+jac.ord+bw,data=nullg,
                  FUN = function(x) c(mean = mean(x), se = std.error(x)))[6]

nullgeo$z=mean_se$z[,1]
nullgeo$sei=mean_se$z[,2]

write.csv(nullgeo,"data/processed/nullgeo.csv")

nullgeo=read.csv("data/processed/nullgeo.csv")

levels(nullgeo$jac.ord)
##WITH STANDARD ERROR

nullgeo.se=nullgeo[!is.na(nullgeo$sei) == TRUE,]
nullgeo.se=nullgeo.se[!nullgeo.se$sei == 0,]
nullgeo.se=nullgeo.se[!is.na(nullgeo.se$z) == TRUE,]
levels(nullgeo.se$jac.ord)
range(nullgeo.se$sei)

range(nullgeo.se$z)

nullgeoprior <- prior(normal(0,2), class = b) + prior(normal(0,1), class = sd)+
                prior(normal(0,1), class = sigma)

null.geo.brm.sei=brm(z|se(sei, sigma = TRUE)~clim*jac.ord+(1|Network),
                      prior=nullgeoprior,
                     data=nullgeo.se,
                     chains=4,cores=4,
                     control = list(adapt_delta = 0.9))

null.geo.brm.sei2=brm(z|se(sei, sigma = TRUE)~clim*bw+(1|Network),
                     prior=nullgeoprior,
                     data=nullgeo.se,
                     chains=4,cores=4,
                     control = list(adapt_delta = 0.9))

pp_check(null.geo.brm.sei,nsamples=100) # GOOD

ppc_violin_grouped(nullgeo.se$z, 
                   yrep = posterior_predict(null.geo.brm.sei, nsamples = 100), 
                   group = nullgeo.se$clim)

ppc_violin_grouped(nullgeo.se$z, 
                   yrep = posterior_predict(null.geo.brm.sei, nsamples = 100), 
                   group = nullgeo.se$jac.ord)

check_all_diagnostics(null.geo.brm.sei$fit)

bayes_R2(null.geo.brm.sei)

##PLOT
library(tidybayes)

niche.plot1=nullgeo.se%>% droplevels() %>%
  add_fitted_draws(null.geo.brm.sei,n=100)%>%
  ggplot(aes(x=z,y=jac.ord))+
  xlab("Z-score")+
  ylab("Order niche overlap")+
  stat_intervalh(aes(x=.value))+
  facet_grid(~clim, scale = "free_y")+
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_color_brewer()+
  stat_pointintervalh(aes(x=z),pch=15,.width=c(0))+
  theme_bw()
niche.plot1

niche.plot2=nullgeo.se%>% droplevels() %>%
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

ppc_intervals_grouped(nullgeo.se$z,yrep = posterior_predict(null.geo.brm.sei, nsamples = 100), 
              x = nullgeo.se$ele,group=nullgeo.se$jac.ord)

              str(nullgeo.se)
              
              
sum(geonet[unique(geonet$PolFamily%in%"Formicidae"),])
            
table(geo.uni[geo.uni$PolFamily%in%"Formicidae",c("PolFamily")])
