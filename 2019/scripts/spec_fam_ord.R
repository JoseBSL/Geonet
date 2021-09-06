
library(bayesplot)
library(brms)
library(plyr)
library(dplyr)
library(emmeans)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(stringi)
library(stringr)
library(tidyverse)

#############
##DATAFRAME##
#############

links.full.sub=links.full.sub %>% mutate_if(is.character,as.factor)

links.fam.sub=links.full.sub %>% 
  group_by(PolFamily) %>%
  filter(all(c("A", "B","C","D","E") %in% clim)) 

links.fam.sub1=links.fam.sub %>% 
  group_by(PolFamily) %>% 
  filter(n() >= 150) %>% droplevels()


levels(as.factor(links.fam.sub1$PolFamily))

#15 top families


spec_fam_sum=aggregate(value~Network+Reference+clim+ele+
                     Size+Latitude+PolOrder+
                     PolFamily,data=links.fam.sub1,
                     FUN = sum)

spec_fam_sum$net_length=aggregate(value~Network+Reference+clim+
                                ele+Size+Latitude+PolOrder+
                                PolFamily,data=links.fam.sub1,
                              FUN = length)[9]$value

spec_fam_sum$abs_lat=abs(spec_fam_sum$Latitude)

###Sum with offset
spec_fam_prior=prior(normal(0,1),class="b")+
  prior(normal(1,1),class="Intercept")+
  prior(normal(0,0.4),class="sd")

spec_fam_mod_1=brm(value ~ PolFamily * clim + 
                     offset(log(net_length)) +
                     (1|Reference/Network),
                   family=poisson(link="log"),
                   prior=spec_fam_prior,
                   cores=4,
                   chains=4,
                   data=spec_fam_sum)

spec_fam_mod_2=brm(value ~ PolFamily * clim + 
                     offset(log(net_length)) + 
                     (1|Reference/Network),
                   family=negbinomial(link="log"),
                   prior=spec_fam_prior,
                   cores=4,
                   chains=4,
                   data=spec_fam_sum)

spec_fam_mod_3=brm(value ~ PolFamily * abs_lat + 
                     offset(log(net_length)) + 
                     (1|Reference/Network),
                   family=negbinomial(link="log"),
                   prior=spec_fam_prior,
                   cores=4,
                   chains=4,
                   data=spec_fam_sum)


pp_check(spec_fam_mod_1,type="freqpoly_grouped",group=c("clim"))
pp_check(spec_fam_mod_1,type="freqpoly_grouped",group=c("PolFamily"))

pp_check(spec_fam_mod_2)

pp_check(spec_fam_mod_2,type="freqpoly_grouped",group=c("clim"))
pp_check(spec_fam_mod_2,type="freqpoly_grouped",group=c("PolFamily"))

pp_check(spec_fam_mod_2)

spec_fam_mod_1=add_ic(spec_fam_mod_1,ic=c("waic"))
spec_fam_mod_2=add_ic(spec_fam_mod_2,ic=c("waic"))
spec_fam_mod_3=add_ic(spec_fam_mod_3,ic=c("waic"))

compare_ic(spec_fam_mod_1,spec_fam_mod_2,spec_fam_mod_3,ic=c("waic"))


###################
#####PAIRWISE#####
#################
library(tidybayes)
spec_fam_mod_emm <- spec_fam_mod_2 %>%
  emmeans( ~ PolFamily|clim) %>% 
  gather_emmeans_draws() %>% 
  mutate(intera = paste(PolFamily,clim , sep = ".")) 

##compact letter differences
#spec_fam_cld<-spec_fam_mod_emm %>% 
  ungroup() %>% ## to get rid of unneeded columns
  select(.value, intera, .draw) %>% 
  spread(intera, .value) %>% 
  select(-.draw) %>% ## we need to get rid of all columns not containing draws
  cld_pmatrix()

#spec_fam_cld

###Compute HDI intervals  - check against CLD
spec_fam_hdi<-spec_fam_mod_emm %>% 
  ungroup %>% 
  compare_levels(.value, by = intera) %>% 
  mode_hdi()

spec_fam_hdi$group1=word(spec_fam_hdi$intera,1)
spec_fam_hdi$group2=word(spec_fam_hdi$intera,3)
write.csv(spec_fam_hdi,"data/outputs/spec_fam_hdi.csv")
#write.csv(spec_fam_cld,"data/outputs/spec_fam_cld.csv")


###############
#####PLOT#####
#############

spec_fam_groups=spec_fam_sum[!duplicated(spec_fam_sum$PolFamily),c("PolOrder","PolFamily")]
spec_fam_groups[spec_fam_groups$PolOrder%in%"Syrphidae","PolOrder"]="Diptera"

spec_fam_order=arrange(spec_fam_groups,PolOrder,PolFamily)

spec_fam_plots=plot(marginal_effects(spec_fam_mod_2))

spec_fam_plots[[3]]
Figure_Spec_Fam=spec_fam_plots[[3]]+theme_bw()+theme(axis.text.x = element_text(angle=90))+
  xlab("Family")+
  ylab("Specialisation")+
  scale_x_discrete(limits=as.character(spec_fam_order$PolFamily))
Figure_Spec_Fam
