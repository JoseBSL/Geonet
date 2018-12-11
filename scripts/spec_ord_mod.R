####
##Specialisation model
###

library(brms)
library(emmeans)
library(plotrix)
library(plyr)
library(dplyr)
links.full.sub
str(links.full.sub)

spec_mean=aggregate(value~Network+Reference+clim+ele+Size+
                    Latitude+PolOrder+PolFamily,data=links.full.sub,
                           FUN = mean)

spec_mean_sei=aggregate(value~Network+Reference+clim+ele+Size+
                        Latitude+PolOrder+PolFamily,data=links.full.sub,
                        FUN = function(x) c(mean = mean(x), se = std.error(x)))[9]
spec_mean_sei

spec_mean$sei=spec_mean_sei$value[,2]

####SUM
spec_sum=aggregate(value~Network+Reference+clim+ele+
                     Size+Latitude+PolOrder+
                     PolFamily,data=links.full.sub,
                    FUN = sum)

spec_sum$net_length=aggregate(value~Network+Reference+clim+
                                ele+Size+Latitude+PolOrder+
                                PolFamily,data=links.full.sub,
                   FUN = length)[9]$value

str(spec_sum)
###Sum with offset
spec_ord_prior=prior(normal(0,1),class="b")+
  prior(normal(1,1),class="Intercept")+
  prior(normal(0,0.4),class="sd")

spec_ord_mod_1=brm(value ~ PolOrder * clim + 
               offset(log(net_length)) +
               (1|Reference/Network),
               family=poisson(link="log"),
               prior=spec_ord_prior,
               cores=4,
               chains=4,
               data=spec_sum)
 
spec_ord_mod_2=brm(value ~ PolOrder * clim + 
               offset(log(net_length)) + 
               (1|Reference/Network),
               family=negbinomial(link="log"),
               prior=spec_ord_prior,
               cores=4,
               chains=4,
               data=spec_sum)

spec_ord_mod_3=brm(value ~ PolOrder * clim + 
                     (1|Reference/Network),
                   family=negbinomial(link="log"),
                   prior=spec_ord_prior,
                   cores=4,
                   chains=4,
                   data=spec_sum)

spec_ord_mod_4=brm(value ~ PolOrder * abs(Latitude) + 
                     offset(log(net_length)) + 
                     (1|Reference/Network),
                   family=negbinomial(link="log"),
                   prior=spec_ord_prior,
                   cores=4,
                   chains=4,
                   data=spec_sum)


pp_check(spec_ord_mod_1,type="freqpoly_grouped",group=c("clim"))
pp_check(spec_ord_mod_1,type="freqpoly_grouped",group=c("PolOrder"))

pp_check(spec_ord_mod_2)

pp_check(spec_ord_mod_2,type="freqpoly_grouped",group=c("clim"))
pp_check(spec_ord_mod_2,type="freqpoly_grouped",group=c("PolOrder"))

pp_check(spec_ord_mod_2)

spec_ord_mod_1=add_ic(spec_ord_mod_1,ic=c("waic"))
spec_ord_mod_2=add_ic(spec_ord_mod_2,ic=c("waic"))
spec_ord_mod_3=add_ic(spec_ord_mod_3,ic=c("waic"))
spec_ord_mod_4=add_ic(spec_ord_mod_4,ic=c("waic"))

compare_ic(spec_ord_mod_1,spec_ord_mod_2,spec_ord_mod_3,spec_ord_mod_4,ic=c("waic"))


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

write.csv(spec_ord_hdi,"data/outputs/spec_ord_hdi.csv")
write.csv(spec_ord_cld,"data/outputs/spec_ord_cld.csv")

spec_ord_hdi_edit=read.csv("data/outputs/spec_ord_hdi_edit.csv")

spec_ord_hdi_edit$group1=word(spec_ord_hdi_edit$intera,1)
spec_ord_hdi_edit$group2=word(spec_ord_hdi_edit$intera,3)

write.csv(spec_ord_hdi_edit,"data/outputs/spec_ord_hdi_edit2.csv")

###############
#####PLOT#####
#############

spec_ord_groups=spec_sum[!duplicated(spec_sum$PolFamily),c("PolOrder","PolFamily")]
spec_order=arrange(spec_ord_groups,PolOrder,PolFamily)

spec_ord_plots=plot(marginal_effects(spec_ord_mod_2))

spec_ord_plots[[3]]
library(DescTools)
spec_ord_plots[[3]]$data$clim <- reorder(spec_ord_plots[[3]]$data$clim, 
                                         new.order=c("A", "B", "C", "D","E"))

Figure_Spec_Ord=spec_ord_plots[[3]]+theme_bw()+
  xlab("Order")+
  ylab("Generalism") 
Figure_Spec_Ord





Fig_Spec=align_plots(Figure_Spec_Ord, Figure_Spec_Fam,align="hv", axis="tblr")
Fig_SpecA <- ggdraw(Fig_Spec[[1]])
Fig_SpecB <- ggdraw(Fig_Spec[[2]])

grid.arrange(Fig_SpecA,Fig_SpecB)
