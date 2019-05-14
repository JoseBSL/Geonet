#network links poisson/negbinomial regression

library(rstan)
library(bayesplot)
library(brms)
library(plyr)
library(dplyr)
library(ggplot2)

##data frame

g_fam=geonet%>%
  group_by(Network, PolOrder,PolFamily) %>%
  summarise(order_links=sum(Int))

g_fam2=g_fam%>%
  group_by(Network) %>%
  summarise(int_tot=sum(order_links))

g_fam3=merge(g_fam,g_fam2)
g_fam3$prop_links=g_fam3$order_links/g_fam3$int_tot

g_fam4=merge(g_fam3,reference,by="Network")

#subset data by insect order
g_fam_sub <- subset(g_fam4, PolOrder %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))

g_fam_sub$Reference=as.factor(g_fam_sub$Reference)
g_fam_sub$Network=as.factor(g_fam_sub$Network)
g_fam_sub$PolOrder=as.factor(g_fam_sub$PolOrder)
g_fam_sub$clim=as.factor(g_fam_sub$clim)
g_fam_sub$scale_ele=scale(g_fam_sub$ele)
g_fam_sub$abs_lat=abs(g_fam_sub$Latitude)

link_prior=prior(normal(0,2),class="b")+
           prior(normal(0,2),class="Intercept")+
          prior(normal(0,1),class="sd")

link_ord_mod1=brm(order_links~PolOrder*clim+
                    (1|Reference/Network),
                  family=poisson(),iter=2000,
                  prior=link_prior,
                  data=g_fam_sub,cores=4)

link_ord_mod2=brm(order_links~PolOrder*clim+offset(log(int_tot))+
                    (1|Reference/Network),
                  family=poisson(),iter=2000,
                  prior=link_prior,
                  data=g_fam_sub,cores=4)

link_ord_mod3=brm(order_links~PolOrder*abs_lat+offset(log(int_tot))+
                    (1|Reference/Network),
                  family=poisson(),iter=2000,
                  prior=link_prior,
                  data=g_fam_sub,cores=4)


launch_shinystan(link_ord_mod2)

g_fam_sub_vector=g_fam_sub$order_links
link_ord_mod1=add_ic(link_ord_mod1,ic=c("waic"))
link_ord_mod2=add_ic(link_ord_mod2,ic=c("waic"))
link_ord_mod3=add_ic(link_ord_mod3,ic=c("waic"))

compare_ic(link_ord_mod1,link_ord_mod2,link_ord_mod3,ic=c("waic"))


link_plot=plot(marginal_effects(link_ord_mod2))

link_plot[[3]]+theme_bw()+theme(axis.text.x = element_text(angle=90))+
  xlab("Family")+
  ylab("No. links")

link_plot[[3]]
###################
#####PAIRWISE#####
#################
link_ord_emm <- link_ord_mod2 %>%
  emmeans( ~ PolOrder|clim) %>% 
  gather_emmeans_draws() %>% 
  mutate(intera = paste(PolOrder,clim , sep = ".")) 

##compact letter differences
link_ord_cld<-link_ord_emm %>% 
  ungroup() %>% ## to get rid of unneeded columns
  select(.value, intera, .draw) %>% 
  spread(intera, .value) %>% 
  select(-.draw) %>% ## we need to get rid of all columns not containing draws
  cld_pmatrix()

link_ord_cld

###Compute HDI intervals  - check against CLD
link_ord_hdi<-link_ord_emm %>% 
  ungroup %>% 
  compare_levels(.value, by = intera) %>% 
  mode_hdi()


link_ord_hdi$group1=word(link_ord_hdi$intera,1)
link_ord_hdi$group2=word(link_ord_hdi$intera,3)


write.csv(link_ord_hdi,"data/outputs/link_ord_hdi.csv")
write.csv(link_ord_cld,"data/outputs/link_ord_cld.csv")


