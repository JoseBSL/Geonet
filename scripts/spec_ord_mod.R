####
##Specialisation model
###


library(brms)
library(emmeans)
library(plotrix)
library(plyr)
library(rstan)
library(dplyr)
library(tidybayes)
library(tidyverse)
library(DescTools)
library(ggplot2)

sp.links.melt.5$clim <- as.factor(sp.links.melt.5$clim)
sp.links.melt.5$PolOrder <- as.factor(sp.links.melt.5$PolOrder)
sp.links.melt.5$Latitude <- abs(sp.links.melt.5$Latitude)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


###Sum with offset
spec_ord_prior=prior(normal(0,2),class="b")+
  prior(normal(0,5),class="Intercept")+
  prior(normal(0,2),class="sd")

spec_ord_mod_1=brm(value-1 ~ PolOrder * clim + 
               offset(log(int_tot)) +
               (1|Reference/Network),
               family=poisson(link="log"),
               control=list(adapt_delta=0.9,max_treedepth=15),
               prior=spec_ord_prior,
               cores=4,
               data=sp.links.melt.5)

spec_ord_mod_2=brm(value-1 ~ PolOrder * clim + 
               offset(log(int_tot)) + 
               (1|Reference/Network),
               family=negbinomial(link="log"),
               prior=spec_ord_prior,
               cores=4,
               data=sp.links.melt.5)


spec_ord_mod_3=brm(value-1 ~ PolOrder * clim + 
                     offset(log(int_tot)) + 
                     (1|Reference/Network),
                   family=zero_inflated_poisson(link="log"),
                   prior=spec_ord_prior,
                   control=list(adapt_delta=0.9,max_treedepth=15),
                   cores=4,
                   data=sp.links.melt.5)

spec_ord_mod_4=brm(value-1 ~ PolOrder * clim + 
                     offset(log(int_tot)) + 
                     (1|Reference/Network),
                   family=zero_inflated_negbinomial(link="log"),
                   prior=spec_ord_prior,
                   control=list(adapt_delta=0.9,max_treedepth=15),
                   cores=4,
                   data=sp.links.melt.5)

spec_ord_mod_5=brm(value-1 ~ PolOrder * Latitude + 
                     offset(log(int_tot)) + 
                     (1|Reference/Network),
                   family=negbinomial(link="log"),
                   prior=spec_ord_prior,
                   cores=4,
                   data=sp.links.melt.5)

spec_ord_prior2 = prior(normal(0,5),class="Intercept")+
  prior(normal(0,2),class="sd")

spec_ord_mod_6=brm(value-1 ~ 1 + 
                     (1|Reference/Network),
                   family=negbinomial(link="log"),
                   prior=spec_ord_prior2,
                   cores=4,
                   data=sp.links.melt.5)

#8:46

spec_ord_mod_1=add_ic(spec_ord_mod_1,ic=c("waic"))
spec_ord_mod_2=add_ic(spec_ord_mod_2,ic=c("waic"))
spec_ord_mod_3=add_ic(spec_ord_mod_3,ic=c("waic"))
spec_ord_mod_4=add_ic(spec_ord_mod_4,ic=c("waic"))
spec_ord_mod_5=add_ic(spec_ord_mod_5,ic=c("waic"))
spec_ord_mod_6=add_ic(spec_ord_mod_6,ic=c("waic"))

compare_ic(spec_ord_mod_1,spec_ord_mod_2,spec_ord_mod_4,
           spec_ord_mod_5,spec_ord_mod_6,ic=c("waic"))

compare_ic(spec_ord_mod_2,spec_ord_mod_5,
           spec_ord_mod_6,ic=c("waic"))

bayes_R2(spec_ord_mod_2)

pp_check(spec_ord_mod_2,type="violin_grouped",group=c("clim"))
pp_check(spec_ord_mod_2,type="violin_grouped",group=c("PolOrder"))

pp_check(spec_ord_mod_2,type="ecdf_overlay",nsamples=100)

pp_check(spec_ord_mod_2,type="freqpoly_grouped",group=c("clim"))
pp_check(spec_ord_mod_2,type="freqpoly_grouped",group=c("PolOrder"))

pp_check(spec_ord_mod_2,nsamples=100)+ 
  coord_cartesian(xlim = c(0, 50))



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

spec_ord_hdi$group1=word(spec_ord_hdi$intera,1)
spec_ord_hdi$group2=word(spec_ord_hdi$intera,3)


write.csv(spec_ord_hdi,"data/outputs/spec_ord_hdi.csv")
write.csv(spec_ord_cld,"data/outputs/spec_ord_cld.csv")


###############
#####PLOT#####
#############

#spec_ord_groups=spec_sum[!duplicated(spec_sum$PolFamily),c("PolOrder","PolFamily")]
#spec_order=arrange(spec_ord_groups,PolOrder,PolFamily)

group.colors.ord <- c("#1b9e77","#d95f02","#66a61e", "#e7298a", "#7570b3")
group.colors.ord <- c("#1b9e77","#d95f02","#66a61e", "#e7298a", "#7570b3")

spec_ord_plots=plot(marginal_effects(spec_ord_mod_2,
                     conditions = data.frame(int_tot = 100)))

Figure_Spec_Ord=spec_ord_plots[[3]]+theme_bw()+
  xlab("Taxa")+
  ylab("Species generalisation") +
  ggtitle("B) Species generalisation")+
  scale_colour_manual(values=group.colors.ord)

Figure_Spec_Ord #3 x 7

graph_spec_df=sp.links.melt.5 %>% 
  distinct(clim,PolOrder,Network,Reference,int_tot,Longitude,Latitude)

spec_predict=predict(spec_ord_mod_2,graph_spec_df,conditions = data.frame(int_tot = 100))

spec_graph_df=cbind(graph_spec_df,spec_predict)

write.csv(spec_graph_df,"data/outputs/spec_graph.csv")

specy=spec_ord_plots[[3]]$data

specy$clim=revalue(specy$clim,
                    c("A"="Tropical", 
                      "B"="Arid",
                      "C" = "Temperate",
                      "D" = "Continental",
                      "E" = "Polar"))

specy$PolOrder=revalue(specy$PolOrder,
                        c("Diptera" = "Non-syrphid Diptera",
                          "Hymenoptera" = "Non-bee Hymenoptera"))


group.colors.ord <- c("#1b9e77","#d95f02","#66a61e", "#e7298a", "#7570b3","#e6ab02")
pd <- position_dodge(width=0.4)
specy_plot=ggplot(specy,aes(x=clim,y=estimate__,col=PolOrder))+
  geom_point(position = pd,size=2)+
  geom_errorbar(aes(ymin=lower__,ymax=upper__),position = pd,width=0.4)+
  scale_color_manual(values=group.colors.ord)+
  ggtitle("B) Pollinator generalism")+
  xlab("Climate zone")+
  ylab("No. of plant partners")+
  theme_bw()+
  theme(legend.position="none")

specy_plot
#Plot together
library(cowplot)
library(gridExtra)
library(grid)
Fig1=align_plots(proppy_plot,specy_plot,align="hv", axis="tblr")
Fig1A <- ggdraw(Fig1[[1]])
Fig1B <- ggdraw(Fig1[[2]])
Fig.one=grid.arrange(Fig1A,Fig1B,ncol=1,nrow=2) #7 x 14 inches
Fig.one
ggsave(Fig.one,file="graphs/Figure1.pdf",width = 8.5, height = 6,
       units = c("in"))
