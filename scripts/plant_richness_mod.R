###Correlation plant ~ poll richness

cor.test(reference$plant_richness,reference$poll_richness)

plot(reference$plant_richness,reference$poll_richness,
     col=as.numeric(as.factor(reference$clim)))


##Plant richness ~ Climate model

richness_prior=prior(normal(0,0.4),class="b")+
  prior(normal(0,2),class="Intercept")+
  prior(normal(0,1),class="sd")

richness_mod_1=brm(plant_richness ~ clim +
                     (1|Reference/Network),
                   family=poisson(link="log"),
                   control=list(adapt_delta=0.9,max_treedepth=15),
                   prior=richness_prior,
                   cores=4,
                   data=reference)

pp_check(richness_mod_1,nsamples=100)
bayes_R2(richness_mod_1)

boxplot(plant_richness~clim,reference)

richness_plot=plot(marginal_effects(richness_mod_1))

richness_plot[[1]]+
  theme_bw()+
  xlab("Climate zone")+
  ylab("Plant richness")

