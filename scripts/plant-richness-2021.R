###plant richness
plant.values <- lapply(myfiles,function(x) nrow(x))

plant.values.df <- as.data.frame(do.call("rbind", plant.values))
colnames(plant.values.df) <- "plant_richness"

plant.values.df$Network <- rownames(plant.values.df)
plant.values.df$Network <- gsub("_"," ",plant.values.df$Network)

reference.plant <- reference%>%
  left_join(plant.values.df)

###model

##Plant richness ~ Climate model
richness_prior=prior(normal(0,2),class="Intercept")+
  prior(normal(0,1),class="b")+
  prior(normal(0,1),class="sd")

richness_mod <- brm(plant_richness ~ clim +
                     (1|Reference),
                   family=negbinomial(link="log"),
                   #control=list(adapt_delta=0.9,max_treedepth=15),
                   prior=richness_prior,
                   cores=4,
                   data=reference.plant)

pp_check(richness_mod,ndraws=100)
r2(richness_mod)

richness.plot <-  as.data.frame(emmeans(richness_mod,~clim,
                           type="response"))

richness.plot$`Climate zone` <- revalue(richness.plot$clim,c("A" = "Tropical",
                                                                           "B" = "Arid",
                                                                           "C" = "Temperate",
                                                                           "D" = "Continental",
                                                                           "E" = "Polar"))

richness.plot$`Climate zone` <- factor(richness.plot$`Climate zone`,
                                       levels=c("Tropical",
                                                "Arid",
                                                "Temperate",
                                                "Continental",
                                                "Polar"))
reference.plant$`Climate zone` <- revalue(reference.plant$clim,c("A" = "Tropical",
                                                             "B" = "Arid",
                                                             "C" = "Temperate",
                                                             "D" = "Continental",
                                                             "E" = "Polar"))

reference.plant$`Climate zone` <- factor(reference.plant$`Climate zone`,
                                       levels=c("Tropical",
                                                "Arid",
                                                "Temperate",
                                                "Continental",
                                                "Polar"))
rich.gg <- ggplot(richness.plot,aes(x=`Climate zone`,
                                      y=prob))+
  geom_point(data=reference.plant,aes(x=`Climate zone`,
                                      y=plant_richness),
             show.legend = F,size=2,shape=21,col="black",fill="grey",
             position=position_jitter(width = 0.1,
                                      height = 0.1),
             alpha=0.5)+
  geom_point(size=3,show.legend = F)+
  geom_errorbar(aes(ymin=lower.HPD,
                    ymax=upper.HPD),
                width=0.2,show.legend =F)+
#  facet_wrap(~`Climate zone`,ncol=5)+
  theme_bw()+
  ylab("Plant richness")+
  xlab(NULL)+
  #scale_y_log10()+
  #scale_y_continuous(breaks=c(1,4,7,10,14),limits=c(1,14))+
  #scale_color_manual(breaks=list_spp,
  #                   values=plot_cols,name="Pollinator taxa")+
  theme(strip.text.x = element_blank(),
        panel.spacing = unit(0.5,"lines"),
        strip.background =element_rect(fill="white"),
        #axis.text.x=element_blank(),
        axis.text.x=element_text(angle = 45, vjust = 1, hjust=1),
        aspect.ratio = 1,axis.ticks.x = element_blank())

ggsave(rich.gg,file="plots/fig SX - plant richness.jpg",
       width=3,
       height=3,
       device = "jpg",
       units="in")
ggsave(rich.gg,file="plots/fig SX - plant richness.pdf",
       width=3,
       height=3,
       device = "pdf",
       units="in")
rich.gg
