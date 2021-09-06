#latitude differences

#prop links
loo_compare(prop_ord_mod1,prop_ord_mod5,criterion="waic")
#-23.8 13.0

#spec
loo_compare(spec_ord_mod_2,spec_ord_mod_5,criterion="waic")
#-18 12.5


#betw
loo_compare(betw.brm2,betw.brm4,criterion="waic")
#-10.0, 14.6


#closeness
loo_compare(cent.brm2,cent.brm3,criterion="waic")
#-22.6, 12.8  


###LATITUDE GRAPHS


prop.lat.plot=plot(marginal_effects(prop_ord_mod5))
prop.lat.plot <- prop.lat.plot[[3]]

spec.lat.plot=plot(marginal_effects(spec_ord_mod_5,
                                conditions = data.frame(int_tot = 100)))

spec.lat.plot <- spec.lat.plot[[3]]

betw.lat.plot=plot(marginal_effects(betw.brm4))

betw.lat.plot <- betw.lat.plot[[3]]
                   
clos.lat.plot=plot(marginal_effects(cent.brm3))

clos.lat.plot <- clos.lat.plot[[3]]


###PLOTS
#PROP
colnames(prop.lat.plot$data)[1:2] <- c("Latitude","Pollinator taxa")

prop.lat.plot$data$`Pollinator taxa` <- revalue(prop.lat.plot$data$`Pollinator taxa`,
                                                c("Bee" = "Bee",
                                                  "Coleoptera" = "Coleoptera",
                                                  "Lepidoptera" = "Lepidoptera",
                                                  "Hymenoptera" = "Non-bee Hymenoptera",
                                                  "Diptera" = "Non-syrphid Diptera",
                                                  "Syrphidae" = "Syrphidae"))

prop.lat.plot$data$`Pollinator taxa` <- factor(prop.lat.plot$data$`Pollinator taxa`,
                                               levels=c("Bee","Coleoptera","Lepidoptera",
                                                        "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

prop.lat.gg=prop.lat.plot+
  facet_wrap(~`Pollinator taxa`,ncol=3)+
  theme_bw()+
  ggtitle("A) Proportion of network links")+
  ylab("Proportion of links")+
  xlab("Latitude")+
  #scale_y_continuous(breaks=c(1,4,7,10,14),limits=c(1,14))+
  #scale_color_manual(breaks=list_spp.2,values=plot_cols.2,name="Pollinator taxa")+
  theme(strip.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.text = element_text(face = "bold",size=10),
        panel.spacing = unit(0.5,"lines"),
        plot.title = element_text(face = "bold",size=12),
        axis.text.x=element_text(),
        axis.text.y=element_text(),
        axis.title.x = element_text(face = "bold",size=10),
        axis.title.y=element_text(face = "bold",size=10),
        aspect.ratio = 1,
        axis.ticks.x = element_blank())+
  guides(fill=FALSE,col=FALSE)
prop.lat.gg

#SPEC
colnames(spec.lat.plot$data)[1:2] <- c("Latitude","Pollinator taxa")

spec.lat.plot$data$`Pollinator taxa` <- revalue(spec.lat.plot$data$`Pollinator taxa`,
                                            c("Bee" = "Bee",
                                              "Coleoptera" = "Coleoptera",
                                              "Lepidoptera" = "Lepidoptera",
                                              "Hymenoptera" = "Non-bee Hymenoptera",
                                              "Diptera" = "Non-syrphid Diptera",
                                              "Syrphidae" = "Syrphidae"))

spec.lat.plot$data$`Pollinator taxa` <- factor(spec.lat.plot$data$`Pollinator taxa`,
                                           levels=c("Bee","Coleoptera","Lepidoptera",
                                                    "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

spec.lat.gg=spec.lat.plot+
  facet_wrap(~`Pollinator taxa`,ncol=3)+
  theme_bw()+
  ggtitle("B) Pollinator generalism")+
  ylab("Normalised generalism")+
  xlab("Latitude")+
  #scale_y_continuous(breaks=c(1,4,7,10,14),limits=c(1,14))+
  #scale_color_manual(breaks=list_spp.2,values=plot_cols.2,name="Pollinator taxa")+
  theme(strip.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.text = element_text(face = "bold",size=10),
        panel.spacing = unit(0.5,"lines"),
        plot.title = element_text(face = "bold",size=12),
        axis.text.x=element_text(),
        axis.text.y=element_text(),
        axis.title.x = element_text(face = "bold",size=10),
        axis.title.y=element_text(face = "bold",size=10),
        aspect.ratio = 1,
        axis.ticks.x = element_blank())+
  guides(fill=FALSE,col=FALSE)
spec.lat.gg

#betweenness
colnames(betw.lat.plot$data)[1:2] <- c("Latitude","Pollinator taxa")

betw.lat.plot$data$`Pollinator taxa` <- revalue(betw.lat.plot$data$`Pollinator taxa`,
                                                c("Bee" = "Bee",
                                                  "Coleoptera" = "Coleoptera",
                                                  "Lepidoptera" = "Lepidoptera",
                                                  "Hymenoptera" = "Non-bee Hymenoptera",
                                                  "Diptera" = "Non-syrphid Diptera",
                                                  "Syrphidae" = "Syrphidae"))

betw.lat.plot$data$`Pollinator taxa` <- factor(betw.lat.plot$data$`Pollinator taxa`,
                                               levels=c("Bee","Coleoptera","Lepidoptera",
                                                        "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

betw.lat.gg=betw.lat.plot+
  facet_wrap(~`Pollinator taxa`,ncol=3)+
  theme_bw()+
  ggtitle("A) Betweenness centrality")+
  ylab("Betweenness centrality")+
  xlab("Latitude")+
  #scale_y_continuous(breaks=c(1,4,7,10,14),limits=c(1,14))+
  #scale_color_manual(breaks=list_spp.2,values=plot_cols.2,name="Pollinator taxa")+
  theme(strip.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.text = element_text(face = "bold",size=10),
        panel.spacing = unit(0.5,"lines"),
        plot.title = element_text(face = "bold",size=12),
        axis.text.x=element_text(),
        axis.text.y=element_text(),
        axis.title.x = element_text(face = "bold",size=10),
        axis.title.y=element_text(face = "bold",size=10),
        aspect.ratio = 1,
        axis.ticks.x = element_blank())+
  guides(fill=FALSE,col=FALSE)
betw.lat.gg

#closeness

colnames(clos.lat.plot$data)[1:2] <- c("Latitude","Pollinator taxa")

clos.lat.plot$data$`Pollinator taxa` <- revalue(clos.lat.plot$data$`Pollinator taxa`,
                                                c("Bee" = "Bee",
                                                  "Coleoptera" = "Coleoptera",
                                                  "Lepidoptera" = "Lepidoptera",
                                                  "Hymenoptera" = "Non-bee Hymenoptera",
                                                  "Diptera" = "Non-syrphid Diptera",
                                                  "Syrphidae" = "Syrphidae"))

clos.lat.plot$data$`Pollinator taxa` <- factor(clos.lat.plot$data$`Pollinator taxa`,
                                               levels=c("Bee","Coleoptera","Lepidoptera",
                                                        "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

clos.lat.gg=clos.lat.plot+
  facet_wrap(~`Pollinator taxa`,ncol=3)+
  theme_bw()+
  ggtitle("B) Closeness centrality")+
  ylab("Closeness centrality")+
  xlab("Latitude")+
  #scale_y_continuous(breaks=c(1,4,7,10,14),limits=c(1,14))+
  #scale_color_manual(breaks=list_spp.2,values=plot_cols.2,name="Pollinator taxa")+
  theme(strip.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.text = element_text(face = "bold",size=10),
        panel.spacing = unit(0.5,"lines"),
        axis.text.x=element_text(),
        plot.title = element_text(face = "bold",size=12),
        axis.text.y=element_text(),
        axis.title.x = element_text(face = "bold",size=10),
        axis.title.y=element_text(face = "bold",size=10),
        aspect.ratio = 1,
        axis.ticks.x = element_blank())+
  guides(fill=FALSE,col=FALSE)
clos.lat.gg


ggsave(prop.lat.gg,file="Fig S1A prop links lat plot.pdf",device = "pdf",dpi=320,width=10,height=5,units = c("in"))
ggsave(spec.lat.gg,file="Fig S1B spec lat plot.pdf",device = "pdf",dpi=320,width=10,height=5,units = c("in"))
ggsave(betw.lat.gg,file="Fig S1C between lat plot.pdf",device = "pdf",dpi=320,width=10,height=5,units = c("in"))
ggsave(clos.lat.gg,file="Fig S1D closeness lat plot.pdf",device = "pdf",dpi=320,width=10,height=5,units = c("in"))

Fig1LAg <- ggplotGrob(prop.lat.gg)
Fig1LBg <- ggplotGrob(spec.lat.gg)
Fig1LCg <- ggplotGrob(betw.lat.gg)
Fig1LDg <- ggplotGrob(clos.lat.gg)

grid.arrange(Fig1LAg,Fig1LBg,nrow=2)
gl1 <- rbind(Fig1LAg, Fig1LBg)
plot(gl1)

ggsave(gl1,file="FigS1 lat_links_spec.pdf",device = "pdf",
       dpi=320,width=6,height=10,units = c("in"))

gl2 <- rbind(Fig1LCg, Fig1LDg)

plot(gl2)

ggsave(gl2,file="FigS2 lat_centrality.pdf",device = "pdf",
       dpi=320,width=6,height=10,units = c("in"))
