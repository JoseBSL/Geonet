library(igraph)
library(tnet)
library(data.table)
library(plyr)
library(dplyr)

#create empty list
cent <- c()
cent <- list(cent)
geonet$Network <- as.factor(geonet$Network)

#run loop over each site
for (j in levels(geonet[, 1])){
  links <- subset(geonet, Network == j)#iterate over site
  links2 <- links[,c(4:6)]
  links2$ij <- paste(links2$Plant,links2$Pollinator, sep="_")
  links2 <- links2[!duplicated(links2$ij), ]
  
  #create igraph object
  pol <-  unique(links2[,c("Pollinator")])
  net <- graph_from_data_frame(d=links2, vertices=NULL, directed=F)
  
  #calculate betweenness
  b <- as.data.frame(betweenness(net,normalized=TRUE))
  colnames(b)[1] <- "betweenness"
  b <- setDT(b, keep.rownames = TRUE)[]
  colnames(b)[1] <- "Pollinator"
  b <- b[b$Pollinator %in% pol, ]
  
  #calculate closeness
  links2$Plant <- as.factor(links2$Plant)
  links2$Pollinator <- as.factor(links2$Pollinator)
  links2$i <- as.integer(links2$Pollinator)
  links2$j <- as.integer(links2$Plant)
  links3 <- as.matrix(links2[,c(5:6)])
  rownames(links3) <- c()
  links3 <- projecting_tm(links3, method="binary")
  c <- closeness_w(links3, gconly=F)
  nodes <- unique(links2[,c(2,5)])
  colnames(nodes)[2] <- "node"
  c <- merge(c, nodes, by="node")
  
  #merge centrality measures
  centrality <- merge(c,b)
  meta <- unique(links[,c("Pollinator","PolOrder","Network","clim","Latitude","Longitude","Reference")])
  centrality2 <- merge(centrality, meta)
  
  #print into list
  cent[[j]] <- centrality2
  
}

#convert list to dataframe
cent <- rbind.fill(lapply(cent, as.data.frame))

#filter orders
ord <- c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera")
cent2 <- filter(cent, PolOrder %in% ord)

#############################################
#END
#############################################


#############################################
#Bayes model
#############################################
cent_priors=prior(normal(0,5),class="Intercept")+
            prior(normal(0,3),class="b")+
            prior(normal(20,10),class="phi")+
            prior(normal(0,1),class="sd")

str(cent2)

cent.brm1 <- brm(n.closeness~PolOrder*clim+(1|Reference/Network),
                 family=gaussian,cores=4,
                 seed=123,
                 data=cent2)

betw.brm <- brm(betweenness~PolOrder*clim+(1|Reference/Network),
                 family=gaussian,cores=4,
                 seed=123,
                 data=cent2)

cent.brm2 <- brm(n.closeness~PolOrder*clim+(1|Reference/Network),
                 family=zero_one_inflated_beta(),cores=4,
                 seed=123,
                 prior=cent_priors,
                 data=cent2)

betw.brm2 <- brm(betweenness~PolOrder*clim+(1|Reference/Network),
                 family=zero_inflated_beta(),cores=4,
                 seed=123,
                 prior=cent_priors,
                 data=cent2)

prior_summary(betw.brm2)
plot(betw.brm2)
cent.brm1 <- add_criterion(cent.brm1,criterion = c("waic","loo"))
cent.brm2 <- add_criterion(cent.brm2,criterion = c("waic","loo"))


betw.brm <- add_criterion(betw.brm,criterion = c("waic","loo"))
betw.brm2 <- add_criterion(betw.brm2,criterion = c("waic","loo"))

pp_check(cent.brm1,nsamples=100,"stat_grouped",group="PolOrder")

loo_compare(cent.brm1,cent.brm2,criterion=c("loo"))
loo_compare(cent.brm1,cent.brm2,criterion=c("waic"))

loo_compare(betw.brm,betw.brm2,criterion=c("loo"))
loo_compare(betw.brm,betw.brm2,criterion=c("waic"))


###############################
#######BETWEENNESS PLOT########
###############################

betw.plot=plot(marginal_effects(betw.brm2))

betw.plot.data <-  betw.plot[[3]]$data

colnames(betw.plot.data)[1:2] <- c("Pollinator taxa","Climate zone")

betw.plot.data$`Pollinator taxa` <- revalue(betw.plot.data$`Pollinator taxa`,
                                            c("Bee" = "Bee",
                                            "Coleoptera" = "Coleoptera",
                                            "Lepidoptera" = "Lepidoptera",
                                            "Hymenoptera" = "Non-bee Hymenoptera",
                                            "Diptera" = "Non-syrphid Diptera",
                                            "Syrphidae" = "Syrphidae"))

betw.plot.data$`Pollinator taxa` <- factor(betw.plot.data$`Pollinator taxa`,
                                           levels=c("Bee","Coleoptera","Lepidoptera",
                                                    "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

cent3 <- cent2
colnames(cent3)[6]
colnames(cent3)[8]="Climate zone"

cent3$`Pollinator taxa` <- cent3$PolOrder

cent3$`Pollinator taxa` <- revalue(cent3$`Pollinator taxa`,
                                            c("Bee" = "Bee",
                                              "Coleoptera" = "Coleoptera",
                                              "Lepidoptera" = "Lepidoptera",
                                              "Hymenoptera" = "Non-bee Hymenoptera",
                                              "Diptera" = "Non-syrphid Diptera",
                                              "Syrphidae" = "Syrphidae"))
betw.plot.data$betweenness=c("")
rbind.betw.plot <- rbind.fill(betw.plot.data,cent3)

rbind.betw.plot$`Climate zone` <- factor(rbind.betw.plot$`Climate zone`,levels=c("A","B","C","D","E"))

rbind.betw.plot$`Climate zone` <- revalue(rbind.betw.plot$`Climate zone`,c("A" = "Tropical",
                                                                         "B" = "Arid",
                                                                         "C" = "Temperate",
                                                                         "D" = "Continental",
                                                                         "E" = "Polar"))

rbind.betw.plot[rbind.betw.plot$`Pollinator taxa`%in%"Non-bee Hymenoptera",]
rbind.betw.plot$PolOrder <- revalue(rbind.betw.plot$PolOrder,c("Bee" = "A",
                                                                           "Coleoptera" = "B",
                                                                           "Lepidoptera" = "C",
                                                                           "Hymenoptera" = "D",
                                                                           "Diptera" = "E",
                                                                           "Syrphidae"  =      "F"))

rbind.betw.plot$PolOrder <-  factor(rbind.betw.plot$PolOrder,levels=c("A","B","C","D","E","F"))
rbind.betw.plot <- droplevels(rbind.betw.plot)
rbind.betw.plot$betweenness <- as.numeric(rbind.betw.plot$betweenness)

betw.gg=ggplot(rbind.betw.plot,aes(x=`Pollinator taxa`,y=estimate__,col=`Pollinator taxa`))+
  geom_point(aes(y=betweenness,col=PolOrder),show.legend = F,size=0.5,
             position=position_jitterdodge(dodge.width=0,jitter.width = 0.7),
             alpha=0.5)+
  geom_point(aes(col=`Pollinator taxa`),
             size=2,show.legend = F)+
  geom_errorbar(aes(ymin=lower__,ymax=upper__,col=`Pollinator taxa`),
                width=0.4,show.legend =F)+
  facet_wrap(~`Climate zone`,ncol=5)+
  ylim(0,0.1)+
  theme_bw()+
  ylab("Betweenness centrality")+
  xlab(NULL)+
scale_color_manual(breaks=list_spp,values=plot_cols,name="Pollinator taxa")+
  theme(
    strip.text.x = element_blank(),
        panel.spacing = unit(0.5,"lines"),
    strip.background = element_rect(fill="white"),
    axis.text.x = element_text(angle = 90,vjust = 0.2),
    aspect.ratio = 1,
    axis.ticks.x = element_blank())
betw.gg
ggsave(betw.gg,file="betweenness plot.pdf",device = "pdf",dpi=320,width=15,height=5,units = c("in"))

#axis.text.x = element_text(angle = 90,vjust = 0.2)
theme(axis.text.x = element_text(angle = 90,hjust = 0),aspect.ratio = 1)


##############################
#########CLOSENESS############
##############################


cent.plot=plot(marginal_effects(cent.brm2))

cent.plot.data <-  cent.plot[[3]]$data

colnames(cent.plot.data)[1:2] <- c("Pollinator taxa","Climate zone")

cent.plot.data$`Pollinator taxa` <- revalue(cent.plot.data$`Pollinator taxa`,
                                            c("Bee" = "Bee",
                                              "Coleoptera" = "Coleoptera",
                                              "Lepidoptera" = "Lepidoptera",
                                              "Hymenoptera" = "Non-bee Hymenoptera",
                                              "Diptera" = "Non-syrphid Diptera",
                                              "Syrphidae" = "Syrphidae"))

cent.plot.data$`Pollinator taxa` <- factor(cent.plot.data$`Pollinator taxa`,
                                           levels=c("Bee","Coleoptera","Lepidoptera",
                                                    "Non-bee Hymenoptera","Non-syrphid Diptera","Syrphidae"))

cent3 <- cent2
colnames(cent3)[6]
colnames(cent3)[8]="Climate zone"

cent3$`Pollinator taxa` <- cent3$PolOrder

cent3$`Pollinator taxa` <- revalue(cent3$`Pollinator taxa`,
                                   c("Bee" = "Bee",
                                     "Coleoptera" = "Coleoptera",
                                     "Lepidoptera" = "Lepidoptera",
                                     "Hymenoptera" = "Non-bee Hymenoptera",
                                     "Diptera" = "Non-syrphid Diptera",
                                     "Syrphidae" = "Syrphidae"))
cent.plot.data$n.closeness=c("")
rbind.cent.plot <- rbind.fill(cent.plot.data,cent3)

rbind.cent.plot$`Climate zone` <- factor(rbind.cent.plot$`Climate zone`,levels=c("A","B","C","D","E"))

rbind.cent.plot$`Climate zone` <- revalue(rbind.cent.plot$`Climate zone`,c("A" = "Tropical",
                                                                           "B" = "Arid",
                                                                           "C" = "Temperate",
                                                                           "D" = "Continental",
                                                                           "E" = "Polar"))

rbind.cent.plot[rbind.cent.plot$`Pollinator taxa`%in%"Non-bee Hymenoptera",]
rbind.cent.plot$PolOrder <- revalue(rbind.cent.plot$PolOrder,c("Bee" = "A",
                                                               "Coleoptera" = "B",
                                                               "Lepidoptera" = "C",
                                                               "Hymenoptera" = "D",
                                                               "Diptera" = "E",
                                                               "Syrphidae"  =      "F"))

rbind.cent.plot$PolOrder <-  factor(rbind.cent.plot$PolOrder,levels=c("A","B","C","D","E","F"))
rbind.cent.plot <- droplevels(rbind.cent.plot)
rbind.cent.plot$n.closeness <- as.numeric(rbind.cent.plot$n.closeness)

cent.gg=ggplot(rbind.cent.plot,aes(x=`Pollinator taxa`,y=estimate__,col=`Pollinator taxa`))+
  geom_point(aes(y=n.closeness,col=PolOrder),show.legend = F,size=0.5,
             position=position_jitterdodge(dodge.width=0,jitter.width = 0.7),
             alpha=0.5)+
  geom_point(aes(col=`Pollinator taxa`),
             size=2,show.legend = F)+
  geom_errorbar(aes(ymin=lower__,ymax=upper__,col=`Pollinator taxa`),
                width=0.4,show.legend =F)+
  facet_wrap(~`Climate zone`,ncol=5)+
  
  theme_bw()+
  ylab("Closeness centrality")+
  xlab(NULL)+
  scale_color_manual(breaks=list_spp,values=plot_cols,name="Pollinator taxa")+
  theme(
    strip.text.x = element_blank(),
        panel.spacing = unit(0.5,"lines"),
    strip.background =element_rect(fill="white"),
    axis.text.x=element_blank(),
    aspect.ratio = 1,
    axis.ticks.x = element_blank())
cent.gg
ggsave(cent.gg,file="closeness plot.pdf",device = "pdf",dpi=320,width=15,height=5,units = c("in"))

###################
#####PAIRWISE#####
#################
cent.brm_emm <- cent.brm1 %>%
  emmeans( ~ PolOrder|clim) %>% 
  gather_emmeans_draws() %>% 
  mutate(intera = paste(PolOrder,clim , sep = ".")) 

##compact letter differences
cent.brm_cld<-cent.brm_emm %>% 
  ungroup() %>% ## to get rid of unneeded columns
  select(.value, intera, .draw) %>% 
  spread(intera, .value) %>% 
  select(-.draw) %>% ## we need to get rid of all columns not containing draws
  cld_pmatrix()

cent.brm_cld

###Compute HDI intervals  - check against CLD
cent.brm_hdi<-cent.brm_emm %>% 
  ungroup %>% 
  compare_levels(.value, by = intera) %>% 
  mode_hdi()


cent.brm_hdi$group1=word(cent.brm_hdi$intera,1)
cent.brm_hdi$group2=word(cent.brm_hdi$intera,3)

cent.brm_hdi$id  <- cent.brm_hdi$.lower * cent.brm_hdi$.upper

cent.brm_hdi[cent.brm_hdi$id>0,c("id")] = "sig***"
cent.brm_hdi[cent.brm_hdi$id<0,c("id")] = "n.s."


write.csv(cent.brm_hdi,"cent_hdi.csv")

####BETWEEM
betw.brm_emm <- betw.brm %>%
  emmeans( ~ PolOrder|clim) %>% 
  gather_emmeans_draws() %>% 
  mutate(intera = paste(PolOrder,clim , sep = ".")) 

##compact letter differences
betw.brm_cld<-betw.brm_emm %>% 
  ungroup() %>% ## to get rid of unneeded columns
  select(.value, intera, .draw) %>% 
  spread(intera, .value) %>% 
  select(-.draw) %>% ## we need to get rid of all columns not containing draws
  cld_pmatrix()

betw.brm_cld

###Compute HDI intervals  - check against CLD
betw.brm_hdi<-betw.brm_emm %>% 
  ungroup %>% 
  compare_levels(.value, by = intera) %>% 
  mode_hdi()

betw.brm_hdi$group1=word(betw.brm_hdi$intera,1)
betw.brm_hdi$group2=word(betw.brm_hdi$intera,3)

betw.brm_hdi$id  <- betw.brm_hdi$.lower * betw.brm_hdi$.upper

betw.brm_hdi[betw.brm_hdi$id>0,c("id")] = "sig***"
betw.brm_hdi[betw.brm_hdi$id<0,c("id")] = "n.s."



write.csv(betw.brm_hdi,"betw_hdi.csv")
