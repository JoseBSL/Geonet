###############################################
#interaction dissimilarity----
###############################################
library(vegan)

#set up dataframe
geonet.int <- geonet2%>%
  filter(!is.na(plant.family)==T)%>%
  filter(!is.na(animal.family)==T)

#combine plants and pollinators
geonet.int$interaction <- paste(geonet.int$plant.family,
                                "_",
                                geonet.int$animal.family,sep="")

#group by climate, network and elevation
intdist=geonet.int %>%
        group_by(Network,clim) %>% 
        count(interaction)

#cast to wide format
intdist.cast=dcast(intdist, Network + clim ~ interaction, 
              fun.aggregate = sum, na.rm = T, value.var="n", fill = 0)

rownames(intdist.cast)=intdist.cast[,1]

#compute dissimilarity matrix 
intdist.matrix=vegdist(decostand(intdist.cast[,3:length(intdist.cast)],
                                 method="log"),
                       method="bray")

mj2=metaMDS(intdist.matrix,
            k=3,
            trymax = 1000)

#extract ordination coordinates
NMDS1 = data.frame(MDS1 = mj2$points[,1],
                   MDS2 = mj2$points[,2],
                   site=unique(intdist.cast$Network),
                   clim=intdist.cast$clim)

NMDS1$clim=as.factor(NMDS1$clim)

#unable to create ellipse for B (to few points)... need to add them manually at some stage
NMDS1 <- subset(NMDS1,
                clim %in% c("A","B","C","D", "E"))%>%droplevels

#create ellipse function (this is hidden in vegan)
veganCovEllipse <- function (cov,
                             center = c(0, 0),
                             scale = 1,
                             npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#compute ellipse values
df_ell1 <- data.frame()
for(g in levels(NMDS1$clim)){
  df_ell1 <- rbind(df_ell1, cbind(as.data.frame(with(NMDS1[NMDS1$clim==g,],
                                                     veganCovEllipse(cov.wt(cbind(MDS1,MDS2),
                                                                            wt=rep(1/length(MDS1),
                                                                                   length(MDS1)))$cov,
                                                                     center=c(mean(MDS1),mean(MDS2))))) ,group=g))}

#run dissimilarity tests
intdist.perm <- adonis(intdist.matrix ~ intdist.cast$clim, permutations = 999)
#intdist.perm.2 <- adonis(intdist.matrix.2 ~ intdist.cast$clim, permutations = 999)

#run beta-dispersion test
beta_int=betadisper(intdist.matrix, intdist.cast$clim, type = c("centroid"))
tukey_beta_int <- TukeyHSD(beta_int) ## all good!
tukey_beta_int
#run pariwise adonis function
pairwise.adonis <- function(x,factors, sim.function = 'vegdist', sim.method = 'bray', p.adjust.m ='fdr')
{
  library(vegan)
  co = combn(unique(as.character(factors)),2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  for(elem in 1:ncol(co)){
    if(sim.function == 'daisy'){
      library(cluster); x1 = daisy(x[factors %in% c(co[1,elem],co[2,elem]),],metric=sim.method)
    } else{x1 = vegdist(x[factors %in% c(co[1,elem],co[2,elem]),])}
    ad = adonis(x1 ~ factors[factors %in% c(co[1,elem],co[2,elem])] );
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  sig = c(rep('',length(p.adjusted)))
  sig[p.adjusted <= 0.05] <-'.'
  sig[p.adjusted <= 0.01] <-'*'
  sig[p.adjusted <= 0.001] <-'**'
  sig[p.adjusted <= 0.0001] <-'***'
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted,sig)
  print("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  return(pairw.res)
}

#run pairwise function on the matrix
pw.int.adonis <- pairwise.adonis(decostand(intdist.cast[,3:length(intdist.cast)],
                                           method="log"),
                                 intdist.cast$clim)
pw.int.adonis
write.csv(pw.int.adonis,"tables/pairwise_adonis_interactions.csv")

#plot the plant-pollinator interaction ordination
group.colors.ord <- c("Tropical"="#1b9e77",
                      "Arid"="#d95f02",
                      "Temperate"="#66a61e",
                      "Continental"="#e7298a",
                      "Polar"="#7570b3")

clim_zones <- c("Tropical","Arid","Temperate","Continental","Polar")
breaks=clim_zones,
values=plot_cols,
name="Pollinator taxa"

NMDS1$clim <- revalue(NMDS1$clim,
                      c("A" = "Tropical",
                        "B" = "Arid",
                        "C" = "Temperate",
                        "D" ="Continental",
                        "E" = "Polar"))

df_ell1$group <- revalue(df_ell1$group,
                      c("A" = "Tropical",
                        "B" = "Arid",
                        "C" = "Temperate",
                        "D" ="Continental",
                        "E" = "Polar"))
                      
clim.nmds <- 
  ggplot(data = NMDS1,
             aes(x = MDS1,
                 y = MDS2)) +
  geom_point(aes(fill = clim),shape=21,
             colour="black",
             size=3,alpha=0.5) + 
  geom_path(data=df_ell1, aes(x=MDS1,
                              y=MDS2,
                              colour=group),show.legend = F,
            size=1,
            linetype=2) + 
  scale_color_manual(breaks=clim_zones,
                     values = group.colors.ord)+
  scale_fill_manual(breaks=clim_zones,values = group.colors.ord)+
  theme_bw()+
  labs(colour="Climate zone",fill="Climate zone")+
  theme(axis.line.x = element_line(size=0, colour = "black"),
        axis.line.y = element_line(size=0, colour = "black"),
        aspect.ratio = 1,
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_rect(size=.8,
                                    colour = "black",
                                    fill=NA),
        #panel.background = element_blank(),
        axis.text.x=element_text(angle= 360,
                                 hjust = 0.5,
                                 vjust = 0.5,
                                 size =12),
        axis.text.y=element_text(angle= 360,
                                 hjust = 0.5,
                                 vjust = 0.5,
                                 size =12),
        axis.title.x=element_text(size=15,
                                  vjust = 1),
        axis.title.y=element_text(size=15,
                                  vjust = 1),
        axis.text=element_text(colour = "black"),
        axis.ticks = element_blank())

clim.nmds

ggsave(clim.nmds,file="plots/fig 1 - nmds.pdf",
       device="pdf",
       width = 20,
       height = 15,
       units = c("cm"),
       dpi = 300)

ggsave(clim.nmds,file="plots/fig 1 - nmds.jpg",
       device="jpg",
       width = 20,
       height = 15,
       units = c("cm"),
       dpi = 300)


