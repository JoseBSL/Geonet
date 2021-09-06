
###############################################
#interaction dissimilarity----
###############################################

#combine plants and pollinators
geonet$interaction <- paste(geonet$PFamily,"_",geonet$PolFamily)

#group by climate, network and elevation
intdist=geonet %>% group_by(Network,clim,ele) %>% count(interaction)

#cast to wide format
intdist=dcast(intdist, Network + clim + ele ~ interaction, 
              fun.aggregate = sum, na.rm = T, value.var="n", fill = 0)
rownames(intdist)=intdist[,1]

#compute dissimilarity matrix 
intdist.matrix=raupcrick(intdist[,4:length(intdist)],null="r1") #Presence-absence
mj2=metaMDS(intdist.matrix,k=3)

#extract ordination coordinates
NMDS1 = data.frame(MDS1 = mj2$points[,1], MDS2 = mj2$points[,2],site=unique(intdist$Network), clim=intdist$clim)
NMDS1$clim=as.factor(NMDS1$clim)
#unable to create ellipse for B (to few points)... need to add them manually at some stage
NMDS1 <- subset(NMDS1, clim %in% c("A","B", "C","D", "E"))%>%droplevels

#create ellipse function (this is hidden in vegan)
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#compute ellipse values
df_ell1 <- data.frame()
for(g in levels(NMDS1$clim)){
  df_ell1 <- rbind(df_ell1, cbind(as.data.frame(with(NMDS1[NMDS1$clim==g,],
                                                     veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,
                                                                     center=c(mean(MDS1),mean(MDS2))))) ,group=g))}

```

```{r dissimilarity tests}

#run dissimilarity tests
intdist.perm <- adonis(intdist.matrix ~ intdist$clim, permutations = 999)
write.csv(intdist.perm[1], "tables_for_publication/interactions_permanova.csv")

#other tests - not for results
adonis(intdist.matrix ~ intdist$clim+intdist$ele, permutations = 999)
adonis(intdist.matrix ~ intdist$clim*(intdist$ele), permutations = 999)
adonis(intdist.matrix ~ intdist$clim+scale(intdist$ele), permutations = 999)
#very small amount explained by elevation

#run beta-dispersion test
beta_int=betadisper(intdist.matrix, intdist$clim, type = c("centroid"))
tukey_beta_int <- TukeyHSD(beta_int) ## all good!
write.csv(tukey_beta_int$group, "tables_for_publication/interactions_betadispersion.csv")

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
    } else{x1 = raupcrick(x[factors %in% c(co[1,elem],co[2,elem]),])}
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
pw.int.adonis <- pairwise.adonis(intdist[,3:length(intdist)],intdist$clim)
write.csv(pw.int.adonis, "tables_for_publication/pairwise_adonis_interactions.csv")

#plot the plant-pollinator interaction ordination

group.colors.ord <- c("#1b9e77","#d95f02","#66a61e", "#e7298a", "#7570b3")

p2 <- ggplot(data = NMDS1, aes(MDS1, MDS2))
p2 <- p2 + geom_point(aes(colour = clim), size=2)
p2 <- p2 + geom_path(data=df_ell1, aes(x=MDS1, y=MDS2,colour=group), size=1, linetype=2)
p2 <- p2 + annotate("text", x = 0.4, y = 0.48, label = "F = 63.23; P < 0.001")
p2 <- p2 + annotate("text", x = 0.35, y = 0.45, label = "italic(R) ^ 2 == 0.59", 
                    parse = TRUE)
p2 <- p2 + annotate("text", x = 0.36, y = 0.42, label = "Stress = 0.12")
p2 <- p2 + scale_color_manual(values = group.colors.ord)
p2 <- p2 + theme(axis.line.x = element_line(size=0, colour = "black"),
                 axis.line.y = element_line(size=0, colour = "black"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_rect(size=.8, colour = "black", fill=NA),
                 panel.background = element_blank()) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.title.x=element_text(size=15, vjust = 1),
        axis.title.y=element_text(size=15, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(axis.ticks.length = unit(2.5, "mm"))
p2 <- p2 + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p2
ggsave("graphs/interactions_ordination.pdf",
       width = 20, height = 15,
       units = c("cm"),dpi = 300)


