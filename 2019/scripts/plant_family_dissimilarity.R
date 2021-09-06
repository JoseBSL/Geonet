##plant family dissimilarity matrix
library(dplyr)
library(reshape2)
library(vegan)

reference=droplevels(reference)
str(geonet)
pfdist=geonet %>% group_by(Network,clim) %>% count(Pfamily)
pfdist=dcast(pfdist, Network +clim ~ Pfamily, 
      fun.aggregate = sum, na.rm =T, value.var="n", fill = 0)
rownames(pfdist)=pfdist[,1]
pfdist=pfdist[,-1]

pfdist.matrix=vegdist(pfdist[,2:length(pfdist)],method="jaccard",binary=TRUE) #Presence-absence

mj=metaMDS(pfdist.matrix)
str(reference)
reference$Clim
NMDS = data.frame(MDS1 = mj$points[,1], MDS2 = mj$points[,2],site=unique(pfdist2$Network), Clim=pfdist2$Clim)

#unable to create ellipse for B (to few points)... need to add them manually at some stage
NMDS <- subset(NMDS, Clim %in% c("A", "C","D", "E"))%>%droplevels

#create ellipse function (this is hidden in vegan)
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell <- data.frame()
for(g in levels(NMDS$Clim)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Clim==g,],
                                                   veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,center=c(mean(MDS1),mean(MDS2)))))
                                ,group=g))
}

p <- ggplot(data = NMDS, aes(MDS1, MDS2))
p <- p + geom_point(aes(colour = Clim), size=2)
#p <- p + geom_text(aes(label = site, colour=landuse), vjust = -0.8)
p <- p + geom_path(data=df_ell, aes(x=MDS1, y=MDS2,colour=group), size=1, linetype=2)
p <- p + theme(panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black")) +
  theme(panel.border=element_rect(colour = "black", fill = "NA", size = 1)) +
  theme(axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.title.y=element_text(size=16, vjust = 1),
        axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.title.x=element_text(size=16, vjust = 1),
        axis.text=element_text(colour = "black"))
p <- p + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p <- p + scale_colour_brewer(palette = "Dark2")
p

#create method ID dataframe
ID <- select(pfdist2, one_of(c("Network", "Clim")))

#Run the permanova
adonis(poldist.matrix ~ Clim, data = ID, permutations = 9999, method = "bray")

beta_pol=betadisper(poldist.matrix, ID$Clim, type = c("centroid"), bias.adjust = FALSE,
           sqrt.dist = FALSE, add = FALSE)
TukeyHSD(beta_pol)

