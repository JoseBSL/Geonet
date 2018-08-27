#poll family dissimilarity
library(gridExtra)
library(vegan)
library(reshape2)

poldist2=geonet %>% group_by(Network,Clim) %>% count(Family)
geonet$Family

poldist2=dcast(poldist2, Network + Clim ~ Family, 
              fun.aggregate = sum, na.rm =T, value.var="n", fill = 0)
rownames(poldist2)=poldist2[,1]
poldist2=poldist2[,-1]
str(poldist2)
poldist.matrix=vegdist(poldist2[,3:314],method="jaccard",binary=TRUE) #Presence-absence

isSymmetric(pfdist.matrix)

mj2=metaMDS(poldist.matrix)

NMDS1 = data.frame(MDS1 = mj2$points[,1], MDS2 = mj2$points[,2],site=unique(poldist2$Network), Clim=poldist2$Clim)

#unable to create ellipse for B (to few points)... need to add them manually at some stage
NMDS1 <- subset(NMDS1, Clim %in% c("A", "C","D", "E"))%>%droplevels

#create ellipse function (this is hidden in vegan)
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell <- data.frame()
for(g in levels(NMDS1$Clim)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS1[NMDS1$Clim==g,],
                                                   veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,center=c(mean(MDS1),mean(MDS2)))))
                                ,group=g))
}

p1 <- ggplot(data = NMDS1, aes(MDS1, MDS2))
p1 <- p1 + geom_point(aes(colour = Clim), size=2)
#p <- p + geom_text(aes(label = site, colour=landuse), vjust = -0.8)
p1 <- p1 + geom_path(data=df_ell, aes(x=MDS1, y=MDS2,colour=group), size=1, linetype=2)
p1 <- p1 + theme(panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black")) +
  theme(panel.border=element_rect(colour = "black", fill = "NA", size = 1)) +
  theme(axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.title.y=element_text(size=16, vjust = 1),
        axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.title.x=element_text(size=16, vjust = 1),
        axis.text=element_text(colour = "black"))
p1 <- p1 + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p1 <- p1 + scale_colour_brewer(palette = "Dark2")
p1

grid.arrange(p,p1)

adonis(poldist.matrix ~ Clim, data = ID, permutations = 9999)
adonis(pfdist.matrix ~ Clim, data = ID, permutations = 9999)

beta=betadisper(pfdist.matrix, ID$Clim, type = c("centroid"), bias.adjust = FALSE,
                sqrt.dist = FALSE, add = FALSE)
TukeyHSD(beta)

pol.plant.pro=protest(poldist.matrix, pfdist.matrix,permutations=999)
pol.plant.pro
        
plot(procrustes(poldist.matrix, pfdist.matrix))

library(ggplot2)
library(grid)


ctest <- data.frame(rda1=pol.plant.pro$Yrot[,1],
                    rda2=pol.plant.pro$Yrot[,2],xrda1=pol.plant.pro$X[,1],
                    xrda2=pol.plant.pro$X[,2])


ggplot(ctest) +
  geom_point(aes(x=rda1, y=rda2),pch=1) +
  geom_point(aes(x=xrda1, y=xrda2),pch=2) +
  geom_segment(aes(x=xrda1,y=xrda2,xend=rda1,yend=xrda2),arrow=arrow(length=unit(0.2,"cm")))+
  theme_bw()+ylab("Dimension 2")+xlab("Dimension 1")



clim.plant.pt = multipatt(pfdist, poldist2$Clim, control = how(nperm=999)) 
clim.poll.pt = multipatt(poldist2[,3:314], poldist2$Clim, control = how(nperm=999)) 
summary(clim.pt, indvalcomp=TRUE) 
summary(clim.poll.pt, indvalcomp=TRUE) 
