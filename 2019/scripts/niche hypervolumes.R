library(ecospat)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer) 
library(hypervolume)

geonet$bin <- geonet$Int[geonet$Int > 0] <- 1
fam.links.melt <- geonet %>%
  group_by(Network, PGenus, PolOrder, Pollinator) %>%
  summarise(value=sum(bin))

fam.links.melt <- na.omit(fam.links.melt)

fam.links.filt <- fam.links.melt %>%
group_by(Network,PolOrder,Pollinator) %>%
  filter(n()>=1)

fam.links.filt=merge(fam.links.filt,reference,by="Network",sort=F, all.x = T)

xy <- dcast(fam.links.filt, PolOrder + Pollinator +  Network + clim ~ PGenus, fill = 0)
target <- c("Syrphidae", "Bee")

xxy <- filter(xy, PolOrder == target)
xxy <- filter(xxy, Network == "M_PL_060_00")

xxy <- with(xxy, xxy[order(PolOrder),])
xxy$PolOrder # right now the order is 5,4,3,2,1


xxy.mat <- as.matrix(xxy[,5:1420])
xxy.mat <- xxy.mat[,!colSums(xxy.mat) < 5]
#estimate_bandwidth(xxy.mat,method="silverman")
#hv = hypervolume(data=xxy.mat)
#summary(hv)
#plot(hv)

pca_Ph_Pl <- dudi.pca(df = na.omit(xxy.mat, center = T, scale = T, scannf = F, nf = 2), 
                      scannf = FALSE, nf = 2) 
niceOverPlot(pca_Ph_Pl, n1=102, n2=134)  














i <- (colSums(xxy[,4:212], na.rm=T) != 0) # T if colSum is not 0, F otherwise
#Then you can either select or drop them e.g.

matnonzero <- xxy[, i] # all the non-zero columns


xxy1 <- xxy[,!colSums(xxy[,5:ncol(xxy)]) ==0]
head(xxy1)



nx <- xxy[,colSums(xxy[,4:128]) > 50] %>% droplevels
hv = hypervolume(data=subset(xxy, PolFamily == "Apidae")[,4:59],method='box')


xy <- xy[-1]

pca_Ph_Pl <- dudi.pca(df = na.omit(xy, center = T, scale = T, scannf = F, nf = 2), scannf = FALSE, nf = 2) 
niceOverPlot(pca_Ph_Pl, n1=1125 , n2= 44)  

library(hypervolume)

data(iris)

hv = hypervolume(data=subset(iris, Species=="setosa")[,1:2],method='box')

summary(hv)

plot(hv)







####################################

geonet$bin <- geonet$Int[geonet$Int > 0] <- 1
fam.links.melt <- geonet %>%
  group_by(Network, PFamily, PolOrder, Pollinator) %>%
  summarise(value=sum(bin))

fam.links.melt <- na.omit(fam.links.melt)

fam.links.filt <- fam.links.melt %>%
  group_by(Network,PolOrder,Pollinator) %>%
  filter(n()>=1)

fam.links.filt=merge(fam.links.filt,reference,by="Network",sort=F, all.x = T)

xy <- dcast(fam.links.filt, PolOrder +Pollinator+  Network + clim ~ PGenus, fill = 0)
target <- c("Syrphidae")

xxy <- filter(xy, PolOrder == target)

no.matrix=raupcrick(as.matrix(xxy[,5:1398],null="r1")) #Presence-absence

mj=metaMDS(no.matrix,k=3)

NMDS = data.frame(MDS1 = mj$points[,1], MDS2 = mj$points[,2],site=xxy$Network, clim=xxy$clim)

#unable to create ellipse for B (to few points)... need to add them manually at some stage
NMDS <- subset(NMDS, clim %in% c("A","B", "C","D", "E"))%>%droplevels
NMDS$clim=as.factor(NMDS$clim)

df_ell <- data.frame()
for(g in levels(NMDS$clim)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$clim==g,],
                                                   veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),
                                                                                                  length(MDS1)))$cov,center=c(mean(MDS1),mean(MDS2))))),group=g))}

p <- ggplot(data = NMDS, aes(MDS1, MDS2))
p <- p + geom_point(aes(colour = clim), size=2)
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

adonis(poldist.matrix ~ pfdist$clim, permutations = 999)
adonis(poldist.matrix ~ pfdist$clim+pfdist$ele, permutations = 999)
adonis(poldist.matrix ~ pfdist$clim*(pfdist$ele), permutations = 999)
adonis(poldist.matrix ~ pfdist$clim+scale(pfdist$ele), permutations = 999)
#more explained by elevation

beta_poll=betadisper(poldist.matrix, poldist$clim, type = c("centroid"))
TukeyHSD(beta_poll) ##ALL GOOD

