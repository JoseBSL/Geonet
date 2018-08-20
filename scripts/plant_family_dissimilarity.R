##plant family dissimilarity matrix
library(dplyr)
library(reshape2)
library(vegan)

reference$Clim=left(reference$ClimateZ,1)
str(geonet)


pfdist=geonet %>% group_by(Network) %>% count(Plant_Family)

pfdist2=geonet %>% group_by(Network) %>% count(Plant)

pfdist=dcast(pfdist, Network ~Plant_Family, 
      fun.aggregate = sum, na.rm =T, value.var="n", fill = 0)

pfdist2=dcast(pfdist2, Network ~Plant, 
             fun.aggregate = sum, na.rm =T, value.var="n", fill = 0)

rownames(pfdist)=pfdist[,1]
pfdist=pfdist[,-1]

rownames(pfdist2)=pfdist2[,1]
pfdist2=pfdist2[,-1]

pfdist.matrix=vegdist(pfdist,method="bray",binary=TRUE) #Presence-absence
pfdist.MDS=metaMDS(pfdist.matrix)
plot(pfdist.MDS$points,pch=as.numeric(as.factor(reference$Clim)))

plot.new()
plot(pfdist.MDS$points)

pfordplot=plot(pfdist.MDS$points,pch=cutree(hclust(pfdist.matrix,"average"),5))


