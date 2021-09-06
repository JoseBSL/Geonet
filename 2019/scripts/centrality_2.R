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
  meta <- unique(links[,c("Pollinator","PolOrder","Network","clim","Latitude","Longitude")])
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