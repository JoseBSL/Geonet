library(igraph)
library(data.table)
library(plyr)
library(dplyr)
library(brms)

#create empty list
cent <- c()
cent <- list(cent)
geonet$Network <- as.factor(geonet$Network)

#run loop over each site
for (j in levels(geonet[, 1])){
  links <- subset(geonet, Network == j)#iterate over site
  links2 <- links[,c(4:6)]
  
  #create igraph object
  pol <-  unique(links2[,c("Pollinator")])
  net <- graph_from_data_frame(d=links2, vertices=NULL, directed=F)
  
  #calculate betweenness
  b <- as.data.frame(betweenness(net,normalized=T))
  colnames(b)[1] <- "betweenness"
  b <- setDT(b, keep.rownames = TRUE)[]
  colnames(b)[1] <- "Pollinator"
  b <- b[b$Pollinator %in% pol, ]
  
  #calculate closeness
  tryCatch({c <-  as.data.frame(closeness(net, normalized=T))},warning=function(w) print(i))
  c <- as.data.frame(closeness(net, normalized=T))
  colnames(c)[1] <- "closeness"
  c <- setDT(c, keep.rownames = TRUE)[]
  colnames(c)[1] <- "Pollinator"
  c <- c[c$Pollinator %in% pol, ]
  
  #merge centrality measures
  centrality <- merge(c,b)
  meta <- unique(links[,c("Pollinator","PolOrder","Reference","Network","clim","Latitude","Longitude")])
  centrality2 <- merge(centrality, meta)

  #print into list
cent[[j]] <- centrality2
}

#convert list to dataframe
cent <- rbind.fill(lapply(cent, as.data.frame))

#filter orders
ord <- c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera")
cent <- filter(cent, PolOrder %in% ord)

#############################################
#END
#############################################
