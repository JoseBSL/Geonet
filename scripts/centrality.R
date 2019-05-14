library(igraph)
library(data.table)
library(plyr)
library(dplyr)

for(i in 1:100){ 
  set.seed(i)                        
  x  = c(runif(100, min=-3, max=3), 200) 
  y  = rbinom(101, size=1, prob=1/(1+exp(-x)))
  tryCatch({m   <-  glm(y~x, family=binomial)}, warning=function(w) print(i))
}


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
  c <- as.data.frame(closeness(links2))
  colnames(c)[1] <- "closeness"
  c <- setDT(c, keep.rownames = TRUE)[]
  colnames(c)[1] <- "Pollinator"
  c <- c[c$Pollinator %in% pol, ]
  
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
cent_1 <- filter(cent, PolOrder %in% ord)

#############################################
#END
#############################################
library(nlme)
library(lme4)
m1 <- lmer(n.closeness ~ PolOrder*clim + (1|Network),
                                   data=cent2)
summary(m1)
library(glmmTMB)
m1 <- glmmTMB(n.closeness ~ PolOrder*clim, data=cent2, family=list(family="beta",link="logit"))

library(DHARMa)
rasp.sc.m1res=simulateResiduals(m1)
plot(rasp.sc.m1res)
testResiduals(rasp.sc.m1res)


library(emmeans)
emm_options(pbkrtest.limit = 10815)
emmeans(m1, pairwise ~ PolOrder|clim)



xy <- unique(geonet[c("Pollinator", "Network")])























set_to_log <- function() {
  tryCatch(closeness(net, normalized=T), warning = function(w) set_to_log())
}



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
  
  #calculate closeness
  tryCatch({c <- as.data.frame(closeness(net, normalized=T))}, warning=function(w) print(i))
  colnames(c)[1] <- "closeness"
  c <- setDT(c, keep.rownames = TRUE)[]
  colnames(c)[1] <- "Pollinator"
  c <- c[c$Pollinator %in% pol, ]
  
  #merge centrality measures
  centrality <- merge(c,b)
  centrality2 <- merge(centrality, meta)
  
  #print into list
  cent[[j]] <- set_to_log()
  
}

