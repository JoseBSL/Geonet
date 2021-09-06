
#create empty list
obs.jac.list <- c()
obs.jac.list <- list(obs.jac.list)

#loop over network
  for (j in levels(geo.wide[, 1])){
  web <- subset(geo.wide, Network == j)#iterate over site
  network <- as.vector(web$Network[1])#create vector of network names
  web <- web[,c(-1,-2)]#remove plant and sites columns
  web = web[rowSums(web) > 0,colSums(web) > 0]#remove species with no links at each site ##added remove 0 rowSums
  web.names <- c(colnames(web))
  obs.jac <- as.matrix(vegdist(t(web), method="jaccard",upper=F,binary=T))#compute jaccard distance
  obs.jac <- melt(obs.jac,value.name="jac")#convert to long format
  obs.jac <- obs.jac[!obs.jac$Var1==obs.jac$Var2,]#remove diagonals
  obs.jac$network <- network#assign network names
  obs.jac.list[[j]] <- obs.jac#print into list
}

#convert list to long format
obs.jac.long <- rbind.fill(lapply(obs.jac.list, as.data.frame))

#other stuff
obs.jac   
obs.jac.long=lapply(obs.jac,function(x) melt(as.matrix(x)))
obs.jac.long <- obs.jac.long[!obs.jac.long$Var1==obs.jac.long$Var2,]
obs.jac.long=do.call("rbind",geo.split.jac.long)


lapply(myfiles,function (x) length(x))
