for (j in levels(geo.wide[1:40, 1]%>%droplevels())){
  web <- subset(geo.wide[1:40,]%>%droplevels(), Network == j)#iterate over site
  network <- as.vector(web$Network[1])
  web <- web[,c(-1,-2)]
  web = web[rowSums(web) > 0,colSums(web) > 0]#remove species with no links at each site ##added remove 0 rowSums
  web.names <- c(colnames(web))
  obs.jac<-vector("list")
  obs.jac[[j]]<-vegdist(t(web), method="jaccard",upper=F,binary=T)
  }
obs.jac   
obs.jac.long=lapply(obs.jac,function(x) melt(as.matrix(x)))
obs.jac.long=do.call("rbind",geo.split.jac.long)

