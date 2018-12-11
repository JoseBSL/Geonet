library(tcltk2)
library(bipartite)
library(vegan)

str(geonet.wgt)
#cast long format dataframe
geo.wgt.wide <- dcast(geonet.wgt, Network + Plant ~ Pollinator, fun.aggregate = sum, value.var = "Int")
geo.wgt.wide[,1]=as.factor(geo.wgt.wide[,1])
head(geo.wgt.wide)

write.csv(geo.wgt.wide,"data/processing/geo.wgt.wide.csv")
geo.wgt.wide=read.csv("data/processing/geo.wgt.wide.csv",row.names = c("X"))
str(geo.wgt.wide)
#create empty list
vaz.list <- c()
vaz.list <- list(vaz.list)
obs.bray.list <- c()
obs.bray.list <- list(obs.bray.list)

pb <- txtProgressBar(title = "progress bar", min = 0,
                    max = length(levels(geo.wgt.wide[, 1])))

#set max vector size
Sys.setenv('R_MAX_VSIZE'=32000000000)
start.time <- Sys.time()

#run loop over each site
for (j in levels(geo.wgt.wide[, 1])){
  setTxtProgressBar(pb, j)

  web <- subset(geo.wgt.wide, Network == j)#iterate over site
  network <- as.vector(web$Network[1])
  web <- web[,c(-1,-2)]
  web = web[,colSums(web) > 0]#remove species with no links at each site
  web.names <- c(colnames(web))
  #web=as.matrix(decostand(t(web),"freq"))
  

  vaz.net=vaznull(200,web)
  
  vaz.net=lapply(vaz.net, function (x) vegdist(t(x),binary=F,"bray"))

  #compute means and standard deviation for each species pair 
  vaz.mean <- apply(simplify2array(lapply(vaz.net, as.matrix)),1:2, mean, na.rm = TRUE)
  colnames(vaz.mean) <- web.names
  rownames(vaz.mean) <- web.names
  
  vaz.mean <- melt(vaz.mean, value.name="mean")
  
  vaz.sd <- apply(simplify2array(lapply(vaz.net, as.matrix)),1:2, sd, na.rm = TRUE)
  colnames(vaz.sd) <- web.names
  rownames(vaz.sd) <- web.names
  vaz.sd           <- melt(vaz.sd, value.name="sd")
  
  vaz.merge <- merge(vaz.mean,vaz.sd,by=c("Var1","Var2"))  
  vaz.merge$network <- network
  vaz.list[[j]] <- vaz.merge
  #mean.null.bray[[i]]=apply(simplify2array(vaz.net[[j]]),1:2,mean)
  #sd.bray[[i]]=apply(simplify2array(vaz.net[[j]]),1:2,sd)         

  obs.bray <- web
  obs.bray <- as.matrix(vegdist(t(obs.bray), method="bray",upper=F,binary=F))#compute jaccard distance
  colnames(obs.bray) <- web.names
  rownames(obs.bray) <- web.names
  obs.bray <- melt(obs.bray,value.name="bray")#convert to long format
  obs.bray <- obs.bray[!obs.bray$Var1==obs.bray$Var2,]#remove diagonals
  obs.bray$network <- network #assign network names
  obs.bray.list[[j]] <- obs.bray#print into list
  
}
##Check system time
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

str(vaz.merge)

#convert list to dataframe
vaz.long <- rbind.fill(lapply(vaz.list, as.data.frame)) #1893282
obs.bray.long <- rbind.fill(lapply(obs.bray.list, as.data.frame)) ##1885448
vaz.long=vaz.long[!vaz.long$mean==0,] ##1885448

write.csv(vaz.long,"data/processing/vaz_long.csv")
write.csv(obs.bray.long,"data/processing/obs.bray.long.csv")

#change column names
colnames(vaz.long)=c("Poll_sp2","Poll_sp1","mean","sd","Network")
colnames(obs.bray.long)=c("Poll_sp1","Poll_sp2","value.true","Network")

head(vaz.long)
head(obs.bray.long)
#Merge null and observational dataframnes
bray.all <- merge(vaz.long,obs.bray.long, by=c("Poll_sp1","Poll_sp2","Network"))
write.csv(bray.all,"data/processing/bray_all.csv")
sum(is.na(bray.all))



#compute standardised dissimilarity values
bray.all$std.bray <- (bray.all$value.true - bray.all$mean)/bray.all$sd
range(bray.all$std.bray)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))



bray.all[is.nan(bray.all)] <- 1
range(bray.all$std.bray)

write.csv(bray.all,"data/processed/bray_all.csv")

#GENUS 1
bray.all$Poll_sp1=gsub("\\."," ",bray.all$Poll_sp1)
bray.all$IGenus=word(gsub("\\."," ",bray.all$Poll_sp1),1)

str(bray.all)
bray.all=merge(bray.all,poll_famord[,c("IGenus","PolOrder","PolFamily")], by = "IGenus",all.y=FALSE)

colnames(bray.all)=c("PGenus1","Poll_sp1","Poll_sp2",
                               "Network","mean","sd","value.true","std.bray","Order1","Family1")

#GENUS 2
bray.all$IGenus=word(gsub("\\."," ",bray.all$Poll_sp2),1)
#geo.split.jac.long$IGenus=word(gsub("\\_"," ",geo.split.jac.long$IGenus),1)

bray.all=merge(bray.all,poll_famord[,c("IGenus","PolOrder","PolFamily")], by = "IGenus",all.y=FALSE)
str(bray.all)

colnames(bray.all)=c("PGenus2","PGenus1","Poll_sp1","Poll_sp2",
                               "Network","mean","sd","value.true",
                               "std.bray","Order1","Family1","Order2","Family2")
str(bray.all)

bray.all=merge(bray.all,reference[,c("Reference","Latitude","Longitude","clim","Network","ele")], by = "Network")
table(bray.all$clim)
head(bray.all)

#########SUBSET########
bray.all <- bray.all %>% mutate_if(is.factor,as.character)
bray.all[bray.all$Family1%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("Order1")]="Bee"
bray.all[bray.all$Family1%in%c("Syrphidae"),c("Order1")]="Syrphidae"

bray.all[bray.all$Family2%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("Order2")]="Bee"
bray.all[bray.all$Family2%in%c("Syrphidae"),c("Order2")]="Syrphidae"

bray.all.sub1=subset(bray.all, Order1 %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))
bray.all.sub1=subset(bray.all.sub1, Order2 %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))
bray.all.sub1 <- bray.all.sub1 %>% mutate_if(is.character,as.factor)

bray.all.sub1$bray.fam=as.factor(paste(bray.all.sub1$Family1,bray.all.sub1$Family2,sep="_"))
bray.all.sub1$bray.ord=as.factor(paste(bray.all.sub1$Order1,bray.all.sub1$Order2,sep="_"))

write.csv(bray.all.sub1,"data/processed/bray.all.sub1.csv")
