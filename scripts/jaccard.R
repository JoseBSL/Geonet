
##HOW TO DEAL WITH MULTIPLE NETWORKS WITH SAME NAME

myfiles.jac=lapply(myfiles,function(x) vegdist(t(x), method="jaccard",binary=TRUE))
myfiles.jac.long=lapply(myfiles.jac,function(x) melt(as.matrix(x)))
myfiles.jac.long=do.call("rbind",myfiles.jac.long)

myfiles.jac.long$Network=left(rownames(myfiles.jac.long),8)

head(myfiles.jac.long)
colnames(myfiles.jac.long)[1:2]=c("Poll_sp1","Poll_sp2")

myfiles.jac.long$IGenus=word(gsub("\\."," ",myfiles.jac.long$Poll_sp1),1)
head(myfiles.jac.long)
myfiles.jac.long=merge(myfiles.jac.long,poll_famord_5[,c("IGenus","Order","Family")], by = "IGenus")
head(myfiles.jac.long)
colnames(myfiles.jac.long)=c("PGenus1","Poll_sp1","Poll_sp2","jac","Network","Order1","Family1")
head(myfiles.jac.long)
myfiles.jac.long$IGenus=word(gsub("\\."," ",myfiles.jac.long$Poll_sp2),1)
myfiles.jac.long=merge(myfiles.jac.long,poll_famord_5[,c("IGenus","Order","Family")], by = "IGenus")
colnames(myfiles.jac.long)=c("PGenus2","PGenus1","Poll_sp1","Poll_sp2","jac","Network","Order1","Family1","Order2","Family2")
str(reference)
head(myfiles.jac.long)
reference$Clim=left(reference$ClimateZ,1)

myfiles.jac.long=merge(myfiles.jac.long,reference[,c("Latitude","Longitude","Clim","Network","Connectance","ele")], by = "Network")
str(myfiles.jac.long)
head(myfiles.jac.long)
myfiles.jac.long[myfiles.jac.long$Family1%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("Order1")]="Bee"
myfiles.jac.long[myfiles.jac.long$Family1%in%c("Syrphidae"),c("Order1")]="Syrphidae"

myfiles.jac.long[myfiles.jac.long$Family1%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("Order1")]="Bee"
myfiles.jac.long[myfiles.jac.long$Family1%in%c("Syrphidae"),c("Order1")]="Syrphidae"

myfiles.jac.long.sub1=subset(myfiles.jac.long, Order1 %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))
myfiles.jac.long.sub1=subset(myfiles.jac.long, Order2 %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))


myfiles.jac.long.sub1$jac.fam=paste(myfiles.jac.long.sub1$Family1,myfiles.jac.long.sub1$Family2,sep="_")
myfiles.jac.long.sub1$jac.ord=paste(myfiles.jac.long.sub1$Order1,myfiles.jac.long.sub1$Order2,sep="_")

levels(as.factor(myfiles.jac.long.sub1$jac.ord))

myfiles.jac.long.sub1$jac.fam



