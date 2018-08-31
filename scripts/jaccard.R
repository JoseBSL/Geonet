library(stringr)

remove_zero_cols <- function(df) {
  rem_vec <- NULL
  for(i in 1:ncol(df)){
    this_sum <- summary(df[,i])
    zero_test <- length(which(this_sum == 0))
    if(zero_test == 6) {
      rem_vec[i] <- names(df)[i]
    }
  }
  features_to_remove <- rem_vec[!is.na(rem_vec)]
  rem_ind <- which(names(df) %in% features_to_remove)
  df <- df[,-rem_ind]
  return(df)
}

geo.split <- split(geo.wide,geo.wide$Network)

geo.split=lapply(geo.split,function(x)remove_zero_cols(x))

geo.split.jac=lapply(geo.split,function(x) vegdist(t(x[,3:length(x)]), method="jaccard",upper=T,binary=T))
geo.split.jac.long=lapply(geo.split.jac,function(x) melt(as.matrix(x)))
geo.split.jac.long=do.call("rbind",geo.split.jac.long)

geo.split.jac.long$Network=left(rownames(geo.split.jac.long),8)

head(geo.split.jac.long)
colnames(geo.split.jac.long)[1:2]=c("Poll_sp1","Poll_sp2")

str(geo.split.jac.long)

#GENUS 1
geo.split.jac.long$IGenus=word(gsub("\\."," ",geo.split.jac.long$Poll_sp1),1)

str(geo.split.jac.long)
geo.split.jac.long=merge(geo.split.jac.long,poll_famord_5[,c("IGenus","Order","Family")], by = "IGenus")

colnames(geo.split.jac.long)=c("PGenus1","Poll_sp1","Poll_sp2","jac","Network","Order1","Family1")

#GENUS 2
geo.split.jac.long$IGenus=word(gsub("\\."," ",geo.split.jac.long$Poll_sp2),1)

geo.split.jac.long=merge(geo.split.jac.long,poll_famord_5[,c("IGenus","Order","Family")], by = "IGenus")
str(geo.split.jac.long)

colnames(geo.split.jac.long)=c("PGenus2","PGenus1","Poll_sp1","Poll_sp2","jac","Network","Order1","Family1","Order2","Family2")
str(geo.split.jac.long)

geo.split.jac.long=merge(geo.split.jac.long,reference[,c("Latitude","Longitude","clim","Network","Connectance","ele")], by = "Network")
str(geo.split.jac.long)
head(geo.split.jac.long)

#MERGE WITH NULL DISTRIBUTION#####
setdiff(geo.split.jac.long$Poll_sp2,null.net$Poll_sp2)
null.geo=merge(geo.split.jac.long, null.net,by=c("Poll_sp1","Poll_sp2","Network"))%>%droplevels()

#compute z scores
null.geo$z <- abs(null.geo$jac - null.geo$mean/null.geo$sd)#print results into list
head(null.geo)
#########SUBSET########
null.geo <- null.geo %>% mutate_if(is.factor,as.character)
null.geo[null.geo$Family1%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("Order1")]="Bee"
null.geo[null.geo$Family1%in%c("Syrphidae"),c("Order1")]="Syrphidae"

null.geo[null.geo$Family2%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("Order1")]="Bee"
null.geo[null.geo$Family2%in%c("Syrphidae"),c("Order1")]="Syrphidae"

null.geo.sub1=subset(null.geo, Order1 %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))
null.geo.sub1=subset(null.geo.sub1, Order2 %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))
null.geo.sub1 <- null.geo.sub1 %>% mutate_if(is.character,as.factor)

null.geo.sub1$jac.fam=as.factor(paste(null.geo.sub1$Family1,null.geo.sub1$Family2,sep="_"))
null.geo.sub1$jac.ord=as.factor(paste(null.geo.sub1$Order1,null.geo.sub1$Order2,sep="_"))

null.geo.sub1$jac.ord

null.geo.sub1[is.finite(null.geo.sub1$z), ]

library(nlme)
library(lme4)
m1 <- lmer(z~jac.ord + (1|Network), data=null.geo.sub1)
