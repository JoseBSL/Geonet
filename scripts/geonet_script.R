##THIS IS JUST THE MARKDOWN COPIED TO A SIMPLE R SCRIPT BECAUSE IT WAS ANNOYING ME

---
  title: "Geonet"
author: "Liam Kendall"
date: "2 October 2018"
output: pdf_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(bayesplot)
library(brms)
library(plyr)
library(dplyr)
library(emmeans)
library(ggplot2)
library(glmmTMB)
library(geonames)
library(gridExtra)
library(kgc)
library(MuMIn)
library(reshape2)
library(stringi)
library(stringr)
library(taxize)
library(tidyr)
library(vegan)

options(stringsAsFactors = FALSE)
reference=read.csv("data/ref/references_update.csv",header=T)
colnames(reference)[1]="Network"

left = function (string,char){
  substr(string,1,char)
}

#reference$ID=left(reference$Network,8)
#reference=reference[!duplicated(reference$ID),]


climate_zone=reference[,c("Network","Longitude","Latitude")]
climate_zone <- data.frame(climate_zone,
                           rndCoord.lon = RoundCoordinates(climate_zone$Longitude),
                           rndCoord.lat = RoundCoordinates(climate_zone$Latitude))
reference <- data.frame(reference,ClimateZ=LookupCZ(climate_zone))
reference[reference$ClimateZ=="Climate Zone info missing",]
reference$clim=left(reference$ClimateZ,1)
table(reference$clim)

##ELEVATION DATA
ele=read.csv("data/processing/elevation.csv")
reference$ele=ele$ele

# First apply read.csv, then rbind
setwd("~/Dropbox/PhD/Rprojects/Geonet/data")
#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/H_drive_DT/Geonet/data")
files = list.files(pattern="*.csv")
myfiles = lapply(files, function(x) read.csv(x,stringsAsFactors = FALSE, sep=","))
setwd("~/Dropbox/PhD/Rprojects/Geonet")
#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/H_drive_DT/Geonet")

#NAme list objects/networks
names(myfiles)=reference$Network[1:183]

##ADd zeros to Traveset
myfiles$`Traveset_13`
myfiles$`Traveset_13`[is.na(myfiles$`Traveset_13`)] <- 0

##Calculate plant richness & amend to reference file
#62 = carvalheiro network already in long form

plant_richness_list=lapply(myfiles,function(x) nrow(x))
poll_richness_list=lapply(myfiles,function(x) length(x[,-1]))

unique(carvalheiro$Plant) #62
unique(carvalheiro$Pollinator) #181

reference$plant_richness=as.integer(c(unlist(plant_richness_list),62))
reference$poll_richness=as.integer(c(unlist(poll_richness_list),181))




#Melt
myfiles.melt=melt(myfiles,id.vars=c(1))
head(myfiles.melt)

#Aggregate - merges #not needed
#myfiles.melt.agg=aggregate(value ~ X + variable+L1,data=myfiles.melt, FUN=sum)
myfiles.melt$value=ifelse(myfiles.melt$value > 0, 1, 0)
head(myfiles.melt)

colnames(myfiles.melt)=c("Plant","Pollinator","Int","Network")

####BUG - NAS### GONE
sum(is.na(myfiles.melt)) ##0

#remove zeros
myfiles.melt.z=myfiles.melt[!myfiles.melt$Int==0,]
sum(is.na(myfiles.melt.z))


myfiles.melt.z$Pollinator=gsub("\\."," ",myfiles.melt.z$Pollinator)
myfiles.melt.z$Pollinator=gsub("\\_"," ",myfiles.melt.z$Pollinator)
myfiles.melt.z$Plant=gsub("\\."," ",myfiles.melt.z$Plant)
myfiles.melt.z$Plant=gsub("\\_"," ",myfiles.melt.z$Plant)

##Merge with poll families
myfiles.melt.z$PGenus=word(myfiles.melt.z$Plant,1)
myfiles.melt.z$IGenus=word(myfiles.melt.z$Pollinator,1)

plant_famord=read.csv("data/processing/plant_famord.csv")
poll_famord=read.csv("data/processing/poll_famord_5.csv")

sum(duplicated(poll_famord$IGenus)==TRUE)
poll_famord=poll_famord[!duplicated(poll_famord$IGenus)==TRUE,]
#check all accounted for
setdiff(myfiles.melt.z$PGenus,plant_famord$PGenus)
setdiff(myfiles.melt.z$IGenus,poll_famord$IGenus)

colnames(plant_famord)=c("PFamily","PGenus","POrder")

colnames(poll_famord)=c("IGenus","PolFamily","PolOrder")

plant_famord=plant_famord[plant_famord$PGenus%in%intersect(plant_famord$PGenus,myfiles.melt.z$PGenus),]
poll_famord=poll_famord[poll_famord$IGenus%in%intersect(poll_famord$IGenus,myfiles.melt.z$IGenus),]

##GEONET DATAFRAME
geonet=merge(myfiles.melt.z,plant_famord, by = "PGenus",all.y=FALSE) 
geonet=merge(geonet,poll_famord, by = "IGenus",all.y=FALSE)

setdiff(geonet$IGenus,myfiles.melt.z$IGenus)

geonet <- geonet %>% mutate_if(is.factor,as.character)

#carvalheiro=read.csv("~/Dropbox/PhD/Rprojects/Geonet/data/newdata/formatted/special/carvalheiro_2_2008.csv")
carvalheiro=read.csv("~/Dropbox/PhD/Rprojects/Geonet/data/newdata/formatted/special/carvalheiro_2_2008.csv")
carvalheiro$Pollinator=word(carvalheiro$Pollinator,1,2)
carvalheiro$Plant=word(carvalheiro$Plant,1,2)
geonet=rbind(geonet,carvalheiro)

##formatting string problems
geonet$Plant=str_trim(geonet$Plant, side = c("both"))
geonet$Pollinator=str_trim(geonet$Pollinator, side = c("both"))

geonet[geonet$PolFamily%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("PolOrder")]="Bee"
geonet[geonet$PolFamily%in%c("Syrphidae"),c("PolOrder")]="Syrphidae"

geonet=merge(geonet,reference,by="Network",sort=F, all.x = T)

g=geonet%>%
  group_by(Network, PolOrder) %>%
  summarise(order_links=sum(Int))

g2=g%>%
  group_by(Network) %>%
  summarise(int_tot=sum(order_links))

g3=merge(g,g2)
g3$prop_links=g3$order_links/g3$int_tot

g4=merge(g3,reference,by="Network")

#subset data by insect order
g.sub <- subset(g4, PolOrder %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))
str(g.sub)

```



```{r dissimilarity}

###############################################
#interaction dissimilarity----
###############################################

#combine plants and pollinators
geonet$interaction <- paste(geonet$PFamily,"_",geonet$PolFamily)

#group by climate, network and elevation
intdist=geonet %>% group_by(Network,clim,ele) %>% count(interaction)

#cast to wide format
intdist=dcast(intdist, Network + clim + ele ~ interaction, 
              fun.aggregate = sum, na.rm =T, value.var="n", fill = 0)
rownames(intdist)=intdist[,1]

#compute dissimilarity matrix 
intdist.matrix=raupcrick(intdist[,4:length(intdist)],null="r1") #Presence-absence
mj2=metaMDS(intdist.matrix,k=3)

#extract ordination coordinates
NMDS1 = data.frame(MDS1 = mj2$points[,1], MDS2 = mj2$points[,2],site=unique(intdist$Network), clim=intdist$clim)
NMDS1$clim=as.factor(NMDS1$clim)
#unable to create ellipse for B (to few points)... need to add them manually at some stage
NMDS1 <- subset(NMDS1, clim %in% c("A","B", "C","D", "E"))%>%droplevels

#create ellipse function (this is hidden in vegan)
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#compute ellipse values
df_ell1 <- data.frame()
for(g in levels(NMDS1$clim)){
  df_ell1 <- rbind(df_ell1, cbind(as.data.frame(with(NMDS1[NMDS1$clim==g,],
             veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,
             center=c(mean(MDS1),mean(MDS2))))) ,group=g))}

```

```{r dissimilarity tests}

#run dissimilarity tests
intdist.perm <- adonis(intdist.matrix ~ intdist$clim, permutations = 999)
write.csv(intdist.perm[1], "tables_for_publication/interactions_permanova.csv")

#other tests - not for results
adonis(intdist.matrix ~ intdist$clim+intdist$ele, permutations = 999)
adonis(intdist.matrix ~ intdist$clim*(intdist$ele), permutations = 999)
adonis(intdist.matrix ~ intdist$clim+scale(intdist$ele), permutations = 999)
#very small amount explained by elevation

#run beta-dispersion test
beta_int=betadisper(intdist.matrix, intdist$clim, type = c("centroid"))
tukey_beta_int <- TukeyHSD(beta_int) ## all good!
write.csv(tukey_beta_int$group, "tables_for_publication/interactions_betadispersion.csv")

#run pariwise adonis function
pairwise.adonis <- function(x,factors, sim.function = 'vegdist', sim.method = 'bray', p.adjust.m ='fdr')
{
  library(vegan)
  co = combn(unique(as.character(factors)),2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  for(elem in 1:ncol(co)){
    if(sim.function == 'daisy'){
      library(cluster); x1 = daisy(x[factors %in% c(co[1,elem],co[2,elem]),],metric=sim.method)
    } else{x1 = raupcrick(x[factors %in% c(co[1,elem],co[2,elem]),])}
    ad = adonis(x1 ~ factors[factors %in% c(co[1,elem],co[2,elem])] );
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  sig = c(rep('',length(p.adjusted)))
  sig[p.adjusted <= 0.05] <-'.'
  sig[p.adjusted <= 0.01] <-'*'
  sig[p.adjusted <= 0.001] <-'**'
  sig[p.adjusted <= 0.0001] <-'***'
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted,sig)
  print("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  return(pairw.res)
}

#run pairwise function on the matrix
pw.int.adonis <- pairwise.adonis(intdist[,3:6764],intdist$clim)
write.csv(pw.int.adonis, "tables_for_publication/pairwise_adonis_interactions.csv")

#plot the plant-pollinator interaction ordination
p2 <- ggplot(data = NMDS1, aes(MDS1, MDS2))
p2 <- p2 + geom_point(aes(colour = clim), size=2)
p2 <- p2 + geom_path(data=df_ell1, aes(x=MDS1, y=MDS2,colour=group), size=1, linetype=2)
p2 <- p2 + annotate("text", x = 0.4, y = 0.48, label = "F = 63.58; P < 0.001")
p2 <- p2 + annotate("text", x = 0.35, y = 0.45, label = "italic(R) ^ 2 == 0.59", 
                    parse = TRUE)
p2 <- p2 + annotate("text", x = 0.36, y = 0.42, label = "Stress = 0.12") 
p2 <- p2 + theme(axis.line.x = element_line(size=0, colour = "black"),
                 axis.line.y = element_line(size=0, colour = "black"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_rect(size=.8, colour = "black", fill=NA),
                 panel.background = element_blank()) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.title.x=element_text(size=15, vjust = 1),
        axis.title.y=element_text(size=15, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(axis.ticks.length = unit(2.5, "mm"))
p2 <- p2 + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p2 <- p2 + scale_colour_brewer(palette = "Dark2")
p2
ggsave("graphs/interactions_ordination.pdf")

###############################################
#PLANT FAMILY DISSIMILARITY----
###############################################

pfdist=geonet %>% group_by(Network,clim,ele) %>% count(PFamily)
pfdist=dcast(pfdist, Network + clim + ele ~ PFamily, 
             fun.aggregate = sum, na.rm =T, value.var="n", fill = 0)
rownames(pfdist)=pfdist[,1]
str(pfdist)

pfdist.matrix=raupcrick(pfdist[,4:length(pfdist)],null="r1") #Presence-absence

mj=metaMDS(pfdist.matrix,k=3)

NMDS = data.frame(MDS1 = mj$points[,1], MDS2 = mj$points[,2],site=unique(pfdist$Network), clim=pfdist$clim)

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

###############################################
#pollinator dissimilarity----
###############################################
poldist=geonet %>% group_by(Network,clim,ele) %>% count(PolFamily)

poldist=dcast(poldist, Network + clim + ele ~ PolFamily, 
              fun.aggregate = sum, na.rm =T, value.var="n", fill = 0)
rownames(poldist)=poldist[,1]

poldist.matrix=raupcrick(poldist[,4:length(poldist)],null="r1") #Presence-absence

mj2=metaMDS(poldist.matrix,k=3)

NMDS1 = data.frame(MDS1 = mj2$points[,1], MDS2 = mj2$points[,2],site=unique(poldist$Network), clim=poldist$clim)
NMDS1$clim=as.factor(NMDS1$clim)
#unable to create ellipse for B (to few points)... need to add them manually at some stage
NMDS1 <- subset(NMDS1, clim %in% c("A","B", "C","D", "E"))%>%droplevels

df_ell1 <- data.frame()
for(g in levels(NMDS1$clim)){
  df_ell1 <- rbind(df_ell1, cbind(as.data.frame(with(NMDS1[NMDS1$clim==g,],
                   veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),
                   length(MDS1)))$cov,center=c(mean(MDS1),mean(MDS2))))),group=g))}

p1 <- ggplot(data = NMDS1, aes(MDS1, MDS2))
p1 <- p1 + geom_point(aes(colour = clim), size=2)
#p <- p + geom_text(aes(label = site, colour=landuse), vjust = -0.8)
p1 <- p1 + geom_path(data=df_ell1, aes(x=MDS1, y=MDS2,colour=group), size=1, linetype=2)
p1 <- p1 + theme(panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black")) +
  theme(panel.border=element_rect(colour = "black", fill = "NA", size = 1)) +
  theme(axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.title.y=element_text(size=16, vjust = 1),
        axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.title.x=element_text(size=16, vjust = 1),
        axis.text=element_text(colour = "black"))
p1 <- p1 + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p1 <- p1 + scale_colour_brewer(palette = "Dark2")

#indicator species code - not using at the moment
pol.ind = multipatt(poldist[,3:342],poldist$clim, control = how(nperm=999)) 
summary(pol.ind) 

####################################
#plant-polinator procrustes----
####################################

pol.plant.pro=protest(poldist.matrix, pfdist.matrix,permutations=999)
pol.plant.pro

ctest <- data.frame(rda1=pol.plant.pro$Yrot[,1],
                    rda2=pol.plant.pro$Yrot[,2],xrda1=pol.plant.pro$X[,1],
                    xrda2=pol.plant.pro$X[,2])

p3=ggplot(ctest) +
  geom_point(aes(x=rda1, y=rda2),pch=1) +
  geom_point(aes(x=xrda1, y=xrda2),pch=2) +
  geom_segment(aes(x=xrda1,y=xrda2,xend=rda1,yend=rda2),arrow=arrow(length=unit(0.2,"cm")))+
  theme_bw()+ylab("Dimension 2")+xlab("Dimension 1")+theme(aspect.ratio = 1)
p3

```

#```{r test prop links across climate and elevation}
#library(MuMIn)
#library(lme4)
#library(stargazer)
#library(DHARMa)#
#
#prop_mod1=brm(prop_links~PolOrder*clim*ele+(1|Reference/Network),
#              family=beta_family(link = "logit"),
#              data=g.sub,cores=4)
#
#pp_check(prop2,nsamples=100)
#loo(prop2)
#bayes_R2(prop2)
#```



```{r specialisation}


#################################################
#correct number of links for sampling effort with Bascompte null model
#################################################
library(reshape2)
library(vegan)
library(plyr)
library(dplyr)
library(ggplot2)

#cast long format dataframe
geo.wide <- dcast(geonet, Network + Plant ~ Pollinator, value.var = "Int")
geo.wide[is.na(geo.wide)] <- 0

#gg <- subset(geo.wide, Network %in% c("M_PL_001")) %>% droplevels
#create empty listy
sp.links <- c()
sp.links <- list(sp.links)

#run loop over each site
for (j in levels(geo.wide[, 1])){
  web <- subset(geo.wide, Network == j)#iterate over site
  web <- web[,c(-1,-2)]
  web = web[,colSums(web) > 0]#remove species with no links at each site
  
  #Null model II from Bascompte et al. (2003). Creates random networks by probabilistically fixing row and column marginal totals. The expected number of links is same as observed number. Rodriguez-Girona and Santamaria (2006) showed that this null model has the best compromise between Type I and Type II errors
  #Run this code once to create the null model function
  null.model.II <- function(web){
    web <- as.matrix(web > 0) + 0
    # calculate the probability based on row marginals. Creates matrix same size as web, with row sums divided by number of columns (to get probability of a 1 in each cell of each row), repeated across all columns for each row.
    row.probs <- matrix(rowSums(web)/ncol(web),nrow(web),ncol(web))
    # calculate the probability based on column marginals (again, repeated for whole column). Transpose used instead of byrow=T
    col.probs <- t(matrix(colSums(web)/nrow(web),ncol(web),nrow(web)))
    # calculate the element by element mean of this probabilities
    mat.probs <- (row.probs + col.probs) / 2.0
    # generate a random matrix with 1s proportional to the above probabilities. rbinom(n, size, prob) n is number of observations, size is number of trials, prob is prob of success in each trial
    mat.null <- matrix(rbinom(nrow(web)*ncol(web),1,as.vector(mat.probs)),nrow(web),ncol(web))  
    # return that matrix in all its glory
    return(mat.null)
  }
  
  #Begin permutation test (two tailed)
  reps <- 999 #set number of permutations
  
  nulls<-vector("list",reps)  # Create a list with spaces for each output matrix
  for (i in 1:reps) {
    nulls[[i]]<-null.model.II(web)
  }
  
  #call any individual matrix from that list using nulls[[x]], where x is the number of the matrix you want to call
  null.links <- data.frame(matrix(, nrow=reps, ncol(web)))
  for (i in 1:reps) {
    null.links[i, ] <- t(colSums(nulls[[i]]))
  }
  
  weblink <- t(colSums(as.matrix(web > 0) + 0))
  colnames(null.links) <- colnames(weblink)
  links <- rbind(null.links,weblink)#Add observed connectance into distribution
  sd <- apply(null.links, 2, sd)#calculate standard deviation
  sp.links[[j]] <- abs(weblink - colMeans(null.links)/sd)#print results into list
  
}

#convert list to dataframe and melt
sp.links.df <- rbind.fill(lapply(sp.links, as.data.frame))
sp.links.df$Network <- levels(geonet$Network)
sp.links.melt <- melt(sp.links.df, "Network", variable.name = "Pollinator", value.name = "value", na.rm = TRUE)

#add order and family to dataframe
geo.uni <- unique(geonet[c("Pollinator", "PolFamily", "PolOrder")])
sp.links.order <- merge(sp.links.melt,geo.uni, by="Pollinator")
sp.links.order <- merge(sp.links.order,reference[,c("Network","ele","clim")], by="Network")
ord <- c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera")
sp.links.order.sub <- filter(sp.links.order, PolOrder %in% ord)



#plot generalisation by climate zone
p <- ggplot()
p <- p + xlab("Climate zone") + ylab("Number of links (Z score)")
p <- p + theme(text = element_text(size=18))
p <- p + geom_boxplot(data=sp.links.order.sub, aes(x=clim, y=log(value), color=PolOrder))
p <- p + geom_jitter(data=links.clim.sub, aes(x=clim, y=value, color=PolOrder, fill=PolOrder),
                     alpha=1, size=2.5, position = position_jitter(width = 0.25))
p
p <- p + facet_wrap(~PolOrder, scales="free")
p <- p + theme(panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black")) +
  theme(panel.border=element_rect(colour = "black", fill = "NA", size = 1)) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =16),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =16),
        axis.title.y=element_text(size=24, vjust = 1),
        axis.title.x=element_text(size=24, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(strip.background = element_rect(colour="NA", fill=NA),
        strip.text = element_text(size=20))
p <- p + theme(panel.spacing.x=unit(1, "lines"),panel.spacing.y=unit(1, "lines"))
p <- p + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p <- p + scale_color_brewer(palette="Set2")
p <- p + scale_fill_brewer(palette="Set2")
p <- p + theme(legend.position="none")
p

#plot generalisation on world map
WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

map <- ggplot()
map <- map + xlab("Longitude") + ylab("Latitude")
map <- map + geom_map(data=WorldData, map=WorldData,
                      aes(x=long, y=lat, group=group, map_id=region))
#map <- map + geom_point(data=filter(links.clim, Order == "Coleoptera"),
map <- map + geom_point(data=sp.links.order.sub,
                        aes(x=Longitude, y=Latitude, colour=log(value)),size=2)
map <- map + scale_colour_gradient(low="green",high="red")
map <- map + facet_wrap(PolOrder~clim,ncol = 5)
map <- map + theme(axis.line.x = element_line(size=.5, colour = "black"),
                   axis.line.y = element_line(size=.5, colour = "black"),
                   panel.grid.major = element_line(colour = "#d3d3d3"),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =14),
        axis.title.x=element_text(size=30, vjust = 1),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =14),
        axis.title.y=element_text(size=30, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(axis.ticks.length = unit(2, "mm"))+
  theme(strip.background = element_rect(colour="NA", fill=NA),
        strip.text = element_text(size=14))
map <- map + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
map

#################################################
#END
#################################################
#################################################
#END
#################################################
```


#```{r specialisation model}
#
#
#str(sp.links.order)
#range(sp.links.order$value)
#gen.spec=brm(value~PolOrder*clim+(scale(ele)|Network),
#              family="gaussian",
#             data=sp.links.order,cores=4)
#
#
#```
