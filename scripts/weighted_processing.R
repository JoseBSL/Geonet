wgt.myfiles=myfiles


#NAme list objects/networks
names(wgt.myfiles)=reference$Network[1:183]

##ADd zeros to Traveset
wgt.myfiles$`Traveset_13`
wgt.myfiles$`Traveset_13`[is.na(wgt.myfiles$`Traveset_13`)] <- 0

sum(is.na(wgt.myfiles$`Chacoff_011`))
wgt.myfiles$`Traveset_13`[is.na(wgt.myfiles$`Traveset_13`)] <- 0

#Melt
wgt.myfiles.melt=melt(wgt.myfiles,id.vars=c(1))
str(wgt.myfiles.melt)


wgt.myfiles.wgt=wgt.myfiles.melt[wgt.myfiles.melt$Type%in%"w",]

head(wgt.myfiles.melt)

#Aggregate - merges #not needed
#wgt.myfiles.melt.agg=aggregate(value ~ X + variable+L1,data=wgt.myfiles.melt, FUN=sum)
#wgt.myfiles.melt$value=ifelse(wgt.myfiles.melt$value > 0, 1, 0)
head(wgt.myfiles.melt)

colnames(wgt.myfiles.melt)=c("Plant","Pollinator","Int","Network")

####BUG - NAS### GONE
sum(is.na(wgt.myfiles.melt)) ##0

#remove zeros
wgt.myfiles.melt.z=wgt.myfiles.melt
sum(is.na(wgt.myfiles.melt.z))


wgt.myfiles.melt.z$Pollinator=gsub("\\."," ",wgt.myfiles.melt.z$Pollinator)
wgt.myfiles.melt.z$Pollinator=gsub("\\_"," ",wgt.myfiles.melt.z$Pollinator)
wgt.myfiles.melt.z$Plant=gsub("\\."," ",wgt.myfiles.melt.z$Plant)
wgt.myfiles.melt.z$Plant=gsub("\\_"," ",wgt.myfiles.melt.z$Plant)

##Merge with poll families
wgt.myfiles.melt.z$PGenus=word(wgt.myfiles.melt.z$Plant,1)
wgt.myfiles.melt.z$IGenus=word(wgt.myfiles.melt.z$Pollinator,1)

plant_famord=read.csv("data/processing/plant_famord.csv")
poll_famord=read.csv("data/processing/poll_famord_5.csv")

#check all accounted for
setdiff(wgt.myfiles.melt.z$PGenus,plant_famord$PGenus)
setdiff(wgt.myfiles.melt.z$IGenus,poll_famord$IGenus)

colnames(plant_famord)=c("PFamily","PGenus","POrder")

colnames(poll_famord)=c("IGenus","PolFamily","PolOrder")


##GEONET DATAFRAME
geonet=merge(wgt.myfiles.melt.z,plant_famord, by = "PGenus",all.y=FALSE) 
geonet=merge(geonet,poll_famord, by = "IGenus",all.y=FALSE)

geonet <- geonet %>% mutate_if(is.factor,as.character)

#carvalheiro=read.csv("~/Dropbox/PhD/Rprojects/Geonet/data/newdata/formatted/special/carvalheiro_2_2008.csv")
carvalheiro=read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/H_drive_DT/Geonet/data/newdata/formatted/special/carvalheiro_2_2008.csv")
carvalheiro$Pollinator=word(carvalheiro$Pollinator,1,2)
carvalheiro$Plant=word(carvalheiro$Plant,1,2)
carvalheiro$Pollinator
str(geonet)
str(carvalheiro)
geonet=rbind(geonet,carvalheiro)
head(geonet)

##formatting string problems
geonet$Plant=str_trim(geonet$Plant, side = c("both"))
geonet$Pollinator=str_trim(geonet$Pollinator, side = c("both"))

geonet[geonet$PolFamily%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("PolOrder")]="Bee"
geonet[geonet$PolFamily%in%c("Syrphidae"),c("PolOrder")]="Syrphidae"

geonet=merge(geonet,reference,by="Network",sort=F, all.x = T)
geonet.wgt=geonet[!geonet$Type%in%"b",]

range(geonet.wgt$Int)
