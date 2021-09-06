setwd("~/Dropbox/PhD/Rprojects/Geonet/data/newdata")
files = list.files(pattern="*.csv")
my_new_files = lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE, sep=","))
setwd("~/Dropbox/PhD/Rprojects/Geonet")
str(my_new_files)

new_ref=read.csv("~/Dropbox/PhD/Rprojects/Geonet/data/ref/newreferences.csv")
new_ref
head(my_new_files)

names(my_new_files)=new_ref[5:19,1]
my_new_files[[1]]

library(reshape2)
library(stringr)
my_new_files.melt=melt(my_new_files,id.vars=c(1))

my_new_files.melt=my_new_files.melt[!is.na(my_new_files.melt$value)==TRUE,]
my_new_files.melt$L1=word(my_new_files.melt$L1,start=1,end=2)
my_new_files.melt$variable=gsub("\\."," ",my_new_files.melt$variable)

##############read long format networks
setwd("~/Dropbox/PhD/Rprojects/Geonet/data/newdata/formatted")
files = list.files(pattern="*.csv")
long_form_new = lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE, sep=","))
setwd("~/Dropbox/PhD/Rprojects/Geonet")

long_form_new=do.call("rbind",long_form_new)

 
head(my_new_files.melt)
head(long_form_new)

new_files_melted=rbind(my_new_files.melt,long_form_new)
head(new_files_melted)
new_files_melted.agg=aggregate(value ~ X + variable+L1,data=new_files_melted, FUN=sum)
head(new_files_melted.agg)
new_files_melted.agg$value=ifelse(new_files_melted.agg$value > 0, 1, 0)

new_files_melted.agg=new_files_melted.agg[!new_files_melted.agg$value==0,]


new_files_melted.agg$PGenus=word(new_files_melted.agg$X,1)
new_files_melted.agg$IGenus=word(new_files_melted.agg$variable,1)

new_files_melted.agg[new_files_melted.agg$PGenus%in%"Lasioglossum",]

####add plant families
#Plants in X but not in Y
minus_plant=setdiff(unique(new_files_melted.agg$PGenus),plant_family$PGenus)
library(taxize)
plant_family_new=tax_name(query=minus_plant,
                          get=c("family","order"),
                          db="ncbi", division_filter = "Plantae",
                          rank_query="genus")
write.csv(plant_family_new,"data/plant_family_new.csv")

#remove plant families from bird networks
plant_family_subset=plant_family[!rowSums(plant_family==minus_plant),]%>%droplevels()

setdiff(new_files_melted.agg$PGenus,plant_family_subset$PGenus)

new_files_melted.agg=merge(new_files_melted.agg,
      plant_family_subset[!duplicated(plant_family_subset$PGenus),
                          c("PGenus","family","order")], by = "PGenus",all.x=TRUE) 
colnames(new_files_melted.agg)[7:8]=c("PFamily","POrder")
str(new_files_melted.agg)
##Insects
new_files_melted.agg=merge(new_files_melted.agg,poll_famord_5[,c("IGenus","Order","Family")],
                           by = "IGenus",all.x=TRUE)
colnames(new_files_melted.agg)[9:10]=c("PolOrder","PolFamily")

sum(is.na(new_files_melted.agg$PolFamily))



######################################
carvalheiro=read.csv("~/Dropbox/PhD/Rprojects/Geonet/data/newdata/formatted/special/carvalheiro 2008.csv")

library(stringr)
carvalheiro$PolOrder=word(carvalheiro$Order_family,1)
carvalheiro$PolFamily=word(carvalheiro$Order_family,2)
head(carvalheiro)
######################################

