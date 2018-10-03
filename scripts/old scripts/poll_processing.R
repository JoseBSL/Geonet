#Pollinator processing

library(taxize)
library(stringr)


Pollinator=as.data.frame(unique(word(myfiles.melt.agg.z$Pollinator),1))
str(Pollinator)
colnames(Pollinator)[1]="Genus"
poll_famord=tax_name(query=Pollinator[,1],
                       get=c("family","order"),division_filter=c("Arthropoda"),db="ncbi",rank_query="genus")

poll_famord$Genus=Pollinator[,1]

str(poll_famord[is.na(poll_famord$family),])

884/5
176+176+176+177+177
poll_fam_ord_split=split(poll_famord[is.na(poll_famord$family),],c(rep(1,176),
                                                              rep(2,177),
                                                              rep(3,177),
                                                              rep(4,177),
                                                              rep(5,177)))


write.csv(poll_fam_ord_split$`1`,"data/pol_family_1.csv")
write.csv(poll_fam_ord_split$`2`,"data/pol_family_2.csv")
write.csv(poll_fam_ord_split$`3`,"data/pol_family_3.csv")
write.csv(poll_fam_ord_split$`4`,"data/pol_family_4.csv")
write.csv(poll_fam_ord_split$`5`,"data/pol_family_5.csv")

Apodiformes
Passeriformes
Squamata
Trombidiformes
poll_famord$Order=as.factor(poll_famord$Order)
poll_famord[poll_famord$Order %in% c("Apodiformes"),]
poll_famord[poll_famord$Order %in% c("Passeriformes"),]

maybe_birds=unique(myfiles.melt.agg.z[myfiles.melt.agg.z$Genus %in% 
                            poll_famord[poll_famord$Order %in% c("Apodiformes","Passeriformes","Trombidiformes"),]$Genus,]$Network)


maybe_geckos=unique(myfiles.melt.agg.z[myfiles.melt.agg.z$Genus %in% 
                                         poll_famord[poll_famord$Order %in% c("Squamata"),]$Genus,])


##BIRD GENERA
poll_famord[poll_famord$Order %in% c("Apodiformes","Passeriformes","Trombidiformes"),]


##REPTILE GENERA
poll_famord[poll_famord$Order %in% c("Squamata"),]$Genus,]
