#Pollinator processing

library(taxize)
library(stringr)


Pollinator=as.data.frame(unique(word(myfiles.melt.agg.z$Pollinator),1))
str(Pollinator)

(7325+1000)-(832*9)

myfiles.melt.agg.z.10=split(Pollinator,c(rep(1,226),
                                                        rep(2,226),
                                                        rep(3,226),
                                                        rep(4,226),
                                                        rep(5,226),
                                                        rep(6,226),
                                                        rep(7,832),
                                                        rep(8,832),
                                                        rep(9,832),
                                                        rep(10,837)))

#poll_famord=tax_name(query=Pollinator[,1],
                       get=c("family","order"),db="ncbi", 
                       division_filter = "Arthropoda",rank_query="genus")
poll_famord$Pollinator=Pollinator[,1]
str(poll_famord[is.na(poll_famord$family),])

885/5
poll_fam_ord_split=split(poll_famord[is.na(poll_famord$family),],c(rep(1,177),
                                                              rep(2,177),
                                                              rep(3,177),
                                                              rep(4,177),
                                                              rep(5,177)))


write.csv(poll_fam_ord_split$`1`,"pol_family_1.csv")
write.csv(poll_fam_ord_split$`2`,"pol_family_2.csv")
write.csv(poll_fam_ord_split$`3`,"pol_family_3.csv")
write.csv(poll_fam_ord_split$`4`,"pol_family_4.csv")
write.csv(poll_fam_ord_split$`5`,"pol_family_5.csv")