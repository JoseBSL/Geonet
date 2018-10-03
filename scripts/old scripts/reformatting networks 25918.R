setwd("~/Dropbox/PhD/Rprojects/Geonet/data/newdata/formatted")
bart=read.csv("Bartomeus_etal_2008_formatted.csv")
str(bart)
bart_split=split(bart,bart$Network)

bart_list=lapply(bart_split,function (x) dcast(x, Plant ~ Pollinator, 
                                     fun.aggregate = sum, na.rm =T, value.var="Int", fill = 0))


lapply(1:length(bart_list), function(i) write.csv(bart_list[[i]], 
                                                file = paste0(names(bart_list[i]), ".csv"),
                                                row.names = FALSE))

Benadi_2014_6=read.csv("Benadi 2014_6.csv")
str(Benadi_2014_1)
write.csv(dcast(Benadi_2014_6, Plant_species ~ Insect_species, 
      fun.aggregate = sum, na.rm =T, value.var="value", fill = 0),"Benadi_2014_6.csv")

kaiser=read.csv("Kaiser_Bunbury_etal_2017_formatted.csv")
colnames(kaiser)
kaiser_split=split(kaiser,kaiser$Site)
kaiser_list=lapply(kaiser_split, function (x) ddply(x,"Plant.species.ID",numcolwise(sum)))
kaiser_list$Bernica

lapply(1:length(kaiser_list), function(i) write.csv(kaiser_list[[i]], 
                                                  file = paste0(names(kaiser_list[i]), ".csv"),
                                                  row.names = FALSE))


mauritius_valerie=read.csv("mauritius_valerie.csv")
str(mauritius_valerie)
write.csv(dcast(mauritius_valerie, X ~ variable, 
                fun.aggregate = sum, na.rm =T, value.var="value", fill = 0),"mauritius_valerie_1.csv")
