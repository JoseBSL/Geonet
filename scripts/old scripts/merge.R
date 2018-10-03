bas_tax=read.csv("data/bas_tax.csv")

myfiles.melt.agg.z$Genus=word(myfiles.melt.agg.z$Pollinator,1)
str(myfiles.poll.merge)

head(bas_tax)

colnames(bas_tax)= c("Order","Family","Genus")
Pollinator
poll_NA=poll_famord[is.na(poll_famord$family),]

merge=merge(poll_famord,bas_tax[unique(bas_tax$Genus),],
      by.x ="Genus",by.y="Genus")
merge=merge[unique(merge$Genus),]

library(dplyr)

colnames(poll_famord)=c("db","query","Family","Order","Genus")

poll_famord_bas <- left_join(poll_famord,merge, by = "Genus") %>% # this will generate age.x and age.y
  mutate(Order = ifelse(is.na(Order), as.character(Order.y), Order),
         Family = ifelse(is.na(Family), as.character(Family.y), Family)) %>% # we generate a joint 'age' variable
  select(-Order.y, -Order.x,-Family.y, -Family.x,-db.y,-query.y) # drop the superfluous columns

poll_NA=poll_famord_bas[is.na(poll_famord_bas$Family),]

levels(as.factor(poll_famord_bas$Order))



write.csv(myfiles.poll.merge,"merge.csv")

sum(is.na(myfiles.poll.merge$FAMILY))
