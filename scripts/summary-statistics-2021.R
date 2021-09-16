##summary statistics
levels(factor(geonet$plant.family))
levels(factor(geonet$plant.order))
levels(factor(geonet$Plant))

levels(factor(geonet$animal.family))
levels(factor(geonet$animal.order))
levels(factor(geonet$Pollinator))

totals <- geonet%>%
  group_by(animal.order)%>%
  count()%>%
  arrange(desc(n))

#insect poll numbers
geonet.insects <- geonet%>%filter(animal.order%in%c("Coleoptera","Bee",
                                  "Hymenoptera","Diptera",
                                  "Lepidoptera"))

levels(factor(geonet.insects$animal.family))
levels(factor(geonet.insects$animal.order))
levels(factor(geonet.insects$Pollinator))


#insect totals
sum(totals[1:6,2])/sum(totals[,2])

sum(totals[totals$animal.order%in%c("Apodiformes",
                                    "Psittaciformes",
                                    "Passeriformes",
                                    "Squamata"),2])/sum(totals[,2])

##totals within their networks
bird.rep <- c("Apodiformes",
              "Psittaciformes",
              "Passeriformes",
              "Squamata")

bird.rep.nets <- geonet%>%
  filter(animal.order%in%bird.rep)%>%
  select((Network))

bird.rep.nets <- unique(bird.rep.nets)

bird.rep.sums <- geonet%>%
  filter(Network%in%bird.rep.nets$Network)%>%
  group_by(animal.order)%>%
  count()%>%
  arrange(desc(n))

sum(bird.rep.sums[bird.rep.sums$animal.order%in%c("Apodiformes",
                                    "Psittaciformes",
                                    "Passeriformes",
                                    "Squamata"),2])/sum(bird.rep.sums[,2])
