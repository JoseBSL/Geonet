str(geonet)
geonet %>%
  group_by(Pollinator)%>%
  summarize(n=distinct())


n_distinct()

#number of plant species
factor(geonet$Plant) #3162
factor(geonet$PFamily) #210
factor(geonet$POrder) #50



geo.pol.sums <- subset(geonet, PolOrder %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))%>%
  droplevels()

44395/50449

#number of pollinator species
factor(geo.pol.sums$Pollinator) #7135
factor(geo.pol.sums$PolFamily) #215
factor(geo.pol.sums$PolOrder) #50


#write.csv(as.data.frame(levels(factor(g.sub$Locality))),"localities.csv")
local=read.csv("localities.csv")
factor(local$Country)
write

#number of networks per climate zone
network.sums <- g.sub[!duplicated(g.sub$Network),]

network.sums %>%
  group_by(clim) %>%
  summarize(n=n())

###number of networks with vertebrates
levels(factor(geonet$PolOrder))

geo.vert.sums <- subset(geonet, PolOrder %in% c("Passeriformes","Psittaciformes","Squamata"))%>%
  droplevels()

geo.vert.sums.2 <- geo.vert.sums[!duplicated(geo.vert.sums$Network),]

factor(geo.vert.sums$Network)

c("Passeriformes","Psittaciformes","Squamata")

244/5603


(244/50449)*100

nrow(subset(geonet, Network %in% geo.vert.sums.2$Network))
