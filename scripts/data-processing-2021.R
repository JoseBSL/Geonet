##removed hummingbird studies: M_PL_063,M_PL_065,  M_PL_067, M_PL_070 -in new folder

##Remove oil flower paper M_PL_059



library(kgc)
library(tidyverse)
library(reshape2)
library(taxize)
library(plyr)
library(dplyr)
library(stringi)
library(stringr)
library(tidyr)
library(glmmTMB)

options(stringsAsFactors = FALSE)
reference=read.csv("data/ref/references.csv",header=T)
colnames(reference)[1]="Network"

left = function (string,char){
  substr(string,1,char)
}
reference$ID=left(reference$Network,8)
reference=reference[!duplicated(reference$ID),]


climate_zone=reference[,c("ID","Longitude","Latitude")]
climate_zone <- data.frame(climate_zone,
                   rndCoord.lon = RoundCoordinates(climate_zone$Longitude),
                   rndCoord.lat = RoundCoordinates(climate_zone$Latitude))
reference <- data.frame(reference,ClimateZ=LookupCZ(climate_zone))
reference[reference$ClimateZ=="Climate Zone info missing",]

##GALAPAGOS and MAURITIUS missing
reference$ClimateZ=as.character(reference$ClimateZ)
reference[26,c("ClimateZ")]=c("BWh")
reference[60,c("ClimateZ")]=c("Af")
table(reference$ClimateZ)

# First apply read.csv, then rbind
setwd("~/Dropbox/PhD/Rprojects/Geonet/data")
files = list.files(pattern="*.csv")
myfiles = lapply(files, function(x) read.csv(x,stringsAsFactors = FALSE,row.names=1, sep=","))
setwd("~/Dropbox/PhD/Rprojects/Geonet")




#NAme list objects/networks
names(myfiles)=left(files,8)
names(myfiles)

##Melt list items to long form
#myfiles.melt=lapply(myfiles, function(x) melt(x,id.vars=c(1)))

#Melt
myfiles.melt=melt(myfiles,id.vars=c(1))

#Aggregate - merges 
myfiles.melt.agg=aggregate(value ~ X + variable+L1,data=myfiles.melt, FUN=sum)
myfiles.melt.agg$value=ifelse(myfiles.melt.agg$value > 0, 1, 0)
colnames(myfiles.melt.agg)=c("Plant","Pollinator","Network","Int")

myfiles.melt.agg.z=myfiles.melt.agg[!myfiles.melt.agg$Int==0,]
myfiles.melt.agg.z$Pollinator=gsub("\\."," ",myfiles.melt.agg.z$Pollinator)

#myfiles.melt.agg.mun=dplyr::filter(myfiles.melt.agg.z, !grepl('Unidentified', Pollinator))

#myfiles.melt.agg.poll=myfiles.melt.agg.mun
#myfiles.melt.agg.plant=myfiles.melt.agg.z

#myfiles.melt.agg.z.4=split(myfiles.melt.agg.z , f = c(rep(1,9555),rep(2,9555),rep(3,9554),rep(4,9554)))


#plant_family_1=tax_name(query=word(myfiles.melt.agg.z.4$`1`$Plant,1),
#get=c("family"),db="ncbi", 
#division_filter = "Plantae",rank_query="genus")

#plant_family_2=tax_name(query=word(myfiles.melt.agg.z.4$`2`$Plant,1),
#                        get=c("family"),db="ncbi", 
#                        division_filter = "Plantae",rank_query="genus")


#plant_family_3=tax_name(query=word(myfiles.melt.agg.z.4$`3`$Plant,1),
#get=c("family"),db="ncbi", 
#division_filter = "Plantae",rank_query="genus")
#plant_family_4=tax_name(query=word(myfiles.melt.agg.z.4$`4`$Plant,1),
#                        get=c("family"),db="ncbi", 
#                        division_filter = "Plantae",rank_query="genus")



plant_family[unique(plant_family$PGenus)]
write.csv(plant_family,"data/plant_family.csv")

#FILL PLANT FAMILY NA's

table(myfiles.melt.agg.un[is.na(myfiles.melt.agg.un),]$Plant,
      myfiles.melt.agg.un[myfiles.melt.agg.un$Pollinator==c("Unidentified"),]$Network)

plant_family=rbind(plant_family_1,plant_family_2,plant_family_3,plant_family_4)

plant_family[plant_family$query ==  c("Linaria") & is.na(plant_family$family),3]="Plantaginaceae"
plant_family[plant_family$query ==  c("Adesmia") & is.na(plant_family$family),3]="Papilionoideae"
plant_family[plant_family$query ==  c("Adesmia") & plant_family$family ==c("Papilionoideae"),3]="Fabaceae"
plant_family[plant_family$query ==  c("Leuceria") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Mustisia") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Perezia") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Salpiglossus") & is.na(plant_family$family),3]="Solanaceae"
plant_family[plant_family$query ==  c("Scypanthus") & is.na(plant_family$family),3]="Loasaceae"
plant_family[plant_family$query ==  c("Laretia") & is.na(plant_family$family),3]="Apiaceae"
plant_family[plant_family$query ==  c("Werneria") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Stenodraba") & is.na(plant_family$family),3]="Brassicaceae"
plant_family[plant_family$query ==  c("Viola") & is.na(plant_family$family),3]="Violaceae"
plant_family[plant_family$query ==  c("Arctostaphylus") & is.na(plant_family$family),3]="Ericaceae"
plant_family[plant_family$query ==  c("Chamaenerium") & is.na(plant_family$family),3]="Onagraceae" ##Bad spelling Chamaenerion
plant_family[plant_family$query ==  c("Mertensia") & is.na(plant_family$family),3]="Boraginaceae"
plant_family[plant_family$query ==  c("Opulaster") & is.na(plant_family$family),3]="Rosaceae" #Synonym Physocarpus
plant_family[plant_family$query ==  c("Pentstemon") & is.na(plant_family$family),3]="Plantaginaceae"
plant_family[plant_family$query ==  c("Rydbergia") & is.na(plant_family$family),3]="Asteraceae" #Synonym Hymenoxys
plant_family[plant_family$query ==  c("Castilleia") & is.na(plant_family$family),3]="Orobanchaceae" #SYnonym Castilleja
plant_family[plant_family$query ==  c("Monarda") & is.na(plant_family$family),3]="Lamiaceae"
plant_family[plant_family$query ==  c("Onagra") & is.na(plant_family$family),3]="Onagraceae"
plant_family[plant_family$query ==  c("Elephantella") & is.na(plant_family$family),3]="Orobanchaceae" #unresolved synonym Pedicularius
plant_family[plant_family$query ==  c("Gilia") & is.na(plant_family$family),3]="Polemoniaceae"
plant_family[plant_family$query ==  c("Dasyphora") & is.na(plant_family$family),3]="Rosaceae" #Synonym Dasiphora
plant_family[plant_family$query ==  c("Ibidium") & is.na(plant_family$family),3]="Orchidaceae" #Synonym  Spiranthes
plant_family[plant_family$query ==  c("Aragalus") & is.na(plant_family$family),3]="Fabaceae" #synonym Astragalus
plant_family[plant_family$query ==  c("Iris") & is.na(plant_family$family),3]="Iridaceae"
plant_family[plant_family$query ==  c("Clementsia") & is.na(plant_family$family),3]="Crassulaceae" #Unresolved
plant_family[plant_family$query ==  c("Prunella") & is.na(plant_family$family),3]="Lamiaceae"
plant_family[plant_family$query ==  c("Spartocytisus") & is.na(plant_family$family),3]="Fabaceae" #Legumonisae
plant_family[plant_family$query ==  c("Dryas") & is.na(plant_family$family),3]="Rosaceae"
plant_family[plant_family$query ==  c("Antennaria") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Arenaria") & is.na(plant_family$family),3]="Caryophyllaceae"
plant_family[plant_family$query ==  c("Stellaria") & is.na(plant_family$family),3]="Caryophyllaceae"
plant_family[plant_family$query ==  c("Lesquerella") & is.na(plant_family$family),3]="Brassicaceae"
plant_family[plant_family$query ==  c("Gastonia") & is.na(plant_family$family),3]="Araliaceae" #Synonym Brassaiopsis
plant_family[plant_family$query ==  c("Stachytapheta") & is.na(plant_family$family),3]="Verbenaceae" #Stachytarpheta
plant_family[plant_family$query ==  c("Tourneortia") & is.na(plant_family$family),3]="Boraginaceae" #Tournefortia
plant_family[plant_family$query ==  c("Dracaena") & is.na(plant_family$family),3]="Asparagaceae"
plant_family[plant_family$query ==  c("Ageretina") & is.na(plant_family$family),3]="Asteraceae" #Ageratina
plant_family[plant_family$query ==  c("Erica") & is.na(plant_family$family),3]="Ericaceae"
plant_family[plant_family$query ==  c("Appolonias") & is.na(plant_family$family),3]="Lauraceae"
plant_family[plant_family$query ==  c("Psoralia") & is.na(plant_family$family),3]="Fabaceae" #Psoralea
plant_family[plant_family$query ==  c("Braya") & is.na(plant_family$family),3]="Brassicaceae"
plant_family[plant_family$query ==  c("Chamomilla") & is.na(plant_family$family),3]="Asteraceae" #Synonym Matricaria
plant_family[plant_family$query ==  c("Chondrilla") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Echinops") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Hymenocarpus") & is.na(plant_family$family),3]="Fabaceae" #Hymenocarpos
plant_family[plant_family$query ==  c("Tremastelma") & is.na(plant_family$family),3]="Caprifoliaceae" #Synonym Lomelosia
plant_family[plant_family$query ==  c("Capsella") & is.na(plant_family$family),3]="Brassicaceae"
plant_family[plant_family$query ==  c("Bellardia") & is.na(plant_family$family),3]="Orobanchaceae"
plant_family[plant_family$query ==  c("Rosmarinus") & is.na(plant_family$family),3]="Lamiaceae"
plant_family[plant_family$query ==  c("Smilax") & is.na(plant_family$family),3]="Brassicaceae"
plant_family[plant_family$query ==  c("Baeckia") & is.na(plant_family$family),3]="Myrtaceae" #Myrtaceae
plant_family[plant_family$query ==  c("Neopaxia") & is.na(plant_family$family),3]="Portulacaceae"
plant_family[plant_family$query ==  c("Ranumculus") & is.na(plant_family$family),3]="Ranunculaceae" #Ranunculus
plant_family[plant_family$query ==  c("Cardiandra") & is.na(plant_family$family),3]="Hydrangeaceae"
plant_family[plant_family$query ==  c("Stenactis") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Benthamida") & is.na(plant_family$family),3]="Cornaceae" #Synonym Cornus
plant_family[plant_family$query ==  c("Mimulus") & is.na(plant_family$family),3]="Phrymaceae" 
plant_family[plant_family$query ==  c("Pyrrhocactus") & is.na(plant_family$family),3]="Cactaceae"
plant_family[plant_family$query ==  c("Hepatica") & is.na(plant_family$family),3]="Ranunculaceae"
plant_family[plant_family$query ==  c("Sangiunaria") & is.na(plant_family$family),3]="Papaveraceae" #Sanguinaria
plant_family[plant_family$query ==  c("Chicocca") & is.na(plant_family$family),3]="Rubiaceae" #Chiococca
plant_family[plant_family$query ==  c("Cryptocarpus") & is.na(plant_family$family),3]="Nyctaginaceae"
plant_family[plant_family$query ==  c("Galactea") & is.na(plant_family$family),3]="Fabaceae" #Galactia
plant_family[plant_family$query ==  c("Inga") & is.na(plant_family$family),3]="Fabaceae"
plant_family[plant_family$query ==  c("Lycopersicon") & is.na(plant_family$family),3]="Solanaceae"
plant_family[plant_family$query ==  c("Macraea") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Mormordica") & is.na(plant_family$family),3]="Cucurbitaceae"
plant_family[plant_family$query ==  c("Scaveola") & is.na(plant_family$family),3]="Goodeniaceae" #Svaevola
plant_family[plant_family$query ==  c("Sida") & is.na(plant_family$family),3]="Malvaceae"
plant_family[plant_family$query ==  c("Stachtarpheta") & is.na(plant_family$family),3]="Verbenaceae" #Stachytarpheta
plant_family[plant_family$query ==  c("Hebe") & is.na(plant_family$family),3]="Plantaginaceae"
plant_family[plant_family$query ==  c("Tribulus") & is.na(plant_family$family),3]="Zygophyllaceae"
plant_family[plant_family$query ==  c("Setaria") & is.na(plant_family$family),3]="Poaceae"
plant_family[plant_family$query ==  c("Cacabus") & is.na(plant_family$family),3]="Solanaceae"
plant_family[plant_family$query ==  c("Hebe") & is.na(plant_family$family),3]="Plantaginaceae"
plant_family[plant_family$query ==  c("Isotoma") & is.na(plant_family$family),3]="Campanulaceae"
plant_family[plant_family$query ==  c("Dracuphyllum") & is.na(plant_family$family),3]="Ericaceae"
plant_family[plant_family$query ==  c("Brachyome") & is.na(plant_family$family),3]="Asteraceae"
#Bad spelling of Brachyome
plant_family[plant_family$query ==  c("Viola") & is.na(plant_family$family),3]="Violaceae"
plant_family[plant_family$query ==  c("Drapetes") & is.na(plant_family$family),3]="Thymelaeaceae"
plant_family[plant_family$query ==  c("Besmoscelis") & is.na(plant_family$family),3]="Unresolved"
#Not able to find it
plant_family[plant_family$query ==  c("Shultesia") & is.na(plant_family$family),3]="Gentianaceae"
plant_family[plant_family$query ==  c("Synqonanthus") & is.na(plant_family$family),3]="Eriocaulaceae"
plant_family[plant_family$query ==  c("Pterolepis") & is.na(plant_family$family),3]="	Melastomataceae"
plant_family[plant_family$query ==  c("Saggitaria") & is.na(plant_family$family),3]="	Alismataceae"
plant_family[plant_family$query ==  c("Tyttnera") & is.na(plant_family$family),3]="Unresolved"
#Not able to find it
plant_family[plant_family$query ==  c("Trfolium") & is.na(plant_family$family),3]="Fabaceae"
plant_family[plant_family$query ==  c("Montricharida") & is.na(plant_family$family),3]="Araceae"
plant_family[plant_family$query ==  c("Thalia") & is.na(plant_family$family),3]="Marantaceae"
plant_family[plant_family$query ==  c("Clusia") & is.na(plant_family$family),3]="Clusiaceae"
plant_family[plant_family$query ==  c("Pagameopsis") & is.na(plant_family$family),3]="Rubiaceae"
plant_family[plant_family$query ==  c("Stegolepsis") & is.na(plant_family$family),3]="Rapateaceae"
plant_family[plant_family$query ==  c("Brocchinia") & is.na(plant_family$family),3]="Bromeliaceae"
plant_family[plant_family$query ==  c("Meriana") & is.na(plant_family$family),3]="Convolvulaceae"
plant_family[plant_family$query ==  c("Befaria") & is.na(plant_family$family),3]="Ericaceae"
plant_family[plant_family$query ==  c("Chromolanea") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Luma") & is.na(plant_family$family),3]="Myrtaceae"
plant_family[plant_family$query ==  c("Tribulus") & is.na(plant_family$family),3]="Zygophyllaceae"
plant_family[plant_family$query ==  c("Blechum") & is.na(plant_family$family),3]="Acanthaceae"
plant_family[plant_family$query ==  c("Ascleptas") & is.na(plant_family$family),3]="Apocynaceae"
plant_family[plant_family$query ==  c("Byrsonia") & is.na(plant_family$family),3]="Malpighiaceae"
plant_family[plant_family$query ==  c("Reseada") & is.na(plant_family$family),3]="Resedaceae"
plant_family[plant_family$query ==  c("Beta") & is.na(plant_family$family),3]="Amaranthaceae"
plant_family[plant_family$query ==  c("Portlandia") & is.na(plant_family$family),3]="Loganiaceae"
plant_family[plant_family$query ==  c("Erica") & is.na(plant_family$family),3]="Ericaceae"
plant_family[plant_family$query ==  c("Stellaria") & is.na(plant_family$family),3]="Caryophyllaceae"
plant_family[plant_family$query ==  c("Unidentified") & is.na(plant_family$family),3]="Unresolved"
plant_family[plant_family$query ==  c("Sida") & is.na(plant_family$family),3]="Malvaceae"
plant_family[plant_family$query ==  c("Chiococa") & is.na(plant_family$family),3]="Rubiaceae"
plant_family[plant_family$query ==  c("Jasminoserius") & is.na(plant_family$family),3]="Cactaceae" #Jasminocereus
plant_family[plant_family$query ==  c("Diospyrus") & is.na(plant_family$family),3]="Ebenaceae" #Diospyros
plant_family[plant_family$query ==  c("Ampetopsis") & is.na(plant_family$family),3]="Vitaceae" #Ampelopsis
plant_family[plant_family$query ==  c("Pollia") & is.na(plant_family$family),3]="Commelinaceae" 
plant_family[plant_family$query ==  c("Alpinia") & is.na(plant_family$family),3]="Zingiberaceae" 
plant_family[plant_family$query ==  c("Umbellifer") & is.na(plant_family$family),3]="Apiaceae" #Umbelliferae = Apiaceae
plant_family[plant_family$query ==  c("Parolinea") & is.na(plant_family$family),3]="Unresolved" #CANT FIND
plant_family[plant_family$query ==  c("Aristotelia") & is.na(plant_family$family),3]="Elaeocarpaceae" #
plant_family[plant_family$query ==  c("Pieris") & is.na(plant_family$family),3]="Ericaceae" #
plant_family[plant_family$query ==  c("Schizophragma") & is.na(plant_family$family),3]="Hydrangeaceae" #
plant_family[plant_family$query ==  c("Stewartia") & is.na(plant_family$family),3]="Theaceae" #
plant_family[plant_family$query ==  c("Leesia") & is.na(plant_family$family),3]="Poaceae" #Leersia
plant_family[plant_family$query ==  c("Eusteratis") & is.na(plant_family$family),3]="Lamiaceae" #Synonym Eusteralis Pogostemon
plant_family[plant_family$query ==  c("Oenanthe") & is.na(plant_family$family),3]="Apiaceae" #
plant_family[plant_family$query ==  c("Digitaria") & is.na(plant_family$family),3]="Poaceae"
plant_family[plant_family$query ==  c("Aetheorrina") & is.na(plant_family$family),3]="Unresolved" #Cant find from Bartomeus 2008
plant_family[plant_family$query ==  c("Dorychnium") & is.na(plant_family$family),3]="Fabaceae" #
plant_family[plant_family$query ==  c("Olea") & is.na(plant_family$family),3]="Oleaceae"
plant_family[plant_family$query ==  c("Deckenia") & is.na(plant_family$family),3]="Arecaceae"
plant_family[plant_family$query ==  c("Lepachys") & is.na(plant_family$family),3]="Asteraceae" #Synonym Ratibida
plant_family[plant_family$query ==  c("Erechthites") & is.na(plant_family$family),3]="Asteraceae" #Erechtites
plant_family[plant_family$query ==  c("Robinia") & is.na(plant_family$family),3]="Fabaceae"
plant_family[plant_family$query ==  c("Gerardia") & is.na(plant_family$family),3]="Orobanchaceae" 
plant_family[plant_family$query ==  c("Liparis") & is.na(plant_family$family),3]="Orchidaceae" 
plant_family[plant_family$query ==  c("Sebatia") & is.na(plant_family$family),3]="Gentianaceae" #Sabatia
plant_family[plant_family$query ==  c("Tephrosia") & is.na(plant_family$family),3]="Fabaceae" 
plant_family[plant_family$query ==  c("Petalostemum") & is.na(plant_family$family),3]="Fabaceae" 
plant_family[plant_family$query ==  c("Velloziela") & is.na(plant_family$family),3]="Orobanchaceae"
plant_family[plant_family$query ==  c("Ipomea") & is.na(plant_family$family),3]="Convolvulaceae" #Ipomoea
plant_family[plant_family$query ==  c("Drymonia") & is.na(plant_family$family),3]="Gesneriaceae" 
plant_family[plant_family$query ==  c("Decagonocarpus") & is.na(plant_family$family),3]="Rutaceae" 
plant_family[plant_family$query ==  c("Mallotus") & is.na(plant_family$family),3]="Euphorbiaceae" 
plant_family[plant_family$query ==  c("Wendtandia") & is.na(plant_family$family),3]="Rubiaceae" #Wendlandia
plant_family[plant_family$query ==  c("Musaenda") & is.na(plant_family$family),3]="Rubiaceae" #Mussaenda
plant_family[plant_family$query ==  c("Machitus") & is.na(plant_family$family),3]="Lauraceae" #machilus
plant_family[plant_family$query ==  c("Ormocaipum") & is.na(plant_family$family),3]="Fabaceae" #Ormocarpum
plant_family[plant_family$query ==  c("Galeopsis") & is.na(plant_family$family),3]="Lamiaceae" 
plant_family[plant_family$query ==  c("Periploca") & is.na(plant_family$family),3]="Apocynaceae" #
plant_family[plant_family$query ==  c("Saussurea") & is.na(plant_family$family),3]="Asteraceae" #=
plant_family[plant_family$query ==  c("Cephalantera") & is.na(plant_family$family),3]="Orchidaceae" #Cephalanthera
plant_family[plant_family$query ==  c("Kerria") & is.na(plant_family$family),3]="Rosaceae" #
plant_family[plant_family$query ==  c("Chelidonium") & is.na(plant_family$family),3]="Papaveraceae" #
plant_family[plant_family$query ==  c("Sagittaria") & is.na(plant_family$family),3]="Alismataceae" #
plant_family[plant_family$query ==  c("Ptantago") & is.na(plant_family$family),3]="Plantaginaceae" #
plant_family[plant_family$query ==  c("Menziesia") & is.na(plant_family$family),3]="Ericaceae" #
plant_family[plant_family$query ==  c("Aralta") & is.na(plant_family$family),3]="Araliaceae" #Aralia
plant_family[plant_family$query ==  c("Vicea") & is.na(plant_family$family),3]="Fabaceae" #
plant_family[plant_family$query ==  c("Bakerella") & is.na(plant_family$family),3]="Loranthaceae" #
plant_family[plant_family$query ==  c("Tristhema") & is.na(plant_family$family),3]="Melastomataceae" #Tristemma
plant_family[plant_family$query ==  c("Amorpha") & is.na(plant_family$family),3]="Fabaceae" #
plant_family[plant_family$query ==  c("Brauneria") & is.na(plant_family$family),3]="Asteraceae" #Synonym Echinacea
plant_family[plant_family$query ==  c("Radicula") & is.na(plant_family$family),3]="Brassicaceae" #
plant_family[plant_family$query ==  c("Collinsia") & is.na(plant_family$family),3]="Plantaginaceae" #
plant_family[plant_family$query ==  c("Evonymus") & is.na(plant_family$family),3]="Celastraceae" #Synonym Echinacea
plant_family[plant_family$query ==  c("Actea") & is.na(plant_family$family),3]="Ranunculaceae" #Actaea

plant_family[plant_family$query ==  c("Pectis") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Kandetia") & is.na(plant_family$family),3]="Rhizophoraceae"
plant_family[plant_family$query ==  c("Maackia") & is.na(plant_family$family),3]="Fabaceae"
plant_family[plant_family$query ==  c("VVedelia") & is.na(plant_family$family),3]="Asteraceae"
#Wrong spelling of Wedelia
plant_family[plant_family$query ==  c("VVedelia") & is.na(plant_family$family),3]="Cucurbitaceae"
plant_family[plant_family$query ==  c("Scorzonea") & is.na(plant_family$family),3]="Asteraceae"
#Wrong spelling of Scorzonera
plant_family[plant_family$query ==  c("Noechamaelea") & is.na(plant_family$family),3]="Rutaceae"
plant_family[plant_family$query ==  c("Benthamidia") & is.na(plant_family$family),3]="Cornaceae"
plant_family[plant_family$query ==  c("Lindera") & is.na(plant_family$family),3]="Lauraceae"
plant_family[plant_family$query ==  c("Besella") & is.na(plant_family$family),3]="Basellaceae"
plant_family[plant_family$query ==  c("Liriope") & is.na(plant_family$family),3]="Asparagaceae"
plant_family[plant_family$query ==  c("Mosta") & is.na(plant_family$family),3]="Unresolved"
plant_family[plant_family$query ==  c("Limophila") & is.na(plant_family$family),3]="Plantaginaceae"
plant_family[plant_family$query ==  c("Alium") & is.na(plant_family$family),3]="Amaryllidaceae"
plant_family[plant_family$query ==  c("Osterium") & is.na(plant_family$family),3]="Apiaceae"
#Ostericum
plant_family[plant_family$query ==  c("Equium") & is.na(plant_family$family),3]="Boraginaceae"
#Echium
plant_family[plant_family$query ==  c("Agauria") & is.na(plant_family$family),3]="Ericaceae"
plant_family[plant_family$query ==  c("Phyllanthus") & is.na(plant_family$family),3]="Phyllanthaceae"
plant_family[plant_family$query ==  c("Ilysanthes") & is.na(plant_family$family),3]="Linderniaceae"
plant_family[plant_family$query ==  c("Dianthera") & is.na(plant_family$family),3]="Acanthaceae"
plant_family[plant_family$query ==  c("Ludvigia") & is.na(plant_family$family),3]="Onagraceae"
plant_family[plant_family$query ==  c("Specularia") & is.na(plant_family$family),3]="Campanulaceae"
plant_family[plant_family$query ==  c("Actinomeris") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Psedera") & is.na(plant_family$family),3]="Vitaceae"
plant_family[plant_family$query ==  c("Anemonella") & is.na(plant_family$family),3]="Ranunculaceae"
plant_family[plant_family$query ==  c("Asiminia") & is.na(plant_family$family),3]="Annonaceae"
plant_family[plant_family$query ==  c("Fridericia") & is.na(plant_family$family),3]="Bignoniaceae"
plant_family[plant_family$query ==  c("Centropogon") & is.na(plant_family$family),3]="Campanulaceae"
plant_family[plant_family$query ==  c("Payparola") & is.na(plant_family$family),3]="Violaceae"
plant_family[plant_family$query ==  c("Symphonia") & is.na(plant_family$family),3]="Clusiaceae"
plant_family[plant_family$query ==  c("Tabernamontana") & is.na(plant_family$family),3]="Loganiaceae"
plant_family[plant_family$query ==  c("Pslicourea") & is.na(plant_family$family),3]="Rubiaceae"
#Palicourea
plant_family[plant_family$query ==  c("Desfontainea") & is.na(plant_family$family),3]="Columelliaceae"
#Desfontainia
plant_family[plant_family$query ==  c("Pitcaimia") & is.na(plant_family$family),3]="Bromeliaceae"
plant_family[plant_family$query ==  c("Osmorrhiza") & is.na(plant_family$family),3]="Apiaceae"
plant_family[plant_family$query ==  c("Ascerates") & is.na(plant_family$family),3]="Apocynaceae"
plant_family[plant_family$query ==  c("Metothria") & is.na(plant_family$family),3]="Cucurbitaceae" #Melothria
plant_family[plant_family$query ==  c("Stanleya") & is.na(plant_family$family),3]="Brassicaceae" #Melothria

#\tAlismataceae
#\tMelastomataceae

plant_family[plant_family$family==c("\tAlismataceae"),]$family="Alismataceae"
plant_family[plant_family$family==c("\tMelastomataceae"),]$family="Melastomataceae"
plant_family[plant_family$family==c("Asparagaceae)"),]$family="Asparagaceae"





###Pollinator processing

pf1=read.csv("data/processing/pol_family_1_LK.csv")
pf2=read.csv("data/processing/pol_family_2_corrected.csv")
pf3=read.csv("data/processing/pol_family_3_JS.csv")
pf4=read.csv("data/processing/Manu networks.csv")
pf5=read.csv("data/processing/pol_family_5_MH.csv")

poll_5=rbind.data.frame(pf1,pf2,pf3,pf4,pf5)

write.csv(poll_5)

poll_5_1=poll_5[,2:6]


poll_5_1$query=as.character(poll_5_1$query)
poll_famord$query=as.character(poll_famord$query)




poll_famord_5 <- left_join(poll_famord,poll_5_1, by = "query") %>% # this will generate age.x and age.y
  mutate(Order = ifelse(is.na(order.x), as.character(order.y), order.x),
         Family = ifelse(is.na(family.x), as.character(family.y), family.x)) %>% # we generate a joint 'age' variable
  select(-db.y,-family.y, -order.y,-family.x,-order.x, -Genus.y) 

write.csv(poll_famord_5,"data/poll_famord_5.csv")

### Plant / Pollinator genera columns
myfiles.melt.agg.z$PGenus=word(myfiles.melt.agg.z$Plant,1)
myfiles.melt.agg.z$IGenus=word(myfiles.melt.agg.z$Pollinator,1)

colnames(plant_family)[2]="PGenus"
colnames(poll_famord_5)[3]="IGenus"

#Remove plants from bird networks - plant_family was made with these networks
minus_plant=setdiff(plant_family$PGenus,myfiles.melt.agg.z$PGenus)

#remove plant families from bird networks
custom.subset <- function(df, keywords) {
  y <- df[apply(df, 1, function(x) all(!x %in% keywords)),]
  return(y)
}

plant_family_subset=custom.subset(plant_family, minus_plant)

#add plant families
geonet=merge(myfiles.melt.agg.z,unique(plant_family_subset), by = "PGenus") 
geonet=merge(geonet,reference, by = "Network")
#add pollinator families
geonet=merge(geonet,poll_famord_5, by = "IGenus")

geonet <- geonet %>% mutate_if(is.factor,as.character)
geonet[geonet$Family%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("Order")]="Bee"
geonet[geonet$Family%in%c("Syrphidae"),c("Order")]="Syrphidae"
geonet=geonet[,-c(1:2)]
colnames(geonet)[8]="Poll_Family"
colnames(geonet)[6]="Plant_Family"

str(geonet)
geonet[,c("Network","Order","Poll_Family","Plant_Family")]

geonet <- geonet %>% mutate_if(is.character,as.factor)


g=geonet%>%
group_by(Network, Order) %>%
  summarise(order_links=sum(Int))

g2=g%>%
  group_by(Network) %>%
  summarise(int_tot=sum(order_links))

g3=merge(g,g2)
g3$prop_links=g3$order_links/g3$int_tot

g4=merge(g3,reference,by="Network")

#subset data by insect order
g.sub <- subset(g4, Order %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))
str(g.sub)
range(g.sub$prop_links)
if(g.sub$prop_links = 1){
  
}
g.sub[g.sub$prop_links==1,]
#change value of 1
rownames(g.sub)=1:313
1:313
g.sub$prop_links[295] = 0.99999
g.sub[295,]
#run insect order:climate region model
prop1=glmmTMB(prop_links~Order*scale(ele)+(1|Network),
              family=beta_family(link = "logit"),
              data=g.sub)

summary(prop1)
range(reference$ele)
#run posthoc pairwise comparision
prop1.ls <- emmeans(prop1, pairwise ~ Clim|Order, level = .95, adjust = "fdr")

#generate letters for groups
prop1.CLD <- CLD(prop1.ls$contrasts, Letters = letters, level = .95, adjust = "fdr")

recover.data.glmmTMB <- function(object, ...) {
fcall <- getCall(object)
recover.data(fcall,delete.response(terms(object)),
attr(model.frame(object),"na.action"), ...)}
lsm.basis.glmmTMB <- function (object, trms, xlev, grid, vcov.,
mode = "asymptotic", component="cond", ...) {
if (mode != "asymptotic") stop("only asymptotic mode is available")
if (component != "cond") stop("only tested for conditional component")
if (missing(vcov.))
V <- as.matrix(vcov(object)[[component]])
else V <- as.matrix(.my.vcov(object, vcov.))
dfargs = misc = list()
if (mode == "asymptotic") {
dffun = function(k, dfargs) NA
}
## use this? misc = .std.link.labels(family(object), misc)
contrasts = attr(model.matrix(object), "contrasts")
m = model.frame(trms, grid, na.action = na.pass, xlev = xlev)
X = model.matrix(trms, m, contrasts.arg = contrasts)
bhat = fixef(object)[[component]]
if (length(bhat) < ncol(X)) {
kept = match(names(bhat), dimnames(X)[[2]])
bhat = NA * X[1, ]
bhat[kept] = fixef(object)[[component]]
modmat = model.matrix(trms, model.frame(object), contrasts.arg = contrasts)
nbasis = estimability::nonest.basis(modmat)
}
else nbasis = estimability::all.estble
list(X = X, bhat = bhat, nbasis = nbasis, V = V, dffun = dffun,
dfargs = dfargs, misc = misc)
}

max.prop <- g.sub %>% group_by(Order, clim.left) %>% summarise(max = max(prop_links+0.2))

max.prop.2 <- merge(max.prop, prop1.CLD)

#plot it
p <- ggplot()
p <- p + xlab("Climate zone") + ylab("Proportion of links")
p <- p + theme(text = element_text(size=18))
p <- p + geom_point(data=g.sub, aes(x=ele, y=prop_links, color=Order),
                     alpha=0.4,adjust = 1,scale = "width")
p <- p +geom_smooth(method="glm")
p <- p + geom_jitter(data=g.sub, aes(x=left(g.sub$Clim,1), y=prop_links, color=Order, fill=Order),
                     alpha=1, size=2.5, position = position_jitter(width = 0.25))
#p <- p + geom_text(data = max.prop.2, aes(x = clim.left, y=max, label=.group))
p <- p + facet_wrap(~Order)
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
#p <- p + scale_color_brewer(palette="Set1")
#p <- p + scale_fill_brewer(palette="Set1")
p <- p + theme(legend.position="none")
p


g5 <- g4 %>% complete(Order, nesting(Network), fill = list(prop_links = 0))

geoNA=geonet[is.na(geonet$Order),]


ggplot(filter(g4, prop_links>0.25),aes(x=prop_links,y=Latitude,col=Order))+geom_point()+
  theme_bw()#+facet_wrap(~left(ClimateZ,1))



##Analyses next steps
#Family ~ Climate
#links~ CLimate * Family * Order 
#links Climate * ORder + 1|Network/Family

str(geonet)
GC=g4[left(g4$ClimateZ,1)%in% "C",]

g6=geonet%>%
  group_by(Network)%>%
  summarise(Plant_Family)
str(g6)



