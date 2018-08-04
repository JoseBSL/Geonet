##removed hummingbird studies: M_PL_063,M_PL_065,  M_PL_067, M_PL_070 -in new folder

library(kgc)
library(tidyverse)
library(reshape)
library(taxize)
devtools::install_github("kunstler/BIOMEplot")
library(ggbiome)

goptions(stringsAsFactors = FALSE)
reference=read.csv("data/ref/references.csv",header=T)
str(reference)

climate_zone=reference[,c("ID","Longitude","Latitude")]
str(climate_zone)
climate_zone <- data.frame(climate_zone,
                   rndCoord.lon = RoundCoordinates(climate_zone$Longitude),
                   rndCoord.lat = RoundCoordinates(climate_zone$Latitude))
reference <- data.frame(reference,ClimateZ=LookupCZ(climate_zone))
table(reference$ClimateZ)
reference[reference$ClimateZ=="Climate Zone info missing",]


##GALAPAGOS and MAURITIUS 

reference[26,11]=c("BWh")
reference[60,11]=c("Af")

gd_get_biome(reference, si_lat="Latitude", si_long=Longitude, merge_deserts = FALSE)



# First apply read.csv, then rbind

files = list.files(pattern="*.csv")
myfiles = lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE, sep=","))

#Read multiple CSVs
path <- "data/"
files <- list.files(path=path, pattern="*.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(sep=",",header=T,paste(path,file,sep="")))
} 



names(myfiles)=left(files,8)
names(myfiles)


myfiles.melt=lapply(myfiles, function(x) melt(x,id.vars=c(1)))
myfiles.melt=melt(myfiles,id.vars=c(1))
myfiles.melt.agg=aggregate(value ~ X + variable+L1,data=myfiles.melt, FUN=sum)
myfiles.melt.agg$value=ifelse(myfiles.melt.agg$value > 0, 1, 0)
colnames(myfiles.melt.agg)=c("Plant","Pollinator","Network","Int")

myfiles.melt.agg.z=myfiles.melt.agg[!myfiles.melt.agg$Int==0,]
myfiles.melt.agg.z$Pollinator=gsub("\\."," ",myfiles.melt.agg.z$Pollinator)

myfiles.melt.agg.mun=dplyr::filter(myfiles.melt.agg.z, !grepl('Unidentified', Pollinator))

myfiles.melt.agg.poll=myfiles.melt.agg.mun
myfiles.melt.agg.plant=myfiles.melt.agg.z

myfiles.melt.agg.z.4=split(myfiles.melt.agg.z , f = c(rep(1,9555),rep(2,9555),rep(3,9554),rep(4,9554)))


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


plant_families=as.data.frame(unique(plant_family$family))


plant_family_split=split(plant_families, c(rep(1,40),rep(2,40),rep(3,40),rep(4,40),rep(5,39)))

write.csv(plant_family_split$`1`,"plant_family_1.csv")
write.csv(plant_family_split$`2`,"plant_family_2.csv")
write.csv(plant_family_split$`3`,"plant_family_3.csv")
write.csv(plant_family_split$`4`,"plant_family_4.csv")
write.csv(plant_family_split$`5`,"plant_family_5.csv")

