##ELEVATION DATA

reference=read.csv("data/ref/references_update.csv",header=T)

require(geonames)

#Two methods
options(geonamesUsername="liamkendall")

ele=by(reference, 1:nrow(reference), function(x) GNsrtm3(lat=x$Latitude,lng=x$Longitude))
ele=do.call("rbind", ele)

ele$Network=reference$ID

#reference$ele=ele$srtm3
colnames(ele)[1]="ele"
#http://www.gpsvisualizer.com/elevation
ele[ele$ele<0,]

ele[ele$Network %in% "M_PL_009_00",c("ele")] = 1009
ele[ele$Network %in%"M_PL_010_00",c("ele")] =  261
ele[ele$Network %in%"M_PL_014_00",c("ele")] = 250
ele[ele$Network %in%"M_PL_020_00",c("ele")] = 250
ele[ele$Network %in%"M_PL_024_00",c("ele")] = 92 
ele[ele$Network %in%"M_PL_026_00",c("ele")] = 1 #iffy
ele[ele$Network %in%"M_PL_045_00",c("ele")] = 1 #iffy
ele[ele$Network %in%"M_PL_052_00",c("ele")] = 139 
ele[ele$Network %in%"Robinson_18",c("ele")] = 31 

write.csv(ele,"data/processing/elevation.csv")
