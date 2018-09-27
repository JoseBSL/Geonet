##ELEVATION DATA
require(geonames)

#Two methods
options(geonamesUsername="liamkendall")

ele=by(reference, 1:nrow(reference), function(x) GNsrtm3(lat=x$Latitude,lng=x$Longitude))
ele=do.call("rbind", ele)

ele$Network=reference$Network

reference$ele=ele2$srtm3
colnames(ele)[1]="ele"
#http://www.gpsvisualizer.com/elevation
ele[ele$ele<0,]

ele[ele$Network %in% "M_PL_009",c("ele")] = 1009
ele[ele$Network %in%"M_PL_010",c("ele")] =  261
ele[ele$Network %in%"M_PL_014",c("ele")] = 250
ele[ele$Network %in%"M_PL_020",c("ele")] = 250
ele[ele$Network %in%"M_PL_024",c("ele")] = 92 
ele[ele$Network %in%"M_PL_026",c("ele")] = 1 #iffy
ele[ele$Network %in%"M_PL_045",c("ele")] = 1 #iffy
ele[ele$Network %in%"M_PL_052",c("ele")] = 139 
ele[ele$Network %in%"Robinson etal 2018",c("ele")] = 31 

write.csv(ele,"data/processing/elevation.csv")
