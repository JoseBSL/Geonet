
##ELEVATION DATA
require(geonames)

#Two methods
ele1=by(reference, 1:nrow(reference), function(x) GNgtopo30(lat=x$Latitude,lng=x$Longitude))
ele1=do.call("rbind", ele)

ele2=by(reference, 1:nrow(reference), function(x) GNsrtm3(lat=x$Latitude,lng=x$Longitude))
ele2=do.call("rbind", ele2)

reference$ele=ele2$srtm3

str(geonet)

a=merge(geonet, reference$ele, by = "Network")
#http://www.gpsvisualizer.com/elevation

reference[reference$ele<0,]
reference[reference$ID %in% "M_PL_009",c("ele")] = 1009
reference[reference$ID %in%"M_PL_010",c("ele")] =  261
reference[reference$ID %in%"M_PL_014",c("ele")] = 250
reference[reference$ID %in%"M_PL_020",c("ele")] = 250
reference[reference$ID %in%"M_PL_024",c("ele")] = 92 
reference[reference$ID %in%"M_PL_026",c("ele")] = 0 #iffy
reference[reference$ID %in%"M_PL_045",c("ele")] = 0 #iffy
reference[reference$ID %in%"M_PL_052",c("ele")] = 139 
reference[reference$ID %in% "M_PL_060",c("ele")] = 0 #iffy
