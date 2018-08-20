
##ELEVATION DATA
require(geonames)

#Two methods
ele1=by(reference, 1:nrow(reference), function(x) GNgtopo30(lat=x$Latitude,lng=x$Longitude))
ele1=do.call("rbind", ele)

ele2=by(reference, 1:nrow(reference), function(x) GNsrtm3(lat=x$Latitude,lng=x$Longitude))
ele2=do.call("rbind", ele2)

reference$ele=ele2$srtm3
