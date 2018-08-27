
##ELEVATION DATA
require(geonames)
require(MuMIn)

#Two methods
options(geonamesUsername="liamkendall")

ele2=by(reference, 1:nrow(reference), function(x) GNsrtm3(lat=x$Latitude,lng=x$Longitude))
ele2=do.call("rbind", ele2)

reference$ele=ele2$srtm3

#http://www.gpsvisualizer.com/elevation
reference[reference$ele<0,]
reference[reference$ID %in% "M_PL_009",c("ele")] = 1009
reference[reference$ID %in%"M_PL_010",c("ele")] =  261
reference[reference$ID %in%"M_PL_014",c("ele")] = 250
reference[reference$ID %in%"M_PL_020",c("ele")] = 250
reference[reference$ID %in%"M_PL_024",c("ele")] = 92 
reference[reference$ID %in%"M_PL_026",c("ele")] = 1 #iffy
reference[reference$ID %in%"M_PL_045",c("ele")] = 1 #iffy
reference[reference$ID %in%"M_PL_052",c("ele")] = 139 
reference[reference$ID %in% "M_PL_060",c("ele")] = 1 #iffy

str(geonet$Network)
str(reference$Network)
reference$Network=reference$ID
geonet2=merge(geonet,reference, by = "Network")

gg=geonet2%>%
  group_by(Network, Order) %>%
  summarise(order_links=sum(Int))

gg2=gg%>%
  group_by(Network) %>%
  summarise(int_tot=sum(order_links))

gg3=merge(gg,gg2)
gg3$prop_links=gg3$order_links/gg3$int_tot

gg4=merge(gg3,reference,by="Network")

#subset data by insect order
gg.sub <- subset(gg4, Order %in% c("Hymenoptera", "Diptera", "Lepidoptera"))
gg.sub$clim.left <- left(gg.sub$ClimateZ,1)

#change value of 1
gg.sub$prop_links[169] = 0.99999

#run insect order:climate region model
prop1=glmmTMB(prop_links~clim.left*Order+(1|Network),
              family=beta_family(link = "logit"),
              data=gg.sub)
summary(prop1)
plot(fitted(prop1)~residuals(prop1))
psm1=simulateResiduals(prop1)
testUniformity(psm1) # good
plot(psm1)

#three way interaction
prop2=glmmTMB(prop_links~clim.left*Order*ele+(1|Network),
              family=beta_family(link = "logit"),
              data=gg.sub)
warnings()

#BREAKDOWN - so work up

prop3=glmmTMB(prop_links~clim.left*Order+ele+(1|Network),
              family=beta_family(link = "logit"),
              data=gg.sub)
warnings() #Can be ignored if convergence is ok
summary(prop3) # No effect of eleveation in addition to climate
psm3=simulateResiduals(prop3)
testUniformity(psm3) # good
plot(psm3)

#What about elevation*Climate + Order
prop4=glmmTMB(prop_links~clim.left*ele+Order+(1|Network),
              family=beta_family(link = "logit"),
              data=gg.sub)
warnings() #Can be ignored if convergence is ok
summary(prop4) # No effect of eleveation in addition to climate
psm4=simulateResiduals(prop4)
testUniformity(psm4) # bad wah but not tooo bad
plot(psm4)


#What about elevation*Climate + Order
prop5=glmmTMB(prop_links~clim.left+ele*Order+(1|Network),
              family=beta_family(link = "logit"),
              data=gg.sub)
warnings()
summary(prop5) # No effect of eleveation in addition to climate
psm5=simulateResiduals(prop5)
testUniformity(psm5) # good
plot(psm5)
AIC(prop1,prop2,prop3,prop4,prop5)

#Dredge full model # three way interaction
prop_dredge=dredge(prop2)
prop_dredge
prop_mods=get.models(prop_dredge,subset=TRUE)

prop6=prop_mods[[1]]
summary(prop6)
psm6=simulateResiduals(prop_mods[[1]])
plot(psm6) # residuals are a-ok
testDispersion(psm6,alternative=c("two.sided")) #underdispersed


library(brms)


#lets get crazy and run some bayes
prop_bayes1=brm(formula(prop_mods[[1]]),
    family=Beta(link = "logit"),inits = "0",iter=4000,
    data=gg.sub,chains=4)

prop_bayes2=brm(prop_links ~ clim.left + Order + (1 | Network) + clim.left:Order,
                family=Beta(link = "logit"),inits = "0",iter=4000,
                cov_ranef = list("Network" = lqmm::make.positive.definite(pfdist.matrix)),
                data=gg.sub,chains=2)

plot(prop_bayes1)
str(gg.sub)
library(stats)
pfdist.matrix=as.matrix(pfdist.matrix)
isSymmetric(lqmm::make.positive.definite(pfdist.matrix))
colnames(pfdist.matrix)
solve(pfdist.matrix)
matrixcalc::is.positive.definite(lqmm::make.positive.definite(pfdist.matrix))
??as.matrix.dist


pfdist.matrix

plot(prop_bayes1)
pp_check(prop_bayes1, nsamples=100)
pp_check(prop_bayes2, nsamples=100)
loo(prop_bayes1) #-190.3 
loo(prop_bayes2) #-192.3
bayes_R2(prop_bayes1)
bayes_R2(prop_bayes2)


##latitude * Climate interactions

reference2=reference

reference2[reference2$Latitude<0,reference2$Latitude]

reference2$latabs=abs(reference2$Latitude)
str(reference2)
boxplot(latabs~Clim,reference2,xlab=c("Climate zone"),ylab=c("Abs. latitude"))


