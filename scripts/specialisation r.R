
#################################################
#correct number of links for sampling effort with Bascompte null model
#################################################
library(reshape2)
library(vegan)
library(plyr)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(emmeans)

#cast long format dataframe
geo.wide <- dcast(geonet, Network + Plant ~ Pollinator, value.var = "Int")
geo.wide[is.na(geo.wide)] <- 0

#gg <- subset(geo.wide, Network %in% c("M_PL_001")) %>% droplevels
#create empty listy
sp.links <- c()
sp.links <- list(sp.links)

#run loop over each site
for (j in levels(geo.wide[, 1])){
  web <- subset(geo.wide, Network == j)#iterate over site
  web <- web[,c(-1,-2)]
  web = web[,colSums(web) > 0]#remove species with no links at each site

#Null model II from Bascompte et al. (2003). Creates random networks by probabilistically fixing row and column marginal totals. The expected number of links is same as observed number. Rodriguez-Girona and Santamaria (2006) showed that this null model has the best compromise between Type I and Type II errors
#Run this code once to create the null model function
null.model.II <- function(web){
  web <- as.matrix(web > 0) + 0
  # calculate the probability based on row marginals. Creates matrix same size as web, with row sums divided by number of columns (to get probability of a 1 in each cell of each row), repeated across all columns for each row.
  row.probs <- matrix(rowSums(web)/ncol(web),nrow(web),ncol(web))
  # calculate the probability based on column marginals (again, repeated for whole column). Transpose used instead of byrow=T
  col.probs <- t(matrix(colSums(web)/nrow(web),ncol(web),nrow(web)))
  # calculate the element by element mean of this probabilities
  mat.probs <- (row.probs + col.probs) / 2.0
  # generate a random matrix with 1s proportional to the above probabilities. rbinom(n, size, prob) n is number of observations, size is number of trials, prob is prob of success in each trial
  mat.null <- matrix(rbinom(nrow(web)*ncol(web),1,as.vector(mat.probs)),nrow(web),ncol(web))  
  # return that matrix in all its glory
  return(mat.null)
}

#Begin permutation test (two tailed)
reps <- 999 #set number of permutations

nulls<-vector("list",reps)  # Create a list with spaces for each output matrix
for (i in 1:reps) {
  nulls[[i]]<-null.model.II(web)
}

#call any individual matrix from that list using nulls[[x]], where x is the number of the matrix you want to call
null.links <- data.frame(matrix(, nrow=reps, ncol(web)))
for (i in 1:reps) {
  null.links[i, ] <- t(colSums(nulls[[i]]))
}

weblink <- t(colSums(as.matrix(web > 0) + 0))
colnames(null.links) <- colnames(weblink)
links <- rbind(null.links,weblink)#Add observed connectance into distribution
sd <- apply(null.links, 2, sd)#calculate standard deviation
sp.links[[j]] <- abs(weblink - colMeans(null.links)/sd)#print results into list

}

#convert list to dataframe and melt
sp.links.df <- rbind.fill(lapply(sp.links, as.data.frame))
sp.links.df$Network <- levels(geonet$Network)
str(sp.links.df)
str(sp.links.df)
sp.links.melt <- melt(sp.links.df, "Network", variable.name = "Pollinator", value.name = "value", na.rm = TRUE)
head(sp.links.melt)
#add order and family to dataframe
geo.uni <- unique(geonet[c("Pollinator", "PolFamily", "PolOrder")])
sp.links.order <- merge(sp.links.melt,geo.uni, by="Pollinator")
table(sp.links.order$PolOrder)

#filter dataframe to retain orders of interest
ord <- c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera")
links.full.sub <- filter(sp.links.order, PolOrder %in% ord)
#add climate data
clim.dat <- unique(g4[c("Network","Latitude","Longitude","ClimateZ","ele")])
clim.dat$ClimateZ[26] = "B"
links.full.sub <- merge(links.full.sub,clim.dat, by="Network")
links.full.sub$clim <- as.factor(left(links.full.sub$ClimateZ,1))
links.full.sub$PolOrder <- as.factor(links.full.sub$PolOrder) %>% droplevels()
str(links.full.sub)

#bayes gamma +1
gammaprior=prior(normal(0,2),class="b")+prior(normal(0,2),class="Intercept")+prior(normal(0,1),class="sd")

prior_summary(sp3)
sp3=brm(value ~ PolOrder*clim + (1|Network),
       family=Gamma(link="log"),prior=gammaprior,
       data=links.full.sub,cores=4,inits=0)

pp_check(sp3,nsamples=100,ylim=10)
check_all_diagnostics(sp3)

#run model
library(glmmTMB)
links.full.sub <- links.full.sub[links.full.sub$Network!="M_PL_062",]
links.full.sub <- links.full.sub[-c(1147, 2407), ]
links.full.sub$ClimateZ <- as.factor(links.full.sub$ClimateZ)

##GRAPHING

str(links.full.sub)


hist(links.full.sub$value)

str(links.full.sub$PolOrder)

bayes_R2(sp3)

##PLOT tidy bayes
spec.plot1=links.full.sub%>%
  add_fitted_draws(sp3,n=100)%>%
  ggplot(aes(x=value,y=as.factor(PolOrder)))+
  stat_intervalh(aes(x=.value))+
  facet_grid(~clim, scale = "free_y")+
  scale_color_brewer()+
  stat_pointintervalh(aes(x=value),pch=15,.width=c(0))+
  theme_bw()+
  xlab("Specialisation")+
  ylab("Pollinator order")
spec.plot1

spec.plot2=links.full.sub%>%
  add_fitted_draws(sp3,n=100)%>%
  ggplot(aes(x=value,y=as.factor(clim)))+
  stat_intervalh(aes(x=.value))+
  facet_grid(~PolOrder, scale = "free_y")+
  scale_color_brewer()+
  theme_bw()+
  xlab("Specialisation")+
  ylab("Pollinator order")
spec.plot2


#model generalism by order and climate zone
m1 <- glmmTMB(log(value+1) ~ PolOrder*clim + (1|Network),
        family=Gamma(link="log"),
        data=links.full.sub)




#model generalism by order and climate zone and latitude
links.full.sub$log <- log(links.full.sub$value+1)
m2 <- glmmTMB(log ~ PolOrder*clim + PolOrder*abs(Latitude.y) + (1|Network),
              family=Gamma(link="log"),
              data=links.full.sub)
library(MuMIn)
MuMIn::dredge(m2)

m3 <- glmmTMB(log ~ PolOrder*abs(Latitude.y) + (1|Network),
              family=Gamma(link="log"),
              data=links.full.sub)

AIC(m1,m2,m3)#climate zone model is much better than the latitude model! 

#compute pairwise comparisons 
library(emmeans)
spec.comp <- emmeans(m1, pairwise ~ Order|clim, by = "clim", level=0.95, adj="fdr")

#generate letters for groups
spec.comp.CLD <- CLD(spec.comp, by = "clim", which = 1, Letters = letters, level = .95, adjust = "fdr")
spec.comp.CLD$value <- exp(spec.comp.CLD$emmean)
spec.comp.CLD$stderr <- exp(spec.comp.CLD$SE)
spec.comp.CLD$low <- exp(spec.comp.CLD$asymp.LCL)
spec.comp.CLD$up <- exp(spec.comp.CLD$asymp.UCL)
max <- spec.comp.CLD %>% group_by(Order, clim) %>% summarise(max = max(up+0.04))
spec.comp.CLD <- merge(spec.comp.CLD, max)

#plot generalisation by climate zone
p <- ggplot()
p <- p + xlab(NULL) + ylab("Standardised number of links")
p <- p + geom_point(data=spec.comp.CLD, aes(x=Order, y=value, color=clim),
                     alpha=1, size=3)
p <- p + geom_errorbar(data=spec.comp.CLD, aes(x=Order, ymin=low, ymax=up, color=clim), width=0)
p <- p + geom_text(data = spec.comp.CLD, aes(x=Order, y=max, label=.group, size=16))
p <- p + coord_cartesian(ylim=c(0,2))
p <- p + scale_y_continuous(breaks=seq(0,2,0.5))
p <- p + facet_wrap(~clim, ncol=5)
p <- p + theme(panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_blank()) +
  theme(panel.border=element_rect(colour = "black", fill = "NA", size = 0.5)) +
  theme(axis.text.x=element_text(angle= 45, hjust = 1, vjust = 1, size =14),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =16),
        axis.title.y=element_text(size=26, vjust = 1),
        axis.title.x=element_text(size=22, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(strip.background = element_rect(colour="NA", fill=NA),
        strip.text = element_text(size=20))+
  theme(axis.ticks.length = unit(2, "mm"))
p <- p + theme(axis.ticks.x = element_line(colour="black"), 
               axis.ticks.y = element_line(colour="black"))
p <- p + theme(panel.spacing.x=unit(1, "lines"),panel.spacing.y=unit(1, "lines"))
p <- p + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p <- p + scale_color_brewer(palette="Set1")
p <- p + theme(legend.position="none")
p

#model generalism byt network and order (for map)
m4 <- glm(log(value+1) ~ Network*Order,
          family=Gamma(link="log"),
          data=links.full.sub)

net.comp <- emmeans(m4, pairwise ~ Order|Network, by = "Network", level=0.95, adj="fdr")

#generate letters for groups
net.comp.CLD <- CLD(net.comp, by = "Network", which = 1, Letters = letters, level = .95, adjust = "fdr")
net.comp.CLD$value <- exp(net.comp.CLD$emmean)

#add climate data
links.clim.net <- merge(net.comp.CLD,clim.dat, by="Network")
links.clim.net$clim <- as.factor(left(links.clim.net$ClimateZ,1))
links.clim.net <- na.omit(links.clim.net)

#plot generalisation on world map
WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

map <- ggplot()
map <- map + xlab("Longitude") + ylab("Latitude")
map <- map + geom_map(data=WorldData, map=WorldData,
                      aes(x=long, y=lat, group=group, map_id=region))
#map <- map + geom_point(data=filter(links.clim, Order == "Coleoptera"),
map <- map + geom_point(data=links.clim.net,
                        aes(x=Longitude, y=Latitude, colour=value),size=1.5)
map <- map + scale_colour_gradient(low="green",high="red")
map <- map + facet_wrap(Order~clim,ncol = 5)
map <- map + theme(axis.line.x = element_line(size=.5, colour = "black"),
                   axis.line.y = element_line(size=.5, colour = "black"),
                   panel.grid.major = element_line(colour = "#d3d3d3"),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =10),
        axis.title.x=element_text(size=24, vjust = 1),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =10),
        axis.title.y=element_text(size=24, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(axis.ticks.length = unit(2, "mm"))+
  theme(strip.background = element_rect(colour="NA", fill=NA),
        strip.text = element_text(size=8))
map <- map + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
map


library(DHARMa)
res = simulateResiduals(m3)
plot(res, rank = T)
library(ggplot2)
resid <- resid(m1, type = "pearson")
fitted <- fitted(m1)
df <- data.frame(resid, fitted)
ggplot(df, aes(fitted, resid)) + geom_point() + ylab("Pearsons residuals") + xlab("Fitted values")
library(brms)
# model specification
mod = brm(value ~ Order*clim + (1|Network),
          data = links.full.sub, 
          family = lognormal(),
          iter = 1000, chains = 4, cores = 4)
plot(mod)
yrep_poisson <- posterior_predict(mod, draws = 10)
pp_check(mod, yrep_poisson)
ppc_dens_overlay(mod, yrep_poisson[1:10, ])

#calculate mean generalism
links.order.ave <- sp.links.order %>%
  group_by(Network, Order) %>%
  summarise(Generalism=mean(value))

#add climate data
clim.dat <- unique(g4[c("Network","Latitude","Longitude","ClimateZ")])
links.clim <- merge(links.order.ave,clim.dat, by="Network")
links.clim$clim <- as.factor(left(links.clim$ClimateZ,1))


#give correct climate data to reamining networks
links.clim$clim[177:187] = "B"

#filter dataframe to retain orders of interest
ord <- c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera")
links.clim.sub <- filter(links.clim, Order %in% ord)

#remove networks with very high generalism
links.clim.sub <- links.clim.sub[links.clim.sub$Network!="M_PL_062",]
links.clim.sub <- links.clim.sub[links.clim.sub$Network!="M_PL_026",]

#plot generalisation by climate zone
p <- ggplot()
p <- p + xlab("Climate zone") + ylab("Number of links (Z score)")
p <- p + theme(text = element_text(size=18))
p <- p + geom_violin(data=links.full.sub, aes(x=clim, y=log(value), color=Order),
                     alpha=0.4,adjust = 1,scale = "width")
p <- p + geom_jitter(data=links.full.sub, aes(x=clim, y=log(value), color=Order, fill=Order),
                     alpha=1, size=2.5, position = position_jitter(width = 0.25))
p <- p + facet_wrap(~Order, scales="free")
p <- p + theme(panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black")) +
  theme(panel.border=element_rect(colour = "black", fill = "NA", size = 1)) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =16),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =16),
        axis.title.y=element_text(size=12, vjust = 1),
        axis.title.x=element_text(size=12, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(strip.background = element_rect(colour="NA", fill=NA),
        strip.text = element_text(size=12))+
  theme(axis.ticks.length = unit(2, "mm"))
p <- p + theme(panel.spacing.x=unit(1, "lines"),panel.spacing.y=unit(1, "lines"))
p <- p + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p <- p + scale_color_brewer(palette="Set1")
p <- p + scale_fill_brewer(palette="Set1")
p <- p + theme(legend.position="none")
p

#plot generalisation on world map
WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

map <- ggplot()
map <- map + xlab("Longitude") + ylab("Latitude")
map <- map + geom_map(data=WorldData, map=WorldData,
                  aes(x=long, y=lat, group=group, map_id=region))
#map <- map + geom_point(data=filter(links.clim, Order == "Coleoptera"),
map <- map + geom_point(data=links.clim.sub,
                        aes(x=Longitude, y=Latitude, colour=Generalism),size=2)
map <- map + scale_colour_gradient(low="green",high="red")
map <- map + facet_wrap(Order~clim,ncol = 5)
map <- map + theme(axis.line.x = element_line(size=.5, colour = "black"),
                   axis.line.y = element_line(size=.5, colour = "black"),
                   panel.grid.major = element_line(colour = "#d3d3d3"),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =14),
        axis.title.x=element_text(size=30, vjust = 1),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =14),
        axis.title.y=element_text(size=30, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(axis.ticks.length = unit(2, "mm"))+
  theme(strip.background = element_rect(colour="NA", fill=NA),
        strip.text = element_text(size=14))
map <- map + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
map

#################################################
#END
#################################################