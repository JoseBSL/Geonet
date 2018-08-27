#################################################
#correct number of links for sampling effort with Bascompte null model
#################################################
library(reshape2)
library(vegan)
library(plyr)
library(dplyr)

#cast long format dataframe
geo.wide <- dcast(geonet, Network + Plant ~ Pollinator, value.var = "Int")
geo.wide[is.na(geo.wide)] <- 0

#gg <- subset(geo.wide, Network %in% c("M_PL_001")) %>% droplevels
#create empty list
sp.comp <- c()
sp.comp <- list(sp.comp)

sp.comp.or <- c()
sp.comp.or <- list(sp.comp.or)

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
  reps <- 100 #set number of permutations
  
  #Create a list with spaces for each output matrix  
  nulls<-vector("list",reps)  
  for (i in 1:reps) {
    nulls[[i]]<-null.model.II(web)
    nulls[[i]] <- nulls[[i]][ rowSums(nulls[[i]])!=0, ]
  }
  
  #call any individual matrix from that list using nulls[[x]], where x is the number of the matrix you want to call
  null.comp <- data.frame(matrix(, nrow=reps, ncol(web)))
  for (i in 1:reps) {
    null.comp[i, ] <- t(colMeans(as.matrix(vegdist(t(nulls[[i]]), binary=T, upper = T))))#add colMeans if this doesn't work
  }
  null.comp <- na.omit(null.comp)
  webcomp <- t(colMeans(as.matrix(vegdist(t(web), binary=T, upper = T))))
  colnames(null.comp) <- colnames(webcomp)
  
  comp <- rbind(null.comp,webcomp)#Add observed connectance into distribution
  
  sd <- apply(null.comp, 2, sd)#calculate standard deviation
  
  sp.comp[[j]] <- abs((webcomp - colMeans(null.comp))/sd)#print results into list
  
  sp.comp.or[[j]] <- webcomp#print results into list
  
}

#convert list to dataframe
sp.comp <- rbind.fill(lapply(sp.comp, as.data.frame))
sp.comp$Network <- levels(geonet$Network)
sp.comp.melt <- melt(sp.comp, "Network", variable.name = "Pollinator", value.name = "value", na.rm = TRUE)

#add order and family to dataframe
geo.uni <- unique(geonet[c("Pollinator", "Family", "Order")])
sp.comp.order <- merge(sp.comp.melt,geo.uni, by="Pollinator")

#calculate mean generalism
comp.order.ave <- sp.comp.order %>%
  group_by(Network, Order) %>%
  summarise(Generalism=mean(value))

#################################################
