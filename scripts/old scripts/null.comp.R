#################################################
#commpute jaccadi parwise dissimilarity for null networks
#################################################

library(reshape2)
library(vegan)
library(plyr)
library(dplyr)

#################################
#start here
#################################

#cast long format dataframe
geo.wide <- dcast(geonet, Network + Plant ~ Pollinator, value.var = "Int")
geo.wide[is.na(geo.wide)] <- 0
geo.wide[,1]=as.factor(geo.wide[,1])

#create empty list
null.net <- c()
null.net <- list(null.net)

#set max vector size
Sys.setenv('R_MAX_VSIZE'=32000000000)

#run loop over each site
for (j in levels(geo.wide[, 1])){
  web <- subset(geo.wide, Network == j)#iterate over site
  network <- as.vector(web$Network[1])
  web <- web[,c(-1,-2)]
  web = web[,colSums(web) > 0]#remove species with no links at each site
  web.names <- c(colnames(web))
  
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
  
  #Create a list with spaces for each output matrix  
  nulls<-vector("list",reps)  
  for (i in 1:reps) {
    nulls[[i]]<-null.model.II(web)
  }
  
  #call any individual matrix from that list using nulls[[x]], where x is the number of the matrix you want to call
  null.list <- vector("list") 
  for (i in 1:reps) {
    null.list[[i]] <- as.matrix(vegdist(t(nulls[[i]]), "jaccard", binary=T))#add colMeans if this doesn't work
  }

  #convert nans to nas
  null.list <- rapply(null.list, f=function(x) ifelse(is.nan(x),NA,x), how="replace" )

  #compute means and standard deviation for each species pair 
  null.mean <- apply(simplify2array(lapply(null.list, as.matrix)),1:2, mean, na.rm = TRUE)
  colnames(null.mean) <- web.names
  rownames(null.mean) <- web.names
  null.mean <- melt(null.mean, value.name="mean")
  null.sd <- apply(simplify2array(lapply(null.list, as.matrix)),1:2, sd, na.rm = TRUE)
  colnames(null.sd) <- web.names
  rownames(null.sd) <- web.names
  null.sd <- melt(null.sd,value.name="sd")
  null.mean.sd <- merge(null.mean,null.sd, by=c("Var1","Var2"))
  null.mean.sd <- null.mean.sd[!null.mean.sd$Var1==null.mean.sd$Var2,]
  null.mean.sd$network <- network
  
  #print into list
  null.net[[j]] <- null.mean.sd
  
}
  
#convert list to dataframe
null.net <- rbind.fill(lapply(null.net, as.data.frame))
colnames(null.net)=c("Poll_sp1","Poll_sp2","mean","sd","Network")

#################################
#END
#################################