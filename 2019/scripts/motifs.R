
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(vegan)
library(bmotif)

#subset out one network to play with
geo.wide <- subset(geonet, Network == "benadi_14_1")

#cast long format dataframe and create interaction matirx
web <- dcast(geo.wide, Network + Pollinator + PolOrder ~ Plant, value.var = "Int")
web[is.na(web)] <- 0
group <- as.vector(web$PolOrder)
web1 <- as.matrix(web[,c(-1:-3)])
rownames(web1) <- web$Pollinator # give the matrix row names

#run motif analysis
#run all the way up to 6 node motifs (gives 148 columns)
#normalise node occurance in motifs by network size
motifs <- positions(M = web1, six_node = TRUE, level = "rows", normalisation = "size class")

#compute dissimilarity matrix from motif matrix
vj <- vegdist(motifs, method = 'bray')
mj2=metaMDS(vj,k=3)

#extract ordination coordinates
NMDS1 = data.frame(MDS1 = mj2$points[,1], MDS2 = mj2$points[,2])
w1 <- cbind(NMDS1,group)
w1$group <- as.factor(w1$group)

#run adonis and pairwise comps
motifs <- cbind(motifs,group)
motifs <- na.omit(motifs)

#subset by the main orders
#not sure how this affects the permanova below
motifs <- subset(motifs, group %in% c("Lepidoptera","Bee", "Diptera","Syrphidae", "Hymenoptera"))%>%droplevels

#run the permanova on the motif matrix
#remove the grouping coulm at end of df
adonis(motifs[,-149] ~ motifs$group, permutations = 999)

#run pairwise comps
pairwise.adonis(motifs[,1:148],motifs$group)

#run beta-dispersion test
beta_int <- betadisper(vj, web$PolOrder, type = c("centroid"))
TukeyHSD(beta_int) ## all good!

#compute convex hulls
hull <- w1 %>% 
  group_by(group) %>%
  slice(chull(MDS1, MDS2))
hull <- subset(hull, group %in% c("Lepidoptera","Bee", "Diptera","Syrphidae", "Hymenoptera"))%>%droplevels

#can also plot the ordination
p2 <- ggplot(data = w1, aes(MDS1, MDS2))
p2 <- p2 + geom_point(aes(colour = group), size=2)
#can add the convex hulls but looks a bit silly
#this shows us that bees and syrphids both have the braaodest niches both interacting with generalists and specialists 
#p2 <- p2 +  aes(fill = factor(group)) + geom_polygon(data = hull, alpha = 0.5)
#p2 <- p2 + geom_path(data=df_ell1, aes(x=MDS1, y=MDS2,colour=group), size=1, linetype=2)
#p2 <- p2 + annotate("text", x = 0.4, y = 0.48, label = "F = 63.23; P < 0.001")
#p2 <- p2 + annotate("text", x = 0.35, y = 0.45, label = "italic(R) ^ 2 == 0.59", 
#                    parse = TRUE)
#p2 <- p2 + annotate("text", x = 0.36, y = 0.42, label = "Stress = 0.12")
#p2 <- p2 + scale_color_manual(values = group.colors.ord)
p2 <- p2 + theme(axis.line.x = element_line(size=0, colour = "black"),
                 axis.line.y = element_line(size=0, colour = "black"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_rect(size=.8, colour = "black", fill=NA),
                 panel.background = element_blank()) +
  theme(axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
        axis.title.x=element_text(size=15, vjust = 1),
        axis.title.y=element_text(size=15, vjust = 1),
        axis.text=element_text(colour = "black"))+
  theme(axis.ticks.length = unit(2.5, "mm"))
p2 <- p2 + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p2


#create ellipse function (this is hidden in vegan)
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#compute ellipse values
df_ell1 <- data.frame()
for(g in levels(w1$group)){
  df_ell1 <- rbind(df_ell1, cbind(as.data.frame(with(w1[w1$group==g,],
             veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,
             center=c(mean(MDS1),mean(MDS2))))) ,group=g))}

####################################
#END
####################################