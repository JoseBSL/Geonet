##JUST BEES AND FLIES
jaccard_sub_bf=jaccard_sub[jaccard_sub$Order1==c("Bee","Diptera","Syrphidae"),] %>% 
  droplevels()
jaccard_sub_bf=jaccard_sub_bf[jaccard_sub_bf$Order2==c("Bee","Diptera","Syrphidae"),] %>% 
  droplevels()



jaccard_bf_mean=aggregate(jac~Reference+Network+clim+ele+Size+
                             Latitude+jac.fam+jac.ord,data=jaccard_sub_bf,
                           FUN = mean)


##ADD STANDARD ERROR
jaccard_bf_sei=aggregate(jac~Reference+Network+clim+ele+Size+
                            Latitude+jac.fam+jac.ord,data=jaccard_sub_bf,
                          FUN = function(x) c(mean = mean(x), se = std.error(x)))[9]

jaccard_bf_mean$jaccard=jaccard_bf_sei$jac[,1]
jaccard_bf_mean$se=jaccard_bf_sei$jac[,2]

jaccard_bf_length=aggregate(jac~Reference+Network+clim+ele+Size+
                               Latitude+jac.fam+jac.ord,
                             data=jaccard_sub_bf,
                             FUN = length)

jaccard_bf_mean$net_length=jaccard_bf_length$jac

levels(jaccard_bf_mean$jac.fam)

jaccard_bf_mean=jaccard_bf_mean %>% 
  group_by(jac.fam)          %>% 
  filter(all(c("A", "B","C","D","E") %in% clim)) %>% 
  droplevels()
##########