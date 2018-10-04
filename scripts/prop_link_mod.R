#Prop links model

library(rstan)
library(bayesplot)
library(brms)
library(plyr)
library(dplyr)

g.sub$Reference=as.factor(g.sub$Reference)
g.sub$Network=as.factor(g.sub$Network)
g.sub$PolOrder=as.factor(g.sub$PolOrder)
g.sub$clim=as.factor(g.sub$clim)
propprior <- prior(normal(0,2), class = b) + prior(normal(0,0.4), class = sd)

str(g.sub)
##FULL MODEL - THREE WAY INTERACTION

prop_mod1=brm(prop_links~PolOrder*clim*ele+(1|Reference/Network),
              family=beta_family(link = "logit"),inits=0,
              data=g.sub,cores=4)

prop_mod2=brm(prop_links~PolOrder*clim*scale(ele)+(1|Reference/Network),
              family=Beta(link = "logit"),inits=0,
              data=g.sub,cores=4)

prop_mod3=brm(prop_links~PolOrder*clim+(1|Reference/Network),
              family=Beta(link = "logit"),inits=0,
              data=g.sub,cores=4)

ppc_violin_grouped(g.sub$prop_links, 
                   yrep = posterior_predict(prop_mod2, nsamples = 100), group = g.sub$clim)

ppc_violin_grouped(g.sub$prop_links, 
                   yrep = posterior_predict(prop_mod2, nsamples = 100), group = g.sub$PolOrder)

detach("tidyr")

check_all_diagnostics(prop_mod3$fit)

bayes_R2(prop_mod3)

##PLOT
library(tidybayes)
prop.plot1=g.sub%>% droplevels() %>%
  add_fitted_draws(prop_mod3,n=100)%>%
  ggplot(aes(x=value,y=as.factor(PolOrder)))+
  stat_intervalh(aes(x=.value*100))+
  facet_grid(~clim, scale = "free_y")+
  scale_color_brewer()+
  theme_bw()+
  xlab("Network links (%)")+
  ylab("Order")
prop.plot1

prop.plot2=g.sub%>% droplevels() %>%
  add_predicted_draws(prop_mod3,n=100)%>%
  ggplot(aes(x=value,y=as.factor(PolOrder)))+
  stat_intervalh(aes(x=.prediction))+
  facet_grid(~clim, scale = "free_y")+
  scale_color_brewer()+
  theme_bw()+
  xlab("Prop. network links")+
  ylab("Pollinator order")
prop.plot2


##EFFECT OF ELEVATION
library(modelr)
prop_ele_plot=g.sub %>%
  data_grid(ele = seq_range(ele, n = 101), clim, PolOrder,Reference,Network) %>%    # add am to the prediction grid
  add_fitted_draws(prop_mod2) %>%
  ggplot(aes(x = ele, y = prop_links)) +
  stat_lineribbon(aes(y = .value), .width = c(.99, .95, .8, .5)) +
  scale_fill_brewer() +
  facet_wrap(~ PolOrder) 
prop_ele_plot