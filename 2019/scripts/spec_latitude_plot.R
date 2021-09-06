##Spec latitude graph


spec_ord_plots2=plot(marginal_effects(spec_ord_mod_5,
                                     conditions = data.frame(int_tot = 100)))

Figure_Spec_Ord2=spec_ord_plots2[[3]]+theme_bw()+
  facet_wrap(~PolOrder)+
  xlab("Latitude")+
  ylab("No.of plant partners")+
  ggtitle("B) Pollinator generalism")+
  scale_colour_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")+ theme(legend.position="none")
  
Figure_Spec_Ord2

