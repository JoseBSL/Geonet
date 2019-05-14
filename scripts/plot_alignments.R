###Plot alignments


Fig1=align_plots(prop_plot, Figure_Spec_Ord,align="hv", axis="tblr")
Fig1A <- ggdraw(Fig1[[1]])
Fig1B <- ggdraw(Fig1[[2]]) #7 x 14 inches

grid.arrange(Fig1A,Fig1B,ncol=1,nrow=2) #6 x 8 inches


Fig2=align_plots(map1, map2,align="hv", axis="tblr")
Fig2A <- ggdraw(Fig2[[1]])
Fig2B <- ggdraw(Fig2[[2]]) #7 x 14 inches

grid.arrange(Fig2A,Fig2B,ncol=1,nrow=2) #7 x 14 inches
ggsave("Figure2.pdf")

Fig3=align_plots(prop_plot2, Figure_Spec_Ord2,align="hv", axis="tblr")
Fig3A <- ggdraw(Fig3[[1]])
Fig3B <- ggdraw(Fig3[[2]]) #7 x 14 inches

grid.arrange(Fig3A,Fig3B,ncol=1,nrow=2) #7 x 14 inches
