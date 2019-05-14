########################################
################MERGE PLOTS#############
########################################
library(ggplot2)
library(cowplot)
library(gridExtra)

prop.gg
spec.gg
cent.gg
betw.gg


Fig1=align_plots(prop.gg, spec.gg, cent.gg,betw.gg,align="hv", axis="l")
Fig1A <- ggdraw(Fig1[[1]])
Fig1B <- ggdraw(Fig1[[2]])
Fig1C <- ggdraw(Fig1[[3]])
Fig1D <- ggdraw(Fig1[[4]])

Fig1.grid <- grid.arrange(Fig1A,Fig1B,Fig1C,Fig1D,ncol=1,nrow=4) #7 x 14 inches

ggsave(Fig1.grid,file="Fig1 grid.pdf",device = "pdf",dpi=320,width=15,height=10,units = c("in"))


Fig1Ag <- ggplotGrob(prop.gg)
Fig1Bg <- ggplotGrob(spec.gg)
Fig1Cg <- ggplotGrob(cent.gg)
Fig1Dg <- ggplotGrob(betw.gg)

grid.arrange(Fig1Ag,Fig1Ag,Fig1Ag,Fig1Dg,nrow=4)
g <- rbind(Fig1Ag, Fig1Bg,Fig1Cg,Fig1Dg)

ggsave(g,file="Fig1 grid.pdf",device = "pdf",dpi=320,width=10,height=10,units = c("in"))
