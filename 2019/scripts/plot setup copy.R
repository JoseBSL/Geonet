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


Fig1=align_plots(prop.gg, spec.gg,align="hv", axis="l")
Fig1A <- ggdraw(Fig1[[1]])
Fig1B <- ggdraw(Fig1[[2]])

grid.arrange(Fig1A,Fig1B)
Fig1C <- ggdraw(Fig1[[3]])
Fig1D <- ggdraw(Fig1[[4]])

Fig1.grid <- grid.arrange(Fig1A,Fig1B,Fig1C,Fig1D,ncol=1,nrow=4) #7 x 14 inches

ggsave(Fig1.grid,file="Fig1 grid.pdf",device = "pdf",dpi=320,width=15,height=10,units = c("in"))


Fig1Ag <- ggplotGrob(prop.gg)
Fig1Bg <- ggplotGrob(spec.gg)
Fig1Cg <- ggplotGrob(betw.gg)
Fig1Dg <- ggplotGrob(cent.gg)

grid.arrange(Fig1Ag,Fig1Bg,nrow=2)
g1 <- rbind(Fig1Ag, Fig1Bg)
plot(g1)
ggsave(g1,file="Fig1 links_spec.pdf",device = "pdf",
       dpi=320,width=10,height=5,units = c("in"))

g2 <- rbind(Fig1Cg, Fig1Dg)
plot(g2)
ggsave(g2,file="Fig2 centrality.pdf",device = "pdf",dpi=320,width=10,height=5,units = c("in"))




