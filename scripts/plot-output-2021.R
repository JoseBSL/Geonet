#combining plots
#Figure 2
prop.grob <- ggplotGrob(prop.gg)
spec.grob <- ggplotGrob(spec.gg)

prop.spec.plots <- rbind(prop.grob,spec.grob)

ggsave(prop.spec.plots,
       file="plots and tables/fig 2 - grid.jpg",
       device = "jpg",
       height = 148, 
       width = 210, 
       units = "mm")

ggsave(prop.spec.plots,
       file="plots and tables/fig 2 - grid.pdf",
       device = "pdf",
       height = 148, 
       width = 210, 
       units = "mm")

#Figure 3
prop.map.grob <- ggplotGrob(prop.map)
spec.map.grob <- ggplotGrob(spec.map)

ggsave(rbind(prop.map.grob,
             spec.map.grob),
       file="plots and tables/fig 3 - map.jpg",
       device = "jpg",
       height = 148, 
       width = 210, 
       units = "mm")

ggsave(rbind(prop.map.grob,
             spec.map.grob),
       file="plots and tables/fig 3 - map.pdf",
       device = "pdf",
       height = 148, 
       width = 210, 
       units = "mm")


