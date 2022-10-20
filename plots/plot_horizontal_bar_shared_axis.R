
CV_vs_metric <- function( data, metric, ptitle ){
g.mid <- ggplot(data,aes(x=1,y=reorder(SPECIES_CD, CV_d)))+geom_text(aes(label=COMNAME),size = 3  )+
  geom_segment(aes(x=0.94,xend=0.96,yend=SPECIES_CD))+
  geom_segment(aes(x=1.04,xend=1.065,yend=SPECIES_CD))+
  ggtitle(ptitle)+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-1,1,-1), "mm"),
        plot.title = element_text(hjust = 0.5))

g1 <- ggplot(data = data, aes(x = reorder(SPECIES_CD, CV_d), y = eval(parse(text = metric)), fill = 'even')) +
  geom_bar(stat = "identity", fill = "deepskyblue4") + ggtitle(paste("Species", metric,sep = " ")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,-1,1,0), "mm"),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_reverse(expand = c(NA, 0)) + coord_flip() + guides(fill = "none")

g2 <- ggplot(data = data, aes(x = reorder(SPECIES_CD, CV_d), y = CV_d *100, fill = 'even')) +xlab(NULL)+
  geom_bar(stat = "identity", fill = "deepskyblue4") + ggtitle("Coefficient of Variation(CV) of Density") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm"), plot.title = element_text(hjust = 0.5)) +
  coord_flip() + guides(fill = "none")

gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

ggsave(filename = paste(data$REGION[1],"_Occ_v_CV_2.jpg", sep = ""), plot = grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(3.5/9,2/9,3.5/9)) , width = 9.8, height = 6.5, dpi = 300, units = "in", device = "jpg")


grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(3.5/9,2/9,3.5/9))

}