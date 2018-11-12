# Load Library #####################
require(latex2exp)
library(viridis)
library(cowplot)
library(reshape2)

## All MC data ##########################
UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=(SUM))) + 
  geom_point(aes(x=LK_CODE, y= SUM, color="SUM"), size=2, alpha=0.5) + 
  geom_point(aes(x=LK_CODE, y=(ELISA), color="ELISA"), size = 2, alpha=0.5) +  
  theme_classic() + 
  ylab(TeX('$\\mu$g/L of MC')) + 
  xlab( "Lakes")  + 
  theme(
    legend.title=element_blank(), 
    legend.position="bottom", 
    axis.text.x = element_text(
      angle = 90, 
      size = 7,
      vjust = 0.4)
  ) + 
  geom_hline(
    yintercept = 4, 
    color="#BB0000", 
    linetype="dashed", 
    show.legend = FALSE
  ) +  
  geom_text(aes(17,4.9, label = "EPA Guideline"), color="#BB0000", show.legend=FALSE) +
  scale_color_manual(labels = c("MC from ELISA", "MC from LC-MS/MS"), values = c("green", "black"))
cairo_ps(filename = "Microcystin.eps",
         width = 7, height = 7, pointsize = 12,
         fallback_resolution = 300)
print(x)
dev.off()

### Manipulation ###################################################################################################
July <- UltimateFactor %>%
  filter(Month == "7") %>% 
  gather(congener, value, Nodul:MC_LF)
August <- UltimateFactor %>% 
  filter(Month == "8") %>% 
  gather(congener, value, Nodul:MC_LF)
Sept <- UltimateFactor %>% 
  filter(Month == "9") %>% 
  gather(congener, value, Nodul:MC_LF)
Oct <- UltimateFactor %>% 
  filter(Month == "10") %>% 
  gather(congener, value, Nodul:MC_LF)

aa <- UltimateFactor %>% 
  filter(Month == "8") %>% 
  gather(congener, value, S_D_Asp3_RR:S_MC_LF)
ss <- UltimateFactor %>% 
  filter(Month == "9") %>% 
  gather(congener, value, S_D_Asp3_RR:S_MC_LF)
oo <- UltimateFactor %>% 
  filter(Month == "10") %>% 
  gather(congener, value, S_D_Asp3_RR:S_MC_LF)

july <- UltimateFactor %>% 
  filter(Month == "7")
august <- UltimateFactor %>% 
  filter(Month == "8")
sept <- UltimateFactor %>% 
  filter(Month == "9")
oct <- UltimateFactor %>% 
  filter(Month == "10")
melt.july <- july %>% 
  select(LK_CODE, SUM, ELISA) %>% melt()
melt.august <- august %>% 
  select(LK_CODE, SUM, ELISA) %>% melt()
melt.sept <- sept %>% 
  select(LK_CODE, SUM, ELISA) %>% melt()
melt.oct <- oct %>% 
  select(LK_CODE, SUM, ELISA) %>% melt() 
## LC-MS to ELISA compare ##################################################################################################
# compare.eps
a<-melt.july %>% 
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge", color="black")  + 
  theme_classic() + 
  scale_fill_viridis(discrete = T, 
                     option = "E",
                     label=c("LC-MS/MS ", "ELISA")) +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX(' MC ($\\mu$g/L)')) +
  xlab("  ")

b<-melt.august %>% 
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge", color="black")  + 
  theme_classic() + 
  scale_fill_viridis(discrete = T, option = "E",
                     label=c("LC-MS/MS", "ELISA")) +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(" ") +
  xlab(" ")

c<-melt.sept %>% 
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge", color="black")  + 
  theme_classic() + 
  scale_fill_viridis(discrete = T, option = "E",
                     label=c("LC-MS/MS", "ELISA")) +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX(' MC ($\\mu$g/L)')) +
  xlab("Lakes")

d<-melt.oct %>% 
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge", color="black")  + 
  theme_classic() + 
  scale_fill_viridis(discrete = T, option = "E",
                     label=c("LC-MS/MS", "ELISA")) +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(" ") +
  xlab("Lakes")

legend_b <- get_legend(a + 
                         theme(legend.position = "bottom", 
                               legend.key.size = unit(0.2,"cm")))
dd <- plot_grid(a + theme(legend.position = "none"),
          b + theme(legend.position = "none"),
          c + theme(legend.position = "none"),
          d + theme(legend.position = "none"), 
          labels = c("A", "B", "C", "D"),
          align = 'vh',
          ncol = 2)
plot_grid(dd, legend_b, ncol=1, rel_heights = c(1, .2), rel_widths = c(1,0.2))
ggsave("compare.eps", device="eps")
# LC-MS month by month#####################################################################################################
## month.eps##
a<-July %>% 
  ggplot(aes(x=LK_CODE)) + 
  geom_bar(stat="identity", aes(y=value, fill=congener, width=1))  + 
  scale_fill_viridis(discrete=TRUE,option="B",
                     label=c( "[D-Asp3] MC-LR",
                              "[D-Asp3] MC-RR", 
                              "MC-HilR",
                              "MC-HtyR",
                              "MC-LA",
                              "MC-LF",
                              "MC-LR",
                              "MC-LW",
                              "MC-LY",
                              "MC-RR", 
                              "MC-WR",
                              "MC-YR", 
                              "Nodularin")) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX(' MC ($\\mu$g/L)')) +
  xlab(" ") + 
  scale_y_continuous(limits = c(0,15))

b<-August %>% 
  ggplot(aes(x=LK_CODE)) + 
  geom_bar(stat="identity", 
           aes(y=value, fill=congener, width=1))  + 
  scale_fill_viridis(discrete=TRUE,option="B",
                     label=c( "[D-Asp3] MC-LR",
                              "[D-Asp3] MC-RR", 
                              "MC-HilR",
                              "MC-HtyR",
                              "MC-LA",
                              "MC-LF",
                              "MC-LR",
                              "MC-LW",
                              "MC-LY",
                              "MC-RR", 
                              "MC-WR",
                              "MC-YR", 
                              "Nodularin")) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(" ") +
  xlab(" ") +
  scale_y_continuous(limits = c(0,15))

c<-Sept %>% 
  ggplot(aes(x=LK_CODE)) + 
  geom_bar(stat="identity", 
           aes(y=value, fill=congener, width=1))  + 
  scale_fill_viridis(discrete=TRUE,option="B",
                     label=c( "[D-Asp3] MC-LR",
                              "[D-Asp3] MC-RR", 
                              "MC-HilR",
                              "MC-HtyR",
                              "MC-LA",
                              "MC-LF",
                              "MC-LR",
                              "MC-LW",
                              "MC-LY",
                              "MC-RR", 
                              "MC-WR",
                              "MC-YR", 
                              "Nodularin")) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX(' MC ($\\mu$g/L)')) +
  xlab("Lakes") + 
  scale_y_continuous(limits = c(0,15))

d<-Oct %>% 
  ggplot(aes(x=LK_CODE)) + 
  geom_bar(stat="identity", aes(y=value, fill=congener, width=1))  + 
  scale_fill_viridis(discrete=TRUE,option="B",
                     label=c( "[D-Asp3] MC-LR",
                              "[D-Asp3] MC-RR", 
                              "MC-HilR",
                              "MC-HtyR",
                              "MC-LA",
                              "MC-LF",
                              "MC-LR",
                              "MC-LW",
                              "MC-LY",
                              "MC-RR", 
                              "MC-WR",
                              "MC-YR", 
                              "Nodularin")) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab("  ") +
  xlab("Lakes") + 
  scale_y_continuous(limits = c(0,15))


legend_b <- get_legend(a + 
                         theme(legend.position = "bottom", 
                               legend.key.size = unit(0.3,"cm")) +
                       guides(colours = guide_legend(nrow = 1 )))
dd <- plot_grid(a + theme(legend.position = "none"),
          b + theme(legend.position = "none"),
          c + theme(legend.position = "none"),
          d + theme(legend.position = "none"), 
          labels = c("A", "B", "C", "D"),
          align = 'vh',
          ncol = 2)
plot_grid(dd, legend_b, ncol=1, nrow = 2, rel_heights = c(1, .3), rel_widths = c(1,0.2))
ggsave("month.eps", height=9, units = "in", device="eps")

# SPATTS month by month######################################################################################
## spatter.eps
a<-aa %>% 
  ggplot(aes(x=LK_CODE)) + 
  geom_bar(stat="identity", aes(y=value, fill=congener, width=1))  + 
  scale_fill_viridis(discrete=TRUE,option="B",
                     label=c( "[D-Asp3] MC-LR",
                              "[D-Asp3] MC-RR", 
                              "MC-HilR",
                              "MC-HtyR",
                              "MC-LA",
                              "MC-LF",
                              "MC-LR",
                              "MC-LW",
                              "MC-LY",
                              "MC-RR", 
                              "MC-WR",
                              "MC-YR", 
                              "Nodularin")) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX('\\frac{ng of MC}{g of resin x day')) +
  xlab("  ") + 
  scale_y_continuous(limits = c(0,800))
b<-ss %>% 
  ggplot(aes(x=LK_CODE)) + 
  geom_bar(stat="identity", aes(y=value, fill=congener, width=1))  + 
  scale_fill_viridis(discrete=TRUE,option="B",
                     label=c( "[D-Asp3] MC-LR",
                              "[D-Asp3] MC-RR", 
                              "MC-HilR",
                              "MC-HtyR",
                              "MC-LA",
                              "MC-LF",
                              "MC-LR",
                              "MC-LW",
                              "MC-LY",
                              "MC-RR", 
                              "MC-WR",
                              "MC-YR", 
                              "Nodularin")) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX('\\frac{ng of MC}{g of resin x day')) +
  xlab(" ") + 
  scale_y_continuous(limits = c(0,800))

c<-oo %>% 
  ggplot(aes(x=LK_CODE)) + 
  geom_bar(stat="identity", aes(y=value, fill=congener, width=1))  + 
  scale_fill_viridis(discrete=TRUE,option="B",
                     label=c( "[D-Asp3] MC-LR",
                              "[D-Asp3] MC-RR", 
                              "MC-HilR",
                              "MC-HtyR",
                              "MC-LA",
                              "MC-LF",
                              "MC-LR",
                              "MC-LW",
                              "MC-LY",
                              "MC-RR", 
                              "MC-WR",
                              "MC-YR", 
                              "Nodularin")) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX('\\frac{ng of MC}{g of resin x day')) +
  xlab("Lakes") + 
  scale_y_continuous(limits = c(0,800))


legend_b <- get_legend(a +
                         theme(legend.position = "bottom",
                               legend.direction = "horizontal", 
                               legend.key.size = unit(0.3,"cm")) + 
                         guides(colour = guide_legend(nrow = 1)))
dd <- plot_grid(a + theme(legend.position = "none"),
          b + theme(legend.position = "none"),
          c + theme(legend.position = "none"),
          labels = c("A", "B", "C"),
          align = 'vh',
          ncol = 1)
plot_grid(dd, legend_b, ncol=1, nrow = 2, rel_heights = c(1, 0.3), rel_widths = c(1,1))
ggsave("spatter.eps", width = 8, height = 11, units = c("in"), device="eps")
