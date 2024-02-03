
#............................................................
# Pair 11 - Margaret and Thomas
#............................................................

Margaret <- DATA_TELEMETRY$Margaret
Thomas <- DATA_TELEMETRY$Thomas

#............................................................
# Home range overlap ----
#............................................................

#subset home range overlap data
AKDE_pair11 <- AKDE[c("Thomas", "Margaret")]
#assign colours
COL_pair11 <- c("#004488", "#A50026")

### home-range overlap with GPS points  ----
png(file = "figures/individual figures/figure4a_overlap_pair11.png", width = 2.86, height = 4, units = "in", res = 600)
par(mgp = c(1.5, 0.5, 0))                           #spacing between text (axis title, axis text, axis ticks)
par(mar = c(2.5, 2.5, 1.75, 0.5))                   #margin defaults (order: bottom, left, top, and right)
plot(AKDE_pair11, col.DF = COL_pair11, 
     col.level = COL_pair11, 
     col.grid = NA, 
     level = NA,
     lwd.level = 1,            #line thickness
     cex.lab = 0.9,                 #size axis title
     #font=2,                       #bold axis text
     cex.axis = 0.7,                #size axis text font
     font.lab = 2,                  #bold axis labels
     tcl  = -0.35)                  #axis ticks size          
title("A", adj = 0)
plot(list(Thomas, Margaret), error = FALSE,         #arguments need to be before colour or won't work
     col = c("#004488", "#A50026"),
     add = TRUE) #to overlay the previous plot
dev.off()

#............................................................
# Distance ----
#............................................................

distance_pair11 <- distance_pair_df[distance_pair_df$pair_ID_number == 11,]

figure4b_pair11_distance <- 
  ggplot() +
  geom_line(data = distance_pair11,
            aes(y = est, x = timestamp, 
            ), size = 0.15) +
  xlab("") +
  ylab("Distance (m)") +
  ggtitle("B") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.1,0.1,0.05,0.2), "cm")) #top, right, bot, left)
figure4b_pair11_distance
ggsave(figure4b_pair11_distance, width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent",
       file="figures/individual figures/figure4b_pair11_distance.png")

#............................................................
# Correlative movement (total correlative) ----
#............................................................

# png(file = "figures/corrmove_pair11.png", width = 6.86, height = 6, units = "in", res = 600)
# plot_cm_pair11 <- 
#   plot.corrMove(cm_pair11)
# dev.off()

#manual plotting, not using plot.corrMove()

figure4c_pair11_cm <-
  ggplot() +
  geom_hline(aes(yintercept = 0),
             linetype = "dashed", alpha = 0.7) +
  geom_point(aes(y = cm_pair11$etaTot.MLE, x = cm_pair11$timestamp, col = cm_pair11$sel.mod.code),
             size = 0.5) +
  xlab("") +
  ylab("Total correlation") +
  ggtitle("C") +
  scale_color_manual(values = c('#EE7733', '#88CCEE', '#BBBBBB'), breaks = c('CU', 'UC', 'UU')) + 
  theme_bw() +
  scale_y_continuous(limits = c(-1,1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.direction = "horizontal",  # Set the legend direction to horizontal
        legend.position = c(0.5,1.1),     
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        #legend.spacing.x = unit(0.1, 'cm'), #change the spacing between legend symbols
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.1,0.1,0.05,0.2), "cm")) + #top, right, bot, left
  guides(colour = guide_legend(override.aes = list(size=3))) # change the size of the symbols in the legend

ggsave(figure4c_pair11_cm, width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent",
       file="figures/individual figures/figure4c_pair11_cm.png")

figure4_right <- grid.arrange(figure4b_pair11_distance, 
                              figure4c_pair11_cm,
                              ncol=1)

ggsave(figure4_right, filename = "figures/individual figures/figure4_right.png", device = NULL,
       path = NULL, scale = 1, width = 4, height = 4, units = "in", dpi = 600)

#multi-panel
#manually combine the two plots due to the nature of base r plot and ggplot
#plot saved as "figures/figure4.png"


