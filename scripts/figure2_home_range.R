
# Home range and home range overlap
# Figure 2

#..................................................
# Home range size: Figure 2 A ----
#..................................................

mean_HR_est <- round(mean(HR_size$HR_est), 2)

figure2a_HR_size <- 
  ggplot() +
  geom_vline(data = HR_size, aes(xintercept = mean_HR_est),
             linetype = "dotdash") +
  geom_linerange(data = HR_size, 
                 aes(xmin = HR_low, xmax = HR_high, y = ID, color = Sex),
                 linewidth = 3, alpha = 0.5) + 
  labs(x = bquote(bold("Home range area" ~ (km^2))),
       y = "") +
  ggtitle("A") +
  scale_color_manual(values = c('#004488', '#A50026'), breaks = c('Male', 'Female')) +
  geom_point(data = HR_size,
             aes(x = HR_est, y = ID, fill = "white"), color = "white",
             size = 2) +
  geom_point(data = HR_size, 
             aes(x = HR_est, y = ID, color = Sex),
             size = 1.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.1, size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))
figure2a_HR_size
ggsave(figure2a_HR_size, filename = "figures/individual figures/figure2a_HR_size.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 4, units = "in", dpi = 600)

#..................................................
# Home range overlap sex comparison: Figure 2 B ----
#..................................................

figure2b_overlap_sex <-
  ggplot(data = overlap_df, 
         mapping = aes(x = sex_comparison, y = overlap_est, fill = sex_comparison)) + 
  geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
  ylab("Home range overlap") +
  xlab("Sex") +
  ggtitle("B") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans", face = "bold"),
        legend.position="none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#A50026", "#9970AB", "#004488"),
                    breaks = c("female-female","female-male","male-male"),
                    labels = c("Female - Female", "Female - Male", "Male - Male")) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(breaks = c("female-female","female-male","male-male"),
                   labels = c("Female - Female", "Female - Male", "Male - Male"))
figure2b_overlap_sex
ggsave(figure2b_overlap_sex, filename = "figures/individual figures/figure2b_overlap_sex.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 3, units = "in", dpi = 600)


#..................................................
# multi-panel ----
#..................................................

figure2_left <- grid.arrange(figure2a_HR_size,
                             #plot_grid(plot_HRO_site1, plot_HRO_site2, ncol = 1),
                             figure2b_overlap_sex,
                             nrow = 2, heights = c(0.65, 0.35))

ggsave(figure2_left, filename = "figures/individual figures/figure2_left.png", device = NULL,
       path = NULL, scale = 1, width = 4, height = 6, units = "in", dpi = 600)


#..................................................
# Figure 2 C,D: Home range overlap ----
#..................................................

names(AKDE_1)
#"Alexander", "Anthony", "Bumpus", "Cate", "Christoffer",
#"Elaine", "Jackson", "Kyle", "Little_Rick", "Makao",
#"Puji", "Rodolfo"

#assign colours based on sex of individual at site 1
COL_1 <- c("#004488", "#004488", "#A50026", "#A50026", "#004488", 
           "#A50026", "#004488", "#004488", "#004488", "#A50026", 
           "#A50026", "#004488")

names(AKDE_2)
#"Annie", "Beto", "Hannah", "Jane", "Larry",
#"Luigi", "Margaret", "Maria", "Reid", "Sheron",
#"Thomas"

#assign colours based on sex of individual at site 2
COL_2 <- c("#A50026", "#004488", "#A50026", "#A50026", "#004488", 
           "#004488", "#A50026", "#A50026", "#004488", "#A50026", 
           "#004488") 

png(file = "figures/individual figures/figure2_right_overlap.png", width = 2.86, height = 6, units = "in", res = 600)
par(mfrow=c(2,1))
par(mgp = c(2, 0.5, 0))             #Adjust the third element (margin for axis title spacing)
par(mar = c(3, 3, 1.25, 0.25))      #margin defaults (order: bottom, left, top, and right)
plot(AKDE_1, 
     col.DF = COL_1, 
     col.level = COL_1, 
     col.grid = NA, 
     level = NA,
     lwd.level = 1,            #line thickness
     #font=2,                  #bold axis text
     cex.lab = 1,              #size of axis title
     cex.axis = 0.8,           #size of axis text font
     font.lab = 2)             #bold axis labels
title("C", adj = 0)
plot(AKDE_2, 
     col.DF = COL_2, 
     col.level = COL_2, 
     col.grid = NA, 
     level = NA,
     lwd.level = 1,            #line thickness
     #font=2,                  #bold axis text
     cex.lab = 1,              #size of axis title
     cex.axis = 0.8,           #size of axis text font
     font.lab = 2)             #bold axis labels
title("D", adj = 0)
dev.off()

#multi-panel
#manually combine the two plots due to the nature of base r plot and ggplot
#plot saved as "figures/figure2.png"