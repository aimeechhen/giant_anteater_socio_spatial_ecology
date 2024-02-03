
# Figure 3
# Proximity and encounters

# Load packages
library(ggplot2)
library(lubridate)

# Load data
overlap_df <- readRDS("data/home_range/overlap_df.rds")
proximity_df <- readRDS("data/proximity/proximity_df.rds")
distance_df <- readRDS("data/proximity/distance_df.rds")
proximity_identified_pairs_df <- readRDS("data/proximity/proximity_identified_pairs_df.rds")

#..................................................
## Figure 3A: Proximity ratio ----
#..................................................

figure3a_proximity_ratio <- 
  ggplot() +
  geom_hline(yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(data = proximity_df, 
             aes(y = proximity_est, x = overlap_est, col = sex_comparison),
             size = 1.2, alpha = 0.3, shape = 16) + #alpha = colour intensity
  geom_segment(data = proximity_df, 
               aes(x = overlap_est, xend = overlap_est, y = proximity_low, yend = proximity_high, col = sex_comparison), 
               linewidth = 0.3, alpha = 0.3) +
  geom_point(data = proximity_identified_pairs_df, 
             aes(y = proximity_est, x = overlap_est, col = sex_comparison),
             size = 1.2) +
  geom_segment(data = proximity_identified_pairs_df,
               aes(x = overlap_est, xend = overlap_est, y = proximity_low, yend = proximity_high, col = sex_comparison), 
               linewidth = 0.3) +
  #scale_y_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#A50026", "#9970AB", "#004488"),
                     labels = c("Female - Female", "Female - Male", "Male - Male"),
                     name = "") +
  ylab("Proximity ratio") +
  xlab("Home-range overlap") +
  ggtitle("A") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.005, size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = c(0.8, 0.9), #horizontal, vertical
        legend.key.height = unit(0.3, "cm"),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) #top, right, bot, left
figure3a_proximity_ratio
ggsave(figure3a_proximity_ratio, 
       file="figures/individual figures/figure3a_proximity_ratio.png",
       width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")

#..................................................
# Figure 3B: Encounters ----
#..................................................

#Run estimating encounters prior to plotting
proximity_df$encounter_count <- NA
pair_ID <- unique(proximity_df$pair_ID)
#calculate total encounters of all individuals based on sex comparison type
for (i in pair_ID){
  subset_A <- distance_df[distance_df$pair_ID == i,]
  #count the number of times distance is below 15
  encounter_count <- sum(subset_A$distance_est < 15)
  #save results
  proximity_df[proximity_df$pair_ID == i, "encounter_count"] <- encounter_count
}

#do not include 0 encounters
proximity_df2 <- proximity_df[proximity_df$encounter_count != 0,]

library(scales)
figure3b_encounters <-
  ggplot(data = proximity_df2,
         aes(y = encounter_count, x = overlap_est)) +
  geom_point(data = proximity_df2, 
             aes(y = encounter_count, x = overlap_est, col = sex_comparison),
             size = 1.2) + 
  geom_smooth(method="lm", formula = y ~ x, se=F, col = "black") +
  scale_y_log10(expand = c(0,0.1), limits = c(0.1,2000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#A50026", "#9970AB", "#004488"),
                     labels = c("Female - Female", "Female - Male", "Male - Male"),
                     name = "") +
  xlab("Home-range overlap") +
  ylab("Encounter count") +
  #ggtitle("B") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        # legend.text = element_text(size=6, family = "sans", face = "bold"),
        # legend.position = c(0.84, 0.2), #horizontal, vertical
        # legend.key.height = unit(0.3, "cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) #top, right, bot, left
figure3b_encounters
# ggsave(figure3b_encounters,
#        file="figures/individual figures/figure3b_encounters.png",
#        width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")
ggsave(figure3b_encounters,
       file="figures/individual figures/encounters_overlap.png",
       height = 2.5, width = 3.33, units = "in", dpi = 600, bg = "transparent")

#..................................................
# Figure 3C: Encounters over time ----
#..................................................

#set threshold of 15m
distance_df$encounter <- ifelse(distance_df$distance_est > 15, 0,1)
encounter_df <- distance_df[which(distance_df$encounter == 1),]
encounter_df$doy <- yday(encounter_df$timestamp) #day of the year
encounter_df$month <- month(encounter_df$timestamp, label = TRUE)
encs <- aggregate(encounter ~ sex_comparison + doy + month + pair_ID, data = encounter_df, FUN = "sum")
encs <- merge(encs, overlap_df[,c("pair_ID","sex_comparison", "site")])
#total_encounters <- aggregate(encounter ~ month, data = encounter_df, sum)

#.....

figure3c_encounters_overtime <-
  ggplot(data = encs,
         aes(y = encounter, x = doy)) +
  geom_bar(stat = "identity", position = "stack", aes(fill = sex_comparison)) + 
  # geom_smooth(method="gam", formula = y ~ s(x, bs = "cc", k = 8),
  #             method.args =list(family = poisson), se=F) +
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +  # Use month abbreviations
  scale_y_continuous(limits = c(0,110), expand = c(0,1)) +
  scale_fill_manual(values = c("#A50026", "#9970AB", "#004488"),
                    labels = c("Female - Female", "Female - Male", "Male - Male"),
                    name = "") +
  xlab("Month") +
  ylab("Encounter count") +
  #ggtitle("C") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text( size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        # legend.text = element_text(size=6, family = "sans", face = "bold"),
        # legend.position = c(0.84, 0.2), #horizontal, vertical
        # legend.key.height = unit(0.3, "cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
figure3c_encounters_overtime
ggsave(figure3c_encounters_overtime, width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent",
       file="figures/individual figures/figure3c_encounters_overtime.png")
ggsave(figure3c_encounters_overtime, width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent",
       file="figures/individual figures/encounters_overtime.png")

#..................................................
# multi-panel v2 ----
#..................................................

library(gridExtra)
figure3_top <- grid.arrange(figure3a_proximity_ratio,
                            figure3b_encounters,
                            ncol = 2)

figure3_v3 <- grid.arrange(figure3_top,
                           figure3c_encounters_overtime,
                           nrow = 2,
                           heights = c(0.35, 0.35))
ggsave(figure3_v3, filename = "figures/figure3_v3.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # OLD FIGURE
# figure3c_encounters_overtime <-
#   ggplot(data = encs,
#          aes(y = encounter, x = doy, col = sex_comparison)) +
#   geom_point(size = 0.7) + 
#   geom_smooth(method="gam", formula = y ~ s(x, bs = "cc", k = 8),
#               method.args =list(family = poisson), se=F) +
#   scale_x_continuous(limits = c(-2,370), expand = c(0,1)) +
#   scale_y_continuous(limits = c(0,110), expand = c(0,1)) +
#   scale_color_manual(values = c("#A50026", "#9970AB", "#004488"),
#                      labels = c("Female - Female", "Female - Male", "Male - Male"),
#                      name = "") +
#   xlab("Day of year") +
#   ylab("Encounter count") +
#   ggtitle("C") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text( size = 14, family = "sans", face = "bold"),
#         axis.title.y = element_text(size=10, family = "sans", face = "bold"),
#         axis.title.x = element_text(size=10, family = "sans", face = "bold"),
#         axis.text.y = element_text(size=8, family = "sans"),
#         axis.text.x  = element_text(size=8, family = "sans"),
#         legend.position="none",
#         # legend.text = element_text(size=6, family = "sans", face = "bold"),
#         # legend.position = c(0.84, 0.2), #horizontal, vertical
#         # legend.key.height = unit(0.3, "cm"),
#         # legend.key=element_blank(),
#         panel.background = element_rect(fill = "transparent"),
#         legend.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
# figure3c_encounters_overtime
# ggsave(figure3c_encounters_overtime, width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent",
#        file="figures/individual figures/figure3c_encounters_overtime.png")


# #..................................................
# # multi-panel ----
# #..................................................
# 
# figure3_top <- grid.arrange(figure3a_proximity_ratio,
#                             figure3b_encounters,
#                             ncol = 2)
# 
# figure3 <- grid.arrange(figure3_top,
#                         figure3c_encounters_overtime,
#                         nrow = 2,
#                         heights = c(0.35, 0.35))
# 
# ggsave(figure3, filename = "figures/figure3.png", device = NULL,
#        path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

## Plot encounter residuals ----
# FIT_ENC <- lm(log10(encounter_count+0.1) ~ log10(overlap_est), data = proximity_pair_df)
# resids <- residuals(FIT_ENC)
# png(file = "figures/encounter_residuals_sex.png", width = 6.86, height = 6, units = "in", res = 600)
# boxplot(resids ~ proximity_pair_df$sex_comparison,
#         xlab = "Encounter Residuals",
#         ylab = "sex")
# dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# #plot encounters overtime

# distance_df$encounter <- ifelse(distance_df$distance_est > 15, 0,1)
# 
# encs<- distance_df[which(distance_df$encounter == 1),]
# encs$toy <- yday(encs$timestamp)
# encs$year <- format(encs$timestamp, "%y")
# test <- aggregate(encounter ~ sex_comparison + toy + year + pair_ID, data = encs, FUN = "sum")
# 
# ggplot(data = test, aes(y = encounter, x = toy, col = sex_comparison)) +
#   
#   geom_point() +
#   geom_smooth(aes(linetype = "gam"), fill = "transparent",
#               method = "gam", formula = y ~ s(x, bs = "cc", k = 8),
#               method.args = list(family = poisson)) +
#   scale_y_log10(breaks = c(0.1,1,10,100,1000,10000),
#                 labels = c(0.1,1,10,100,1000,10000))
# 
# test$sex_comparison <- as.factor(test$sex_comparison)
# fit <- gam(encounter ~ s(toy, bs = "cc", k = 6) + sex_comparison, family = poisson, data = test)  #add random effect on sex -> refer to parks
# summary(fit)


#..................................................
# Encounters ----
#..................................................

#Run estimating encounters prior to plotting
proximity_df$encounter_count <- NA
pair_ID <- unique(proximity_df$pair_ID)
#calculate total encounters of all individuals based on sex comparison type
for (i in pair_ID){
  subset_A <- distance_df[distance_df$pair_ID == i,]
  #count the number of times distance is below 15
  encounter_count <- sum(subset_A$distance_est < 15)
  #save results
  proximity_df[proximity_df$pair_ID == i, "encounter_count"] <- encounter_count
}

#do not include 0 encounters
proximity_df2 <- proximity_df[proximity_df$encounter_count != 0,]

library(scales)

plot_overlap_encounters <-
  ggplot(data = proximity_df2,
         aes(y = overlap_est, x = encounter_count)) +
  geom_point(data = proximity_df2, 
             aes(y = overlap_est, x = encounter_count, col = sex_comparison),
             size = 1.2) + 
  geom_smooth(method="lm", formula = y ~ x, se=F, col = "black") +
  scale_x_log10(expand = c(0,0.1), limits = c(0.1,2000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(expand = c(0,0.02),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = c("#A50026", "#9970AB", "#004488"),
                     labels = c("Female - Female", "Female - Male", "Male - Male"),
                     name = "") +
  ylab("Home-range overlap") +
  xlab("Encounter count") +
  #ggtitle("B") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        # legend.text = element_text(size=6, family = "sans", face = "bold"),
        # legend.position = c(0.84, 0.2), #horizontal, vertical
        # legend.key.height = unit(0.3, "cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) #top, right, bot, left
plot_overlap_encounters
ggsave(plot_overlap_encounters,
       file="figures/individual figures/overlap_encounters.png",
       height = 2.5, width = 3.33, units = "in", dpi = 600, bg = "transparent")
