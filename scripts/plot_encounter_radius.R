

# Sensitivity analysis figures

#encounter radius vs encounter counts

plot_enc_radius_count <- 
  ggplot() +
  geom_line(aes(x = enc_radius, y = enc_count)) +
  labs(x = "Encounter radius (m)", 
       y = "Encounter count (x1000)") +
  scale_y_continuous(labels = function(enc_count) enc_count / 1000) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text( size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggsave(plot_enc_radius_count, 
       file = "figures/individual figures/enc_radius_count.png", 
       width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")

#encounter radius vs pvalue

plot_enc_radius_pvalue <-
  ggplot() +
  geom_line(data = encounter_radius_df, 
            aes(x = x, y = y),
            size = 0.15) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  xlab("Encounter radius (m)") +
  ylab("p-value") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x = element_text(size=8, family = "sans"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.1,0.1,0.05,0.2), "cm")) #top, right, bot, left)

ggsave(plot_enc_radius_pvalue, 
       file = "figures/individual figures/enc_radius_pvalue.png", 
       width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")