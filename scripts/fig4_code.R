## CODE TO RECREATE FIGURE 4 ##

# read in packages
library(tidyverse)


# read in csv
ave_orig_and_reweighted_seg_by_city_fig4 <- read.csv("fig4.csv")


# plot Figure 4
p_fig4 <- ggplot(ave_orig_and_reweighted_seg_by_city_fig4, aes(x = seg, y = fct_rev(city), group = seg_label)) + 
  geom_point(aes(color = seg_label), size = 2.5) +
  geom_errorbar(aes(xmin = seg - std_error_x2, xmax = seg + std_error_x2, width = 0.2, color = seg_label), show.legend = FALSE) +
  scale_color_manual(values = c("#D41159", "#1A85ff")) +
  labs(x = "Average Income Segregation", y="City", color = "") +
  facet_grid(.~poi_type, scales ="free_x") +
  theme_bw(base_size = 16) +
  theme(strip.background =element_rect(fill="white"),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1, 
        axis.title.y = element_text(color = "black", vjust = +2.25),
        axis.title.x = element_text(color = "black", vjust = -0.75),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black",),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = "inside",
        legend.position.inside = c(0.93,0.11)) +
  geom_text(aes(label = sig_label_fc), nudge_y = 0.15, nudge_x = -0.002, size.unit = "pt", size = 14) +
  geom_text(aes(label = sig_label_grocery), nudge_y = 0.15, nudge_x = 0.005, size.unit = "pt", size = 14) +
  geom_text(aes(label = sig_label_gym), nudge_y = 0.15, nudge_x = 0.012, size.unit = "pt", size = 14)


# print Figure 4
p_fig4

# save Figure 4 at this size
ggsave("p_fig4.png", p_fig4, width = 14, height = 5, units = "in")
