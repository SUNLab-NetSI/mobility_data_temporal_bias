## CODE TO RECREATE FIGURE 5 ##

# read in packages
library(tidyverse)


# read in csv
ave_orig_and_reweighted_seg_by_hour_fig5 <- read.csv("fig5.csv")


# add column for the time labels on the x-axis
ave_orig_and_reweighted_seg_by_hour_fig5 <- ave_orig_and_reweighted_seg_by_hour_fig5 %>%
  mutate(time_label = as.POSIXct(hour * 3600, origin = "1970-01-01", tz = "UTC"))


# set modified start and end time variables
start_time_6 <- as.POSIXct(6 * 3600, origin = "1970-01-01", tz = "UTC")
end_time <- as.POSIXct(23 * 3600, origin = "1970-01-01", tz = "UTC")


# plot Figure 5
p_fig5 <- ggplot(ave_orig_and_reweighted_seg_by_hour_fig5, aes(x = time_label, y = seg, group = seg_label)) + 
  geom_point(aes(fill = seg_label, color = seg_label), shape = 21) +
  geom_errorbar(aes(ymin = seg - std_error_x2, ymax = seg + std_error_x2, width = 1, color = seg_label), show.legend = FALSE) +
  scale_fill_manual(values = c("#D41159", "#1A85ff"),  labels = c('Original', 'Weighted'), name = "Legend") +
  scale_color_manual(values = c("#D41159", "#1A85ff"), labels = c('Original', 'Weighted'), name = "Legend") +
  scale_x_datetime(date_labels = "%H:%M",
                   date_breaks = "3 hours",
                   limits = c(start_time_6, end_time)) +
  labs(x = "Hour of Day", y="Segregation", color = "") +
  facet_grid(.~poi_type) +
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
        legend.position.inside = c(0.93,0.12))


# print Figure 5
p_fig5

# save Figure 5 at this size
ggsave("p_fig5.png", p_fig5, width = 12, height = 5, units = "in")
