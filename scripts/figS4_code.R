## CODE TO RECREATE FIGURE S4 ##

# read in packages
library(tidyverse)


# read in csv
num_ave_unique_visitors_by_hour_figS4 <- read.csv("figS4.csv")


# add column for the time labels on the x-axis
num_ave_unique_visitors_by_hour_figS4 <- num_ave_unique_visitors_by_hour_figS4 %>%
  mutate(time_label = as.POSIXct(hour * 3600, origin = "1970-01-01", tz = "UTC"))

# set start and end time variables
start_time <- as.POSIXct(6 * 3600, origin = "1970-01-01", tz = "UTC")
end_time <- as.POSIXct(23 * 3600, origin = "1970-01-01", tz = "UTC")



p_figS4 <- ggplot(num_ave_unique_visitors_by_hour_figS4, aes(x = time_label, y = ave_num_uv_hour)) +
  geom_point(color = "purple", size = 2) +
  scale_x_datetime(date_labels = "%H:%M",
                   date_breaks = "3 hours",
                   limits = c(start_time, end_time)) +
  labs(x = "Hour of Day", y="Sample Size", color = "") +
  facet_wrap(.~poi_type, scales = "free_y") +
  theme_bw(base_size = 16) +
  theme(strip.background =element_rect(fill="white"),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        axis.title.y = element_text(color = "black", vjust = +2.25),
        axis.title.x = element_text(color = "black", vjust = -0.75),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black",),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.92,0.87))


p_figS4

ggsave("p_figS4.png", p_figS4, width = 14, height = 5, units = "in")
