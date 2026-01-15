## CODE TO RECREATE FIGURE S5 ##

# read in packages
library(tidyverse)


# read in csv
multi_linear_regression_results_by_city_figS5 <- read.csv("figS5.csv")


# set level order for cities
level_order <- c('Washington', 'Seattle', 'San Francisco', 'Philadelphia', 'New York', 'Miami', 'Los Angeles', 'Detroit', 'Dallas', 'Chicago', 'Boston')



p_figS5 <- ggplot(multi_linear_regression_results_by_city_figS5, aes(x = estimate, y = factor(city, level = level_order))) +
  geom_point(aes(color = significance)) +
  geom_errorbar(aes(xmin = estimate - std.error.x2, xmax = estimate + std.error.x2, width = 0.2, color = significance), show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "lightgrey") +
  scale_color_manual(values = c("darkgrey", "purple")) +
  facet_grid(poi_type ~ factor(term, levels = c('Segregation', '# POIs per CBG', '# Unique visitors', 'Fraction of Q1 visitors', 'Fraction of Q4 visitors'))) +
  theme_bw(base_size = 14) +
  theme(strip.background =element_rect(fill="white"),
        legend.position = "inside",
        legend.position.inside = c(0.95,0.25),
        legend.key.size = unit(0.15, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8))+
  labs(x = "Estimate", y = "City", color = "")


p_figS5

ggsave("p_figS5.png", p_figS5, width = 14, height = 8, units = "in")

