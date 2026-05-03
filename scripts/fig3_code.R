## CODE TO RECREATE FIGURE 3 ##

# read in packages
library(tidyverse)


# read in csv
orig_and_reweighted_time_spent_2_example_food_POIs_Boston_fig3 <- read.csv("fig3.csv")


# plot Figure 3
p_fig3 <- ggplot(data = orig_and_reweighted_time_spent_2_example_food_POIs_Boston_fig3, aes(x = bar, y = fct_rev(inc_quart_label), color = inc_quart_label, fill = inc_quart_label)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~bar_label, scales = "free_x") +
  theme_classic(base_size = 22) +
  theme(axis.title.x = element_text(vjust = -0.75),
        plot.margin = unit(c(0.01,0.01,0.3,1), "cm"),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "") +
  scale_fill_manual("", values = c("Q1 (Low)" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4 (High)" = "#1e6091")) +
  scale_color_manual("", values = c("Q1 (Low)" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4 (High)" = "#1e6091")) 



# print Figure 3
p_fig3

# save Figure 3 at this size
ggsave("p_fig3.png", p_fig3, width = 1, height = 5, units = "in")


