## CODE TO RECREATE FIGURE 3 ##

# read in packages
library(tidyverse)
library(patchwork)


# read in csv
orig_and_reweighted_time_spent_2_example_food_POIs_Boston_fig3 <- read.csv("fig3.csv")


# PANEL A 
orig_and_reweighted_time_spent_2_example_food_POIs_Boston_fig3_panel_A <- orig_and_reweighted_time_spent_2_example_food_POIs_Boston_fig3 %>%
  filter(bar_label == "A orig min" | bar_label == "A wt min")

# plot Figure 3 panel A
p_fig3_A <- ggplot(data = orig_and_reweighted_time_spent_2_example_food_POIs_Boston_fig3_panel_A, aes(x = bar, y = fct_rev(inc_quart_label), color = inc_quart_label, fill = inc_quart_label)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~bar_label) +
  theme_classic(base_size = 22) +
  theme(axis.title.x = element_text(vjust = -0.75),
        plot.margin = unit(c(0.01,0.01,0.3,1), "cm"),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "") +
  scale_fill_manual("", values = c("Q1 (Low)" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4 (High)" = "#1e6091")) +
  scale_color_manual("", values = c("Q1 (Low)" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4 (High)" = "#1e6091")) +
  scale_x_continuous(limits = c(0,38), breaks = c(0, 10, 20, 30))



# PANEL B 
orig_and_reweighted_time_spent_2_example_food_POIs_Boston_fig3_panel_B <- orig_and_reweighted_time_spent_2_example_food_POIs_Boston_fig3 %>%
  filter(bar_label == "B orig min" | bar_label == "B wt min")

# plot Figure 3 panel B
p_fig3_B <- ggplot(data = orig_and_reweighted_time_spent_2_example_food_POIs_Boston_fig3_panel_B, aes(x = bar, y = fct_rev(inc_quart_label), color = inc_quart_label, fill = inc_quart_label)) +
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
  scale_color_manual("", values = c("Q1 (Low)" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4 (High)" = "#1e6091")) +
  scale_x_continuous(limits = c(0,440), breaks = c(0, 100, 200, 300, 400))



# bind panels A and B together
p_fig3 <- (p_fig3_A) /
  (p_fig3_B)


# print Figure 3
p_fig3

# save Figure 3, at this size
ggsave("p_fig3.png", p_fig3, width = 10, height = 5, units = "in")
