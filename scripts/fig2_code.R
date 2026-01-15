## CODE TO RECREATE FIGURE 2 ##

# read in packages
library(tidyverse)
library(patchwork)
library(cowplot)


# read in ATUS csv
atus_fig2 <- read.csv("fig2_atus.csv")

# add column for the time labels on the x-axis
atus_fig2 <- atus_fig2 %>%
  mutate(time_label = as.POSIXct(key * 3600, origin = "1970-01-01", tz = "UTC"))

# set start and end time variables
start_time <- as.POSIXct(6 * 3600, origin = "1970-01-01", tz = "UTC")
end_time <- as.POSIXct(23 * 3600, origin = "1970-01-01", tz = "UTC")


## ATUS FOOD & COFFEE ##
# csv file combines all 3 poi types, plotted each separately to ensure best possible formatting
atus_fc_fig2 <- atus_fig2 %>%
  filter(poi_type == "Food & Coffee")

p_atus_fc_fig2 <- ggplot() +
  geom_path(data = atus_fc_fig2, linewidth = 1, aes(x = time_label, y = atus_time_spent, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +
  theme_classic(base_size = 22) +
  theme(legend.position = "none",
        axis.title.y = element_text(vjust = 2.5),
        plot.margin = unit(c(0.01,0.01,2,0.3), "cm"),
        axis.title.x = element_blank()) +
  labs(x = "", y = "Min. per Hour", colour = 'Income', title = "American Time Use Survey", subtitle = "(All 11 cities)") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(breaks = c(0,2,4))


## ATUS GROCERY ##
# csv file combines all 3 poi types, plotted each separately to ensure best possible formatting
atus_grocery_fig2 <- atus_fig2 %>%
  filter(poi_type == "Grocery")

p_atus_grocery_fig2 <- ggplot() +
  geom_path(data = atus_grocery_fig2, linewidth = 1, aes(x = time_label, y = atus_time_spent, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +
  theme_classic(base_size = 22) +
  theme(legend.position = "none",
        axis.title.y = element_text(vjust = 2.5),
        plot.margin = unit(c(0.01,0.01,2,0.3), "cm"),
        axis.title.x = element_blank()) +
  labs(x = "", y = "Min. per Hour", colour = 'Income') +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(breaks = c(0,0.5,1))


## ATUS GYM ##
# csv file combines all 3 poi types, plotted each separately to ensure best possible formatting
atus_gym_fig2 <- atus_fig2 %>%
  filter(poi_type == "Gym")

p_atus_gym_fig2 <- ggplot() +
  geom_path(data = atus_gym_fig2, linewidth = 1, aes(x = time_label, y = atus_time_spent, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +
  theme_classic(base_size = 22) +
  theme(legend.position = "none",
        axis.title.y = element_text(vjust = 2.5),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = unit(c(0.01,0.01,0.3,0.3), "cm"))+
  labs(x = "Hour of Day", y = "Min. per Hour", colour = 'Income') +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091"))



# read in MD csv
md_fig2 <- read.csv("fig2_md.csv")

# add column for the time labels on the x-axis
md_fig2 <- md_fig2 %>%
  mutate(time_label = as.POSIXct(key * 3600, origin = "1970-01-01", tz = "UTC"))

# set start and end time variables
start_time <- as.POSIXct(6 * 3600, origin = "1970-01-01", tz = "UTC")
end_time <- as.POSIXct(23 * 3600, origin = "1970-01-01", tz = "UTC")


## MD FOOD & COFFEE ##
# csv file combines all 3 poi types, plotted each separately to ensure best possible formatting
md_fc_fig2 <- md_fig2 %>%
  filter(poi_type == "Food & Coffee")

p_md_fc_fig2 <- ggplot() +
  geom_path(data = md_fc_fig2, linewidth = 1, aes(x = time_label, y = md_time_spent, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +
  theme_classic(base_size = 22) +
  theme(legend.position = "none",
        plot.margin = unit(c(0.01,0.01,2,1), "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "", colour = 'Income', title = "Mobility Data", subtitle = "(Boston)") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(breaks = c(0,1.5,3))


## MD GROCERY ##
# csv file combines all 3 poi types, plotted each separately to ensure best possible formatting
md_grocery_fig2 <- md_fig2 %>%
  filter(poi_type == "Grocery")

p_md_grocery_fig2 <- ggplot() +
  geom_path(data = md_grocery_fig2, linewidth = 1, aes(x = time_label, y = md_time_spent, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +
  theme_classic(base_size = 22) +
  theme(legend.position = "none",
        plot.margin = unit(c(0.01,0.01,2,1), "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "", colour = 'Income') +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(breaks = c(0,0.15,0.3))


## MD GYM ##
# csv file combines all 3 poi types, plotted each separately to ensure best possible formatting
md_gym_fig2 <- md_fig2 %>%
  filter(poi_type == "Gym")

p_md_gym_fig2 <- ggplot() +
  geom_path(data = md_gym_fig2, linewidth = 1, aes(x = time_label, y = md_time_spent, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +
  theme_classic(base_size = 22) +
  theme(axis.title.x = element_text(vjust = -0.75),
        legend.position = "none",
        plot.margin = unit(c(0.01,0.01,0.3,1), "cm"),
        axis.title.y = element_blank()) +
  labs(x = "Hour of Day", y = "", colour = 'Income') +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(breaks = c(0,0.15,0.3))


# read in RATIO csv
ratio_fig2 <- read.csv("fig2_ratio.csv")

# add column for the time labels on the x-axis
ratio_fig2 <- ratio_fig2 %>%
  mutate(time_label = as.POSIXct(key * 3600, origin = "1970-01-01", tz = "UTC"))

# set start and end time variables
start_time_ratio <- as.POSIXct(5 * 3600, origin = "1970-01-01", tz = "UTC")
end_time_ratio <- as.POSIXct(24 * 3600, origin = "1970-01-01", tz = "UTC")


## RATIO FOOD & COFFEE ##
# csv file combines all 3 poi types, plotted each separately to ensure best possible formatting
ratio_fc_fig2 <- ratio_fig2 %>%
  filter(poi_type == "Food & Coffee")

p_ratio_fc_fig2 <- ggplot() +
  geom_path(data=ratio_fc_fig2, linewidth = 1, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  geom_hline(yintercept = mean(ratio_fc_fig2$atusmd_ratio), linetype = "dashed", color = "red", linewidth = 1) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 22) +
  theme(plot.margin = unit(c(0.01,0.01,2,1), "cm"),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "ATUS / MD Ratio", subtitle = "(Boston)") +
  scale_fill_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(breaks = c(0,0.5,1))


## RATIO GROCERY ##
# csv file combines all 3 poi types, plotted each separately to ensure best possible formatting
ratio_grocery_fig2 <- ratio_fig2 %>%
  filter(poi_type == "Grocery")

p_ratio_grocery_fig2 <- ggplot() +
  geom_path(data=ratio_grocery_fig2, linewidth = 1, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  geom_hline(yintercept = mean(ratio_grocery_fig2$atusmd_ratio), linetype = "dashed", color = "red", linewidth = 1) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 22) +
  theme(axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = unit(c(0,0.3,0,0.01), "cm"),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  labs(x = "", y = "") +
  scale_fill_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(breaks = c(0,2,4))


## RATIO GYM ##
# csv file combines all 3 poi types, plotted each separately to ensure best possible formatting
ratio_gym_fig2 <- ratio_fig2 %>%
  filter(poi_type == "Gym")

p_ratio_gym_fig2 <- ggplot() +
  geom_path(data=ratio_gym_fig2, linewidth = 1, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  geom_hline(yintercept = mean(ratio_gym_fig2$atusmd_ratio), linetype = "dashed", color = "red", linewidth = 1) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 22) +
  theme(axis.title.x = element_text(vjust = -0.75),
        plot.margin = unit(c(0.01,0.01,0.3,1), "cm"),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Hour of Day", y = "") +
  scale_fill_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(breaks = c(0,2,4))



# this provides main parts of figures
# additional labels, legend placement and final formatting were completed in PowerPoint
p_fig2_patchwork <- (p_atus_fc_fig2 | p_md_fc_fig2 | p_ratio_fc_fig2) / 
                    (p_atus_grocery_fig2 | p_md_grocery_fig2 | p_ratio_grocery_fig2) /
                    (p_atus_gym_fig2 | p_md_gym_fig2 | p_ratio_gym_fig2)


# print Figure 2
p_fig2_patchwork

# save Figure 2 at this size
ggsave("p_fig2_patchwork.png", p_fig2_patchwork, width = 21, height = 10, units = "in")




## extract legend from ratio food & coffee plot and save separately 
p_ratio_fc_fig2_legend <- ggplot(data=ratio_fc_fig2, aes(x = time_label, y = atusmd_ratio, color = inc_quart_label, fill = inc_quart_label)) +
  geom_bar(stat="identity") +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time_ratio, end_time_ratio)) +  
  geom_hline(yintercept = mean(ratio_fc_fig2$atusmd_ratio), linetype = "dashed", color = "red", linewidth = 1) +
  facet_wrap(~inc_quart, ncol = 2, nrow = 2) +
  theme_classic(base_size = 22) +
  theme(axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  labs(x = "", y = "", title = "American Time Use Survey / Mobility Data Ratio", subtitle = "(Boston)") +
  scale_fill_manual("", values = c("Q1 (Low)" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4 (High)" = "#1e6091")) +
  scale_color_manual("", values = c("Q1 (Low)" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4 (High)" = "#1e6091")) +
  scale_y_continuous(limits = c(0,1.5), breaks = c(0,1.5))

# extract legend
fig2_legend <- get_plot_component(p_ratio_fc_fig2_legend, 'guide-box-bottom', return_all = TRUE)

# save as a separate figure
ggsave("fig2_legend.png", fig2_legend, width = 6, height = 1, units = "in")
