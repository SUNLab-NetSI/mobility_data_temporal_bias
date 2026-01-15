## CODE TO RECREATE FIGURE S1 ##

# read in packages
library(tidyverse)
library(patchwork)
library(cowplot)

# read in csv
atus_md_ratio_fc_figS1 <- read.csv("figS1.csv")

# add column for the time labels on the x-axis
atus_md_ratio_fc_figS1 <- atus_md_ratio_fc_figS1 %>%
  mutate(time_label = as.POSIXct(key * 3600, origin = "1970-01-01", tz = "UTC"))

# set start and end time variables
start_time <- as.POSIXct(6 * 3600, origin = "1970-01-01", tz = "UTC")
end_time <- as.POSIXct(23 * 3600, origin = "1970-01-01", tz = "UTC")


## BOSTON ##
# csv file combines all 11 cities, plotted each separately to ensure best possible formatting
atus_md_ratio_fc_boston_figS1 <- atus_md_ratio_fc_figS1 %>%
  filter(city == "Boston")

si_boston_atusmd_fc_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_fc_boston_figS1, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(title = "ATUS / MD Ratios: Food", subtitle = "Boston", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2))


## CHICAGO ##
atus_md_ratio_fc_chicago_figS1 <- atus_md_ratio_fc_figS1 %>%
  filter(city == "Chicago")

si_chicago_atusmd_fc_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_fc_chicago_figS1, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(subtitle = "Chicago", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(breaks = c(0,1,2))


## DALLAS ##
atus_md_ratio_fc_dallas_figS1 <- atus_md_ratio_fc_figS1 %>%
  filter(city == "Dallas")

si_dallas_atusmd_fc_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_fc_dallas_figS1, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(subtitle = "Dallas", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2))


## DETROIT ##
atus_md_ratio_fc_detroit_figS1 <- atus_md_ratio_fc_figS1 %>%
  filter(city == "Detroit")

si_detroit_atusmd_fc_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_fc_detroit_figS1, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(subtitle = "Detroit", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2))


## LOS ANGELES ##
atus_md_ratio_fc_losangeles_figS1 <- atus_md_ratio_fc_figS1 %>%
  filter(city == "Los Angeles")

si_losangeles_atusmd_fc_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_fc_losangeles_figS1, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(subtitle = "Los Angeles", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2))


## MIAMI ##
atus_md_ratio_fc_miami_figS1 <- atus_md_ratio_fc_figS1 %>%
  filter(city == "Miami")

si_miami_atusmd_fc_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_fc_miami_figS1, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(subtitle = "Miami", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2))


## NEW YORK ##
atus_md_ratio_fc_newyork_figS1 <- atus_md_ratio_fc_figS1 %>%
  filter(city == "New York")

si_newyork_atusmd_fc_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_fc_newyork_figS1, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(subtitle = "New York", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2))


## PHILADELPHIA ##
atus_md_ratio_fc_philadelphia_figS1 <- atus_md_ratio_fc_figS1 %>%
  filter(city == "Philadelphia")

si_philadelphia_atusmd_fc_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_fc_philadelphia_figS1, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) + 
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(subtitle = "Philadelphia", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2))


## SAN FRANCISCO ##
atus_md_ratio_fc_sanfrancisco_figS1 <- atus_md_ratio_fc_figS1 %>%
  filter(city == "San Francisco")

si_sanfrancisco_atusmd_fc_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_fc_sanfrancisco_figS1, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(subtitle = "San Francisco", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2))


## SEATTLE ##
atus_md_ratio_fc_seattle_figS1 <- atus_md_ratio_fc_figS1 %>%
  filter(city == "Seattle")

si_seattle_atusmd_fc_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_fc_seattle_figS1, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(subtitle = "Seattle", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091"))  +
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2))


## WASHINGTON ##
atus_md_ratio_fc_washington_figS1 <- atus_md_ratio_fc_figS1 %>%
  filter(city == "Washington")

si_washington_atusmd_fc_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_fc_washington_figS1, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(subtitle = "Washington", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2))


## legend ##
atus_md_ratio_fc_figS1_boston_legend <- ggplot(data=atus_md_ratio_fc_boston_figS1, aes(x = time_label, y = atusmd_ratio, color = inc_quart_legend_label, fill = inc_quart_legend_label)) +
  geom_bar(stat="identity") +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  facet_wrap(~inc_quart, ncol = 2, nrow = 2) +
  theme_classic(base_size = 22) +
  theme(axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"),
        legend.position = "right",
        legend.key.spacing.y = unit(0.25, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  labs(x = "", y = "", title = "American Time Use Survey / Mobility Data Ratio", subtitle = "(Boston)") +
  scale_fill_manual("", values = c("Q1 (Low)" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4 (High)" = "#1e6091")) +
  scale_color_manual("", values = c("Q1 (Low)" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4 (High)" = "#1e6091")) +
  scale_y_continuous(limits = c(0,1.5), breaks = c(0,1.5))

# extract legend 
figS1_legend <- get_plot_component(atus_md_ratio_fc_figS1_boston_legend, 'guide-box-right', return_all = TRUE)


## combine 11 city plots and legend to create Figure S1 ##
p_figS1 <- (si_boston_atusmd_fc_ratio | si_chicago_atusmd_fc_ratio |si_dallas_atusmd_fc_ratio) / 
           (si_detroit_atusmd_fc_ratio | si_losangeles_atusmd_fc_ratio | si_miami_atusmd_fc_ratio) /
           (si_newyork_atusmd_fc_ratio | si_philadelphia_atusmd_fc_ratio | si_sanfrancisco_atusmd_fc_ratio) /
           (si_seattle_atusmd_fc_ratio | si_washington_atusmd_fc_ratio | figS1_legend)

# print Figure S1
p_figS1

# save at this size
ggsave("p_figS1.png", p_figS1, width = 14, height = 10, units = "in")
