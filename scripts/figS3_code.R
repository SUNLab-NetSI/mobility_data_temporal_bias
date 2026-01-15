## CODE TO RECREATE FIGURE S3 ##

# read in packages
library(tidyverse)
library(patchwork)
library(cowplot)

# read in csv
atus_md_ratio_gym_figS3 <- read.csv("figS3.csv")

# add column for the time labels on the x-axis
atus_md_ratio_gym_figS3 <- atus_md_ratio_gym_figS3 %>%
  mutate(time_label = as.POSIXct(key * 3600, origin = "1970-01-01", tz = "UTC"))

# set start and end time variables
start_time <- as.POSIXct(6 * 3600, origin = "1970-01-01", tz = "UTC")
end_time <- as.POSIXct(23 * 3600, origin = "1970-01-01", tz = "UTC")


## BOSTON ##
# csv file combines all 11 cities, plotted each separately to ensure best possible formatting
atus_md_ratio_gym_boston_figS3 <- atus_md_ratio_gym_figS3 %>%
  filter(city == "Boston")

si_boston_atusmd_gym_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_gym_boston_figS3, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
  scale_x_datetime(date_labels = "%H:%M",  
                   date_breaks = "4 hours",
                   limits = c(start_time, end_time)) +  
  theme_classic(base_size = 18) +
  theme(title = element_text(size = 16),
        axis.title.y = element_text(vjust = +2.25),
        axis.title.x = element_text(vjust = -0.75), 
        plot.margin = unit(c(0.01,0.3,0.01,0.01), "cm"),
        legend.position = "none") +
  labs(title = "ATUS / MD Ratios: Gym", subtitle = "Boston", x = "", y = "") +
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) +
  scale_y_continuous(breaks = c(0,2,4))


## CHICAGO ##
atus_md_ratio_gym_chicago_figS3 <- atus_md_ratio_gym_figS3 %>%
  filter(city == "Chicago")

si_chicago_atusmd_gym_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_gym_chicago_figS3, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
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
  scale_y_continuous(breaks = c(0,4,8))


## DALLAS ##
atus_md_ratio_gym_dallas_figS3 <- atus_md_ratio_gym_figS3 %>%
  filter(city == "Dallas")

si_dallas_atusmd_gym_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_gym_dallas_figS3, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
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
  scale_y_continuous(breaks = c(0,4,8))


## DETROIT ##
atus_md_ratio_gym_detroit_figS3 <- atus_md_ratio_gym_figS3 %>%
  filter(city == "Detroit")

si_detroit_atusmd_gym_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_gym_detroit_figS3, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
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
  scale_y_continuous(breaks = c(0,6,12))


## LOS ANGELES ##
atus_md_ratio_gym_losangeles_figS3 <- atus_md_ratio_gym_figS3 %>%
  filter(city == "Los Angeles")

si_losangeles_atusmd_gym_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_gym_losangeles_figS3, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
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
  scale_y_continuous(breaks = c(0,4,8))


## MIAMI ##
atus_md_ratio_gym_miami_figS3 <- atus_md_ratio_gym_figS3 %>%
  filter(city == "Miami")

si_miami_atusmd_gym_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_gym_miami_figS3, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
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
  scale_y_continuous(breaks = c(0,3,6))


## NEW YORK ##
atus_md_ratio_gym_newyork_figS3 <- atus_md_ratio_gym_figS3 %>%
  filter(city == "New York")

si_newyork_atusmd_gym_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_gym_newyork_figS3, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
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
  scale_y_continuous(breaks = c(0,3,6))


## PHILADELPHIA ##
atus_md_ratio_gym_philadelphia_figS3 <- atus_md_ratio_gym_figS3 %>%
  filter(city == "Philadelphia")

si_philadelphia_atusmd_gym_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_gym_philadelphia_figS3, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
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
  scale_color_manual("", values = c("Q1" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4" = "#1e6091")) 



## SAN FRANCISCO ##
atus_md_ratio_gym_sanfrancisco_figS3 <- atus_md_ratio_gym_figS3 %>%
  filter(city == "San Francisco")

si_sanfrancisco_atusmd_gym_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_gym_sanfrancisco_figS3, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
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
  scale_y_continuous(breaks = c(0,4,8)) 


## SEATTLE ##
atus_md_ratio_gym_seattle_figS3 <- atus_md_ratio_gym_figS3 %>%
  filter(city == "Seattle")

si_seattle_atusmd_gym_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_gym_seattle_figS3, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
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
  scale_y_continuous(breaks = c(0,5,10)) 


## WASHINGTON ##
atus_md_ratio_gym_washington_figS3 <- atus_md_ratio_gym_figS3 %>%
  filter(city == "Washington")

si_washington_atusmd_gym_ratio <- ggplot() +
  geom_path(data=atus_md_ratio_gym_washington_figS3, linewidth = 0.6, aes(x = time_label, y = atusmd_ratio, color = inc_quart)) +
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
  scale_y_continuous(breaks = c(0,5,10))


## legend ##
atus_md_ratio_gym_figS3_boston_legend <- ggplot(data=atus_md_ratio_gym_boston_figS3, aes(x = time_label, y = atusmd_ratio, color = inc_quart_legend_label, fill = inc_quart_legend_label)) +
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
  scale_color_manual("", values = c("Q1 (Low)" = "#b5e48c", "Q2" = "#76c893", "Q3" = "#34a0a4", "Q4 (High)" = "#1e6091"))


# extract legend 
figS3_legend <- get_plot_component(atus_md_ratio_gym_figS3_boston_legend, 'guide-box-right', return_all = TRUE)


## combine 11 city plots and legend to create Figure S3 ##
p_figS3 <- (si_boston_atusmd_gym_ratio | si_chicago_atusmd_gym_ratio |si_dallas_atusmd_gym_ratio) / 
           (si_detroit_atusmd_gym_ratio | si_losangeles_atusmd_gym_ratio | si_miami_atusmd_gym_ratio) /
           (si_newyork_atusmd_gym_ratio | si_philadelphia_atusmd_gym_ratio | si_sanfrancisco_atusmd_gym_ratio) /
           (si_seattle_atusmd_gym_ratio | si_washington_atusmd_gym_ratio | figS3_legend)

# print Figure S3
p_figS3

# save at this size
ggsave("p_figS3.png", p_figS3, width = 14, height = 10, units = "in")
