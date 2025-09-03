
# 28/08/2025 - Discharge Ready Data (DRD) Trust-level descriptive analysis

# Drivers of DD improvement ###################################################

# 1 Bed occupancy #############################################################

bed_occupancy_plot <- ggplot(figure_4_data, aes(x = month, y = occupancy_rate, group = org_code, color = org_code)) +
  geom_line(linewidth = 1.2) +
  geom_point(color = "black", size = 1.0) +
  labs(title = "Rates of bed occupancy amongst best performing trusts April 2024 - June 2025.",
       x = "Month",
       y = "Percentage of beds occupied") +
  scale_y_continuous(
    limits = c(0.75, 1),        
    breaks = seq(0.75, 1, 0.05),
    labels = label_percent(scale = 100)) +
  scale_x_date(
    date_labels = "%b-%y",
    date_breaks = "1 month") +
  theme_gray() +
  theme(axis.text.x = element_text(size = 6.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

bed_occupancy_plot
ggsave("bed_occupancy_plot.png", plot = bed_occupancy_plot, width = 8, height = 6, dpi = 300)


# 2 Delay length by trust

average_trust_delay_plot <- ggplot(figure_7_data, aes(x = month, y = average_delay_los_minus_0_day_delay, group = org_code, color = org_code)) +
  geom_line(linewidth = 1.2) +
  geom_point(color = "black", size = 1.0) +
  stat_summary(
    aes(group = 1),       
    fun = mean, 
    geom = "line", 
    colour = "black", 
    linewidth = 1.5, 
    linetype = "22") +
  labs(title = "Average length of delay amongst best performing trusts April 2024 - June 2025.",
       x = "Month",
       y = "Average days from DRD to DoD (exc 0-day)") +
  scale_y_continuous(
    limits = c(0, 15),        
    breaks = seq(0, 15, 1)) +
  scale_x_date(
    date_labels = "%b-%y",
    date_breaks = "1 month") +
  theme_gray() +
  theme(axis.text.x = element_text(size = 6.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

average_trust_delay_plot
ggsave("average_trust_delay_plot.png", plot = average_trust_delay_plot, width = 8, height = 6, dpi = 300)
































