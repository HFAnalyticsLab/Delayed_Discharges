
# 28/08/2025 - Discharge Ready Data (DRD) Trust-level descriptive analysis

# Drivers of DD improvement ###################################################

best_trusts <- c(greater_than_2.5p_reduction$org_code)

# 1 Bed occupancy #############################################################

# Pull out best performing trusts 

figure_3.1_data <- hospital_beds %>%
  filter(org_code %in% best_trusts) %>%
  mutate(month = my(month))
  group_by(org_code)
  
bed_occupancy_plot <- ggplot(figure_3.1_data, aes(x = month, y = occupancy_rate, group = org_code, color = org_code)) +
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


































