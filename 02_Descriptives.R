
# 08/07/2025 - Discharge Ready Data (DRD) descriptive analysis
# National level descriptives #################################################


# 1 Average % of discharges that are delayed ##################################

# Total delayed column
England_DRD$Total_delayed <- England_DRD$`1 day delay between DoD & DRD (#)`+ 
                                England_DRD$`2-3 day delay between DoD & DRD (#)`+ 
                                England_DRD$`4-6 day delay between DoD & DRD (#)`+
                                England_DRD$`7-13 day delay between DoD & DRD (#)`+
                                England_DRD$`14-20 day delay between DoD & DRD (#)`+
                                England_DRD$`21+ day delay between DoD & DRD (#)` 

# Average bed days with delayed discharge
England_DRD <- England_DRD %>%
  mutate(Avg_DD_bed_days = sum())

# O y-axis scale
delayed_discharges_plot <- ggplot(England_DRD, aes(x = Month, y = `DoD is 1+ days after DRD (%)`, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              linewidth = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Percentage of discharges delayed, England, September 2023 - May 2025.",
       x = "Month",
       y = "DOD is 1+ days after DRD (%)") +
  scale_y_continuous(
    limits = c(0, 0.15),        
    breaks = seq(0, 0.15, 0.01)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

ggsave("delayed discharges.png", plot = delayed_discharges_plot, width = 8, height = 6, dpi = 300)

# 12 y-axis scale 
delayed_discharges_plot_12 <- ggplot(England_DRD, aes(x = Month, y = `DoD is 1+ days after DRD (%)`, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              linewidth = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Percentage of discharges delayed, England, September 2023 - May 2025.",
       x = "Month",
       y = "DOD is 1+ days after DRD (%)") +
  scale_y_continuous(
    limits = c(0.12, 0.15),        
    breaks = seq(0.12, 0.15, 0.01)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

ggsave("delayed discharges 12.png", plot = delayed_discharges_plot_12, width = 8, height = 6, dpi = 300)

# 2 DoD = DRD #################################################################

no_delay <- ggplot(England_DRD, aes (x = Month, y = `DoD is same as DRD (%)`, group = 1)) +
  geom_line(color = 'blue', size = 1.2) +
  geom_smooth(method = 'lm', se = FALSE,
              color = 'black',
              linetype = 'longdash',
              linewidth = 0.3) +
  geom_point(color = "black", size = 1.5) +
  labs (title = 'Percentage of successful discharges, England, September 2023 - May 2025',
  x = 'Month',
  y = 'DRD = DoD') +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.05)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))
  
ggsave("no_delay.png", plot = no_delay, width = 8, height = 6, dpi = 300) 

no_delay_80 <- ggplot(England_DRD, aes (x = Month, y = `DoD is same as DRD (%)`, group = 1)) +
  geom_line(color = 'blue', size = 1.2) +
  geom_smooth(method = 'lm', se = FALSE,
              color = 'black',
              linetype = 'longdash',
              linewidth = 0.3) +
  geom_point(color = "black", size = 1.5) +
  labs (title = 'Percentage of successful discharges, England, September 2023 - May 2025',
        x = 'Month',
        y = 'DRD = DoD') +
  scale_y_continuous(
    limits = c(0.8, 0.9),
    breaks = seq(0.8, 0.9, 0.05)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("no_delay_80.png", plot = no_delay_80, width = 8, height = 6, dpi = 300) 

# 3 Total bed days lost to delayed discharge ##################################

total_bed_days_lost <- ggplot(England_DRD, aes (x = Month, y = `Total bed days lost to DD` , group = 1)) +
  geom_line(color = 'blue', size = 1.2) +
  geom_smooth(method = 'lm', se = FALSE,
              color = 'black',
              linetype = 'longdash',
              linewidth = 0.3) +
  geom_point(color = "black", size = 1.5) +
  labs (title = 'Total bed days lost to delayed discharge, England, September 2023 - May 2025',
        x = 'Month',
        y = '# of bed days') +
  scale_y_continuous(
    limits = c(0, 350000),
    breaks = seq(0, 350000, 50000)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("total_bed_days_lost.png", plot = total_bed_days_lost, width = 8, height = 6, dpi = 300) 

# 4 Average days from DRD to DoD (exc 0-day delays) ###########################
Average_DRD_to_DoD <- ggplot(England_DRD, aes(x = Month, y = `Average days from DRD to DoD (exc 0-day delays)` , group = 1)) +
  geom_line(color = "blue", size = 1.2) +
    geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
      labs(title = "Average length of delay, England, October 2023 - May 2025",
       x = "Month",
       y = "Average days from DRD to DoD (exc 0-day)") +
  scale_y_continuous(
    limits = c(0, 7),
    breaks = seq(0, 7, 0.5)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("AverageDRD_to_DoD.png", plot = Average_DRD_to_DoD, width = 8, height = 6, dpi = 300)

Average_DRD_to_DoD_5 <- ggplot(England_DRD, aes(x = Month, y = `Average days from DRD to DoD (exc 0-day delays)` , group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Average length of delay, England, October 2023 - May 2025",
       x = "Month",
       y = "Average days from DRD to DoD (exc 0-day)") +
  scale_y_continuous(
    limits = c(5, 7),
    breaks = seq(5, 7, 0.2)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("AverageDRD_to_DoD_5.png", plot = Average_DRD_to_DoD_5, width = 8, height = 6, dpi = 300)

# 5 Discharge delays +1 day ###################################################
delay_1_day <- ggplot(England_DRD, aes(x = Month, y = `1 day delay between DoD & DRD (%)` , group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Discharges with 1 day delay between the DRD and DoD, England, April 2024 - May 2025 ",
       x = "Month",
       y = "% of total discharges") +
  scale_y_continuous(
    limits = c(0, 0.05),
    breaks = seq(0, 0.05, 0.01)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("delay_1_day.png", plot = delay_1_day, width = 8, height = 6, dpi = 300)

# 6 Discharge delays 2-3 days #################################################
delay_2_3_day <- ggplot(England_DRD, aes(x = Month, y = `2-3 day delay between DoD & DRD (%)` , group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Discharges with 2-3 day delay between the DRD and DoD, England, April 2024 - May 2025 ",
       x = "Month",
       y = "% of total discharges") +
  scale_y_continuous(
    limits = c(0, 0.05),
    breaks = seq(0, 0.05, 0.01)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("delay_2_3_day.png", plot = delay_2_3_day, width = 8, height = 6, dpi = 300)

# 7 Discharge delays 4-6 days  ################################################
delay_4_6_day <- ggplot(England_DRD, aes(x = Month, y = `4-6 day delay between DoD & DRD (%)` , group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Discharges with 4-6 day delay between the DRD and DoD, England, April 2024 - May 2025 ",
       x = "Month",
       y = "% of total discharges") +
  scale_y_continuous(
    limits = c(0, 0.05),
    breaks = seq(0, 0.05, 0.01)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("delay_4_6_day.png", plot = delay_4_6_day, width = 8, height = 6, dpi = 300)

# 8 Discharge delays 7-13 days  ###############################################
delay_7_13_day <- ggplot(England_DRD, aes(x = Month, y = `7-13 day delay between DoD & DRD (%)` , group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Discharges with 7-13 day delay between the DRD and DoD, England, April 2024 - May 2025 ",
       x = "Month",
       y = "% of total discharges") +
  scale_y_continuous(
    limits = c(0, 0.05),
    breaks = seq(0, 0.05, 0.01)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("delay_7_13_day.png", plot = delay_7_13_day, width = 8, height = 6, dpi = 300)

# 9 Discharge delays 14-20 days  ##############################################
delay_14_20_day <- ggplot(England_DRD, aes(x = Month, y = `14-20 day delay between DoD & DRD (%)` , group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Discharges with 14-20 day delay between the DRD and DoD, England, April 2024 - May 2025 ",
       x = "Month",
       y = "% of total discharges") +
  scale_y_continuous(
    limits = c(0, 0.05),
    breaks = seq(0, 0.05, 0.01)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("delay_14_20_day.png", plot = delay_14_20_day, width = 8, height = 6, dpi = 300)

# 10 Discharge delays 21+ days  ###############################################
delay_21_day <- ggplot(England_DRD, aes(x = Month, y = `21+ day delay between DoD & DRD (%)` , group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Discharges with 21+ day delay between the DRD and DoD, England, April 2024 - May 2025 ",
       x = "Month",
       y = "% of total discharges") +
  scale_y_continuous(
    limits = c(0, 0.05),
    breaks = seq(0, 0.05, 0.01)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("delay_21+_day.png", plot = delay_21_day, width = 8, height = 6, dpi = 300)

# 11 Patients DD but within 1 day  ############################
DD_1_day <- ggplot(England_DRD, aes(x = Month, y = `Patients delayed but discharged within 1 day (%)`, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Delayed patients discharged within 1 day, England, September 2023 - May 2025 ",
       x = "Month",
       y = "% of delayed patients") +
  scale_y_continuous(
    limits = c(0, 0.35),
    breaks = seq(0, 0.35, 0.05)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("DD_1_day.png", plot = DD_1_day, width = 8, height = 6, dpi = 300)

# 12 Patients DD but within 2-3 days  #########################
DD_2_3_day <- ggplot(England_DRD, aes(x = Month, y = `Patients delayed but discharged within 2-3 days (%)`, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Delayed patients discharged within 2-3 days, England, September 2023 - May 2025 ",
       x = "Month",
       y = "% of delayed patients") +
  scale_y_continuous(
    limits = c(0, 0.35),
    breaks = seq(0, 0.35, 0.05)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("DD_2_3_day.png", plot = DD_2_3_day, width = 8, height = 6, dpi = 300)

# 13 Patients DD but within 4-6 days  #########################
DD_4_6_day <- ggplot(England_DRD, aes(x = Month, y = `Patients delayed but discharged within 4-6 days (%)`, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Delayed patients discharged within 4-6 days, England, September 2023 - May 2025 ",
       x = "Month",
       y = "% of delayed patients") +
  scale_y_continuous(
    limits = c(0, 0.35),
    breaks = seq(0, 0.35, 0.05)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("DD_4_6_day.png", plot = DD_4_6_day, width = 8, height = 6, dpi = 300)

# 14 Patients DD but within 7-13 days  ########################
DD_7_13_day <- ggplot(England_DRD, aes(x = Month, y = `Patients delayed but discharged within 7-13 days (%)`, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Delayed patients discharged within 7-13 days, England, September 2023 - May 2025 ",
       x = "Month",
       y = "% of delayed patients") +
  scale_y_continuous(
    limits = c(0, 0.35),
    breaks = seq(0, 0.35, 0.05)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("DD_7_13_day.png", plot = DD_7_13_day, width = 8, height = 6, dpi = 300)

# 15 Patients DD but within 14-20 days  #######################
DD_14_20_day <- ggplot(England_DRD, aes(x = Month, y = `Patients delayed but discharged within 14-20 days (%)`, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Delayed patients discharged within 14-20 days, England, September 2023 - May 2025 ",
       x = "Month",
       y = "% of delayed patients") +
  scale_y_continuous(
    limits = c(0, 0.35),
    breaks = seq(0, 0.35, 0.05)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("DD_14_20_day.png", plot = DD_14_20_day, width = 8, height = 6, dpi = 300)

# 16 Patients DD but within 21+ days  #########################
DD_21_day <- ggplot(England_DRD, aes(x = Month, y = `Patients delayed but discharged within 21 days or more (%)`, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Delayed patients discharged within 21 days or more, England, September 2023 - May 2025 ",
       x = "Month",
       y = "% of delayed patients") +
  scale_y_continuous(
    limits = c(0, 0.35),
    breaks = seq(0, 0.35, 0.05)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

ggsave("DD_21+_day.png", plot = DD_21_day, width = 8, height = 6, dpi = 300)

















