
# 08/07/2025 - Discharge Ready Data (DRD) descriptive analysis
# National level descriptives #################################################

dd_file_national$month <- factor(dd_file_national$month, 
                                 levels = unique(dd_file_national$month))

# 1 % of discharges that are delayed ##################################

# Total delayed column
England_FULL$Total_delayed <- England_FULL$`1 day delay between DoD & DRD (#)`+ 
  England_FULL$`2-3 day delay between DoD & DRD (#)`+ 
  England_FULL$`4-6 day delay between DoD & DRD (#)`+
  England_FULL$`7-13 day delay between DoD & DRD (#)`+
  England_FULL$`14-20 day delay between DoD & DRD (#)`+
  England_FULL$`21+ day delay between DoD & DRD (#)` 

# Average bed days with delayed discharge
England_FULL <- England_FULL %>%
  mutate(Avg_DD_bed_days = sum())

# % of discharges that are delayed
delayed_discharges_plot <- ggplot(dd_file_national, aes(x = month, y = as.numeric(delay_perc), group = 1)) +
  geom_line(color = "firebrick1", linewidth = 1.2) +
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
  theme(axis.text.x = element_text(size = 6.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

delayed_discharges_plot
ggsave("delayed discharges.png", plot = delayed_discharges_plot, width = 8, height = 6, dpi = 300)

# 2 Total discharges
dd_file_national$patients_discharged_volume[dd_file_national$patients_discharged_volume == 0] <- NA

total_discharges_plot <- ggplot(dd_file_national, aes(x = month, y = as.numeric(`patients_discharged_volume`), group = 1)) +
  geom_line(color = "firebrick1", linewidth = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              linewidth = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Total number of patients discharged, England, April 2024 - May 2025 ",
       x = "Month",
       y = "Number of patients") +
  scale_y_continuous(
    limits = c(0, 400000),
    breaks = seq(0, 400000, 50000)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 6.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12))) +
  theme(axis.title.y = element_text(margin = margin(r = 12)))

total_discharges_plot
ggsave("total discharges.png", plot = total_discharges_plot, width = 8, height = 6, dpi = 300)

# 2 DoD = DRD #################################################################

no_delay <- ggplot(England_FULL, aes (x = Month, y = `DoD is same as DRD (%)`, group = 1)) +
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

no_delay_80 <- ggplot(England_FULL, aes (x = Month, y = `DoD is same as DRD (%)`, group = 1)) +
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

total_bed_days_lost <- ggplot(England_FULL, aes (x = Month, y = `Total bed days lost to DD` , group = 1)) +
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
Average_DRD_to_DoD <- ggplot(England_FULL, aes(x = Month, y = `Average days from DRD to DoD (exc 0-day delays)` , group = 1)) +
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

Average_DRD_to_DoD_5 <- ggplot(England_FULL, aes(x = Month, y = `Average days from DRD to DoD (exc 0-day delays)` , group = 1)) +
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
delay_1_day <- ggplot(England_FULL, aes(x = Month, y = `1 day delay between DoD & DRD (%)` , group = 1)) +
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
delay_2_3_day <- ggplot(England_FULL, aes(x = Month, y = `2-3 day delay between DoD & DRD (%)` , group = 1)) +
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
delay_4_6_day <- ggplot(England_FULL, aes(x = Month, y = `4-6 day delay between DoD & DRD (%)` , group = 1)) +
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
delay_7_13_day <- ggplot(England_FULL, aes(x = Month, y = `7-13 day delay between DoD & DRD (%)` , group = 1)) +
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
delay_14_20_day <- ggplot(England_FULL, aes(x = Month, y = `14-20 day delay between DoD & DRD (%)` , group = 1)) +
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
delay_21_day <- ggplot(England_FULL, aes(x = Month, y = `21+ day delay between DoD & DRD (%)` , group = 1)) +
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
DD_1_day <- ggplot(England_FULL, aes(x = Month, y = `Patients delayed but discharged within 1 day (%)`, group = 1)) +
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
DD_2_3_day <- ggplot(England_FULL, aes(x = Month, y = `Patients delayed but discharged within 2-3 days (%)`, group = 1)) +
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
DD_4_6_day <- ggplot(England_FULL, aes(x = Month, y = `Patients delayed but discharged within 4-6 days (%)`, group = 1)) +
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
DD_7_13_day <- ggplot(England_FULL, aes(x = Month, y = `Patients delayed but discharged within 7-13 days (%)`, group = 1)) +
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
DD_14_20_day <- ggplot(England_FULL, aes(x = Month, y = `Patients delayed but discharged within 14-20 days (%)`, group = 1)) +
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
DD_21_day <- ggplot(England_FULL, aes(x = Month, y = `Patients delayed but discharged within 21 days or more (%)`, group = 1)) +
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

<<<<<<< HEAD
# Grouped delay lengths #######################################################

England_FULL$Group1discharges <- rowSums(England_FULL[, c(
    'Patients delayed but discharged within 1 day (%)',
    'Patients delayed but discharged within 2-3 days (%)',
    'Patients delayed but discharged within 4-6 days (%)')], na.rm = TRUE)

England_FULL$Group2discharges <- (England_FULL$`Patients delayed but discharged within 7-13 days (%)`)

England_FULL$Group3discharges <- rowSums(England_FULL[, c(
  'Patients delayed but discharged within 14-20 days (%)',
  'Patients delayed but discharged within 21 days or more (%)')], na.rm = TRUE)                                     

England_FULL <- England_FULL %>% 
  rename('Delayed discharges 0-6 days' = Group1discharges,
         'Delayed discharges 7-13 days' = Group2discharges,
         'Delayed discharges over 14 days' = Group3discharges)

# Apr/May comparisons

England_FULL_AprMay <- England_FULL %>%
  filter(grepl('Apr|May', Month))

# 0-6 days chart GROUP 1
Group1_delay <- ggplot(AprMay, aes(x = Month, y = `Delayed discharges 0-6 days`, group = 1)) +
  geom_bar()

Group1_delay
ggsave("Group1_delay.png", plot = Group1_delay, width = 8, height = 6, dpi = 300)

# 14+ days chart GROUP 3
Group3_delay <- ggplot(England_FULL, aes(x = Month, y = `Delayed discharges over 14 days`, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              linewidth = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Delayed patients discharged beyond two weeks, England, September 2023 - May 2025 ",
       x = "Month",
       y = "% of delayed patients") +
  scale_y_continuous(
    limits = c(0, 0.8),
    breaks = seq(0, 0.8, 0.05)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(t = 12)))

Group3_delay
ggsave("Group3_delay.png", plot = Group3_delay, width = 8, height = 6, dpi = 300)

# 17 Delay composition stacks #################################################

grouped_delays <- England_FULL %>%
  select(Month, `Delayed discharges 0-6 days`, `Delayed discharges 7-13 days`, `Delayed discharges over 14 days`)
  
grouped_delays <- grouped_delays %>%
  pivot_longer(
    cols = starts_with('Delayed discharges'),
    names_to = 'Delay_Category',
    values_to = 'Value') %>%
  group_by(Month) %>%
  arrange((Value), .by_group = TRUE) %>%
  mutate(Value = Value*100)

delay_length_timeseries <- ggplot(grouped_delays, aes(x = Month, y = `Value`, fill =`Delay_Category`)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = paste0(round(Value, digits = 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 2.5) +
  labs(title = "",
       x = "Month",
       y = "Percentage of patients delayed (%)") +
  scale_y_continuous(
    limits = c(0,100.1),
    breaks = seq(0, 100.1, 5)) +
  theme_gray() +
  theme(legend.text = element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 7.5))

delay_length_timeseries
ggsave("delay_length_timeseries.png", plot = delay_length_timeseries, width = 8, height = 6, dpi = 300)



# Trust-level tests ############################################################

output_test_2 <- output_test_2 %>%
  arrange(proportion_delayed_diff)

change_in_delayed_discharge_plot <- ggplot(data = output_test_2, aes(x = reorder(org_code, proportion_delayed_diff), y = as.numeric(proportion_delayed_diff))) +
  geom_bar(stat = "identity", fill = "firebrick2", color = "black", linewidth = 0.3) +
  labs(
    title = "Absolute Difference in % Admissions with Delay between Apr-May 2025 and Apr-May 2024",
    x = "NHS Trust",
    y = "Absolute Difference in % Admissions with Delay"
  ) +
  theme(axis.text.x = element_blank())

change_in_delayed_discharge_plot
ggsave("change_in_delayed_discharge.png", plot = change_in_delayed_discharge_plot, width = 8, height = 6, dpi = 300)


change_in_delay_length_plot <- ggplot(data = output_test_3, aes(x = reorder(org_code, delay_los_diff), y = delay_los_diff)) +
  geom_bar(stat = "identity", fill = "firebrick2", color = "black", linewidth = 0.3) +
  labs(
    title = "Difference in Average Delay Length between Apr-May 2025 vs Apr-May 2024",
    x = "NHS Trust",
    y = "Difference in Delayed LoS (Days)"
  ) +
  theme(axis.text.x = element_blank())

change_in_delay_length_plot
ggsave("change_in_delay_length.png", plot = change_in_delay_length_plot, width = 8, height = 6, dpi = 300)

