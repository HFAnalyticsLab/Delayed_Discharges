
# 08/07/2025 - Discharge Ready Data (DRD) descriptive analysis
# National level descriptives #################################################

dd_file_national[dd_file_national == 0] <- NA
dd_file_national[ , -c(1, 2)] <- lapply(dd_file_national[ , -c(1, 2)], as.numeric)
dd_file_national$month <- factor(dd_file_national$month, 
                                 levels = unique(dd_file_national$month))

# 1 % of discharges that are delayed ##################################

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

# 2 Total discharges ##########################################################
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

# 3 Total bed days lost to delayed discharge ##################################

total_bed_days_lost <- ggplot(dd_file_national, aes (x = month, y = as.numeric(`dd_bed_days`) , group = 1)) +
  geom_line(color = 'firebrick1', linewidth = 1.2) +
  geom_smooth(method = 'lm', se = FALSE,
              color = 'black',
              linetype = 'longdash',
              linewidth = 0.3) +
  geom_point(color = "black", size = 1.5) +
  labs (title = 'Total bed days lost to delayed discharge, England, April 2024 - May 2025',
        x = 'Month',
        y = '# of bed days') +
  scale_y_continuous(
    limits = c(0, 350000),
    breaks = seq(0, 350000, 50000)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

total_bed_days_lost
ggsave("total_bed_days_lost.png", plot = total_bed_days_lost, width = 8, height = 6, dpi = 300) 

# 4 Average days from DRD to DoD (exc 0-day delays) ###########################
Average_DRD_to_DoD <- ggplot(dd_file_national, aes(x = month, y = as.numeric(`average_delay_los_minus_0_day_delay`) , group = 1)) +
  geom_line(color = "firebrick1", linewidth = 1.2) +
    geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              linewidth = 0.5) +
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
        axis.title.y = element_text(margin = margin(r = 12)))

Average_DRD_to_DoD
ggsave("AverageDRD_to_DoD.png", plot = Average_DRD_to_DoD, width = 8, height = 6, dpi = 300)

## Discharge delays +1 day ###################################################
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

## Discharge delays 2-3 days #################################################
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

## Discharge delays 4-6 days  ################################################
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

## Discharge delays 7-13 days  ###############################################
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

## Discharge delays 14-20 days  ##############################################
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

## Discharge delays 21+ days  ###############################################
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

## Patients DD but within 1 day  ############################
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

## Patients DD but within 2-3 days  #########################
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

## Patients DD but within 4-6 days  #########################
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

## Patients DD but within 7-13 days  ########################
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

## Patients DD but within 14-20 days  #######################
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

## Patients DD but within 21+ days  #########################
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

# 5 Grouped delay lengths #######################################################

dd_file_national$Group1discharges <- rowSums(dd_file_national[, c(
<<<<<<< HEAD
    '1_day_delay_perc',
    '2_3_day_delay_perc',
    '4_6_day_delay_perc')], na.rm = FALSE)

dd_file_national$Group2discharges <- rowSums(dd_file_national[, c(
  '7_13_day_delay_perc')], na.rm = FALSE)

dd_file_national$Group3discharges <- rowSums(dd_file_national[, c(
  '14_20_day_delay_perc',
  '21plus_day_delay_perc')], na.rm = FALSE)                                     
=======
    'delayed_perc_1_day',
    'delayed_perc_2_3_days',
    'delayed_perc_4_6_days')], na.rm = FALSE)

dd_file_national$Group2discharges <- rowSums(dd_file_national[, c(
  'delayed_perc_7_13_days')], na.rm = FALSE)

dd_file_national$Group3discharges <- rowSums(dd_file_national[, c(
  'delayed_perc_14_20_days',
  'delayed_perc_21plus_days')], na.rm = FALSE)                                     
>>>>>>> 4635b80d183e155b38a554300be258d4e559a9f0

### Group 1 (1-6) ##############################################################
### Group 2 (7-13) #############################################################
group2delay <- ggplot(dd_file_national, aes(x = month, y = Group2discharges, group = 1)) +
  geom_line(color = "firebrick1", linewidth = 1.2) +
  geom_smooth(method = "loess", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Delayed patients discharged within 7-13 days, England, September 2023 - May 2025 ",
       x = "Month",
       y = "% of delayed patients") +
  scale_y_continuous(
    limits = c(0, 0.8),
    breaks = seq(0, 0.8, 0.05)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

group2delay
ggsave("group2delay.png", plot = group2delay, width = 8, height = 6, dpi = 300)


### Group 3 (14+) ##############################################################
<<<<<<< HEAD
group3_delay <- ggplot(dd_file_national, aes(x = month, y = Group3discharges, group = 1)) +
=======
Group3_delay <- ggplot(dd_file_national, aes(x = month, y = Group3discharges, group = 1)) +
>>>>>>> 4635b80d183e155b38a554300be258d4e559a9f0
  geom_line(color = "firebrick1", linewidth = 1.2) +
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
        axis.title.y = element_text(margin = margin(r = 12)))

<<<<<<< HEAD
group3_delay
ggsave("Group3_delay.png", plot = group3_delay, width = 8, height = 6, dpi = 300)
=======
Group3_delay
ggsave("Group3_delay.png", plot = Group3_delay, width = 8, height = 6, dpi = 300)
>>>>>>> 4635b80d183e155b38a554300be258d4e559a9f0




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

grouped_delays <- dd_file_national %>%
  select(month, `Group1discharges`, `Group2discharges`, `Group3discharges`)
  
grouped_delays <- grouped_delays %>%
  pivot_longer(
    cols = starts_with('Group'),
    names_to = 'Delay_Category',
    values_to = 'Value') %>%
  group_by(month) %>%
  arrange((Value), .by_group = TRUE) %>%
  mutate(Value = Value*100)

delay_length_timeseries <- ggplot(grouped_delays, aes(x = month, y = `Value`, fill =`Delay_Category`)) +
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




#####
#Figure 6.

figure_6_data <- left_join(dd_file_acute_trusts_FINAL,hospital_beds,by=c('month','org_code')) %>% 
  mutate(month = my(month),
    days_in_month = days_in_month(month)) %>% 
  filter(dd_bed_days > 0, #only select out trusts which have delayed discharges
         !is.na(acute_beds)) %>%  #remove one trust which doesn't have some bed info for Nov/Dec due to merger
  group_by(month) %>% 
  summarise(dd_bed_days = sum(as.numeric(dd_bed_days)),
            total_acute_beds = sum(acute_beds),
            days_in_month = max(days_in_month)) %>% 
  ungroup() %>% 
  mutate(perc_bed_delays = (dd_bed_days/days_in_month)/total_acute_beds*100)


ggplot(figure_6_data,aes(x=month,y=perc_bed_delays)) +
  geom_line(size = 1) +
  labs(
    title = "Proportion of Beds with a Delayed Discharge",
    y = "Percentage (%)"
  ) +
  theme_classic()

  
#####
#Figure 9.

figure_9_data <- left_join(dd_file_acute_trusts_FINAL,hospital_beds,by=c('month','org_code')) %>% 
  filter(month %in% c('Apr-24','May-24','Apr-25','May-25')) %>% 
  mutate(period = if_else(month %in% c('Apr-24','May-24'),'pre','post')) %>% 
  group_by(period,org_code) %>% 
  summarise(patients_discharged_volume = sum(as.numeric(patients_discharged_volume)),
            no_delay_volume = sum(as.numeric(no_delay_volume))) %>% 
  ungroup() %>% 
  mutate(perc_delayed = (patients_discharged_volume - no_delay_volume)/patients_discharged_volume*100) %>% 
  select(period,org_code,perc_delayed) %>% 
  pivot_wider(
    names_from = period,  
    values_from = perc_delayed
  ) %>% 
  filter(if_all(everything(), ~ !is.nan(.))) %>% 
  mutate(difference = post - pre)


ggplot(data = figure_9_data, aes(x = reorder(org_code, difference), y = as.numeric(difference))) +
  geom_bar(stat = "identity", fill = "firebrick2", color = "black", linewidth = 0.3) +
  labs(
    title = "Absolute Difference in % Admissions with Delay between Apr-May 2025 and Apr-May 2024",
    x = "NHS Trust",
    y = "Absolute Difference in % Admissions with Delay"
  ) +
  theme(axis.text.x = element_blank())



#####
#Figure 10.

figure_10_data <- left_join(dd_file_acute_trusts_FINAL,hospital_beds,by=c('month','org_code')) %>% 
  filter(month %in% c('Apr-24','May-24','Apr-25','May-25')) %>% 
  mutate(period = if_else(month %in% c('Apr-24','May-24'),'pre','post')) %>% 
  group_by(period,org_code) %>% 
  summarise(patients_discharged_volume = sum(as.numeric(patients_discharged_volume)),
            no_delay_volume = sum(as.numeric(no_delay_volume)),
            dd_bed_days = sum(as.numeric(dd_bed_days))) %>% 
  ungroup() %>% 
  mutate(dd_los = dd_bed_days/(patients_discharged_volume - no_delay_volume)) %>% 
  select(period,org_code,dd_los) %>% 
  pivot_wider(
    names_from = period,  
    values_from = dd_los
  ) %>% 
  filter(if_all(everything(), ~ !is.nan(.))) %>% 
  mutate(difference = post - pre)


ggplot(data = figure_10_data, aes(x = reorder(org_code, difference), y = as.numeric(difference))) +
  geom_bar(stat = "identity", fill = "firebrick2", color = "black", linewidth = 0.3) +
  labs(
    title = "Difference in Average Delay Length between Apr-May 2025 vs Apr-May 2024",
    x = "NHS Trust",
    y = "Difference in Delayed LoS (Days)"
  ) +
  theme(axis.text.x = element_blank())


#####
#Figure 11.

figure_11_data <- left_join(dd_file_acute_trusts_FINAL,hospital_beds,by=c('month','org_code')) %>% 
  filter(month %in% c('Apr-24','May-24','Apr-25','May-25')) %>% 
  mutate(period = if_else(month %in% c('Apr-24','May-24'),'pre','post')) %>% 
  mutate(month = my(month),
         days_in_month = days_in_month(month)) %>%
  filter(dd_bed_days > 0, #only select out trusts which have delayed discharges
         !is.na(acute_beds)) %>%  #remove one trust which doesn't have some bed info for Nov/Dec due to merger
  group_by(period,org_code) %>% 
  summarise(dd_bed_days = sum(as.numeric(dd_bed_days)),
            total_acute_beds = sum(acute_beds),
            days_in_month = max(days_in_month)) %>% 
  ungroup() %>% 
  mutate(perc_bed_delays = (dd_bed_days/days_in_month)/total_acute_beds*100) %>% 
  select(period,org_code,perc_bed_delays) %>% 
  pivot_wider(
    names_from = period,  
    values_from = perc_bed_delays
  ) %>% 
  filter(if_all(everything(), ~ !is.nan(.))) %>% 
  mutate(difference = post - pre) %>% 
  filter(!is.na(difference))


ggplot(data = figure_11_data, aes(x = reorder(org_code, difference), y = as.numeric(difference))) +
  geom_bar(stat = "identity", fill = "firebrick2", color = "black", linewidth = 0.3) +
  labs(
    title = "Absolute Diff. in % of Bed Days used for DD between Apr-May 2025 vs Apr-May 2024",
    x = "NHS Trust",
    y = "Absolute Difference in % of Bed Days used for Delayed Discharges"
  ) +
  theme(axis.text.x = element_blank())




figure_12_data <- figure_11_data %>% 
  mutate(rank_pre = min_rank(pre),
         rank_post = min_rank(post),
         rank_change = min_rank(difference))


test <- figure_12_data %>% 
  filter(rank_change <= 10) %>%
  pivot_longer(
    cols = c(rank_pre, rank_post),
    names_to = "period",
    values_to = "rank"
  ) %>%
  mutate(
    period = ifelse(period == "rank_pre", "Pre", "Post")
  ) %>% 
  mutate(
    period = recode(period, rank_pre = "Pre", rank_post = "Post"),
    period = factor(period, levels = c("Pre", "Post"))  # ensure correct order
  ) %>% 
  select(org_code,period,rank)

ggplot(test, aes(x = period, y = rank, group = org_code)) +
  geom_line(aes(color = org_code), size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = org_code), 
            data = test %>% filter(period == "Pre"), 
            hjust = 1.2, size = 3) +
  geom_text(aes(label = org_code), 
            data = test %>% filter(period == "Post"), 
            hjust = -0.2, size = 3) +
  scale_y_reverse(
    breaks = seq(10, 110, by = 10) %>% c(1),
    limits = c(110, 1)
  ) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  labs(
    title = "Top 10 Best Improved Trusts by % bed days used for delayed discharge",
    x = NULL,
    y = "Rank"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(face = "bold"),
    legend.position = "none"
  )
