
# 30/07/2025 - Discharge Ready Data (DRD) National descriptive analysis
# 05/08/2025 - Cleaned

# National level descriptives #################################################

dd_file_national_FINAL[dd_file_national_FINAL == 0] <- NA
dd_file_national_FINAL[ , -c(1, 2)] <- lapply(dd_file_national_FINAL[ , -c(1, 2)], as.numeric)
dd_file_national_FINAL$month <- factor(dd_file_national_FINAL$month, 
                                 levels = unique(dd_file_national_FINAL$month))

# 1 Percentage of discharges that are delayed #################################
# sum of 1-21+ day delay volumes/total patients discharged

dd_file_national_FINAL$`total_delay_volume` <- rowSums(dd_file_national_FINAL[, c(
  '1_day_delay_volume',
  '2_3_day_delay_volume',
  '4_6_day_delay_volume',
  '7_13_day_delay_volume',
  '14_20_day_delay_volume',
  '21plus_day_delay_volume')], na.rm = FALSE)

dd_file_national_FINAL$`perc_patients_delayed` <- (dd_file_national_FINAL$total_delay_volume/dd_file_national_FINAL$patients_discharged_volume) *100

delayed_discharges_plot <- ggplot(dd_file_national_FINAL, aes(x = month, y = perc_patients_delayed, group = 1)) +
  geom_line(color = "firebrick1", linewidth = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              linewidth = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Percentage of discharges delayed, England, April 2024 - May 2025.",
       x = "Month",
       y = "Percentage of patients discharged beyond 1 day") +
  scale_y_continuous(
    limits = c(0, 15),        
    breaks = seq(0, 15, 1),
    labels = label_percent(scale = 1)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 6.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

delayed_discharges_plot
ggsave("delayed discharges.png", plot = delayed_discharges_plot, width = 8, height = 6, dpi = 300)

# 2 Proportion of beds occupied by delayed discharges #########################
dd_bed_days_plot <- ggplot(figure_6_data, aes(x = month, y = perc_bed_delays, group = 1)) +
  geom_line(color = "firebrick1", linewidth = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              linewidth = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Proportion of beds with a delayed discharge, England, April 2024 - May 2025.",
       x = "Month",
       y = "Percentage (%)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(
    limits = c(0, 11),        
    breaks = seq(0, 11, 1),
    labels = label_percent(scale = 1)) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 6.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

dd_bed_days_plot
ggsave("delayed bed days.png", plot = dd_bed_days_plot, width = 8, height = 6, dpi = 300)

## Number of beds occupied by patients fit for discharge, latest date ########

latest_occupied <- figure_6_data %>%
  filter(month == max(month, na.rm = TRUE)) %>%
  select(perc_bed_delays) %>%

latest_occupied
  
# 3 Total discharges ##########################################################
# plot total volume of patients discharged

total_discharges_plot <- ggplot(dd_file_national_FINAL, aes(x = month , y = patients_discharged_volume, group = 1)) +
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
    breaks = seq(0, 400000, 50000),
    labels = comma) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 6.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12))) +
  theme(axis.title.y = element_text(margin = margin(r = 12)))

total_discharges_plot
ggsave("total discharges.png", plot = total_discharges_plot, width = 8, height = 6, dpi = 300)

# 4 Total bed days lost to delayed discharge ##################################
# plot total volume of bed days lost to delayed discharge

total_bed_days_lost_plot <- ggplot(dd_file_national_FINAL, aes (x = month, y = dd_bed_days, group = 1)) +
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
    limits = c(0, 400000),
    breaks = seq(0, 400000, 50000),
    labels = comma) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

total_bed_days_lost_plot
ggsave("total_bed_days_lost.png", plot = total_bed_days_lost_plot, width = 8, height = 6, dpi = 300) 

# 5 Average length of delay ###################################################
# plot average delay los exc 0 day delays

average_length_delay_plot <- ggplot(dd_file_national_FINAL, aes(x = month, y = average_delay_los_minus_0_day_delay, group = 1)) +
  geom_line(color = "firebrick1", linewidth = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              linewidth = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Average length of delay, England, April 2024 - May 2025",
       x = "Month",
       y = "Average days from DRD to DoD (exc 0-day)") +
  scale_y_continuous(
    limits = c(0, 7),
    breaks = seq(0, 7, 0.5),
    labels = comma) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 7.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

average_length_delay_plot
ggsave("AverageDRD_to_DoD.png", plot = average_length_delay_plot, width = 8, height = 6, dpi = 300)

# Calculate the change in average length of delay 

dd_file_national_FINAL$delay_length_change <- c(NA, diff(dd_file_national_FINAL$average_delay_los_minus_0_day_delay))

CAGR_dl <- (dd_file_national_FINAL$average_delay_los_minus_0_day_delay[dd_file_national_FINAL$month == 'May-25']/
              dd_file_national_FINAL$average_delay_los_minus_0_day_delay[dd_file_national_FINAL$month == 'Apr-24']
              ^(1/number_of_months))

CAGR_dl

number_of_months <- nrow(dd_file_national_FINAL)

# 6 Composition of delays #####################################################
## grouped monthly sums of delay volumes / total sum of delay volumes

dd_file_national_FINAL <- dd_file_national_FINAL %>%
  group_by(month) %>%
  mutate(total_delayed = rowSums(across(c(`1_day_delay_volume`, `2_3_day_delay_volume`,`4_6_day_delay_volume`, `7_13_day_delay_volume`, `14_20_day_delay_volume`, `21plus_day_delay_volume`))),
         group1 = rowSums(across(c(`1_day_delay_volume`, `2_3_day_delay_volume`,`4_6_day_delay_volume`))),
         group2 = rowSums(across(c(`7_13_day_delay_volume`))),
         group3 = rowSums(across(c(`14_20_day_delay_volume`, `21plus_day_delay_volume`))))

dd_file_national_FINAL <- dd_file_national_FINAL %>%
  group_by(month) %>%
  mutate(`group1(%)` = group1/total_delayed,
         `group2(%)` = group2/total_delayed,
         `group3(%)` = group3/total_delayed)

dd_file_national_FINAL <- dd_file_national_FINAL %>%
  rename('0-6 days (%)' = `group1(%)`,
         '7-13 days (%)' = `group2(%)`,
         '14+ days (%)' = `group3(%)`)

# Pull out groups and pivot

grouped_delays <- dd_file_national_FINAL %>%
  select(month, `0-6 days (%)`, `7-13 days (%)`, `14+ days (%)`)

grouped_delays <- grouped_delays %>%
  pivot_longer(
    cols = c(`0-6 days (%)`, `7-13 days (%)`, `14+ days (%)`),
    names_to = 'Delay_Category',
    values_to = 'Value') %>%
  group_by(month) %>%
  arrange((Value), .by_group = TRUE) %>%
  mutate(Value = Value*100)

grouped_delays <- grouped_delays %>%
  mutate(Delay_Category = factor(Delay_Category, levels = c('14+ days (%)','7-13 days (%)','0-6 days (%)')))

delay_length_timeseries <- ggplot(grouped_delays, aes(x = month, y = `Value`, fill =`Delay_Category`)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = paste0(round(Value, digits = 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3,
            fontface = "bold") +
  labs(title = "Breakdown of delayed patients by length of delay, England, April 2024 - May 2025",
       fill = "Delay length",
       x = "Month",
       y = "Percentage of delayed patients") +
  scale_y_continuous(
    limits = c(0,100.1),
    breaks = seq(0, 100.1, 5),
    labels = label_percent(scale = 1)) +
  scale_fill_manual(values = c(
    '14+ days (%)' = desaturate("firebrick4", 0.1),
    '7-13 days (%)' = desaturate("firebrick3", 0.1),
    '0-6 days (%)' = desaturate("firebrick1", 0.1))) +
  theme_gray() +
  theme(legend.text = element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 7.5))

delay_length_timeseries
ggsave("delay_length_stack.png", plot = delay_length_timeseries, width = 8, height = 6, dpi = 300)

## same as above but pull out Apr/May 24 vs 25

# Pull out Apr May
dd_file_apr_may_national <- dd_file_national_FINAL %>%
  filter(month %in% c('Apr-24','May-24','Apr-25','May-25')) %>% 
  mutate(time_period = if_else(month %in% c('Apr-24','May-24'),'pre','post'))

dd_file_apr_may_national[ , -c(1, 2)] <- lapply(dd_file_apr_may_national[ , -c(1, 2)], as.numeric)

AprMay2024 <- colMeans(dd_file_apr_may_national[1:2, 3:42])
AprMay2025 <- colMeans(dd_file_apr_may_national[3:4, 3:42])

dd_file_apr_may_avg <- rbind(AprMay2024, AprMay2025)
dd_file_apr_may_avg <- as.data.frame(dd_file_apr_may_avg)
dd_file_apr_may_avg$month <- c("AprMay2024","AprMay2025")

# Pivot the delay composition
grouped_delays_24_25 <- dd_file_apr_may_avg %>%
  select(month, `0-6 days (%)`, `7-13 days (%)`, `14+ days (%)`)

grouped_delays_24_25 <- grouped_delays_24_25 %>%
  pivot_longer(
    cols = c(`0-6 days (%)`, `7-13 days (%)`, `14+ days (%)`),
    names_to = 'Delay_Category',
    values_to = 'Value') %>%
  group_by(month) %>%
  arrange((Value), .by_group = TRUE) %>%
  mutate(Value = Value*100)

# Reverse stacking order
grouped_delays_24_25$Delay_Category <- factor(
  grouped_delays_24_25$Delay_Category,
  levels = c("14+ days (%)", "7-13 days (%)", "0-6 days (%)"))

delay_length_comp <- ggplot(grouped_delays_24_25, aes(x = month, y = `Value`, fill =`Delay_Category`)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = paste0(round(Value, digits = 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3,
            fontface = "bold") +
  labs(title = "Breakdown of delayed patients by length of delay, England, (April/May) 2024 vs 2025",
       fill = "Delay length",
       x = "Month",
       y = "Percentage of delayed patients") +
  scale_y_continuous(
    limits = c(0,100.1),
    breaks = seq(0, 100.1, 5),
    labels = label_percent(scale = 1)) +
  scale_fill_manual(values = c(
    '14+ days (%)' = desaturate("firebrick4", 0.1),
    '7-13 days (%)' = desaturate("firebrick3", 0.1),
    '0-6 days (%)' = desaturate("firebrick1", 0.1))) +
  theme_gray() +
  theme(legend.text = element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 7.5))

delay_length_comp
ggsave("delay_length_comp.png", plot = delay_length_comp, width = 8, height = 6, dpi = 300)

# 7 Figure 1 National Picture 'not going in the right direction' ################

figure_1_data <- dd_file_national_FINAL %>%
  select(month, org_code, perc_patients_delayed, patients_discharged_volume)

figure_1_data$perc_bed_delays <- figure_6_data$perc_bed_delays

write_xlsx(figure_1_data, 'figure_1_data.xlsx')












