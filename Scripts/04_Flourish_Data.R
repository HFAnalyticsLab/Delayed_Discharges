
# 29/08/2025 - Discharge Ready Data (DRD) Flourish Data
# 13/11/2025 - Final Publication update

# 07 Flourish data #############################################################

# Number of beds occupied by patients fit for discharge, latest date ###########
latest_occupied <- hospital_beds %>%
  filter(month == 'Sep-25')%>%
  mutate(total_acute_beds = sum(acute_beds)) 

latest_beds <- latest_occupied$total_acute_beds[nrow(latest_occupied)]
latest_delayed <- dd_file_national_FINAL$total_delay_volume[nrow(dd_file_national_FINAL)]

current_DD_occupied_beds <- latest_delayed/latest_beds

over_occupied_trusts <- latest_occupied %>%
  mutate(flag = latest_occupied$occupancy_rate >= 0.92) %>%
  count(flag)

over_occupied_trusts <- over_occupied_trusts %>%
  mutate(Total = sum(n),
         pct_of_trusts = (n/Total))

# Number of trusts in the analysis
trust_count <- unique(figure_2_data$org_code)

full_trusts_data <- dd_file_acute_trusts_FINAL %>%
  group_by(org_code) %>%
  filter(!any(is.na(patients_discharged_volume))) %>%
  ungroup()

# Number of trusts with full data
full_trust_count <- unique(full_trusts_data$org_code)

# Consistency of most improved trusts ##########################################
best_trusts_dd <- dd_file_acute_trusts_FINAL %>%
  filter(org_code %in% best_trusts)

best_trusts_beds <- hospital_beds %>%
  filter(org_code %in% best_trusts)

# Clean & standardize month columns to real Date objects
dd_clean <- dd_file_acute_trusts_FINAL %>%
  filter(org_code %in% best_trusts) %>%
  mutate(
    org_code = trimws(as.character(org_code)),
    month = my(as.character(month)))

beds_clean <- hospital_beds %>%
  filter(org_code %in% best_trusts) %>%
  mutate(
    org_code = trimws(as.character(org_code)),
    month = my(as.character(month)))

best_trusts_series <- dd_clean %>%
  left_join(beds_clean, by = c("month", "org_code")) %>%
  mutate(
    days_in_month = days_in_month(month),
    perc_bed_delays = (dd_bed_days / days_in_month) / adult_acute_beds * 100)

best_trusts_timeseries <- ggplot(best_trusts_series, aes(x = month, y = perc_bed_delays, color = org_code, group = org_code)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(breaks = scales::pretty_breaks((n=10))) +
  labs(title = "Consistency of the most improved trusts",
       subtitle = "Proportion of bed days occupied by delayed discharge patients",
       x = "Month",
       y = "Proportion of bed days (%)",
       color = "Trust") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

best_trusts_timeseries
ggsave("best_trusts_timeseries.png", best_trusts_timeseries)

# FYI for trust selection
greater_than_3.5p_reduction <- figure_3_data %>%
  select(org_code,difference) %>%
  filter(difference <= -3.5)

# FYI for trust selection
greater_than_3.0p_reduction <- figure_3_data %>%
  select(org_code,difference) %>%
  filter(difference <= -3.0)

# FYI for trust selection 
greater_than_2.0p_reduction <- figure_3_data %>%
  select(org_code,difference) %>%
  filter(difference <= -2.0)

# Flourish data ################################################################

DD_Flourish_Data <- createWorkbook()

addWorksheet(DD_Flourish_Data, "Best_Trusts")
writeData(DD_Flourish_Data, "Best_Trusts", best_trusts)

# Percentage of discharges that are delayed + Proportion of beds occupied
addWorksheet(DD_Flourish_Data, "Figure 1")
writeData(DD_Flourish_Data, "Figure 1", figure_1_data)

# Delay length national
national_delay_length <- dd_file_national_FINAL %>%
  select(month, org_code, average_delay_los_minus_0_day_delay)

# Percentage of discharges that are delayed + Proportion of beds occupied
addWorksheet(DD_Flourish_Data, "National Delay length")
writeData(DD_Flourish_Data, "National Delay length", national_delay_length)

# Percentage change in proportion of bed days used for DD (trust variation)
addWorksheet(DD_Flourish_Data, "Figure 2")
writeData(DD_Flourish_Data, "Figure 2", figure_2_data)

# Ranking change in the proportion of bed days used for DD
addWorksheet(DD_Flourish_Data, "Figure 3")
writeData(DD_Flourish_Data, "Figure 3", figure_3_data)

# Rate of bed occupancy
addWorksheet(DD_Flourish_Data, "Figure 4")
writeData(DD_Flourish_Data, "Figure 4", figure_4_data)

addWorksheet(DD_Flourish_Data, "Figure 4b")
writeData(DD_Flourish_Data, "Figure 4b", figure_4b_data)

# Staffing levels
addWorksheet(DD_Flourish_Data, "Figure 5")
writeData(DD_Flourish_Data, "Figure 5", figure_5_data)

# Discharge destination
addWorksheet(DD_Flourish_Data, "Figure 6")
writeData(DD_Flourish_Data, "Figure 6", figure_6_data)

# Delay length
addWorksheet(DD_Flourish_Data, "Figure 7")
writeData(DD_Flourish_Data, "Figure 7", figure_7_data)



addWorksheet(DD_Flourish_Data, "Figure 7b")
writeData(DD_Flourish_Data, "Figure 7b", national_delay_length)

# Most improved consistency
addWorksheet(DD_Flourish_Data, "Most Improved Consistency")
writeData(DD_Flourish_Data, "Most Improved Consistency", best_trusts_series)

# All trusts staffing
addWorksheet(DD_Flourish_Data, "All trusts staffing")
writeData(DD_Flourish_Data, "All trusts staffing", all_trust_staffing)

# Save Flourish Data
saveWorkbook(DD_Flourish_Data, "DD_Flourish_Data.xlsx", overwrite = TRUE)

# CLEAN #######################################################################

rm(n)
rm(Month_list)
rm(DRD_url)
rm(DRDlinks)
rm(Timeframe)
rm(temp_file)
rm(overall_means)
rm(output)
rm(output_test)
rm(output_test_2)
rm(output_test_3)

total_discharges_plot <- ggplot(dd_file_national_FINAL, aes(x = month, y = patients_discharged_volume, group = 1)) +
  geom_line(color = "firebrick1", linewidth = 1.2) +
  geom_point() +
    scale_y_continuous(breaks = scales::pretty_breaks((n=10))) +
  labs(title = "Total discharges",
       subtitle = "Discharges",
       x = "Month",
       y = "Discharges") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

total_discharges_plot

trusts_in <- unique(dd_file_acute_trusts_FINAL$org_code)

# General bed days (capacity) check

general_bed_days <- figure_1b_data %>%
  select(month, dd_bed_days, total_acute_beds, total_adult_beds, days_in_month) %>%
  group_by(month) %>%
  mutate(general_bed_days = (total_acute_beds*days_in_month))

general_bed_days_sheet <- createWorkbook()

addWorksheet(general_bed_days_sheet, "Bed_days")
writeData(general_bed_days_sheet, "Bed_days", general_bed_days)
saveWorkbook(general_bed_days_sheet, "general_bed_days_sheet.xlsx", overwrite = TRUE)























