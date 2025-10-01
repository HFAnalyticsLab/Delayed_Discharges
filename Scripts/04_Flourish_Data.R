
# 29/08/2025 - Discharge Ready Data (DRD) Flourish Data

# 07 Flourish data #############################################################

# Number of beds occupied by patients fit for discharge, latest date ###########
latest_occupied <- hospital_beds %>%
  filter(month == 'Jul-25')%>%
  mutate(total_acute_beds = sum(acute_beds)) 

latest_beds <- latest_occupied$total_acute_beds[nrow(latest_occupied)]
latest_delayed <- dd_file_national_FINAL$total_delay_volume[nrow(dd_file_national_FINAL)]

current_DD_occupied_beds <- latest_delayed/latest_beds

over_occupied_trusts <- hospital_beds %>%
  mutate(flag = hospital_beds$occupancy_rate >= 0.85) %>%
  count(flag)

over_occupied_trusts <- over_occupied_trusts %>%
  mutate(Total = sum(n),
         pct_of_trusts = (n/Total))

# Number of trusts in the analysis
trust_count <- unique(figure_2_data$org_code)

colMeans(!is.na (dd_file_acute_trusts_FINAL) & dd_file_acute_trusts_FINAL != "", na.rm = FALSE)

full_trusts_data <- dd_file_acute_trusts_FINAL %>%
  group_by(org_code) %>%
  filter(!any(is.na(patients_discharged_volume))) %>%
  ungroup()

colMeans(!is.na (full_trusts_data) & full_trusts_data != "", na.rm = FALSE)

# Number of trusts with full data
full_trust_count <- unique(full_trusts_data$org_code)

# Hospital bed rankings

hospital_bed_change <- hospital_beds %>%
  filter(org_code %in% best_trusts) %>%
  group_by(org_code) %>%
  summarize(
    Pre_beds = mean(acute_beds[month %in% c("May-24","Jun-24","Jul-24")]),
    Post_beds = mean(acute_beds[month %in% c("May-25","Jun-25","Jul-25")]),
    difference = (Post_beds-Pre_beds),
    pct_difference = ((Post_beds-Pre_beds)/Pre_beds))
  



# Flourish data ################################################################

DD_Flourish_Data <- createWorkbook()

addWorksheet(DD_Flourish_Data, "Best_Trusts")
writeData(DD_Flourish_Data, "Best_Trusts", best_trusts)

# Percentage of discharges that are delayed + Proportion of beds occupied
addWorksheet(DD_Flourish_Data, "Figure 1")
writeData(DD_Flourish_Data, "Figure 1", figure_1_data)

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
writeData(DD_Flourish_Data, "Figure 4b", hospital_bed_change)

# Staffing levels
addWorksheet(DD_Flourish_Data, "Figure 5")
writeData(DD_Flourish_Data, "Figure 5", figure_5_data)

# Discharge destination
addWorksheet(DD_Flourish_Data, "Figure 6")
writeData(DD_Flourish_Data, "Figure 6", figure_6_data)

# Delay length
addWorksheet(DD_Flourish_Data, "Figure 7")
writeData(DD_Flourish_Data, "Figure 7", figure_7_data)

# Delay length national
national_delay_length <- dd_file_national_FINAL %>%
  select(month, org_code, average_delay_los_minus_0_day_delay)

addWorksheet(DD_Flourish_Data, "Figure 7b")
writeData(DD_Flourish_Data, "Figure 7b", national_delay_length)

# Save Flourish Data
saveWorkbook(DD_Flourish_Data, "DD_Flourish_Data.xlsx", overwrite = TRUE)


# CLEAN #######################################################################











