
# 29/08/2025 - Discharge Ready Data (DRD) Flourish Data

# 07 Flourish data #############################################################

# Number of beds occupied by patients fit for discharge, latest date ###########
latest_occupied <- figure_1_data %>%
  slice_tail(n = 1 ) %>%
  select(perc_bed_delays) 



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

figure_3_data <- figure_3_data %>%
  filter(org_code %in% best_trusts)

# Ranking change in the proportion of bed days used for DD
addWorksheet(DD_Flourish_Data, "Figure 3")
writeData(DD_Flourish_Data, "Figure 3", figure_3_data)

# Rate of bed occupancy
addWorksheet(DD_Flourish_Data, "Figure 4")
writeData(DD_Flourish_Data, "Figure 4", figure_4_data)

# Staffing levels
addWorksheet(DD_Flourish_Data, "Figure 5")
writeData(DD_Flourish_Data, "Figure 5", figure_5_data)

# Discharge destination
addWorksheet(DD_Flourish_Data, "Figure 6")
writeData(DD_Flourish_Data, "Figure 6", figure_6_data)

# Delay length
addWorksheet(DD_Flourish_Data, "Figure 7")
writeData(DD_Flourish_Data, "Figure 7", figure_7_data)

saveWorkbook(DD_Flourish_Data, "DD_Flourish_Data.xlsx", overwrite = TRUE)


