
# 29/08/2025 - Discharge Ready Data (DRD) Flourish Data

# 07 Flourish data #############################################################

# Figure 1 National Picture 'not going in the right direction' ###############

figure_1_data <- dd_file_national_FINAL %>%
  select(month, org_code, perc_patients_delayed, patients_discharged_volume)

figure_1_data$perc_bed_delays <- figure_6_data$perc_bed_delays

write_xlsx(figure_1_data, 'figure_1_data.xlsx')

# Trust names





# Flourish data ################################################################

write_xlsx(best_trusts, 'best_trusts.xlsx')
write_xlsx(figure_1_data, 'figure_1_data.xlsx')
write_xlsx(figure_3.1_data, 'figure_3.1_data.xlsx')
write_xlsx(figure_3.2_data, 'figure_3.2_data.xlsx')
write_xlsx(figure_6_data, 'figure_6_data.xlsx')
write_xlsx(figure_9_data, 'figure_9_data.xlsx')
write_xlsx(figure_10_data, 'figure_10_data.xlsx')
write_xlsx(figure_11_data, 'figure_11_data.xlsx')
write_xlsx(figure_12_data, 'figure_12_data.xlsx')
write_xlsx(figure_13_data, 'figure_1_data.xlsx')

