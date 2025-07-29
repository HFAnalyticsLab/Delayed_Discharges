
# 29/07/2025 - Discharge Ready Data (DRD) AprMay 24 vs AprMay25

# Construct 2024 vs 2025 datasets

# Apr/May 24 vs Apr/May 25 prep ###############################################

Apr_24_redux <- Apr_24 %>%
  rename(total_discharge_Apr_24 = `patients_discharged_volume`,
         no_delay_Apr_24 = `no_delay_volume`,
         total_delay_beddays_Apr_24 = `dd_bed_days`) %>% 
  mutate(delays_Apr_24 = as.numeric(total_discharge_Apr_24) - as.numeric(no_delay_Apr_24)) %>% 
  select(Region,org_code,total_discharge_Apr_24,no_delay_Apr_24,delays_Apr_24,total_delay_beddays_Apr_24)

May_24_redux <- May_24 %>%
  rename(total_discharge_May_24 = `patients_discharged_volume`,
         no_delay_May_24 = `no_delay_volume`,
         total_delay_beddays_May_24 = `dd_bed_days`) %>%
  mutate(delays_May_24 = as.numeric(total_discharge_May_24) - as.numeric(no_delay_May_24)) %>% 
  select(Region,org_code,total_discharge_May_24,no_delay_May_24,delays_May_24,total_delay_beddays_May_24)


Apr_25_redux <- Apr_25 %>%
  rename(total_discharge_Apr_25 = `patients_discharged_volume`,
         no_delay_Apr_25 = `no_delay_volume`,
         total_delay_beddays_Apr_25 = `dd_bed_days`) %>%
  mutate(delays_Apr_25 = as.numeric(total_discharge_Apr_25) - as.numeric(no_delay_Apr_25)) %>% 
  select(Region,org_code,total_discharge_Apr_25,no_delay_Apr_25,delays_Apr_25,total_delay_beddays_Apr_25)

May_25_redux <- May_25 %>%
  rename(total_discharge_May_25 = `patients_discharged_volume`,
         no_delay_May_25 = `no_delay_volume`,
         total_delay_beddays_May_25 = `dd_bed_days`) %>%
  mutate(delays_May_25 = as.numeric(total_discharge_May_25) - as.numeric(no_delay_May_25)) %>% 
  select(Region,org_code,total_discharge_May_25,no_delay_May_25,delays_May_25,total_delay_beddays_May_25)


output <- full_join(Apr_24_redux,May_24_redux, by=c("Region","org_code"))
output <- full_join(output,Apr_25_redux, by=c("Region","org_code"))
output <- full_join(output,May_25_redux, by=c("Region","org_code"))

# National filter

Apr_May_national <- output %>%
  filter(org_code == 'National') %>%
  mutate(total_discharge24 = sum(as.numeric(total_discharge_Apr_24), as.numeric(total_discharge_May_24))/2,
         total_discharge25 = sum(as.numeric(total_discharge_Apr_25), as.numeric(total_discharge_May_25)/2)


output_test <- output %>% 
  group_by(Region,org_code) %>% 
  mutate(pre_total_discharge = as.numeric(total_discharge_Apr_24) + as.numeric(total_discharge_May_24),
         pre_no_delay = as.numeric(no_delay_Apr_24) + as.numeric(no_delay_May_24),
         pre_delays = as.numeric(delays_Apr_24) + as.numeric(delays_May_24),
         pre_delayed_beddays = as.numeric(total_delay_beddays_Apr_24) + as.numeric(total_delay_beddays_May_24),
         pre_proportion_delayed = pre_delays / pre_total_discharge,
         pre_delay_los = pre_delayed_beddays / pre_delays,
         post_total_discharge = as.numeric(total_discharge_Apr_25) + as.numeric(total_discharge_May_25),
         post_no_delay = as.numeric(no_delay_Apr_25) + as.numeric(no_delay_May_25),
         post_delays = as.numeric(delays_Apr_25) + as.numeric(delays_May_25),
         post_delayed_beddays = as.numeric(total_delay_beddays_Apr_25) + as.numeric(total_delay_beddays_May_25),
         post_proportion_delayed = post_delays / post_total_discharge,
         post_delay_los = post_delayed_beddays / post_delays) %>% 
  ungroup() %>% 
  select(org_code,pre_proportion_delayed,pre_delay_los,post_proportion_delayed,post_delay_los) %>% 
  filter(str_starts(org_code, "R")) %>% 
  mutate(proportion_delayed_diff = post_proportion_delayed - pre_proportion_delayed,
         delay_los_diff = post_delay_los - pre_delay_los)

output_test_2 <- output_test %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>% 
  filter(proportion_delayed_diff != 0)

output_test_3 <- output_test %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>% 
  filter(delay_los_diff != 0)
