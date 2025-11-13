
# 28/07/2025 - Trust-level beds data extraction
# 13/11/2025 - Final Publication update

# 1 Load packages & filelinks #################################################
library(readxl)
library(curl)

# APRIL 24  
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202404-April-2024-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
apr_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 2,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  mutate(org_code = str_trim(org_code, side = "left")) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% ## QA JH all flags are 1 is that correct?
  mutate(month = 'Apr-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# MAY 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202405-May-2024-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
may_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'May-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# JUNE 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202406-June-2024-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jun_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jun-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# JULY 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202407-July-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jul_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jul-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# AUGUST 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202408-August-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
aug_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Aug-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# SEPTEMBER 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202409-September-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
sep_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Sept-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# OCTOBER 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202410-October-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
oct_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Oct-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# NOVEMBER 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202411-November-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
nov_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Nov-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# DECEMBER 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202412-December-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
dec_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Dec-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# JANUARY 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202501-January-2025-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jan_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jan-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# FEBRUARY 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202502-February-2025-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
feb_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Feb-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# MARCH 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202503-March-2025-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
mar_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Mar-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# APRIL 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/202504-April-2025-beds-sitrep-data-finalversion.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
apr_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Apr-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# MAY 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202505-May-2025-beds-sitrep-data-finalversion.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
may_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'May-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# JUN 25
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/202506-June-2025-beds-sitrep-data-finalversion.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jun_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jun-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# JUL 25
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/202506-July-2025-beds-sitrep-data-finalversion-v2.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jul_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jul-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# AUG 25
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/10/202508-August-2025-beds-sitrep-data-finalversion-v1.1.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
aug_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Aug-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# SEP 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/11/202509-September-2025-beds-sitrep-data-finalversion.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
sep_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`,
         occupied_beds = `G&A beds occupied`,
         occupancy_rate = `G&A occupancy rate`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Sep-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds,occupied_beds,occupancy_rate)

# Bind bed data ###############################################################
hospital_beds <- rbind(apr_24_beds,
                       may_24_beds,
                       jun_24_beds,
                       jul_24_beds,
                       aug_24_beds,
                       sep_24_beds,
                       oct_24_beds,
                       nov_24_beds,
                       dec_24_beds,
                       jan_25_beds,
                       feb_25_beds,
                       mar_25_beds,
                       apr_25_beds,
                       may_25_beds,
                       jun_25_beds,
                       jul_25_beds,
                       aug_25_beds,
                       sep_25_beds)

# 2 Proportion of beds used for DD data #######################################
# Convert month columns to date before joining
dd_file_acute_trusts_FINAL <- dd_file_acute_trusts_FINAL %>%
  mutate(month = my(as.character(month)))

hospital_beds <- hospital_beds %>%
  mutate(month = my(as.character(month)))

figure_1b_data <- left_join(dd_file_acute_trusts_FINAL,hospital_beds,by=c('month','org_code')) %>% 
  mutate(days_in_month = days_in_month(month)) %>% 
  filter(dd_bed_days > 0, # only select out trusts which have delayed discharges
         !is.na(acute_beds)) %>%  # remove one trust which doesn't have some bed info for Nov/Dec due to merger
  group_by(month) %>% 
  summarise(dd_bed_days = sum(as.numeric(dd_bed_days)),
            total_acute_beds = sum(acute_beds),
            total_adult_beds = sum(adult_acute_beds),
            days_in_month = max(days_in_month)) %>% 
  ungroup() %>% 
  mutate(perc_bed_delays = (dd_bed_days/days_in_month)/total_adult_beds*100)

# Figure 1 data
figure_1_data <- dd_file_national_FINAL %>%
  select(month, org_code, perc_patients_delayed, patients_discharged_volume)

figure_1_data$perc_bed_delays <- figure_1b_data$perc_bed_delays

# 3 Variation by trust in change of the % of discharges delayed DATA ##########
# Convert back to month format for figure 2
dd_file_acute_trusts_FINAL <- dd_file_acute_trusts_FINAL %>%
  mutate(month = format(month, "%b-%y"))

hospital_beds <- hospital_beds %>%
  mutate(month = format(month, "%b-%y"))

figure_2b_data <- left_join(dd_file_acute_trusts_FINAL,hospital_beds,by=c('month','org_code')) %>% 
  filter(month %in% c('Jul-24','Aug-24','Sep-24','Jul-25','Aug-25','Sep-25')) %>% 
  mutate(period = if_else(month %in% c('Jul-24','Aug-24','Sep-24'),'pre','post')) %>% 
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

# 4 Variation by trust in change in average delay length DATA #################
figure_2c_data <- left_join(dd_file_acute_trusts_FINAL,hospital_beds,by=c('month','org_code')) %>% 
  filter(month %in% c('Jul-24','Aug-24','Sep-24','Jul-25','Aug-25','Sep-25')) %>% 
  mutate(period = if_else(month %in% c('Jul-24','Aug-24','Sep-24'),'pre','post')) %>% 
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

# 5 Variation by trust in change in the % of bed days used by delayed discharge DATA #####
figure_2_data <- left_join(dd_file_acute_trusts_FINAL,hospital_beds,by=c('month','org_code')) %>% 
  filter(month %in% c('Jul-24','Aug-24','Sep-24','Jul-25','Aug-25','Sep-25')) %>% 
  mutate(period = if_else(month %in% c('Jul-24','Aug-24','Sep-24'),'pre','post')) %>% 
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

figure_2_data <- figure_2_data %>%
  ungroup() %>%
  mutate(pre_median = median(pre),
         post_median = median(post),
         median_difference = median(difference),
           
         pre_mean = mean(pre),
         post_mean = mean(post),
         mean_difference = mean(difference))

# Count number of improving trusts vs 
trust_change <- figure_2_data %>%
  mutate(change_type = case_when(
    difference > 0 ~ "Positive",
    difference < 0 ~ "Negative",
    TRUE ~ "Zero")) %>%
  count(change_type)

trust_change <- trust_change %>%
  mutate(Total = sum(n),
         pct_of_trusts = (n/Total))

# 6 Ranking trusts by the % of bed days used by delayed discharge DATA ########
figure_3_data <- figure_2_data %>% 
  mutate(rank_pre = min_rank(pre),
         rank_post = min_rank(post),
         rank_change = min_rank(difference))

# 7 Ranking trusts by the % of discharges that are delayed DATA ###############
## QA JH introducing NA here
dd_file_acute_trusts_FINAL[dd_file_acute_trusts_FINAL == 0] <- NA
dd_file_acute_trusts_FINAL[ , -c(1, 2)] <- lapply(dd_file_acute_trusts_FINAL[ , -c(1, 2)], as.numeric)

dd_file_acute_trusts_FINAL <- dd_file_acute_trusts_FINAL %>%
  group_by(org_code) %>%
  mutate(total_delay_volume = rowSums(across(c(
    '1_day_delay_volume',
    '2_3_day_delay_volume',
    '4_6_day_delay_volume',
    '7_13_day_delay_volume',
    '14_20_day_delay_volume',
    '21plus_day_delay_volume'), ~ .x), na.rm = FALSE),
    perc_patients_delayed = ((total_delay_volume/patients_discharged_volume) *100))

figure_3b_data <- left_join(dd_file_acute_trusts_FINAL,hospital_beds,by=c('month','org_code')) %>% 
  filter(month %in% c('Jul-24','Aug-24','Sep-24','Jul-25','Aug-25','Sep-25')) %>% 
  mutate(period = if_else(month %in% c('Jul-24','Aug-24','Sep-24'),'pre','post')) %>% 
  mutate(month = my(month),
         days_in_month = days_in_month(month)) %>%
  filter(dd_bed_days > 0, #only select out trusts which have delayed discharges
         !is.na(acute_beds)) #remove one trust which doesn't have some bed info for Nov/Dec due to merger

# Figure 1 data
figure_1_data <- dd_file_national_FINAL %>%
  select(month, org_code, perc_patients_delayed, patients_discharged_volume)

figure_1_data$perc_bed_delays <- figure_1b_data$perc_bed_delays

# Greater than 2.5% reduction
greater_than_2.5p_reduction <- figure_3_data %>%
  select(org_code,difference) %>%
  filter(difference <= -2.5)

best_trusts_frame <- as.data.frame(best_trusts <- c(greater_than_2.5p_reduction$org_code))
best_trusts <- (best_trusts <- c(greater_than_2.5p_reduction$org_code))

# Remove RNS and RVR as lacking DRD data for recent months 
# and remove RAX and RJR for inconsistent performance over the long term (see plot)
remove_orgs <- c('RNS','RAX','RJR','RVR')
best_trusts <- best_trusts[!best_trusts %in% remove_orgs]
best_trusts_frame <- best_trusts_frame %>%
  rename(org_code = 'best_trusts <- c(greater_than_2.5p_reduction$org_code)') %>%
  filter(!org_code %in% remove_orgs)

# Hospital bed rankings
hospital_bed_change <- hospital_beds %>%
  filter(org_code %in% best_trusts) %>%
  group_by(org_code) %>%
  summarize(
    Pre_beds = mean(acute_beds[month %in% c("Jul-24","Aug-24","Sep-24")]),
    Post_beds = mean(acute_beds[month %in% c("Jul-25","Aug-25","Sep-25")]),
    difference = (Post_beds-Pre_beds),
    pct_difference = ((Post_beds-Pre_beds)/Pre_beds))

# Mean of means
overall_means <- hospital_bed_change %>%
  summarize(
    avgbest_Pre_beds = mean(Pre_beds, na.rm = TRUE),
    avgbest_Post_beds = mean(Post_beds, na.rm = TRUE))

# Median beds for ALL trusts
all_trusts_beds <- hospital_beds %>%
  summarize(
    All_Median_Pre_beds = median(acute_beds[month %in% c("Jul-24","Aug-24","Sep-24")], na.rm = TRUE),
    All_Median_Post_beds = median(acute_beds[month %in% c("Jul-25","Aug-25","Sep-25")], na.rm = TRUE))

figure_4b_data <- bind_cols(hospital_bed_change, all_trusts_beds, overall_means)

# Clean #######################################################################
rm(url)
rm(apr_24_beds)
rm(may_24_beds)
rm(jun_24_beds)
rm(jul_24_beds)
rm(aug_24_beds)
rm(sep_24_beds)
rm(oct_24_beds)
rm(nov_24_beds)
rm(dec_24_beds)
rm(jan_25_beds)
rm(feb_25_beds)
rm(mar_25_beds)
rm(apr_25_beds)
rm(may_25_beds)
rm(jun_25_beds)
rm(jul_25_beds)
rm(aug_25_beds)
rm(sep_25_beds)
rm(greater_than_2.5p_reduction)

