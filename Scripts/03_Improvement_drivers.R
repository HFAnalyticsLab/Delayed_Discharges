
# 03/09/2025 - Improvement drivers
# 13/11/2025 - Final Publication update

# Rate of bed occupancy ########################################################

# Mean occupancy for best trusts
best_trust_occupancy <- hospital_beds %>%
  filter(org_code %in% best_trusts) %>%
  group_by(org_code) %>%
  summarize(
    Pre_occupancy = mean(occupancy_rate[month %in% c("Jul-24","Aug-24","Sep-24")]),
    Post_occupancy = mean(occupancy_rate[month %in% c("Jul-25","Aug-25","Sep-25")]),
    Pre_beds = mean(acute_beds[month %in% c("Jul-24","Aug-24","Sep-24")]),
    Post_beds = mean(acute_beds[month %in% c("Jul-25","Aug-25","Sep-25")]))

# Mean of means
overall_means <- best_trust_occupancy %>%
  summarize(
    avgbest_Pre_occupancy = mean(Pre_occupancy, na.rm = TRUE),
    avgbest_Post_occupancy = mean(Post_occupancy, na.rm = TRUE),
    avgbest_Pre_beds = mean(Pre_beds, na.rm = TRUE),
    avgbest_Post_beds = mean(Post_beds, na.rm = TRUE))

# Median occupancy for ALL trusts
all_trusts_occupancy <- hospital_beds %>%
  summarize(
    All_Median_Pre_occupancy = median(occupancy_rate[month %in% c("Jul-24","Aug-24","Sep-24")], na.rm = TRUE),
    All_Median_Post_occupancy = median(occupancy_rate[month %in% c("Jul-25","Aug-25","Sep-25")], na.rm = TRUE),
    All_Median_Pre_beds = median(acute_beds[month %in% c("Jul-24","Aug-24","Sep-24")], na.rm = TRUE),
    All_Median_Post_beds = median(acute_beds[month %in% c("Jul-25","Aug-25","Sep-25")], na.rm = TRUE))

figure_4_data <- bind_cols(best_trust_occupancy, all_trusts_occupancy, overall_means)

figure_4_data <- figure_4_data %>%
  mutate(occupancy_change_best_trusts =  (Post_occupancy-Pre_occupancy),
         occupancy_change_all_trusts = (All_Median_Post_occupancy-All_Median_Pre_occupancy),
         beds_change_best_trusts = (Post_beds-Pre_beds)/Pre_beds,
         beds_change_all_trusts = (All_Median_Post_beds-All_Median_Pre_beds)/All_Median_Pre_beds)
         
# Delay length ################################################################

# Mean delay length for best trusts
best_trust_delay <- dd_file_acute_trusts_FINAL %>%
  filter(org_code %in% best_trusts) %>%
  select(month,org_code,average_delay_los_minus_0_day_delay) %>%
  group_by(org_code) %>%
  summarize(
    Pre_delay = mean(average_delay_los_minus_0_day_delay[month %in% c("Jul-24","Aug-24","Sep-24")]),
    Post_delay = mean(average_delay_los_minus_0_day_delay[month %in% c("Jul-25","Aug-25","Sep-25")]))

# Mean of means
overall_means <- best_trust_delay %>%
  summarize(
    avgbest_Pre_delay = mean(Pre_delay, na.rm = TRUE),
    avgbest_Post_delay = mean(Post_delay, na.rm = TRUE))

# Median delay length for ALL trusts
all_trusts_delay <- dd_file_acute_trusts_FINAL %>%
  ungroup() %>%
  summarize(
    All_Median_Pre_delay = median(average_delay_los_minus_0_day_delay[month %in% c("Jul-24","Aug-24","Sep-24")], na.rm = TRUE),
    All_Median_Post_delay = median(average_delay_los_minus_0_day_delay[month %in% c("Jul-25","Aug-25","Sep-25")], na.rm = TRUE))

figure_7_data <- bind_cols(best_trust_delay, all_trusts_delay, overall_means)

# Staffing #####################################################################

# APRIL 24 
url <- "https://files.digital.nhs.uk/A9/97B711/NHS%20Workforce%20Statistics%2C%20April%202024%20England%20and%20Organisation.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
apr_24_staffing <- read_excel(temp_file, sheet = 4,skip=5) %>% 
  rename(org_code = `Organisation code`,
         doctors = `HCHS Doctors`,
         nurses = `Nurses & health visitors`) %>% 
  mutate(org_code = str_trim(org_code, side = "left")) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Apr-24') %>% 
  select(month,org_code,doctors,nurses)

# MAY 24 
url <- "https://files.digital.nhs.uk/BF/3AEE39/NHS%20Workforce%20Statistics%2C%20May%202024%20England%20and%20Organisation.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
may_24_staffing <- read_excel(temp_file, sheet = 4,skip=5) %>% 
  rename(org_code = `Organisation code`,
         doctors = `HCHS Doctors`,
         nurses = `Nurses & health visitors`) %>% 
  mutate(org_code = str_trim(org_code, side = "left")) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'May-24') %>% 
  select(month,org_code,doctors,nurses)

# JUNE 24 
url <- "https://files.digital.nhs.uk/73/FFEA9E/NHS%20Workforce%20Statistics%2C%20June%202024%20England%20and%20Organisation.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jun_24_staffing <- read_excel(temp_file, sheet = 4,skip=5) %>% 
  rename(org_code = `Organisation code`,
         doctors = `HCHS Doctors`,
         nurses = `Nurses & health visitors`) %>% 
  mutate(org_code = str_trim(org_code, side = "left")) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jun-24') %>% 
  select(month,org_code,doctors,nurses)

# JULY 24 
url <- "https://files.digital.nhs.uk/61/0281D4/NHS%20Workforce%20Statistics%2C%20July%202024%20England%20and%20Organisation.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jul_24_staffing <- read_excel(temp_file, sheet = 4,skip=5) %>% #ft - changed to jul to be consistent w 25
  rename(org_code = `Organisation code`,
         doctors = `HCHS Doctors`,
         nurses = `Nurses & health visitors`) %>% 
  mutate(org_code = str_trim(org_code, side = "left")) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jul-24') %>% 
  select(month,org_code,doctors,nurses)

# AUGUST 24 
url <- "https://files.digital.nhs.uk/8C/709E19/NHS%20Workforce%20Statistics%2C%20August%202024%20England%20and%20Organisation.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
august_24_staffing <- read_excel(temp_file, sheet = 4,skip=5) %>% 
  rename(org_code = `Organisation code`,
         doctors = `HCHS Doctors`,
         nurses = `Nurses & health visitors`) %>% 
  mutate(org_code = str_trim(org_code, side = "left")) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Aug-24') %>% 
  select(month,org_code,doctors,nurses)

# APRIL 25
url <- "https://files.digital.nhs.uk/C4/0B7A35/NHS%20Workforce%20Statistics%2C%20April%202025%20England%20and%20Organisation.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
apr_25_staffing <- read_excel(temp_file, sheet = 4,skip=5) %>% 
  rename(org_code = `Organisation code`,
         doctors = `HCHS Doctors`,
         nurses = `Nurses & health visitors`) %>% 
  mutate(org_code = str_trim(org_code, side = "left")) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Apr-25') %>% 
  select(month,org_code,doctors,nurses)

# MAY 25 
url <- "https://files.digital.nhs.uk/00/BB260E/NHS%20Workforce%20Statistics%2C%20May%202025%20England%20and%20Organisation.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
may_25_staffing <- read_excel(temp_file, sheet = 4,skip=5) %>% 
  rename(org_code = `Organisation code`,
         doctors = `HCHS Doctors`,
         nurses = `Nurses & health visitors`) %>% 
  mutate(org_code = str_trim(org_code, side = "left")) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'May-25') %>% 
  select(month,org_code,doctors,nurses)

# JUN 25 
url <- "https://files.digital.nhs.uk/DB/25526E/NHS%20Workforce%20Statistics%2C%20June%202025%20England%20and%20Organisation.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jun_25_staffing <- read_excel(temp_file, sheet = 4,skip=5) %>% 
  rename(org_code = `Organisation code`,
         doctors = `HCHS Doctors`,
         nurses = `Nurses & health visitors`) %>% 
  mutate(org_code = str_trim(org_code, side = "left")) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jun-25') %>% 
  select(month,org_code,doctors,nurses)

# JUL 25 
url <- "https://files.digital.nhs.uk/68/4F6A1E/NHS%20Workforce%20Statistics%2C%20July%202025%20England%20and%20Organisation.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jul_25_staffing <- read_excel(temp_file, sheet = 4,skip=5) %>% 
  rename(org_code = `Organisation code`,
         doctors = `HCHS Doctors`,
         nurses = `Nurses & health visitors`) %>% 
  mutate(org_code = str_trim(org_code, side = "left")) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jul-25') %>% 
  select(month,org_code,doctors,nurses)



# Pathway Data ################################################################

# APRIL 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/05/Daily-discharge-sitrep-monthly-data-webfile-April2024.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
apr_24_pathway <- read_excel(temp_file, sheet = 6, skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = rowSums(select(., starts_with("P0"))),
         pathway_one = rowSums(select(., starts_with("P1"))),
         pathway_two = rowSums(select(., starts_with("P2"))),
         pathway_three = rowSums(select(., starts_with("P3")))) %>% 
  mutate(month = 'Apr-24') %>% 
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# MAY 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/07/Daily-discharge-sitrep-monthly-data-webfile-May2024-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
may_24_pathway <- read_excel(temp_file, sheet = 6, skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = rowSums(select(., starts_with("P0"))),
         pathway_one = rowSums(select(., starts_with("P1"))),
         pathway_two = rowSums(select(., starts_with("P2"))),
         pathway_three = rowSums(select(., starts_with("P3")))) %>% 
  mutate(month = 'May-24') %>% 
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# JUNE 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/08/Daily-discharge-sitrep-monthly-data-webfile-June2024-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
june_24_pathway <- read_excel(temp_file, sheet = 6, skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = `Pathway 0 Total`,
         pathway_one = `Pathway 1 Total`,
         pathway_two = `Pathway 2 Total`,
         pathway_three = `Pathway 3 Total`) %>% 
  mutate(month = 'Jun-24') %>% 
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# JULY 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/08/Daily-discharge-sitrep-monthly-data-webfile-July2024.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
july_24_pathway <- read_excel(temp_file, sheet = 6, skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = `Pathway 0 Total`,
         pathway_one = `Pathway 1 Total`,
         pathway_two = `Pathway 2 Total`,
         pathway_three = `Pathway 3 Total`) %>% 
  mutate(month = 'Jul-24') %>% #FT - I changed this from July-24 to Jul-24 as all the later code uses Jul-24
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# AUGUST 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/10/Daily-discharge-sitrep-monthly-data-webfile-August2024-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
august_24_pathway <- read_excel(temp_file, sheet = 6, skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = `Pathway 0 Total`,
         pathway_one = `Pathway 1 Total`,
         pathway_two = `Pathway 2 Total`,
         pathway_three = `Pathway 3 Total`) %>% 
  mutate(month = 'Aug-24') %>% 
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# SEP 24 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/10/Daily-discharge-sitrep-monthly-data-webfile-September2024v2.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
september_24_pathway <- read_excel(temp_file, sheet = 6, skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = `Pathway 0 Total`,
         pathway_one = `Pathway 1 Total`,
         pathway_two = `Pathway 2 Total`,
         pathway_three = `Pathway 3 Total`) %>% 
  mutate(month = 'Sep-24') %>% 
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# APRIL 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/05/Daily-discharge-sitrep-monthly-data-webfile-April2025.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
april_25_pathway <- read_excel(temp_file, sheet = 'Table 4', skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = `Pathway 0 Total`,
         pathway_one = `Pathway 1 Total`,
         pathway_two = `Pathway 2 Total`,
         pathway_three = `Pathway 3 Total`) %>% 
  mutate(month = 'Apr-25') %>% 
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# MAY 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/Daily-discharge-sitrep-monthly-data-webfile-May2025.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
may_25_pathway <- read_excel(temp_file, sheet = 6, skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = `Pathway 0 Total`,
         pathway_one = `Pathway 1 Total`,
         pathway_two = `Pathway 2 Total`,
         pathway_three = `Pathway 3 Total`) %>% 
  mutate(month = 'May-25') %>% 
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# JUNE 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/Daily-discharge-sitrep-monthly-data-webfile-June2025.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
june_25_pathway <- read_excel(temp_file, sheet = 6, skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = `Pathway 0 Total`,
         pathway_one = `Pathway 1 Total`,
         pathway_two = `Pathway 2 Total`,
         pathway_three = `Pathway 3 Total`) %>% 
  mutate(month = 'Jun-25') %>% 
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# JULY 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/Daily-discharge-sitrep-monthly-data-webfile-July2025.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
july_25_pathway <- read_excel(temp_file, sheet = 6, skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = `Pathway 0 Total`,
         pathway_one = `Pathway 1 Total`,
         pathway_two = `Pathway 2 Total`,
         pathway_three = `Pathway 3 Total`) %>% 
  mutate(month = 'July-25') %>% 
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# AUGUST 25 
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/Daily-discharge-sitrep-monthly-data-webfile-August2025.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
august_25_pathway <- read_excel(temp_file, sheet = 6, skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = `Pathway 0 Total`,
         pathway_one = `Pathway 1 Total`,
         pathway_two = `Pathway 2 Total`,
         pathway_three = `Pathway 3 Total`) %>% 
  mutate(month = 'Aug-25') %>% 
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# SEP 25
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/10/Daily-discharge-sitrep-monthly-data-webfile-September2025v1.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
september_25_pathway <- read_excel(temp_file, sheet = 6, skip=4) %>% 
  rename(org_code = `...2`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>% 
  mutate(across(-org_code, ~ as.numeric(ifelse(grepl("^-?[0-9.]+$", .x), .x, 0)))) %>% 
  mutate(pathway_zero = `Pathway 0 Total`,
         pathway_one = `Pathway 1 Total`,
         pathway_two = `Pathway 2 Total`,
         pathway_three = `Pathway 3 Total`) %>% 
  mutate(month = 'Sep-25') %>% 
  select(month,org_code,pathway_zero,pathway_one,pathway_two,pathway_three)

# Join datasets and export ####################################################

# Staffing - May-Jul 24 vs May-Jul 25
# This is latest data available - will update on 27th Nov 2025
all_trust_staffing <- may_24_staffing %>%
  rbind(jun_24_staffing, jul_24_staffing, may_25_staffing, jun_25_staffing, jul_25_staffing) %>% #FT - changed this 
  group_by(org_code)
  
all_trusts_staff_medians <- all_trust_staffing %>%
  ungroup() %>%
  summarize(
    All_Median_Pre_nurses = median(nurses[month %in% c("May-24", "Jun-24", "Jul-24")], na.rm = TRUE),
    All_Median_Pre_doctors = median(doctors[month %in% c("May-24", "Jun-24", "Jul-24")], na.rm = TRUE),
    All_Median_Post_nurses = median(nurses[month %in% c("May-25", "Jun-25", "Jul-25")], na.rm = TRUE),
        All_Median_Post_doctors = median(doctors[month %in% c("May-25", "Jun-25", "Jul-25")], na.rm = TRUE))

best_trust_staff <- all_trust_staffing %>%
  filter(org_code %in% best_trusts) %>%
  group_by(org_code) %>%
  summarize(
    Pre_doctors = mean(doctors[month %in% c("May-24","Jun-24", "Jul-24")]),
    Pre_nurses = mean(nurses[month %in% c("May-24","Jun-24", "Jul-24")]),
    Post_doctors = mean(doctors[month %in% c("May-25","Jun-25", "Jul-25")]),
    Post_nurses = mean(nurses[month %in% c("May-25","Jun-25", "Jul-25")]))

overall_means <- best_trust_staff %>%
  summarize(
    avgbest_Pre_nurses = mean(Pre_nurses, na.rm = TRUE),
    avgbest_Post_nurses = mean(Post_nurses, na.rm = TRUE),
    avgbest_Pre_doctors = mean(Pre_doctors, na.rm = TRUE),
    avgbest_Post_doctors = mean(Post_doctors, na.rm = TRUE))

figure_5_data <- bind_cols(best_trust_staff, all_trusts_staff_medians, overall_means)

figure_5_data <- figure_5_data %>%
  mutate(doc_change_best_trusts =  (Post_doctors-Pre_doctors)/Pre_doctors,
         nurse_change_best_trusts = (Post_nurses-Pre_nurses)/Pre_nurses,
         doc_change_all_trusts = (All_Median_Post_doctors-All_Median_Pre_doctors)/All_Median_Pre_doctors,
         nurse_change_all_trusts = (All_Median_Post_nurses-All_Median_Pre_nurses)/All_Median_Pre_nurses)

# Discharge destination - Jul-Sep 24 vs 25
all_trusts_destination <- july_24_pathway %>%
  rbind(august_24_pathway, september_24_pathway, july_25_pathway, august_25_pathway, september_25_pathway) 

all_trusts_destination_medians <- all_trusts_destination %>%
  ungroup() %>%
  summarize(
    All_Median_Pre_Pathway_1 = median(pathway_one[month %in% c("Jul-24", "Aug-24","Sep-24")], na.rm = TRUE),
    All_Median_Post_Pathway_1 = median(pathway_one[month %in% c("Jul-25", "Aug-25", "Sep-25")], na.rm = TRUE),
    
    All_Median_Pre_Pathway_2 = median(pathway_two[month %in% c("Jul-24", "Aug-24","Sep-24")], na.rm = TRUE),
    All_Median_Post_Pathway_2 = median(pathway_two[month %in% c("Jul-25", "Aug-25", "Sep-25")], na.rm = TRUE),
    
    All_Median_Pre_Pathway_3 = median(pathway_three[month %in% c("Jul-24", "Aug-24","Sep-24")], na.rm = TRUE),
    All_Median_Post_Pathway_3 = median(pathway_three[month %in% c("Jul-25", "Aug-25", "Sep-25")], na.rm = TRUE))
    
best_trusts_destination <- all_trusts_destination %>%
  filter(org_code %in% best_trusts) %>%
  group_by(org_code) %>%
  summarize(
  pre_pathway_1 = mean(pathway_one[month %in% c("Jul-24", "Aug-24","Sep-24")], na.rm = TRUE),
  pre_pathway_2 = mean(pathway_two[month %in% c("Jul-24", "Aug-24","Sep-24")], na.rm = TRUE),
  pre_pathway_3 = mean(pathway_three[month %in% c("Jul-24", "Aug-24","Sep-24")], na.rm = TRUE),
  
  post_pathway_1 = mean(pathway_one[month %in% c("Jul-25", "Aug-25", "Sep-25")], na.rm = TRUE),
  post_pathway_2 = mean(pathway_two[month %in% c("Jul-25", "Aug-25", "Sep-25")], na.rm = TRUE),
  post_pathway_3 = mean(pathway_three[month %in% c("Jul-25", "Aug-25", "Sep-25")], na.rm = TRUE))

overall_means <- best_trusts_destination %>%
    summarize(
      avgbest_Pre_pathway_1 = mean(pre_pathway_1, na.rm = TRUE),
      avgbest_Post_pathway_1 = mean(post_pathway_1, na.rm = TRUE),
      
      avgbest_Pre_pathway_2 = mean(pre_pathway_2, na.rm = TRUE),
      avgbest_Post_pathway_2 = mean(post_pathway_2, na.rm = TRUE),
      
      avgbest_Pre_pathway_3 = mean(pre_pathway_3, na.rm = TRUE),
      avgbest_Post_pathway_3 = mean(post_pathway_3, na.rm = TRUE))
  
figure_6_data <- bind_cols(best_trusts_destination, all_trusts_destination_medians, overall_means)
  
# Clean #######################################################################
rm(all_trusts_destination_medians)
rm(all_trusts_staff_medians)
rm(apr_24_pathway)
rm(apr_24_staffing)
rm(apr_25_staffing)
rm(april_25_pathway)
rm(august_24_pathway)
rm(august_24_staffing)
rm(september_24_pathway)
rm(july_24_pathway)
rm(jul_24_staffing)
rm(july_25_pathway)
rm(august_25_pathway)
rm(september_25_pathway)
rm(jul_25_staffing)
rm(june_24_pathway)
rm(jun_24_staffing)
rm(june_25_pathway)
rm(may_24_pathway)
rm(may_24_staffing)
rm(may_25_pathway)
rm(may_25_staffing)
rm(jun_25_staffing)
rm(url)
