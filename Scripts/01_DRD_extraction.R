
# 16/06/2025 - Discharge Ready Date (DRD) & Beds data extraction
# 05/08/2025 - Cleaned

# Clear environment

rm(list = ls())

# 1 Load packages & filelinks #################################################

library (httr)
library (stringr)
library (aws.s3)
library (tidyverse)
library (dplyr)
library (data.table)
library (purrr)
library (lubridate)
library (readr)
library (readxl)
library (here)
library (janitor)
library (openxlsx)
library (scales)
library (colorspace)
library (writexl)
library (zoo)

# Load functions

source('https://raw.githubusercontent.com/zeyadissa/open_health_data/main/src/functions.R')

# 2 Read in DRD data ##########################################################

DRD_url <- ('https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays/discharge-ready-date/')

# Zeyad's function captures 12 files

DRDlinks <- GetLinks(DRD_url, 'Discharge-Ready-Date-monthly-data-webfile')
print(DRDlinks)

# Remaining file links

DRD_Sep23 <- ('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/01/Discharge-Ready-Date-monthly-data-Sept-2023-revised.xlsx')
DRD_Oct23 <- ('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/01/Discharge-Ready-Date-monthly-data-Oct-2023-revised.xlsx')
DRD_Dec23 <- ('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/01/Discharge-Ready-Date-monthly-data-Dec-2023-revised.xlsx')
DRD_Jan24 <- ('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/01/Discharge-Ready-Date-monthly-data-Jan-2024-revised.xlsx')
DRD_Feb24 <- ('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/01/Discharge-Ready-Date-monthly-data-Feb-2024-revised.xlsx')
DRD_Mar24 <- ('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/01/Discharge-Ready-Date-monthly-data-March-2024-revised.xlsx')
DRD_Jul24 <- ('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/01/Discharge-Ready-Date-monthly-data-July-2024-revised-1.xlsx')
DRD_Aug24 <- ('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/01/Discharge-Ready-Date-monthly-data-Aug-2024-revised.xlsx')

Remaining_DRD <- c(DRD_Sep23, DRD_Oct23, DRD_Dec23, DRD_Jan24, DRD_Feb24, DRD_Mar24, DRD_Jul24, DRD_Aug24)
DRDlinks <- c(DRDlinks, Remaining_DRD) 

# Tidy file links

rm(Remaining_DRD)
rm(DRD_Sep23) 
rm(DRD_Oct23) 
rm(DRD_Dec23)
rm(DRD_Jan24)
rm(DRD_Feb24) 
rm(DRD_Mar24) 
rm(DRD_Jul24)
rm(DRD_Aug24)

# Check if the file exists

sapply(DRDlinks, function(link) {
  status_code(HEAD(link))
})

# Names files Month/Year (only works for webfiles)

names_vec <- str_extract(DRDlinks, "(?<=webfile-)[A-Za-z]+[-]?[0-9]{4}")
names_vec <- str_replace_all(names_vec, "-", "")

# Replace NA values with fallback (NA1, NA2)
na_indices <- which(is.na(names_vec))
names_vec[na_indices] <- paste0("NA", seq_along(na_indices))
print(names_vec)

DRD_data_list <- setNames(vector("list", length(DRDlinks)), names_vec)

# Download 'Provider' sheets
for (i in seq_along(DRDlinks)) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(DRDlinks[i], temp_file, mode = "wb")
  df <- read_excel(temp_file, sheet = 'Provider')
  assign(names_vec[i], df, envir = .GlobalEnv)
}

# 3 Individual DRD cleaning ###################################################

# Sep 23
Sep_23 <- NA1

Sep_23 <- Sep_23 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Sep_23 <- Sep_23[, sapply(Sep_23, function(col) !all(is.na(col)))]

# Oct 23
Oct_23 <- NA2

Oct_23 <- Oct_23 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Oct_23 <- Oct_23[, sapply(Oct_23, function(col) !all(is.na(col)))]

# Nov 23 - Appears to lack Region, ICB and Data Source, thus 19 variables
Nov_23 <- November2023

Nov_23 <- Nov_23 %>%
  {
    header_row_index <- which(.[[1]] == "Code")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Nov_23 <- Nov_23[, sapply(Nov_23, function(col) !all(is.na(col)))]

# Dec 23
Dec_23 <- NA3

Dec_23 <- Dec_23 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Dec_23 <- Dec_23[, sapply(Dec_23, function(col) !all(is.na(col)))]

# Jan 24
Jan_24 <- NA4

Jan_24 <- Jan_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Jan_24 <- Jan_24[, sapply(Jan_24, function(col) !all(is.na(col)))]

# Feb 24
Feb_24 <- NA5

Feb_24 <- Feb_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Feb_24 <- Feb_24[, sapply(Feb_24, function(col) !all(is.na(col)))]

# Mar 24
Mar_24 <- NA6

Mar_24 <- Mar_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Mar_24 <- Mar_24[, sapply(Mar_24, function(col) !all(is.na(col)))]

# Apr 24
Apr_24 <- April2024

Apr_24 <- Apr_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

#Apr_24 <- Apr_24[-c(40)] # April 2024 appears to have additional code column
Apr_24 <- Apr_24[, sapply(Apr_24, function(col) !all(is.na(col)))]
Apr_24 <- Apr_24[, -ncol(Apr_24)]

# May 24
May_24 <- May2024

May_24 <- May_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

May_24 <- May_24[, sapply(May_24, function(col) !all(is.na(col)))]

# Jun 24
Jun_24 <- June2024

Jun_24 <- Jun_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Jun_24 <- Jun_24[, sapply(Jun_24, function(col) !all(is.na(col)))]

# Jul 24
Jul_24 <- July2024

Jul_24 <- Jul_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Jul_24 <- Jul_24[, sapply(Jul_24, function(col) !all(is.na(col)))]

# Aug 24
Aug_24 <- August2024

Aug_24 <- Aug_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Aug_24 <- Aug_24[, sapply(Aug_24, function(col) !all(is.na(col)))]

# Sep 24
Sep_24 <- September2024

Sep_24 <- Sep_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Sep_24 <- Sep_24[, sapply(Sep_24, function(col) !all(is.na(col)))]

# Oct 24
Oct_24 <- October2024

Oct_24 <- Oct_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Oct_24 <- Oct_24[, sapply(Oct_24, function(col) !all(is.na(col)))]

# Nov 24
Nov_24 <- November2024

Nov_24 <- Nov_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Nov_24 <- Nov_24[, sapply(Nov_24, function(col) !all(is.na(col)))]

# Dec 24
Dec_24 <- December2024

Dec_24 <- Dec_24 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Dec_24 <- Dec_24[, sapply(Dec_24, function(col) !all(is.na(col)))]

# Jan 25
Jan_25 <- January2025

Jan_25 <- Jan_25 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Jan_25 <- Jan_25[, sapply(Jan_25, function(col) !all(is.na(col)))]

# Appear to lose North Middlesex University Hospital in Jan 2025 - merged with Royal Free London.
setdiff(Dec_24$`Organisation Name`, Jan_25$`Organisation Name`)

# Feb 25
Feb_25 <- February2025

Feb_25 <- Feb_25 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Feb_25 <- Feb_25[, sapply(Feb_25, function(col) !all(is.na(col)))]

# Mar 25
Mar_25 <- March2025

Mar_25 <- Mar_25 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Mar_25 <- Mar_25[, sapply(Mar_25, function(col) !all(is.na(col)))]

# Apr 25
Apr_25 <- April2025

Apr_25 <- Apr_25 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Apr_25 <- Apr_25[, sapply(Apr_25, function(col) !all(is.na(col)))]

# May 25
May_25 <- May2025

May_25 <- May_25 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

May_25 <- May_25[, sapply(May_25, function(col) !all(is.na(col)))]


# Future months of data (uncheck as DRD updated)
# June 25
Jun_25 <- June2025

Jun_25 <- Jun_25 %>%
 {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Jun_25 <- Jun_25[, sapply(Jun_25, function(col) !all(is.na(col)))]

# July 25
Jul_25 <- July2025

Jul_25 <- Jul_25 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Jul_25 <- Jul_25[, sapply(Jul_25, function(col) !all(is.na(col)))]

# August 25
#Aug_25 <- August2025
#
#Aug_25 <- Aug_25 %>%
#  {
#    header_row_index <- which(.[[1]] == "Region")
#    colnames(.) <- as.character(unlist(.[header_row_index, ]))
#    .[(header_row_index + 1):nrow(.), ]
#  }
#
#Aug_25 <- Aug_25[, sapply(Aug_25, function(col) !all(is.na(col)))]
#
# September 25
#Sep_25 <- September2025
#
#Sep_25 <- Sep_25 %>%
#  {
#    header_row_index <- which(.[[1]] == "Region")
#    colnames(.) <- as.character(unlist(.[header_row_index, ]))
#    .[(header_row_index + 1):nrow(.), ]
#  }
#
#Sep_25 <- Sep_25[, sapply(Sep_25, function(col) !all(is.na(col)))]
#
# October 25
#Oct_25 <- October2025
#
#Oct_25 <- Oct_25 %>%
#  {
#    header_row_index <- which(.[[1]] == "Region")
#    colnames(.) <- as.character(unlist(.[header_row_index, ]))
#    .[(header_row_index + 1):nrow(.), ]
#  }
#
#Oct_25 <- Oct_25[, sapply(Oct_25, function(col) !all(is.na(col)))]

# Remove dead sheets

rm(list = c(paste0("NA", 1:8), "df", "April2024", "April2025", "August2024", "December2024", 
            "February2025", "January2025", "July2024", "June2024", "June2025", "March2025", 
            "May2024", "May2025", "November2023", "November2024", "October2024", "September2024", "July2025"))


# 4 Adjust column names ########################################################

colnames_22 <- c('Region', 'ICB', 'org_code', 'Org Name', '# of providers with acceptable data', '% of providers with acceptable data', 'Data Source', 'no_delay_perc',
'delay_perc','1_day_delay_perc', '2_3_day_delay_perc', '4_6_day_delay_perc','7_13_day_delay_perc', '14_20_day_delay_perc','21plus_day_delay_perc', 
'1_day_delay_beddays', '2_3_day_delay_beddays', '4_6_day_delay_beddays','7_13_day_delay_beddays','14_20_day_delay_beddays', '21plus_day_delay_beddays','average_delay_los_minus_0_day_delay')

colnames_39 <- c('Region', 'ICB', 'org_code', 'Org Name', '# of providers with acceptable data', '% of providers with acceptable data', 'Data Source', 'patients_discharged_volume', 'dd_bed_days', 
'no_delay_perc', 'delay_perc',

'no_delay_volume', '1_day_delay_volume', '2_3_day_delay_volume', '4_6_day_delay_volume','7_13_day_delay_volume', '14_20_day_delay_volume','21plus_day_delay_volume',

'no_delay_percent', '1_day_delay_perc', '2_3_day_delay_perc', '4_6_day_delay_perc','7_13_day_delay_perc', '14_20_day_delay_perc','21plus_day_delay_perc',

'delayed_perc_1_day', 'delayed_perc_2_3_days', 'delayed_perc_4_6_days','delayed_perc_7_13_days', 'delayed_perc_14_20_days','delayed_perc_21plus_days', 

'1_day_delay_beddays', '2_3_day_delay_beddays', '4_6_day_delay_beddays','7_13_day_delay_beddays','14_20_day_delay_beddays', '21plus_day_delay_beddays',

'average_delay_los_inc_0_day_delay','average_delay_los_minus_0_day_delay')

# Pre-Apr 2024
colnames(Sep_23) <- colnames_22
colnames(Oct_23) <- colnames_22
colnames(Dec_23) <- colnames_22
colnames(Jan_24) <- colnames_22
colnames(Feb_24) <- colnames_22
colnames(Mar_24) <- colnames_22

# Post-Apr 2024
colnames(Apr_24) <- colnames_39
colnames(May_24) <- colnames_39
colnames(Jun_24) <- colnames_39
colnames(Jul_24) <- colnames_39
colnames(Aug_24) <- colnames_39
colnames(Sep_24) <- colnames_39
colnames(Oct_24) <- colnames_39
colnames(Nov_24) <- colnames_39
colnames(Dec_24) <- colnames_39
colnames(Jan_25) <- colnames_39
colnames(Feb_25) <- colnames_39
colnames(Mar_25) <- colnames_39
colnames(Apr_25) <- colnames_39
colnames(May_25) <- colnames_39
colnames(Jun_25) <- colnames_39
colnames(Jul_25) <- colnames_39

# November 2023
colnames_Nov <- colnames_22
colnames_Nov <- colnames_Nov[-c(1,2,7)]
colnames(Nov_23) <- colnames_Nov

# 5 Pull general and acute beds data ##########################################

beds_timeseries_url <- c('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/Beds-publication-Timeseries-March-2020-August-2025.xlsx')
beds_timeseries <- read.xlsx(beds_timeseries_url, sheet = 'Timeseries all acute trusts')

beds_timeseries <- beds_timeseries %>%
  {
    header_row_index <- which(.[[1]] == 'Month')
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

beds_timeseries <- beds_timeseries[-nrow(beds_timeseries), ]

abbreviate_date <- function(x) {
  paste0(substr(x, 1, 3), substr(x, nchar(x) - 1, nchar(x)))
}

beds_timeseries$Month <- sapply(beds_timeseries$Month, abbreviate_date)

beds_timeseries <- beds_timeseries[, sapply(beds_timeseries, function(col) !all(is.na(col)))]
colnames(beds_timeseries) <- c("Month","G&A beds available","G&A core beds available","G&A escalation beds available","G&A covid void beds",
                               "G&A beds occupied","G&A occupancy rate (%)","G&A occupancy rate adjusted for covid void beds (%)",
                               "Adult G&A beds available","Adult core beds available","Adult escalation beds available",
                               "Adult G&A covid void beds","Adult G&A beds occupied","Adult G&A occupancy rate (%)",
                               "Adult G&A occupancy rate adjusted for covid void beds (%)","Paediatric G&A beds available",
                               "Paediatric core beds available","Paediatric escalation beds available","Paediatric G&A covid void beds",
                               "Paediatric G&A beds occupied","Paediatric G&A occupancy rate (%)","Paediatric G&A occupancy rate adjusted for covid void beds (%)",
                               "Adult critical care beds available","Adult critical care beds occupied","Adult critical care occupancy rate (%)",
                               "Paediatric intensive care beds available","Paediatric intensive care beds occupied",
                               "Paediatric intensive care occupancy rate (%)","Neonatal intensive care beds available",
                               "Neonatal intensive care beds occupied","Neonatal intensive care occupancy rate (%)",
                               "G&A beds occupied by patients with LOS of 7 or more days","G&A beds occupied by patients with LOS of 14 or more days",
                               "G&A beds occupied by patients with LOS of 21 or more days",
                               "G&A beds occupied by patients with LOS of 7 or more days (%)","G&A beds occupied by patients with LOS of 14 or more days (%)",
                               "G&A beds occupied by patients with LOS of 21 or more days (%)")                                         

# 6 England DRD timeseries ##################################################

England_FULL <- read.xlsx("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/Discharge-Ready-Date-Timeseries-webfile-Sept-2023-June-2025.xlsx", sheet = "Timeseries")
England_FULL <- England_FULL %>%
  {
    header_row_index <- which(.[[1]] == "Month")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

England_FULL <- England_FULL[, sapply(England_FULL, function(col) !all(is.na(col)))]

colnames_Eng <- c('Month', '# of providers with acceptable data', '% of providers with acceptable data', 'Total # of patients discharged', 'Total bed days lost to DD', 
                  'DoD is same as DRD (%)', 'DoD is 1+ days after DRD (%)','No delay between DoD & DRD (#)', '1 day delay between DoD & DRD (#)', '2-3 day delay between DoD & DRD (#)', '4-6 day delay between DoD & DRD (#)', '7-13 day delay between DoD & DRD (#)', '14-20 day delay between DoD & DRD (#)', '21+ day delay between DoD & DRD (#)',
                  'No delay between DoD & DRD (%)', '1 day delay between DoD & DRD (%)', '2-3 day delay between DoD & DRD (%)', '4-6 day delay between DoD & DRD (%)', '7-13 day delay between DoD & DRD (%)', '14-20 day delay between DoD & DRD (%)', '21+ day delay between DoD & DRD (%)',
                  'Patients delayed but discharged within 1 day (%)', 'Patients delayed but discharged within 2-3 days (%)', 'Patients delayed but discharged within 4-6 days (%)',
                  'Patients delayed but discharged within 7-13 days (%)', 'Patients delayed but discharged within 14-20 days (%)','Patients delayed but discharged within 21 days or more (%)', 
                  'Total bed days after DRD for patients discharged within 1 day', 'Total bed days after DRD for patients discharged within 2-3 days', 'Total bed days after DRD for patients discharged within 4-6 days', 
                  'Total bed days after DRD for patients discharged within 7-13 days','Total bed days after DRD for patients discharged within 14-20 days', 'Total bed days after DRD for patients discharged within 21 days or more',
                  'Average days from DRD to DoD (inc 0-day delays)','Average days from DRD to DoD (exc 0-day delays)')

colnames(England_FULL) <- colnames_Eng

Month_list <- c("Sep23","Oct23","Nov23","Dec23","Jan24","Feb24","Mar24","Apr24",
                "May24","Jun24","Jul24","Aug24","Sep24","Oct24","Nov24","Dec24",
                "Jan25","Feb25","Mar25","Apr25","May25","Jun25")

England_FULL$Month <- Month_list
print(colnames_Eng)
England_FULL[, -1] <- lapply(England_FULL[, -1], as.numeric)
rownames(England_FULL) <- NULL

England_FULL$Month <- factor(England_FULL$Month, levels = England_FULL$Month)

# 7 Bind beds and England-level DRD ###########################################

# Full month list

Timeframe <- England_FULL$Month
Timeframe

# Remove bed data that doesn't match discharges

DDbeds_timeseries <- beds_timeseries %>%
  filter(Month %in% Timeframe)

# Calculate average bed days for patients with delayed discharge

DDbeds_totalGA <- DDbeds_timeseries$`G&A beds available`
England_FULL$totalGAbeds <- DDbeds_totalGA

England_FULL <- England_FULL %>%
  mutate(parsed_date = my(Month),  
        DaysInMonth = days_in_month(parsed_date))

England_FULL <- England_FULL %>%
  mutate(`bed_delays` = round(`Total bed days lost to DD`/DaysInMonth),
        `pct_bed_delays` = (bed_delays/as.numeric(totalGAbeds)))

# 8 Combine all trust DRD datasets ############################################

# Create a new variable (month) for each dataset.
Sep_23 <- Sep_23 %>% 
  mutate(month = 'Sept-23')
Oct_23 <- Oct_23 %>% 
  mutate(month = 'Oct-23')
Nov_23 <- Nov_23 %>% 
  mutate(month = 'Nov-23')
Dec_23 <- Dec_23 %>% 
  mutate(month = 'Dec-23')

Jan_24 <- Jan_24 %>% 
  mutate(month = 'Jan-24')
Feb_24 <- Feb_24 %>% 
  mutate(month = 'Feb-24')
Mar_24 <- Mar_24 %>% 
  mutate(month = 'Mar-24')
Apr_24 <- Apr_24 %>% 
  mutate(month = 'Apr-24')
May_24 <- May_24 %>% 
  mutate(month = 'May-24')
Jun_24 <- Jun_24 %>% 
  mutate(month = 'Jun-24')
Jul_24 <- Jul_24 %>% 
  mutate(month = 'Jul-24')
Aug_24 <- Aug_24 %>% 
  mutate(month = 'Aug-24')
Sep_24 <- Sep_24 %>% 
  mutate(month = 'Sept-24')
Oct_24 <- Oct_24 %>% 
  mutate(month = 'Oct-24')
Nov_24 <- Nov_24 %>% 
  mutate(month = 'Nov-24')
Dec_24 <- Dec_24 %>% 
  mutate(month = 'Dec-24')

Jan_25 <- Jan_25 %>% 
  mutate(month = 'Jan-25')
Feb_25 <- Feb_25 %>% 
  mutate(month = 'Feb-25')
Mar_25 <- Mar_25 %>% 
  mutate(month = 'Mar-25')
Apr_25 <- Apr_25 %>% 
  mutate(month = 'Apr-25')
May_25 <- May_25 %>% 
  mutate(month = 'May-25')

names(Jun_25) <- make.unique(names(Jun_25))
Jun_25 <- Jun_25 %>%
  mutate(month = 'Jun-25')

names(Jul_25) <- make.unique(names(Jul_25))
Jul_25 <- Jul_25 %>%
  mutate(month = 'Jul-25')



# Bring together the Sept-23 to Mar-24 data files.
delayed_discharges_sep23_mar24 <- rbind(Sep_23,Oct_23,Dec_23,Jan_24,Feb_24,Mar_24) %>% 
  select(-c(Region,ICB,`Data Source`)) %>% 
  rbind(Nov_23) %>% 
  mutate(patients_discharged_volume = 0, 
         dd_bed_days = 0,
         no_delay_volume = 0, 
         `1_day_delay_volume` = 0, 
         `2_3_day_delay_volume` = 0,
         `4_6_day_delay_volume` = 0,
         `7_13_day_delay_volume` = 0,
         `14_20_day_delay_volume` = 0,
         `21plus_day_delay_volume` = 0,
         no_delay_percent = 0,
         delayed_perc_1_day = 0,
         delayed_perc_2_3_days = 0,
         delayed_perc_4_6_days = 0,
         delayed_perc_7_13_days = 0,
         delayed_perc_14_20_days = 0,
         delayed_perc_21plus_days = 0,
         average_delay_los_inc_0_day_delay = 0)

# Bring together the Apr-24 to May-25 data files.
delayed_discharges_apr24_jul25 <- rbind(Apr_24,
                                 May_24,
                                 Jun_24,
                                 Jul_24,
                                 Aug_24,
                                 Sep_24,
                                 Oct_24,
                                 Nov_24,
                                 Dec_24,
                                 Jan_25,
                                 Feb_25,
                                 Mar_25,
                                 Apr_25,
                                 May_25,
                                 Jun_25,
                                 Jul_25) %>% 
  select(-c(Region,ICB,`Data Source`))

# Bring all the files together and select out the variables of interest.
dd_file <- rbind(delayed_discharges_sep23_mar24,
                 delayed_discharges_apr24_jul25) %>% 
  select(month,org_code,patients_discharged_volume, dd_bed_days,no_delay_perc,delay_perc,
         no_delay_volume, `1_day_delay_volume`, `2_3_day_delay_volume`, `4_6_day_delay_volume`,
         `7_13_day_delay_volume`, `14_20_day_delay_volume`, `21plus_day_delay_volume`,
         
         no_delay_percent, `1_day_delay_perc`, `2_3_day_delay_perc`, `4_6_day_delay_perc`,
         `7_13_day_delay_perc`, `14_20_day_delay_perc`, `21plus_day_delay_perc`,
         
         delayed_perc_1_day, delayed_perc_2_3_days, delayed_perc_4_6_days, delayed_perc_7_13_days,
         delayed_perc_14_20_days, delayed_perc_21plus_days,
         
         `1_day_delay_beddays`, `2_3_day_delay_beddays`, `4_6_day_delay_beddays`,
         `7_13_day_delay_beddays`, `14_20_day_delay_beddays`, `21plus_day_delay_beddays`,
         
         average_delay_los_inc_0_day_delay, average_delay_los_minus_0_day_delay)

# Load in the acute trust codes.
trust_codes <- read.csv('trust_codes.csv') %>% 
  select(org_code,Flag)

# Join the trust codes to the DD file and filter for acute trusts.
dd_file_acute_trusts <- left_join(dd_file,trust_codes,by='org_code') %>% 
  filter(Flag==1)

# Select data from Apr-24 onwards.
dd_file_acute_trusts_FINAL <- dd_file_acute_trusts %>% 
  filter(!month %in% c('Sept-23','Oct-23','Nov-23','Dec-23','Jan-24','Feb-24','Mar-24'))

# Create dataset for Apr/May.
dd_file_May_Jun_Jul <- dd_file_acute_trusts_FINAL %>% 
  filter(month %in% c('May-24','Jun-24','Jul-24','May-25','Jun-25','Jul-25')) %>% 
  mutate(time_period = if_else(month %in% c('May-24','Jun-24','Jul-24'),'pre','post'))

# Create England level dataset.
dd_file_national_FINAL <- dd_file %>% 
  filter(org_code=='National') %>% 
  filter(!month %in% c('Sept-23','Oct-23','Nov-23','Dec-23','Jan-24','Feb-24','Mar-24'))



# 9 May-Jul 24 ############################################################

Jul_24_redux <- Jul_24 %>%
  rename(total_discharge_Jul_24 = `patients_discharged_volume`,
         no_delay_Jul_24 = `no_delay_volume`,
         total_delay_beddays_Jul_24 = `dd_bed_days`) %>% 
  mutate(delays_Jul_24 = as.numeric(total_discharge_Jul_24) - as.numeric(no_delay_Jul_24)) %>% 
  select(Region,org_code,total_discharge_Jul_24,no_delay_Jul_24,delays_Jul_24,total_delay_beddays_Jul_24)

May_24_redux <- May_24 %>%
  rename(total_discharge_May_24 = `patients_discharged_volume`,
         no_delay_May_24 = `no_delay_volume`,
         total_delay_beddays_May_24 = `dd_bed_days`) %>%
  mutate(delays_May_24 = as.numeric(total_discharge_May_24) - as.numeric(no_delay_May_24)) %>% 
  select(Region,org_code,total_discharge_May_24,no_delay_May_24,delays_May_24,total_delay_beddays_May_24)

Jun_24_redux <- Jun_24 %>%
  rename(total_discharge_Jun_24 = `patients_discharged_volume`,
         no_delay_Jun_24 = `no_delay_volume`,
         total_delay_beddays_Jun_24 = `dd_bed_days`) %>%
  mutate(delays_Jun_24 = as.numeric(total_discharge_Jun_24) - as.numeric(no_delay_Jun_24)) %>% 
  select(Region,org_code,total_discharge_Jun_24,no_delay_Jun_24,delays_Jun_24,total_delay_beddays_Jun_24)

# 10 May-Jul 25 ###########################################################

Jul_25_redux <- Jul_25 %>%
  rename(total_discharge_Jul_25 = `patients_discharged_volume`,
         no_delay_Jul_25 = `no_delay_volume`,
         total_delay_beddays_Jul_25 = `dd_bed_days`) %>%
  mutate(delays_Jul_25 = as.numeric(total_discharge_Jul_25) - as.numeric(no_delay_Jul_25)) %>% 
  select(Region,org_code,total_discharge_Jul_25,no_delay_Jul_25,delays_Jul_25,total_delay_beddays_Jul_25)

May_25_redux <- May_25 %>%
  rename(total_discharge_May_25 = `patients_discharged_volume`,
         no_delay_May_25 = `no_delay_volume`,
         total_delay_beddays_May_25 = `dd_bed_days`) %>%
  mutate(delays_May_25 = as.numeric(total_discharge_May_25) - as.numeric(no_delay_May_25)) %>% 
  select(Region,org_code,total_discharge_May_25,no_delay_May_25,delays_May_25,total_delay_beddays_May_25)

Jun_25_redux <- Jun_25 %>%
rename(total_discharge_Jun_25 = `patients_discharged_volume`,
       no_delay_Jun_25 = `no_delay_volume`,
       total_delay_beddays_Jun_25 = `dd_bed_days`) %>%
  mutate(delays_Jun_25 = as.numeric(total_discharge_Jun_25) - as.numeric(no_delay_Jun_25)) %>% 
  select(Region,org_code,total_discharge_Jun_25,no_delay_Jun_25,delays_Jun_25,total_delay_beddays_Jun_25)

output <- full_join(May_24_redux, Jun_24_redux, by=c("Region","org_code"))
output <- full_join(output,Jul_24_redux, by=c("Region","org_code"))
output <- full_join(output,May_25_redux, by=c("Region","org_code"))
output <- full_join(output,Jun_25_redux, by=c("Region","org_code"))
output <- full_join(output,Jul_25_redux, by=c("Region","org_code"))

# National filter

May_Jun_Jul_national <- output %>%
  filter(org_code == 'National') %>%
  mutate(total_discharge24 = sum(as.numeric(c(total_discharge_Jul_24, total_discharge_May_24, total_discharge_Jun_24)))/3,
         total_discharge25 = sum(as.numeric(c(total_discharge_Jul_25, total_discharge_May_25, total_discharge_Jun_25)))/3)

output_test <- output %>% 
  group_by(Region,org_code) %>% 
  mutate(pre_total_discharge = as.numeric(total_discharge_Jul_24) + as.numeric(total_discharge_May_24) + as.numeric(total_discharge_Jun_24),
         pre_no_delay = as.numeric(no_delay_Jul_24) + as.numeric(no_delay_May_24) + as.numeric(no_delay_Jun_24),
         pre_delays = as.numeric(delays_Jul_24) + as.numeric(delays_May_24) + as.numeric(delays_Jun_24),
         pre_delayed_beddays = as.numeric(total_delay_beddays_Jul_24) + as.numeric(total_delay_beddays_May_24) + as.numeric(total_delay_beddays_Jun_24),
         pre_proportion_delayed = pre_delays / pre_total_discharge,
         pre_delay_los = pre_delayed_beddays / pre_delays,
         
         post_total_discharge = as.numeric(total_discharge_Jul_25) + as.numeric(total_discharge_May_25) + as.numeric(total_discharge_Jun_25),
         post_no_delay = as.numeric(no_delay_Jul_25) + as.numeric(no_delay_May_25) + as.numeric(no_delay_Jun_25),
         post_delays = as.numeric(delays_Jul_25) + as.numeric(delays_May_25) + as.numeric(delays_Jun_25),
         post_delayed_beddays = as.numeric(total_delay_beddays_Jul_25) + as.numeric(total_delay_beddays_May_25) + as.numeric(total_delay_beddays_Jun_25),
         post_proportion_delayed = post_delays / post_total_discharge,
         post_delay_los = post_delayed_beddays / post_delays,
         change_in_total_discharge = (post_total_discharge - pre_total_discharge)/pre_total_discharge) %>%
  ungroup() %>% 
  select(org_code,change_in_total_discharge,pre_proportion_delayed,pre_delay_los,post_proportion_delayed,post_delay_los) %>% 
  filter(str_starts(org_code, "R")) %>% 
  mutate(proportion_delayed_diff = post_proportion_delayed - pre_proportion_delayed,
         delay_los_diff = post_delay_los - pre_delay_los)

output_test_2 <- output_test %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>% 
  filter(proportion_delayed_diff != 0)

output_test_3 <- output_test %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>% 
  filter(delay_los_diff != 0)

# 11 Other cleaning for flourish export ########################################

# Convert dataset to numerics
dd_file_national_FINAL[dd_file_national_FINAL == 0] <- NA
dd_file_national_FINAL[ , -c(1, 2)] <- lapply(dd_file_national_FINAL[ , -c(1, 2)], as.numeric)
dd_file_national_FINAL$month <- factor(dd_file_national_FINAL$month, 
                                       levels = unique(dd_file_national_FINAL$month))

# Figure 1 (percentage of discharges that are delayed)
# Sum of 1-21+ day delay volumes/total patients discharged
dd_file_national_FINAL$`total_delay_volume` <- rowSums(dd_file_national_FINAL[, c(
  '1_day_delay_volume',
  '2_3_day_delay_volume',
  '4_6_day_delay_volume',
  '7_13_day_delay_volume',
  '14_20_day_delay_volume',
  '21plus_day_delay_volume')], na.rm = FALSE)

# Percentage of patients that are delayed
dd_file_national_FINAL$`perc_patients_delayed` <- (dd_file_national_FINAL$total_delay_volume/dd_file_national_FINAL$patients_discharged_volume) *100



# 12 Grouped delays############################################################
# Monthly sums of delay volumes / total sum of delay volumes
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

# Pull out May-Jul 
dd_file_May_Jun_Jul_national <- dd_file_national_FINAL %>%
  filter(month %in% c('May-24','Jun-24','Jul-24','May-25','Jun-25','Jul-25' )) %>% 
  mutate(time_period = if_else(month %in% c('May-24','Jun-24','Jul-24'),'pre','post'))

# Convert to numerics
dd_file_May_Jun_Jul_national[ , -c(1, 2)] <- lapply(dd_file_May_Jun_Jul_national[ , -c(1, 2)], as.numeric)

May_Jul2024 <- colMeans(dd_file_May_Jun_Jul_national[1:3, -(1:2)])
May_Jul2025 <- colMeans(dd_file_May_Jun_Jul_national[4:6, -(1:2)])

dd_file_May_Jun_Jul_avg <- rbind(May_Jul2024, May_Jul2025)
dd_file_May_Jun_Jul_avg <- as.data.frame(dd_file_May_Jun_Jul_avg)
dd_file_May_Jun_Jul_avg$month <- c("MayJunJul2024","MayJunJul2025")

# Pivot the delay composition
grouped_delays_24_25 <- dd_file_May_Jun_Jul_avg %>%
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

full_trusts <- unique(dd_file_acute_trusts_FINAL$org_code)

# Clean #######################################################################

rm(DRD_data_list)
rm(colnames_22)
rm(colnames_39)
rm(colnames_Nov)
rm(i)
rm(na_indices)
rm(colnames_Eng)
rm(delayed_discharges_apr24_jul25)
rm(delayed_discharges_sep23_mar24)
rm(DDbeds_totalGA)
rm(May_24_redux)
rm(Jun_24_redux)
rm(Jul_24_redux)
rm(May_25_redux)
rm(Jun_25_redux)
rm(Jul_25_redux)
rm(beds_timeseries_url)
rm(temp_file)
rm(May_Jul2024)
rm(May_Jul2025)
rm(names_vec)
rm(grouped_delays)
rm(grouped_delays_24_25)
rm(May_Jun_Jul_national)

rm(Sep_23)
rm(Oct_23)
rm(Nov_23)
rm(Dec_23)
rm(Jan_24)
rm(Feb_24)
rm(Mar_24)
rm(Apr_24)
rm(May_24)
rm(Jun_24)
rm(Jul_24)
rm(Aug_24)
rm(Sep_24)
rm(Oct_24)
rm(Nov_24)
rm(Dec_24)
rm(Jan_25)
rm(Feb_25)
rm(Mar_25)
rm(Apr_25)
rm(May_25)
rm(Jun_25)
rm(Jul_25)
