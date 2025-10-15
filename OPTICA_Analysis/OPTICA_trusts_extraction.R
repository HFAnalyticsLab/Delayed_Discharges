
# 15/10/2025 - OPTICA trusts | Discharge Ready Date (DRD) extraction

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
Aug_25 <- August2025

Aug_25 <- Aug_25 %>%
  {
    header_row_index <- which(.[[1]] == "Region")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

Aug_25 <- Aug_25[, sapply(Aug_25, function(col) !all(is.na(col)))]

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
            "May2024", "May2025", "November2023", "November2024", "October2024", "September2024", "July2025","August2025"))


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
colnames(Aug_25) <- colnames_39

# November 2023
colnames_Nov <- colnames_22
colnames_Nov <- colnames_Nov[-c(1,2,7)]
colnames(Nov_23) <- colnames_Nov



# 5 Combine all trust DRD datasets ############################################

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

names(Aug_25) <- make.unique(names(Aug_25))
Aug_25 <- Aug_25 %>%
  mutate(month = 'Aug_25')

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
                                        Jul_25,
                                        Aug_25) %>% 
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

# 6 Select OPTICA trusts #######################################################

OPTICA_trust_names <- c('North West Anglia NHS Foundation Trust',
                        'Isle of Wight NHS Trust',
                        'Royal United Hospitals Bath NHS Foundation Trust',
                        'West Hertfordshire Teaching Hospitals NHS Trust',
                        'Somerset NHS Foundation Trust',
                        'Northern Lincolnshire and Goole NHS Foundation Trust',
                        'York and Scarborough Teaching Hospitals NHS Foundation',
                        'Mid Yorkshire Teaching NHS Trust',
                        'James Paget University Hospitals NHS Foundation Trust',
                        'Harrogate and District NHS Foundation Trust',
                        'Queen Elizabeth Hospital Kings Lynn NHS Foundation Trust',
                        'Norfolk & Norwich University Hospitals NHS Foundation Trust',
                        'The Newcastle Upon Tyne Hospitals NHS Foundation Trust',
                        'Chesterfield Royal Hospital NHS Foundation Trust',
                        'University Hospitals of Derby and Burton NHS Foundation Trust',
                        'Hull University Teaching Hospitals NHS Trust',
                        'County Durham & Darlington NHS Foundation Trust',
                        'South Tyneside and Sunderland NHS Foundation Trust',
                        'Countess of Chester Hospital NHS Foundation Trust',
                        'London North West University Healthcare NHS Trust',
                        'The Hillingdon Hospitals NHS Foundation Trust',
                        'Liverpool University Hospitals NHS Foundation Trust',
                        'Imperial College Healthcare NHS Trust',
                        'Chelsea & Westminster Hospital NHS Foundation Trust',
                        'North Tees and Hartlepool NHS Foundation Trust')

# To check: Somerset & Taunton, Liverpool, North West Anglia.

OPTICA_trusts <- c('RGN', 'R1F', 'RD1', 'RWG', 'RBA', 'RJL', 'RCB', 'RXF', 'RGP',
                   'RCD', 'RCX', 'RM1', 'RTD', 'RFS', 'RTG', 'RWA', 'RXP', 'R0B',
                   'RJR', 'R1K', 'RAS', 'RQ6', 'RYJ', 'RQM', 'RVW')

dd_file_optica_trusts <- dd_file_acute_trusts %>%
  filter(org_code %in% OPTICA_trusts)


# CLEAN ########################################################################
rm(list = c('Sep_23','Oct_23','Nov_23','Dec_23','Jan_24','Feb_24','Mar_24','Apr_24','May_24',
            'Jun_24','Jul_24','Aug_24','Sep_24','Oct_24','Nov_24','Dec_24','Jan_25','Feb_25',
            'Mar_25','Apr_25','May_25','Jun_25','Jul_25','Aug_25'))


rm(i)
rm(colnames_22)
rm(colnames_39)
rm(colnames_Nov)
rm(na_indices)
rm(temp_file)
rm(DRDlinks)
rm(DRD_url)
rm(DRD_data_list)
rm(delayed_discharges_apr24_jul25)
rm(delayed_discharges_sep23_mar24)




