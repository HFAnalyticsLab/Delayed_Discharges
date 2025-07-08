
# 16/06/2025 - Discharge Ready Data (DRD) extraction

# Clear environment

rm(list = ls())

# 1 Load packages #############################################################

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

# 3 Individual cleaning #######################################################

# Sep 23
Sep_23 <- NA1
rm(NA1)

colnames(Sep_23) <- as.character(Sep_23[14, ])
Sep_23 <- Sep_23[-c(1:12, 14), ]
Sep_23 <- Sep_23[, -c(8, 11, 18, 25)]

# Oct 23
Oct_23 <- NA2
rm(NA2)

colnames(Oct_23) <- as.character(Oct_23[14, ])
Oct_23 <- Oct_23[-c(1:12, 14), ]
Oct_23 <- Oct_23[, -c(8, 11, 18, 25)]

# Nov 23 - Appears to lack Region, ICB and Data Source, thus 19 variables
Nov_23 <- November2023
rm(November2023)

colnames(Nov_23) <- as.character(Nov_23[13, ])
Nov_23 <- Nov_23[-c(1:11,13,15,95), ]
Nov_23 <- Nov_23[, -c(5, 8, 15, 22)]

# Dec 23
Dec_23 <- NA3
rm(NA3)

colnames(Dec_23) <- as.character(Dec_23[14, ])
Dec_23 <- Dec_23[-c(1:12, 14), ]
Dec_23 <- Dec_23[, -c(8, 11, 18, 25)]

# Jan 24
Jan_24 <- NA4
rm(NA4)

colnames(Jan_24) <- as.character(Jan_24[14, ])
Jan_24 <- Jan_24[-c(1:12, 14), ]
Jan_24 <- Jan_24[, -c(8, 11, 18, 25)]

# Feb 24
Feb_24 <- NA5
rm(NA5)

colnames(Feb_24) <- as.character(Feb_24[14, ])
Feb_24 <- Feb_24[-c(1:12, 14), ]
Feb_24 <- Feb_24[, -c(8, 11, 18, 25)]

# Mar 24
Mar_24 <- NA6
rm(NA6)

colnames(Mar_24) <- as.character(Mar_24[14, ])
Mar_24 <- Mar_24[-c(1:12, 14), ]
Mar_24 <- Mar_24[, -c(8, 11, 18, 25)]

# Apr 24
Apr_24 <- April2024
rm(April2024)

colnames(Apr_24) <- as.character(Apr_24[13, ])
Apr_24 <- Apr_24[-c(1:11,13,15,111,120), ]
Apr_24 <- Apr_24[, -c(8, 11, 18, 25)]

# May 24
May_24 <- May2024
rm(May2024)

colnames(May_24) <- as.character(May_24[13, ])
May_24 <- May_24[-c(1:11,13,15,116,129), ]
May_24 <- May_24[, -c(8, 11, 18, 25)]

# Jun 24
Jun_24 <- June2024
rm(June2024)

colnames(Jun_24) <- as.character(Jun_24[13, ])
Jun_24 <- Jun_24[-c(1:11,13,15,120,130), ]
Jun_24 <- Jun_24[, -c(8, 11, 18, 25)]

# Jul 24
Jul_24 <- NA7
rm(NA7)

colnames(Jul_24) <- as.character(Jul_24[14, ])
Jul_24 <- Jul_24[-c(1:12, 14), ]
Jul_24 <- Jul_24[, -c(8, 11, 18, 25)]

# Aug 24
Aug_24 <- df
rm(df)
rm(NA8)

colnames(Aug_24) <- as.character(Aug_24[14, ])
Aug_24 <- Aug_24[-c(1:12, 14), ]
Aug_24 <- Aug_24[, -c(8, 11, 18, 25)]

# Sep 24
Sep_24 <- September2024
rm(September2024)

colnames(Sep_24) <- as.character(Sep_24[14, ])
Sep_24 <- Sep_24[-c(1:12, 14), ]
Sep_24 <- Sep_24[, -c(8, 11, 14, 22, 30, 37, 44)]

# Oct 24
Oct_24 <- October2024
rm(October2024)

colnames(Oct_24) <- as.character(Oct_24[14, ])
Oct_24 <- Oct_24[-c(1:12, 14), ]
Oct_24 <- Oct_24[, -c(8, 11, 14, 22, 30, 37, 44)]

# Nov 24
Nov_24 <- November2024
rm(November2024)

colnames(Nov_24) <- as.character(Nov_24[14, ])
Nov_24 <- Nov_24[-c(1:12, 14), ]
Nov_24 <- Nov_24[, -c(8, 11, 14, 22, 30, 37, 44)]

# Dec 24
Dec_24 <- December2024
rm(December2024)

colnames(Dec_24) <- as.character(Dec_24[14, ])
Dec_24 <- Dec_24[-c(1:12, 14), ]
Dec_24 <- Dec_24[, -c(8, 11, 14, 22, 30, 37, 44)]

# Jan 25
Jan_25 <- January2025
rm(January2025)

colnames(Jan_25) <- as.character(Jan_25[14, ])
Jan_25 <- Jan_25[-c(1:12, 14), ]
Jan_25 <- Jan_25[, -c(8, 11, 14, 22, 30, 37, 44)]

# Appear to lose North Middlesex University Hospital in Jan 2025 - merged with Royal Free London.
setdiff(Dec_24$`Organisation Name`, Jan_25$`Organisation Name`)

# Feb 25
Feb_25 <- February2025
rm(February2025)

colnames(Feb_25) <- as.character(Feb_25[14, ])
Feb_25 <- Feb_25[-c(1:12, 14), ]
Feb_25 <- Feb_25[, -c(8, 11, 14, 22, 30, 37, 44)]

# Mar 25
Mar_25 <- March2025
rm(March2025)

colnames(Mar_25) <- as.character(Mar_25[14, ])
Mar_25 <- Mar_25[-c(1:12, 14), ]
Mar_25 <- Mar_25[, -c(8, 11, 14, 22, 30, 37, 44)]

# Apr 25
Apr_25 <- April2025
rm(April2025)

colnames(Apr_25) <- as.character(Apr_25[14, ])
Apr_25 <- Apr_25[-c(1:12, 14), ]
Apr_25 <- Apr_25[, -c(8, 11, 14, 22, 30, 37, 44)]

# 4 Adjust column names #########################################################

colnames_22 <- c('Region', 'ICB', 'Org Code', 'Org Name', '# of providers with acceptable data', '% of providers with acceptable data', 'Data Source', 'DoD is same as DRD (%)',
'DoD is 1+ days after DRD (%)','Patients delayed but discharged within 1 day (%)', 'Patients delayed but discharged within 2-3 days (%) ', 'Patients delayed but discharged within 4-6 days (%) ',
'Patients delayed but discharged within 7-13 days (%) ', 'Patients delayed but discharged within 14-20 days (%) ','Patients delayed but discharged within 21 days or more (%) ', 
'Total bed days after DRD for patients discharged within 1 day', 'Total bed days after DRD for patients discharged within 2-3 days', 'Total bed days after DRD for patients discharged within 4-6 days', 
'Total bed days after DRD for patients discharged within 7-13 days','Total bed days after DRD for patients discharged within 14-20 days', 'Total bed days after DRD for patients discharged within 21 days or more',
'Average days from DRD to DoD (exc 0-day delays)')

colnames_39 <- c('Region', 'ICB', 'Org Code', 'Org Name', '# of providers with acceptable data', '% of providers with acceptable data', 'Data Source', 'Total # of patients discharged', 'Total bed days lost to DD', 
'DoD is same as DRD (%)', 'DoD is 1+ days after DRD (%)',

'No delay between DoD & DRD (#)', '1 day delay between DoD & DRD (#)', '2-3 day delay between DoD & DRD (#)', '4-6 day delay between DoD & DRD (#)', '7-13 day delay between DoD & DRD (#)', '14-20 day delay between DoD & DRD (#)', '21 day delay between DoD & DRD (#)',

'No delay between DoD & DRD (%)', '1 day delay between DoD & DRD (%)', '2-3 day delay between DoD & DRD (%)', '4-6 day delay between DoD & DRD (%)', '7-13 day delay between DoD & DRD (%)', '14-20 day delay between DoD & DRD (%)', '21 day delay between DoD & DRD (%)',

'Patients delayed but discharged within 1 day (%)', 'Patients delayed but discharged within 2-3 days (%) ', 'Patients delayed but discharged within 4-6 days (%) ',
'Patients delayed but discharged within 7-13 days (%) ', 'Patients delayed but discharged within 14-20 days (%) ','Patients delayed but discharged within 21 days or more (%) ', 

'Total bed days after DRD for patients discharged within 1 day', 'Total bed days after DRD for patients discharged within 2-3 days', 'Total bed days after DRD for patients discharged within 4-6 days', 
'Total bed days after DRD for patients discharged within 7-13 days','Total bed days after DRD for patients discharged within 14-20 days', 'Total bed days after DRD for patients discharged within 21 days or more',

'Average days from DRD to DoD (inc 0-day delays)','Average days from DRD to DoD (exc 0-day delays)')

colnames_Nov <- colnames_22
colnames_Nov <- colnames_Nov[-c(1,2,7)]


# Pre-Sep 2024
colnames(Sep_23) <- colnames_22
Sep_23 <- Sep_23[-c(1), ]
colnames(Oct_23) <- colnames_22
Oct_23 <- Oct_23[-c(1), ]
colnames(Dec_23) <- colnames_22
Dec_23 <- Dec_23[-c(1), ]
colnames(Jan_24) <- colnames_22
Jan_24 <- Jan_24[-c(1), ]
colnames(Feb_24) <- colnames_22
Feb_24 <- Feb_24[-c(1), ]
colnames(Mar_24) <- colnames_22
Mar_24 <- Mar_24[-c(1), ]
colnames(Apr_24) <- colnames_22
Apr_24 <- Apr_24[-c(1), ]
colnames(May_24) <- colnames_22
May_24 <- May_24[-c(1), ]
colnames(Jun_24) <- colnames_22
Jun_24 <- Jun_24[-c(1), ]
colnames(Jul_24) <- colnames_22
Jul_24 <- Jul_24[-c(1), ]
colnames(Aug_24) <- colnames_22
Aug_24 <- Aug_24[-c(1), ]

# Post-Sep 2024

colnames(Sep_24) <- colnames_39
Sep_24 <- Sep_24[-c(1), ]
colnames(Oct_24) <- colnames_39
Oct_24 <- Oct_24[-c(1), ]
colnames(Nov_24) <- colnames_39
Nov_24 <- Nov_24[-c(1), ]
colnames(Dec_24) <- colnames_39
Dec_24 <- Dec_24[-c(1), ]
colnames(Jan_25) <- colnames_39
Jan_25 <- Jan_25[-c(1), ]
colnames(Feb_25) <- colnames_39
Feb_25 <- Feb_25[-c(1), ]
colnames(Mar_25) <- colnames_39
Mar_25 <- Mar_25[-c(1), ]
colnames(Apr_25) <- colnames_39
Apr_25 <- Apr_25[-c(1), ]

# November 2023

colnames(Nov_23) <- colnames_Nov
Nov_23 <- Nov_23[-c(1), ]
































































































































