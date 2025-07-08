
# 08/07/2025 - Discharge Ready Data (DRD) descriptive analysis

# Clear environment

rm(list = ls())

# 1 Create England dataframe ##################################################

# England subsets (22)
Sep_23_ENG <- Sep_23[1, c(1:22)]
Oct_23_ENG <- Oct_23[1, c(1:22)]
Nov_23_ENG <- Nov_23[1, c(1:19)]
Dec_23_ENG <- Dec_23[1, c(1:22)]
Jan_24_ENG <- Jan_24[1, c(1:22)]
Feb_24_ENG <- Feb_24[1, c(1:22)]
Mar_24_ENG <- Mar_24[1, c(1:22)]
Apr_24_ENG <- Apr_24[1, c(1:22)]
May_24_ENG <- May_24[1, c(1:22)]
Jun_24_ENG <- Jun_24[1, c(1:22)]
Jul_24_ENG <- Jul_24[1, c(1:22)]
Aug_24_ENG <- Aug_24[1, c(1:22)]

# England subsets (39)

England_DRD <- rbind(Sep_23_ENG, Oct_23_ENG, Dec_23_ENG,
                     Jan_24_ENG, Feb_24_ENG, Mar_24_ENG, Apr_24_ENG,
                     May_24_ENG, Jun_24_ENG, Jul_24_ENG, Aug_24_ENG)  



















