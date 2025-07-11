
# 08/07/2025 - Discharge Ready Data (DRD) descriptive analysis

# 1 Download England timeseries ##################################################

England_DRD <- read.xlsx("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/Discharge-Ready-Date-Timeseries-webfile-Sept-2023-May-2025.xlsx", sheet = "Timeseries")
England_DRD <- England_DRD %>%
  {
    header_row_index <- which(.[[1]] == "Month")
    colnames(.) <- as.character(unlist(.[header_row_index, ]))
    .[(header_row_index + 1):nrow(.), ]
  }

England_DRD <- England_DRD[, sapply(England_DRD, function(col) !all(is.na(col)))]

colnames_Eng <- c('Month', '# of providers with acceptable data', '% of providers with acceptable data', 'Total # of patients discharged', 'Total bed days lost to DD', 
                 'DoD is same as DRD (%)', 'DoD is 1+ days after DRD (%)','No delay between DoD & DRD (#)', '1 day delay between DoD & DRD (#)', '2-3 day delay between DoD & DRD (#)', '4-6 day delay between DoD & DRD (#)', '7-13 day delay between DoD & DRD (#)', '14-20 day delay between DoD & DRD (#)', '21 day delay between DoD & DRD (#)',
                 'No delay between DoD & DRD (%)', '1 day delay between DoD & DRD (%)', '2-3 day delay between DoD & DRD (%)', '4-6 day delay between DoD & DRD (%)', '7-13 day delay between DoD & DRD (%)', '14-20 day delay between DoD & DRD (%)', '21 day delay between DoD & DRD (%)',
                 'Patients delayed but discharged within 1 day (%)', 'Patients delayed but discharged within 2-3 days (%) ', 'Patients delayed but discharged within 4-6 days (%) ',
                 'Patients delayed but discharged within 7-13 days (%) ', 'Patients delayed but discharged within 14-20 days (%) ','Patients delayed but discharged within 21 days or more (%) ', 
                 'Total bed days after DRD for patients discharged within 1 day', 'Total bed days after DRD for patients discharged within 2-3 days', 'Total bed days after DRD for patients discharged within 4-6 days', 
                 'Total bed days after DRD for patients discharged within 7-13 days','Total bed days after DRD for patients discharged within 14-20 days', 'Total bed days after DRD for patients discharged within 21 days or more',
                 'Average days from DRD to DoD (inc 0-day delays)','Average days from DRD to DoD (exc 0-day delays)')

colnames(England_DRD) <- colnames_Eng

Month_list <- c("Sep23","Oct23","Nov23","Dec23","Jan24","Feb24","Mar24","Apr24",
                "May24","Jun24","Jul24","Aug24","Sep24","Oct24","Nov24","Dec24",
                "Jan25","Feb25","Mar25","Apr25","May25")

England_DRD$Month <- Month_list
print(colnames_Eng)
England_DRD[, -1] <- lapply(England_DRD[, -1], as.numeric)
rownames(England_DRD) <- NULL

England_DRD$Month <- factor(England_DRD$Month, levels = England_DRD$Month)

# 2 National-level descriptives ###############################################

Total_delayed <- sum(England_DRD$`1 day delay between DoD & DRD (#)`, 
                     England_DRD$`2-3 day delay between DoD & DRD (#)`, 
                     England_DRD$`4-6 day delay between DoD & DRD (#)`,
                     England_DRD$`7-13 day delay between DoD & DRD (#)`,
                     England_DRD$`14-20 day delay between DoD & DRD (#)`,
                     England_DRD$`21 day delay between DoD & DRD (#)`, na.rm = TRUE)
England_DRD$Total_delayed <- England_DRD$`1 day delay between DoD & DRD (#)`+ 
                                England_DRD$`2-3 day delay between DoD & DRD (#)`+ 
                                England_DRD$`4-6 day delay between DoD & DRD (#)`+
                                England_DRD$`7-13 day delay between DoD & DRD (#)`+
                                England_DRD$`14-20 day delay between DoD & DRD (#)`+
                                England_DRD$`21 day delay between DoD & DRD (#)` 
England_DRD$Average_delayed_discharge <- England_DRD$Total_delayed/England_DRD$`Total # of patients discharged`

Total_discharged <- sum(England_DRD$`No delay between DoD & DRD (#)`, na.rm = TRUE)
Average_delayed <- Total_delayed/nrow(England_DRD)-7
Average_discharged <- Total_discharged/nrow(England_DRD)-7
Average_delayed_discharge <- Average_delayed/Average_discharged
print(Average_delayed_discharge)

# % of discharges that are delayed (Sum of delayed discharges / total discharges)

delayed_discharges_plot <- ggplot(England_DRD, aes(x = Month, y = Average_delayed_discharge, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  labs(title = "Percentage of discharges that are delayed ",
       x = "Month",
       y = "% of total discharges") +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) 

ggsave("delayed discharges.png", plot = delayed_discharges_plot, width = 8, height = 6, dpi = 300)

# Average bed days with delayed discharge
ggplot(England_DRD, aes(x = Month, y = Average_bed_days_with_delayed_discharge, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(title = "Percentage of bed days used for delayed discharge ",
       x = "Month",
       y = "% of beds") +
  theme_gray() +
  theme(axis.text.x = element_text(size = 5)) 

# DoD = DRD over time + 
DRDequalsDoD <- ggplot(England_DRD, aes(x = Month, y = `DoD is same as DRD (%)` , group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  labs(title = "Discharge ready date is the same as Date of Discharge",
       x = "Month",
       y = "%") +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) 

ggsave("DRD equals DoD.png", plot = DRDequalsDoD, width = 8, height = 6, dpi = 300)

# No delay between DoD and DRD
No_delay <- ggplot(England_DRD, aes(x = Month, y = `No delay between DoD & DRD (%)` , group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
  labs(title = "No delay between DoD and DRD",
       x = "Month",
       y = "%") +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) 

ggsave("no delay.png", plot = No_delay, width = 8, height = 6, dpi = 300)

# Discharge delays +1 day

# Discharge delays 2-3 days
# Discharge delays 4-6 days
# Discharge delays 7-13 days
# Discharge delays 14-20 days
# Discharge delays 21+ days

# Average days from DRD to DoD (exc 0-day delays)
Average_DRD_to_DoD <- ggplot(England_DRD, aes(x = Month, y = `Average days from DRD to DoD (exc 0-day delays)` , group = 1)) +
  geom_line(color = "blue", size = 1.2) +
    geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              size = 0.5) +
      labs(title = "Average days from discharge ready date to date of discharge",
       x = "Month",
       y = "Days") +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7)) 

ggsave("AverageDRD_to_DoD.png", plot = Average_DRD_to_DoD, width = 8, height = 6, dpi = 300)



