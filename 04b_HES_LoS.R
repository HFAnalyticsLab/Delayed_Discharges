
# 12/08/2025 - Discharge Ready Data (DRD) National descriptive analysis
# 

# Mean length of stay ######################################################

LoS_filelink_23_24 <- c('https://files.digital.nhs.uk/09/D3BF1B/hosp-epis-stat-admi-summary-tabs-2023-24.xlsx')
LoS_filelink_22_23 <- c('https://files.digital.nhs.uk/4A/D4D927/hosp-epis-stat-admi-summary-tabs-2022-23-tab.xlsx')
LoS_filelink_21_22 <- c('https://files.digital.nhs.uk/5A/46E9C7/hosp-epis-stat-admi-rep-tabs-2021-22-tab-v2.xlsx')
LoS_filelink_20_21 <- c('https://files.digital.nhs.uk/89/E2079D/hosp-epis-stat-admi-rep-tabs-2020-21-tab-v2.xlsx')
LoS_filelink_19_20 <- c('https://files.digital.nhs.uk/1A/B4BD4D/hosp-epis-stat-admi-rep-tabs-2019-20-tab.xlsx')
LoS_filelink_18_19 <- c('https://files.digital.nhs.uk/05/A594E3/hosp-epis-stat-admi-rep-tabs-2018-19-tab.xlsx')
LoS_filelink_17_18 <- c('https://files.digital.nhs.uk/43/8664F3/hosp-epis-stat-admi-rep-tabs-2017-18-tab.xlsx')

# Beyond 2017 - Mean length of stay taken from the main specialty sheets
LoS_16_17 <- 4.914368888499
LoS_15_16 <- 4.93389591367462
LoS_14_15 <- 5
LoS_13_14 <- 5.1
LoS_12_13 <- 5.2
LoS_11_12 <- 5.3

# Load 2017-2024 sheets

LoS_data_23_24 <- read.xlsx(LoS_filelink_23_24, sheet = 'Summary table 8')
LoS_23_24 <- LoS_data_23_24[14,5]

LoS_data_22_23 <- read.xlsx(LoS_filelink_22_23, sheet = 'Summary table 8')
LoS_22_23 <- LoS_data_22_23[14,5]

LoS_data_21_22 <- read.xlsx(LoS_filelink_21_22, sheet = 'Summary report 15 & 16')
LoS_21_22 <- LoS_data_21_22[11,5]

LoS_data_20_21 <- read.xlsx(LoS_filelink_20_21, sheet = 'Summary report 15 & 16')
LoS_20_21 <- LoS_data_20_21[11,5]

LoS_data_19_20 <- read.xlsx(LoS_filelink_19_20, sheet = 'Summary report 15 & 16')
LoS_19_20 <- LoS_data_19_20[11,5]

LoS_data_18_19 <- read.xlsx(LoS_filelink_18_19, sheet = 'Summary report 15, 16 & 17')
LoS_18_19 <- LoS_data_18_19[11,5]

LoS_data_17_18 <- read.xlsx(LoS_filelink_17_18, sheet = 'Summary report 15 & 16')
LoS_17_18 <- LoS_data_17_18[11,5]

# Construct timeseries & plot

LoS_timeseries <- data.frame(Year = c('2023/24','2022/23','2021/22','2020/21','2019/20','2018/19','2017/18',
                                      '2016/17','2015/16','2014/15','2013/14','2012/13','2011/12'),
                             LoS = c(LoS_23_24,LoS_22_23,LoS_21_22,LoS_20_21,LoS_19_20,LoS_18_19,LoS_17_18,
                                     LoS_16_17,LoS_15_16,LoS_14_15,LoS_13_14,LoS_12_13,LoS_11_12))
LoS_timeseries <- LoS_timeseries %>%
  mutate(LoS = as.numeric(LoS))

LoS_plot <- ggplot(LoS_timeseries, aes(x = Year, y = LoS, group = 1)) +
  geom_line(color = "firebrick1", linewidth = 1.2) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", 
              linetype = "longdash",
              linewidth = 0.5) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "Average length of stay, England, 2011/12 - 2023/24.",
       x = "Financial Year",
       y = "Average length of stay (days)") +
  scale_y_continuous(
    limits = c(0, 7),        
    breaks = seq(0, 7, 0.5),
    labels = number_format(accuracy = 0.1)) +
    theme_gray() +
  theme(axis.text.x = element_text(size = 6.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

LoS_plot
ggsave("Average Length of Stay.png", plot = LoS_plot, width = 8, height = 6, dpi = 300)

# Clean

rm(LoS_11_12)
rm(LoS_12_13)
rm(LoS_13_14)
rm(LoS_14_15)
rm(LoS_15_16)
rm(LoS_16_17)
rm(LoS_17_18)
rm(LoS_18_19)
rm(LoS_19_20)
rm(LoS_20_21)
rm(LoS_21_22)
rm(LoS_22_23)
rm(LoS_23_24)
rm(LoS_data_17_18)
rm(LoS_data_18_19)
rm(LoS_data_19_20)
rm(LoS_data_20_21)
rm(LoS_data_21_22)
rm(LoS_data_22_23)
rm(LoS_data_23_24)
















