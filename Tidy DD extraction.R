#Read in the data



#Clear environment

rm(list = ls())



#Load packages

library(aws.s3)
library(Rpublic)
#install.packages('tidyverse') #purr (not available for r 4.0.2)
pacman::p_load(dplyr,
               
               janitor,
               
               data.table,
               
               purrr,
               
               lubridate,
               
               readr,
               
               readxl,
               
               here)

library(tidyverse)
library(dplyr)
library(janitor)
library(readr)

#Load file pathways

#source('0-file_pathways.R') 



#Load functions

source('https://raw.githubusercontent.com/zeyadissa/open_health_data/main/src/functions.R')





#Read in discharge ready date data - Oct 2023 - Aug 2024 ####

dis_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays/discharge-ready-date/'

dislinks <- GetLinks(dis_url, 'Discharge-Ready-Date-monthly-data-webfile') #Monthly data for this is a month behind all other releases - latest is up to Sept 2024 - but could aggregate from the daily discharge file that is the same month aka Oct 2024

dislinks_2 <- dislinks[grepl(pattern = 'September-2024', dislinks)] #Read in Sep 2024 data - format is different so keep separate

dislinks_2

dislinks_3 <- dislinks[grepl(pattern = 'October-2024', dislinks)] #Read in Oct 2024 data - format is different so keep separate

dislinks_3

dislinks <- dislinks[!grepl(pattern = 'September-2024', dislinks)] #Read in the rest of the data and remove Sep 2024 data 

dislinks <- dislinks[!grepl(pattern = 'October-2024', dislinks)] #Read in the rest of the data and remove Sep 2023 and 2024 data 

dislinks_4 <- dislinks[1] #need to see how the data was in Sept 2023 but will filter it out for the graphs

dislinks <- dislinks[2:12]

# Prep data

dis_data <- lapply(dislinks,
                   
                   function(x){
                     
                     data <-ReadExcel(files = x, sheet = 2) #Sheet 2 has all of the providers (both those submitting acceptable and unacceptable data) w/ % of pts dc'd after drd but dc'd w/in and bed days (avg. and absolute) for 1,2-3,4-6,7-13,14-20,21+ days
                     
                   }) %>% 
  
  flatten()

dis_data_a <- lapply(dis_data,
                     
                     function(x){
                       
                       names(x) <- x[13,]
                     
                       x['date'] <- x[2,2]
                    
                       data <- x[-c(1:15),]
                       
                       data <- data %>% janitor::clean_names()
                       
                       return(data)
                       
                     }) %>%
  
  data.table::rbindlist(fill=T)

dis_data_a <- dis_data_a %>% mutate(org_code = if_else(is.na(org_code), code, org_code)) #Sub in code if org_code is NA before removing the code

dis_data_a <- dis_data_a[,-c(1)] #Remove org_code without removing region - should fix above

dim(dis_data_a)

# Format of Sep/Oct 2024 is different - need to read in sheet 3 instead of 2
# % of patients discharged after their Discharge Ready Date but discharged within - and Total bed days after Discharge Ready Date for patients discharged within -
dis_data_2 <- lapply(dislinks_2,
                   
                   function(x){
                     
                     data <-ReadExcel(files = x, sheet = 3) #Sheet 3 has all of the providers (both those submitting acceptable and unacceptable data) w/ % of pts dc'd after drd but dc'd w/in and bed days (avg. and absolute) for 1,2-3,4-6,7-13,14-20,21+ days
                     
                   }) %>% 
  
  flatten()

dis_data_2a <- lapply(dis_data_2,
                     
                     function(x){
                       
                       names(x) <- x[14,]
                       
                       x['date'] <- x[2,2]
                       
                       data <- x[-c(1:64),c(4:6,8,12:14,31:44,46,47,1:3,7)]
                       
                       data <- data %>% janitor::clean_names()
                       
                       return(data)
                       
                     }) %>%
  
  data.table::rbindlist(fill=T)

dim(dis_data_2a)

dis_data_3 <- lapply(dislinks_3,
                     
                     function(x){
                       
                       data <-ReadExcel(files = x, sheet = 3) #Sheet 3 has all of the providers (both those submitting acceptable and unacceptable data) w/ % of pts dc'd after drd but dc'd w/in and bed days (avg. and absolute) for 1,2-3,4-6,7-13,14-20,21+ days
                       
                     }) %>% 
  
  flatten()

dis_data_3a <- lapply(dis_data_3,
                      
                      function(x){
                        
                        names(x) <- x[14,]
                        
                        x['date'] <- x[2,2]
                        
                        data <- x[-c(1:64),c(4:6,8,12:14,31:44,46,47,1:3,7)]
                        
                        data <- data %>% janitor::clean_names()
                        
                        return(data)
                        
                      }) %>%
  
  data.table::rbindlist(fill=T)

dim(dis_data_3a)

#Format of Sep 2023 is different 
dis_data_4 <- lapply(dislinks_4,
                   
                   function(x){
                     
                     data <-ReadExcel(files = x, sheet = 2) #Sheet 2 has all of the providers (both those submitting acceptable and unacceptable data) w/ % of pts dc'd after drd but dc'd w/in and bed days (avg. and absolute) for 1,2-3,4-6,7-13,14-20,21+ days
                     
                   }) %>% 
  
  flatten()

dis_data_4a <- lapply(dis_data_4,
                      
                      function(x){
                        
                        names(x) <- x[16,]
                        
                        x['date'] <- x[2,2]
                        
                        data <- x[-c(1:18),c(2:22,1)] #No region/ICB
                        
                        data <- data %>% janitor::clean_names()
                        
                        data <- data %>% filter(.$code %in% c('RXQ','RTX','RAE','RNA','RK5','RM1', 'RBL'))
                        return(data)
                        
                      }) %>%
  
  data.table::rbindlist(fill=T)

dim(dis_data_4a)

dis_data_4a <- dis_data_4a %>% 
  
  mutate(date = case_when(date == 45170 ~ 'September 2023')) %>% 
  
  mutate(date = my(date),
         
         org_code = code) %>% 
  
  select(-c(na, na_2, na_3, code)) %>%
  
  rename(n_prov_acceptable = number_of_providers_submitting_acceptable_data, 
         
         dis_on_ready = date_of_discharge_is_same_as_discharge_ready_date, 
         
         dis_after_ready = date_of_discharge_is_1_days_after_discharge_ready_date) %>% 
  
  mutate(dis_on_ready = round(as.numeric(dis_on_ready)*100, 1), 
         
         dis_after_ready = round(as.numeric(dis_after_ready)*100, 1), 
         
         x1_day = round(as.numeric(x1_day)*100, 1),
         
         x2_3_days = round(as.numeric(x2_3_days)*100, 1), 
         
         x4_6_days = round(as.numeric(x4_6_days)*100, 1), 
         
         x7_13_days = round(as.numeric(x7_13_days)*100, 1), 
         
         x14_20_days = round(as.numeric(x14_20_days)*100, 1),
         
         x21_days_or_more = round(as.numeric(x21_days_or_more)*100, 1), 
         
         x1_day_2 = as.numeric(x1_day_2),
         
         x2_3_days_2 = as.numeric(x2_3_days_2), 
         
         x4_6_days_2 = as.numeric(x4_6_days_2), 
         
         x7_13_days_2 = as.numeric(x7_13_days_2), 
         
         x14_20_days_2 = as.numeric(x7_13_days_2),
         
         x21_days_or_more_2 = as.numeric(x21_days_or_more_2),
         
         total_beddays_delay = rowSums(across(c(x1_day_2, x2_3_days_2,x4_6_days_2, x7_13_days_2, x14_20_days_2, x21_days_or_more_2))))

         

#Join data
dis_data_a <-  rbind(dis_data_a, dis_data_2a, fill=TRUE)

dis_data_a <- rbind(dis_data_a, dis_data_3a, fill = TRUE)

dim(dis_data_a)

## data cleaning - national data ####

# October 2023 - August 2024 #

dis_data_national <- lapply(dis_data,
                            
                            function(x){
                              
                              names(x) <- x[13,]
                              
                              x['date'] <- x[2,2]
                              
                              data <- x[-c(1:13),]
                              
                              data <- data %>% janitor::clean_names()
                              
                              return(data)
                              
                            }) %>%
  
   data.table::rbindlist(fill=T) %>%
  
  mutate(date = case_when(date == 45200 ~ 'October 2023',
                          
                          date == 45231 ~ 'November 2023', 
                          
                          date == 45261 ~ 'December 2023', 
                          
                          date == 45292 ~ 'January 2024',
                          
                          date == 45323 ~ 'February 2024',
                          
                          date == 45352 ~ 'March 2024', 
                          
                          date == 45383 ~ 'April 2024',
                          
                          date == 45413 ~ 'May 2024',
                          
                          date == 45444 ~ 'June 2024',
                          
                          date == 45474 ~ 'July 2024',
                          
                          date == 45505 ~ 'August 2024')) %>%
  
  mutate(date = my(date)) %>%
  
  mutate(code = if_else(is.na(code), org_code, code)) %>%
  
  filter(code == 'National') %>%
  
  select(-c(na, na_2, na_3, na_4, percent_of_providers_submitting_acceptable_data, code)) %>%
  
  rename(n_prov_acceptable = number_of_providers_submitting_acceptable_data, 
         
         dis_on_ready = date_of_discharge_is_same_as_discharge_ready_date, 
         
         dis_after_ready = date_of_discharge_is_1_days_after_discharge_ready_date,
         
         av_delay_after = average_days_from_discharge_ready_date_to_date_of_discharge_exc_0_day_delays) %>%
  
  mutate(dis_on_ready = round(as.numeric(dis_on_ready)*100, 1), 
         
         dis_after_ready = round(as.numeric(dis_after_ready)*100, 1), 
         
         av_delay_after = round(as.numeric(av_delay_after), 1), 
         
         x1_day = round(as.numeric(x1_day)*100, 1),
         
         x2_3_days = round(as.numeric(x2_3_days)*100, 1), 
         
         x4_6_days = round(as.numeric(x4_6_days)*100, 1), 
         
         x7_13_days = round(as.numeric(x7_13_days)*100, 1), 
         
         x14_20_days = round(as.numeric(x14_20_days)*100, 1),
         
         x21_days_or_more = round(as.numeric(x21_days_or_more)*100, 1))

# September 2024 - onwards? #

dis_data_national_2 <- lapply(dis_data_2,
                            
                            function(x){
                              
                              names(x) <- x[14,]
                              
                              x['date'] <- x[2,2]
                              
                              data <- x[-c(1:14),c(4:6,8,12:14,31:44,46,47,1:3,7)]
                              
                              data <- data %>% janitor::clean_names()
                              
                              return(data)
                              
                            }) %>%
  
  data.table::rbindlist(fill=T) %>%
  
  mutate(date = case_when(date == 45536 ~ 'September 2024')) %>%
  
  mutate(date = my(date)) %>%
  
  filter(org_code == 'National') %>%
  
  select(-c(na, na_2, na_3, na_4, percent_of_providers_submitting_acceptable_data)) %>%
  
  rename(n_prov_acceptable = number_of_providers_submitting_acceptable_data, 
         
         dis_on_ready = date_of_discharge_is_same_as_discharge_ready_date, 
         
         dis_after_ready = date_of_discharge_is_1_days_after_discharge_ready_date,
         
         av_delay_after = average_days_from_discharge_ready_date_to_date_of_discharge_exc_0_day_delays) %>%
  
  mutate(dis_on_ready = round(as.numeric(dis_on_ready)*100, 1), 
         
         dis_after_ready = round(as.numeric(dis_after_ready)*100, 1), 
         
         av_delay_after = round(as.numeric(av_delay_after), 1), 
         
         x1_day = round(as.numeric(x1_day)*100, 1),
         
         x2_3_days = round(as.numeric(x2_3_days)*100, 1), 
         
         x4_6_days = round(as.numeric(x4_6_days)*100, 1), 
         
         x7_13_days = round(as.numeric(x7_13_days)*100, 1), 
         
         x14_20_days = round(as.numeric(x14_20_days)*100, 1),
         
         x21_days_or_more = round(as.numeric(x21_days_or_more)*100, 1))

#Join data

dis_data_national <-  rbind(dis_data_national, dis_data_national_2, fill=TRUE)

dim(dis_data_national)

# Save data

write_rds(dis_data_national,here('dis_nat.RDS'))



## data cleaning - regional data ####

# October 2023 - August 2024 #

dis_data_regional <- lapply(dis_data,
                            
                            function(x){
                              
                              names(x) <- x[13,]
                              
                              x['date'] <- x[2,2]
                              
                              data <- x[-c(1:13),]
                              
                              data <- data %>% janitor::clean_names()
                              
                              return(data)
                              
                            }) %>%
  
  data.table::rbindlist(fill=T) %>%
  
  mutate(date = case_when(date == 45200 ~ 'October 2023',
                          
                          date == 45231 ~ 'November 2023', 
                          
                          date == 45261 ~ 'December 2023', 
                          
                          date == 45292 ~ 'January 2024',
                          
                          date == 45323 ~ 'February 2024',
                          
                          date == 45352 ~ 'March 2024', 
                          
                          date == 45383 ~ 'April 2024',
                          
                          date == 45413 ~ 'May 2024',
                          
                          date == 45444 ~ 'June 2024',
                          
                          date == 45474 ~ 'July 2024',
                          
                          date == 45505 ~ 'August 2024')) %>%
  
  mutate(date = my(date)) %>%
  
  mutate(code = if_else(is.na(code), org_code, code)) %>%
  
  filter(!code == 'National') #%>%
  
  filter(!is.na(code)) %>% 
  
  filter(!is.na(region)) %>% 
  
  select(date, region, date_of_discharge_is_same_as_discharge_ready_date, date_of_discharge_is_1_days_after_discharge_ready_date, average_days_from_discharge_ready_date_to_date_of_discharge_exc_0_day_delays, x1_day, x2_3_days, x4_6_days, x7_13_days, x14_20_days, x21_days_or_more) %>% 
  
  # select(-c(organisation_name, na, na_2, na_3, na_4, percent_of_providers_submitting_acceptable_data, code, icb, org_code, data_source)) %>%
  
  rename(
         dis_on_ready = date_of_discharge_is_same_as_discharge_ready_date,

         dis_after_ready = date_of_discharge_is_1_days_after_discharge_ready_date,
         
         av_delay_after = average_days_from_discharge_ready_date_to_date_of_discharge_exc_0_day_delays) %>%
  
   mutate(dis_on_ready = round(as.numeric(dis_on_ready)*100, 1), 
         
         dis_after_ready = round(as.numeric(dis_after_ready)*100, 1), 
         
         av_delay_after = round(as.numeric(av_delay_after), 1),

         x1_day = round(as.numeric(x1_day)*100, 1),

         x2_3_days = round(as.numeric(x2_3_days)*100, 1),

         x4_6_days = round(as.numeric(x4_6_days)*100, 1),

         x7_13_days = round(as.numeric(x7_13_days)*100, 1),

         x14_20_days = round(as.numeric(x14_20_days)*100, 1),

         x21_days_or_more = round(as.numeric(x21_days_or_more)*100, 1)) %>%
        
         group_by(date, region) %>% 
  
        summarise(dis_on_ready = mean(dis_on_ready, na.rm = TRUE),
                   dis_after_ready = mean(dis_after_ready, na.rm = TRUE),
                   av_delay_after = mean(av_delay_after, na.rm = TRUE),
                   x1_day = mean(x1_day, na.rm = TRUE),
                   x2_3_days = mean(x2_3_days, na.rm = TRUE),
                   x4_6_days = mean(x4_6_days, na.rm = TRUE),
                   x7_13_days = mean(x7_13_days, na.rm = TRUE),
                   x14_20_days = mean(x14_20_days, na.rm = TRUE),
                   x21_days_or_more = mean(x21_days_or_more, na.rm = TRUE))

view(dis_data_regional)

# September 2024 - onwards? #

dis_data_regional_2 <- lapply(dis_data_2,
                              
                              function(x){
                                
                                names(x) <- x[14,]
                                
                                x['date'] <- x[2,2]
                                
                                data <- x[-c(1:14),c(4:6,8,12:14,31:44,46,47,1:3,7)]
                                
                                data <- data %>% janitor::clean_names()
                                
                                return(data)
                                
                              }) %>%
  
  data.table::rbindlist(fill=T) %>%
  
  mutate(date = case_when(date == 45536 ~ 'September 2024')) %>%
  
  mutate(date = my(date)) %>%
  
  filter(org_code %in% c('Y61', 'Y60', 'Y63', 'Y56', 'Y58', 'Y59', 'Y62')) %>%
  
  select(organisation_name, date, date_of_discharge_is_same_as_discharge_ready_date, date_of_discharge_is_1_days_after_discharge_ready_date, average_days_from_discharge_ready_date_to_date_of_discharge_exc_0_day_delays, x1_day, x2_3_days, x4_6_days, x7_13_days, x14_20_days, x21_days_or_more) %>% 
  
  # select(-c(na, na_2, na_3, na_4, percent_of_providers_submitting_acceptable_data)) %>%

  rename(dis_on_ready = date_of_discharge_is_same_as_discharge_ready_date, 
         
         dis_after_ready = date_of_discharge_is_1_days_after_discharge_ready_date,
         
         av_delay_after = average_days_from_discharge_ready_date_to_date_of_discharge_exc_0_day_delays) %>%
  
  mutate(region = organisation_name, 
    
         dis_on_ready = round(as.numeric(dis_on_ready)*100, 1), 
         
         dis_after_ready = round(as.numeric(dis_after_ready)*100, 1), 
         
         av_delay_after = round(as.numeric(av_delay_after), 1), 
         
         x1_day = round(as.numeric(x1_day)*100, 1),
         
         x2_3_days = round(as.numeric(x2_3_days)*100, 1), 
         
         x4_6_days = round(as.numeric(x4_6_days)*100, 1), 
         
         x7_13_days = round(as.numeric(x7_13_days)*100, 1), 
         
         x14_20_days = round(as.numeric(x14_20_days)*100, 1),
         
         x21_days_or_more = round(as.numeric(x21_days_or_more)*100, 1)) %>% 
  select(-organisation_name)


#Join data

dis_data_regional <-  rbind(dis_data_regional, dis_data_regional_2)

dim(dis_data_regional)

# Save data
#here <- '/home/hannah.maconochie@tier0'
write_rds(dis_data_regional,here('dis_reg.RDS'))

## ICB data ###

# October 2023 - August 2024 #

dis_data_icb <- lapply(dis_data,
                            
                            function(x){
                              
                              names(x) <- x[13,]
                              
                              x['date'] <- x[2,2]
                              
                              data <- x[-c(1:13),]
                              
                              data <- data %>% janitor::clean_names()
                              
                              return(data)
                              
                            }) %>%
  
  data.table::rbindlist(fill=T) %>%
  
  mutate(date = case_when(date == 45200 ~ 'October 2023',
                          
                          date == 45231 ~ 'November 2023', 
                          
                          date == 45261 ~ 'December 2023', 
                          
                          date == 45292 ~ 'January 2024',
                          
                          date == 45323 ~ 'February 2024',
                          
                          date == 45352 ~ 'March 2024', 
                          
                          date == 45383 ~ 'April 2024',
                          
                          date == 45413 ~ 'May 2024',
                          
                          date == 45444 ~ 'June 2024',
                          
                          date == 45474 ~ 'July 2024',
                          
                          date == 45505 ~ 'August 2024')) %>%
  
  mutate(date = my(date)) %>%
  
  mutate(code = if_else(is.na(code), org_code, code)) %>%
  
  filter(!code == 'National') %>%

filter(!is.na(code)) %>% 
  
  filter(!is.na(icb)) %>% 
  
  select(date, icb, date_of_discharge_is_same_as_discharge_ready_date, date_of_discharge_is_1_days_after_discharge_ready_date, average_days_from_discharge_ready_date_to_date_of_discharge_exc_0_day_delays, x1_day, x2_3_days, x4_6_days, x7_13_days, x14_20_days, x21_days_or_more) %>% 
  
  # select(-c(organisation_name, na, na_2, na_3, na_4, percent_of_providers_submitting_acceptable_data, code, icb, org_code, data_source)) %>%
  
  rename(
    dis_on_ready = date_of_discharge_is_same_as_discharge_ready_date,
    
    dis_after_ready = date_of_discharge_is_1_days_after_discharge_ready_date,
    
    av_delay_after = average_days_from_discharge_ready_date_to_date_of_discharge_exc_0_day_delays) %>%
  
  mutate(dis_on_ready = round(as.numeric(dis_on_ready)*100, 1), 
         
         dis_after_ready = round(as.numeric(dis_after_ready)*100, 1), 
         
         av_delay_after = round(as.numeric(av_delay_after), 1),
         
         x1_day = round(as.numeric(x1_day)*100, 1),
         
         x2_3_days = round(as.numeric(x2_3_days)*100, 1),
         
         x4_6_days = round(as.numeric(x4_6_days)*100, 1),
         
         x7_13_days = round(as.numeric(x7_13_days)*100, 1),
         
         x14_20_days = round(as.numeric(x14_20_days)*100, 1),
         
         x21_days_or_more = round(as.numeric(x21_days_or_more)*100, 1)) %>%
  
  group_by(date, icb) %>% 
  
  summarise(#across(everything(), sum))
    dis_on_ready = mean(dis_on_ready, na.rm = TRUE),
    dis_after_ready = mean(dis_after_ready, na.rm = TRUE),
    av_delay_after = mean(av_delay_after, na.rm = TRUE),
    x1_day = mean(x1_day, na.rm = TRUE),
    x2_3_days = mean(x2_3_days, na.rm = TRUE),
    x4_6_days = mean(x4_6_days, na.rm = TRUE),
    x7_13_days = mean(x7_13_days, na.rm = TRUE),
    x14_20_days = mean(x14_20_days, na.rm = TRUE),
    x21_days_or_more = mean(x21_days_or_more, na.rm = TRUE))

# September 2024 - onwards? #

dis_data_icb_2 <- lapply(dis_data_2,
                              
                              function(x){
                                
                                names(x) <- x[14,]
                                
                                x['date'] <- x[2,2]
                                
                                data <- x[-c(1:14),c(4:6,8,12:14,31:44,46,47,1:3,7)]
                                
                                data <- data %>% janitor::clean_names()
                                
                                return(data)
                                
                              }) %>%
  
  data.table::rbindlist(fill=T) %>%
  
  mutate(date = case_when(date == 45536 ~ 'September 2024')) %>%
  
  mutate(date = my(date)) %>%
  
  filter(org_code %in% c()) %>%
  
  select(organisation_name, date, date_of_discharge_is_same_as_discharge_ready_date, date_of_discharge_is_1_days_after_discharge_ready_date, average_days_from_discharge_ready_date_to_date_of_discharge_exc_0_day_delays, x1_day, x2_3_days, x4_6_days, x7_13_days, x14_20_days, x21_days_or_more) %>% 
  
  # select(-c(na, na_2, na_3, na_4, percent_of_providers_submitting_acceptable_data)) %>%
  
  rename(dis_on_ready = date_of_discharge_is_same_as_discharge_ready_date, 
         
         dis_after_ready = date_of_discharge_is_1_days_after_discharge_ready_date,
         
         av_delay_after = average_days_from_discharge_ready_date_to_date_of_discharge_exc_0_day_delays) %>%
  
  mutate(region = organisation_name, 
         
         dis_on_ready = round(as.numeric(dis_on_ready)*100, 1), 
         
         dis_after_ready = round(as.numeric(dis_after_ready)*100, 1), 
         
         av_delay_after = round(as.numeric(av_delay_after), 1), 
         
         x1_day = round(as.numeric(x1_day)*100, 1),
         
         x2_3_days = round(as.numeric(x2_3_days)*100, 1), 
         
         x4_6_days = round(as.numeric(x4_6_days)*100, 1), 
         
         x7_13_days = round(as.numeric(x7_13_days)*100, 1), 
         
         x14_20_days = round(as.numeric(x14_20_days)*100, 1),
         
         x21_days_or_more = round(as.numeric(x21_days_or_more)*100, 1), 
         
         ) %>% 
  select(-organisation_name)

#Join data

dis_data_regional <-  rbind(dis_data_regional, dis_data_regional_2)

dim(dis_data_regional)

# Save data
write_rds(dis_data_regional,here('dis_reg.RDS'))

## data cleaning - all trusts ####

dis_data_a <- dis_data_a %>%
  
  mutate(date = case_when(date == 45200 ~ 'October 2023',
                          
                          date == 45231 ~ 'November 2023', 
                          
                          date == 45261 ~ 'December 2023', 
                          
                          date == 45292 ~ 'January 2024',
                          
                          date == 45323 ~ 'February 2024',
                          
                          date == 45352 ~ 'March 2024', 
                          
                          date == 45383 ~ 'April 2024',
                          
                          date == 45413 ~ 'May 2024',
                          
                          date == 45444 ~ 'June 2024',
                          
                          date == 45474 ~ 'July 2024',
                          
                          date == 45505 ~ 'August 2024',
                          
                          date == 45536 ~ 'September 2024', 
                          
                          date == 45566 ~ 'October 2024')) %>%
  
  mutate(date = my(date)) %>%
  
  select(-c(na, na_2, na_3, na_4, percent_of_providers_submitting_acceptable_data)) %>%

  rename(n_prov_acceptable = number_of_providers_submitting_acceptable_data, 
         
         dis_on_ready = date_of_discharge_is_same_as_discharge_ready_date, 
         
         dis_after_ready = date_of_discharge_is_1_days_after_discharge_ready_date,
         
         av_delay_after = average_days_from_discharge_ready_date_to_date_of_discharge_exc_0_day_delays) %>%
  
  mutate(#org_code = if_else(is.na(org_code), organisation_code, org_code),
    
         dis_on_ready = round(as.numeric(dis_on_ready)*100, 1), 
         
         dis_after_ready = round(as.numeric(dis_after_ready)*100, 1), 
         
         av_delay_after = round(as.numeric(av_delay_after), 1), 
         
         x1_day = round(as.numeric(x1_day)*100, 1),
         
         x2_3_days = round(as.numeric(x2_3_days)*100, 1), 
         
         x4_6_days = round(as.numeric(x4_6_days)*100, 1), 
         
         x7_13_days = round(as.numeric(x7_13_days)*100, 1), 
         
         x14_20_days = round(as.numeric(x14_20_days)*100, 1),
         
         x21_days_or_more = round(as.numeric(x21_days_or_more)*100, 1),
         
         x1_day_2 = as.numeric(x1_day_2),
         
         x2_3_days_2 = as.numeric(x2_3_days_2), 
         
         x4_6_days_2 = as.numeric(x4_6_days_2), 
         
         x7_13_days_2 = as.numeric(x7_13_days_2), 
         
         x14_20_days_2 = as.numeric(x7_13_days_2),
         
         x21_days_or_more_2 = as.numeric(x21_days_or_more_2),
         
         total_beddays_delay = rowSums(across(c(x1_day_2, x2_3_days_2,x4_6_days_2, x7_13_days_2, x14_20_days_2, x21_days_or_more_2))))

#Read in bed occupancy data ####

bed_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports/'

bedlinks <- GetLinks(bed_url,'/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports-')

bedlinks <- GetLinks(bedlinks, 'sitrep-data')

#bedlinks <- bedlinks[!grepl(pattern = 'csv', ae_files)]

bedlinks <- bedlinks[!grepl(pattern = 'csv', bedlinks)]

bedfiles <- bedlinks[44:47]

bedfiles

bedfiles2 <- bedlinks[48:51]

bedfiles2

bedfiles3 <- bedlinks[52:55]

bedfiles3

bedfiles4 <- bedlinks[56]

bedfiles4

bedfiles5 <- bedlinks[43]

bedfiles5


## Prep data - Oct 2023 to Jan 2024 ####

bed_data <- lapply(bedfiles,
                   
                   function(x){
                     
                     data <-ReadExcel(files = x, sheet = 3)
                     
                   }) %>% 
  
  flatten()



bed_data_a <- lapply(bed_data,
                     
                     function(x){
                       
                       names(x) <- x[13,]
                       
                       x['date'] <- x[2,2]
                       
                       data <- x[-c(1:67),]
                       
                       data <- data %>% janitor::clean_names()
                       
                       return(data)
                       
                     }) %>%
  
  data.table::rbindlist(fill=T)

dim(bed_data_a)



# data cleaning

bed_data_a <- bed_data_a %>%
  
  select(na_2, date, g_a_beds_available, g_a_core_beds_available, g_a_escalation_beds_available, g_a_covid_void_beds, g_a_occupancy_rate, g_a_occupancy_rate_adjusted_for_covid_void_beds) %>%
  
  rename(org_code = na_2, 
         
         g_a_occupancy = g_a_occupancy_rate, 
         
         g_a_occupancy_adj = g_a_occupancy_rate_adjusted_for_covid_void_beds) %>%
  
  mutate(date = my(date)) %>%
  
  filter(!is.na(org_code)) %>%
  
  mutate(g_a_occupancy = round(as.numeric(g_a_occupancy)*100, 1), 
         
         g_a_occupancy_adj = round(as.numeric(g_a_occupancy_adj)*100, 1),
         
         g_a_occupancy_adj = if_else(is.na(g_a_occupancy_adj), g_a_occupancy, g_a_occupancy_adj))





## Prep data - Feb to May 2024 ####

bed_data <- lapply(bedfiles2,
                   
                   function(x){
                     
                     data <-ReadExcel(files = x, sheet = 2)
                     
                   }) %>% 
  
  flatten()



bed_data_b <- lapply(bed_data,
                     
                     function(x){
                       
                       names(x) <- x[13,]
                       
                       x['date'] <- x[2,2]
                       
                       data <- x[-c(1:67),]
                       
                       data <- data %>% janitor::clean_names()
                       
                       return(data)
                       
                     }) %>%
  
  data.table::rbindlist(fill=T)

dim(bed_data_b)



# data cleaning

bed_data_b <- bed_data_b %>%
  
  mutate(na_2 = case_when(date == 45413 ~ na_3,
                          
                          TRUE ~ na_2)) %>%
  
  mutate(na_3 = case_when(date == 45413 ~ na_4,
                          
                          TRUE ~ na_3)) %>%
  
  select(na_2, date, g_a_beds_available, g_a_core_beds_available, g_a_escalation_beds_available, g_a_covid_void_beds, g_a_occupancy_rate, g_a_occupancy_rate_adjusted_for_covid_void_beds) %>%
  
  rename(org_code = na_2, 
         
         g_a_occupancy = g_a_occupancy_rate, 
         
         g_a_occupancy_adj = g_a_occupancy_rate_adjusted_for_covid_void_beds) %>%
  
  mutate(date = case_when(date == 45383 ~ 'April 2024',
                          
                          date == 45413 ~ 'May 2024',
                          
                          TRUE ~ date)) %>%
  
  mutate(date = my(date)) %>%
  
  filter(!is.na(org_code)) %>%
  
  mutate(g_a_occupancy = round(as.numeric(g_a_occupancy)*100, 1), 
         
         g_a_occupancy_adj = round(as.numeric(g_a_occupancy_adj)*100, 1),
         
         g_a_occupancy_adj = if_else(is.na(g_a_occupancy_adj), g_a_occupancy, g_a_occupancy_adj))



## Prep data - Jun 2024 to Sep 2024 ####

bed_data <- lapply(bedfiles3,
                   
                   function(x){
                     
                     data <-ReadExcel(files = x, sheet = 2)
                     
                   }) %>% 
  
  flatten()



bed_data_c <- lapply(bed_data,
                     
                     function(x){
                       
                       names(x) <- x[13,]
                       
                       x['date'] <- x[2,2]
                       
                       data <- x[-c(1:67),]
                       
                       data <- data %>% janitor::clean_names()
                       
                       return(data)
                       
                     }) %>%
  
  data.table::rbindlist(fill=T)

dim(bed_data_c)



# data cleaning

bed_data_c <- bed_data_c %>%
  
  select(na_3, date, g_a_beds_available, g_a_core_beds_available, g_a_escalation_beds_available, g_a_covid_void_beds, g_a_occupancy_rate, g_a_occupancy_rate_adjusted_for_covid_void_beds) %>%
  
  rename(org_code = na_3, 
         
         g_a_occupancy = g_a_occupancy_rate, 
         
         g_a_occupancy_adj = g_a_occupancy_rate_adjusted_for_covid_void_beds) %>%
  
  mutate(date = case_when(date == 45444 ~ 'June 2024',
                          
                          date == 45474 ~ 'July 2024',
                          
                          date == 45505 ~ 'August 2024',
                          
                          date == 45536 ~ 'September 2024',
                          
                          TRUE ~ date)) %>%
  
  mutate(date = my(date)) %>%
  
  filter(!is.na(org_code)) %>%
  
  mutate(g_a_occupancy = round(as.numeric(g_a_occupancy)*100, 1), 
         
         g_a_occupancy_adj = round(as.numeric(g_a_occupancy_adj)*100, 1),
         
         g_a_occupancy_adj = if_else(is.na(g_a_occupancy_adj), g_a_occupancy, g_a_occupancy_adj))


## Prep data - Sep 2023 ####

bed_data <- lapply(bedfiles5,
                   
                   function(x){
                     
                     data <-ReadExcel(files = x, sheet = 2)
                     
                   }) %>% 
  
  flatten()

bed_data_e <- lapply(bed_data,
                     
                     function(x){
                       
                       names(x) <- x[13,]
                       
                       x['date'] <- x[2,2]
                       
                       data <- x[-c(1:67),]
                       
                       data <- data %>% janitor::clean_names()
                       
                       return(data)
                       
                     }) %>%
  
  data.table::rbindlist(fill=T)

dim(bed_data_e)

# data cleaning

bed_data_e <- bed_data_e %>%
  
  select(na_2, date, g_a_beds_available, g_a_core_beds_available, g_a_escalation_beds_available, g_a_covid_void_beds, g_a_occupancy_rate, g_a_occupancy_rate_adjusted_for_covid_void_beds) %>%
  
  rename(org_code = na_2, 
         
         g_a_occupancy = g_a_occupancy_rate, 
         
         g_a_occupancy_adj = g_a_occupancy_rate_adjusted_for_covid_void_beds) %>%
  
  mutate(date = case_when(date == 45170 ~ 'September 2023',
                          TRUE ~ date)) %>%
  
  mutate(date = my(date)) %>%
  
  filter(!is.na(org_code)) %>%
  
  mutate(g_a_occupancy = round(as.numeric(g_a_occupancy)*100, 1), 
         
         g_a_occupancy_adj = round(as.numeric(g_a_occupancy_adj)*100, 1),
         
         g_a_occupancy_adj = if_else(is.na(g_a_occupancy_adj), g_a_occupancy, g_a_occupancy_adj))  %>% 
  filter(org_code %in% c('RXQ','RTX','RAE','RNA','RK5','RM1', 'RBL'))


#Bind together

bed_data_a <- rbind(bed_data_a, bed_data_b)

bed_data_a <- rbind(bed_data_a, bed_data_c)

bed_data_a <- rbind(bed_data_a, bed_data_d)

dim(bed_data_a)

bed_data <- lapply(bedfiles,
                   
                   function(x){
                     
                     data <-ReadExcel(files = x, sheet = 3)
                     
                   }) %>% 
  
  flatten()



bed_data_a <- lapply(bed_data,
                     
                     function(x){
                       
                       names(x) <- x[13,]
                       
                       x['date'] <- x[2,2]
                       
                       data <- x[-c(1:67),]
                       
                       data <- data %>% janitor::clean_names()
                       
                       return(data)
                       
                     }) %>%
  
  data.table::rbindlist(fill=T)

dim(bed_data_a)



# data cleaning

bed_data_a <- bed_data_a %>%
  
  select(na_2, date, g_a_beds_available, g_a_core_beds_available, g_a_escalation_beds_available, g_a_covid_void_beds, g_a_occupancy_rate, g_a_occupancy_rate_adjusted_for_covid_void_beds) %>%
  
  rename(org_code = na_2, 
         
         g_a_occupancy = g_a_occupancy_rate, 
         
         g_a_occupancy_adj = g_a_occupancy_rate_adjusted_for_covid_void_beds) %>%
  
  mutate(date = my(date)) %>%
  
  filter(!is.na(org_code)) %>%
  
  mutate(g_a_occupancy = round(as.numeric(g_a_occupancy)*100, 1), 
         
         g_a_occupancy_adj = round(as.numeric(g_a_occupancy_adj)*100, 1),
         
         g_a_occupancy_adj = if_else(is.na(g_a_occupancy_adj), g_a_occupancy, g_a_occupancy_adj))








# Calculate medians for each month 

#timeseries_dds <- dd_data %>%

#  select(1,delayed_discharges=4) %>%

#  mutate(month = lubridate::month(date)) %>%

#  mutate(year = year(date)) %>%

#  filter((date > '2023-10-01' & date <= '2024-04-30' )) %>%

#  group_by(month, year) %>%

#  summarise(delayed_discharges = median(delayed_discharges)) %>%

#  arrange(year, month)





# join to main data table and join Sep 2023 data####

dis_data_a <- dis_data_a %>% left_join(bed_data_a)

dis_data_4a <- dis_data_4a %>%  left_join(bed_data_e)

# calculate % beds occupied by delayed patients

# NOTE: we are dividing monthly bed days used for delays by the number of days in the month

# to get average daily number of beds occupied by patients with delays

# then we divide by the number of General and Acute beds for that month



dis_data_a <- dis_data_a %>%
  
  mutate(days_in_month = days_in_month(date), 
         
         bed_delays = round(total_beddays_delay/days_in_month, 1),
         
         pct_bed_delays = round(bed_delays/as.numeric(g_a_beds_available)*100, 1))

dis_data_4a <- dis_data_4a %>%
  
  mutate(days_in_month = days_in_month(date), 
         
         bed_delays = round(total_beddays_delay/days_in_month, 1),
         
         pct_bed_delays = round(bed_delays/as.numeric(g_a_beds_available)*100, 1))



# Save data

write_rds(dis_data_a, here('dis_data.RDS'))

write_rds(dis_data_4a, here('dis_data_sep_23.RDS'))





# import delayed discharge patients



#Read in discharge ready date data ####

delay_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-acute-data/'

delaylinks <- GetLinks(delay_url, 'Daily-discharge-sitrep-monthly-data-webfile')

delaylinks <- delaylinks[!grepl(pattern = 'csv', delaylinks)]

delaylinks <- delaylinks[19:31] # this needs to be from [19:26] |(now update to 31 to reflect data up to Oct 2024) but for some reason, the 19 (October 2023) throws an error. Will look into.

delaylinks



# Prep data

delay_data <- lapply(delaylinks,
                     
                     function(x){
                       
                       data <-ReadExcel(files = x, sheet = "Table 2")
                       
                     }) %>% 
  
  flatten()



delay_data_a <- lapply(delay_data,
                       
                       function(x){
                         
                         names(x) <- x[3,]
                         
                         x['date'] <- x[2,4]
                         
                         #data <- x[-c(1:15),]
                         
                         data <- x[58:nrow(x),c(2, 4:ncol(x))]
                         
                         data <- data %>% janitor::clean_names()
                         
                         return(data)
                         
                       }) %>%
  
  data.table::rbindlist(fill=T)

dim(delay_data_a)





# data cleaning

delay_data_a <- delay_data_a %>%
  
  mutate(date = case_when(date == 45200 ~ 'October 2023',
                          
                          date == 45231 ~ 'November 2023', 
                          
                          date == 45261 ~ 'December 2023', 
                          
                          date == 45292 ~ 'January 2024',
                          
                          date == 45323 ~ 'February 2024',
                          
                          date == 45352 ~ 'March 2024', 
                          
                          date == 45383 ~ 'April 2024',
                          
                          date == 45413 ~ 'May 2024',
                          
                          date == 45444 ~ 'June 2024',
                          
                          date == 45474 ~ 'July 2024',
                          
                          date == 45505 ~ 'August 2024',
                          
                          date == 45536 ~ 'September 2024',
                          
                          date == 45566 ~ 'October 2024')) %>%
  
  mutate(date = my(date)) %>%
  
  rename(code = na) %>%
  
  mutate(across(c(where(is.character), -c(code, date)), as.numeric)) %>%
  
  group_by(date)%>%
  
  rowwise() %>%
  
  mutate(med_del_dis = median(c_across(starts_with("number_of_patients_remaining_in_hospital")), na.rm = TRUE),
         
         med_daily_dis = median(c_across(starts_with("number_of_patients_discharged")), na.rm = TRUE),
         
         med_ready_to_dis = median(c_across(starts_with("number_of_patients_who_no_longer")), na.rm = TRUE)) %>%
  
  select(code, date, med_del_dis, med_daily_dis, med_ready_to_dis)









### import discharge destination data from delay files

# Prep data

delayfiles <- delaylinks[1:8]

delayfiles

delayfiles2 <- delaylinks[9:13]

delayfiles2


dest_data <- lapply(delayfiles,
                    
                    function(x){
                      
                      data <-ReadExcel(files = x, sheet = "Table 4")
                      
                    }) %>% 
  
  flatten()



dest_data_a <- lapply(dest_data,
                      
                      function(x){
                        
                        names(x) <- x[4,]
                        
                        x['date'] <- x[3,4]
                        
                        data <- x[59:nrow(x),-1]
                        
                        data <- data %>% janitor::clean_names()

                        return(data)
                        
                      }) %>%
  
  data.table::rbindlist(fill=T)

dim(dest_data_a)

dest_data2 <- lapply(delayfiles2,
                    
                    function(x){
                      
                      data <-ReadExcel(files = x, sheet = "Table 4")
                      
                    }) %>% 
  
  flatten()



dest_data_2a <- lapply(dest_data2,
                      
                      function(x){
                        
                        names(x) <- x[4,]
                        
                        x['date'] <- x[3,4]
                        
                        data <- x[59:nrow(x),-1]
                        
                        data <- data %>% janitor::clean_names()
                        
                        return(data)
                        
                      }) %>%
  
  data.table::rbindlist(fill=T)

dim(dest_data_2a)

# data cleaning

  dest_data_a[,3:14] <- dest_data_a[,3:14] %>% mutate_if(is.character,as.numeric)
  

  dest_data_a <- dest_data_a %>%  mutate(date = str_remove(date, fixed(" - Total number of patients discharged")),
         
         organisation_code = na,

         organisation_name = na_2,
         
         pathway_0_total = rowSums(across(c(p0_domestic_home_without_reablement_support, p0_other_without_reablement_support))),
         
         pathway_1_total = rowSums(across(c(p1_domestic_home_with_reablement_support, p1_other_with_reablement_support, p1_hotel_with_reablement_support))),
         
         pathway_2_total = rowSums(across(c(p2_care_home_short_term_24hr_support, p2_designated_setting_isolation_before_moving_to_care_home, p2_hospice_short_term_24hr_support, p2_community_rehab_setting_short_term_24hr_support)), na.rm = TRUE), 
         
         pathway_3_total = rowSums(across(c(p3_care_home_new_admission_likely_permanent, p3_designated_setting_isolation_before_moving_to_care_home_as_new_admission, p3b_care_home_existing_resident_discharged_back)))) %>%
  
  mutate(date = my(date))  
  
  dest_data_a <- dest_data_a[,c(15:21)]
 
  dest_data_2a <- dest_data_2a %>%
    
    mutate(date = str_remove(date, fixed(" - Total number of patients discharged")),
           
           organisation_code = na,
           
           organisation_name = na_2) %>%
    
    mutate(date = my(date))  
  
  dest_data_2a <- dest_data_2a[, c(12:18)]

#join data together
  
dest_data_a <- rbind(dest_data_a, dest_data_2a, fill = TRUE)

#save data 

write_rds(dest_data_a,here('destination_data.RDS'))

### import barriers to discharge data from delay files

# Prep data

barriersfiles <- delaylinks[c(1:6, 8)]

barriersfiles

barriersfiles2 <- delaylinks[7]

barriersfiles2

barriersfiles3 <- delaylinks[c(9:11,13)]

barriersfiles3

barriersfiles4 <- delaylinks[12]

barriersfiles4

barriers_data <- lapply(barriersfiles,
                        
                        function(x){
                          
                          data <-ReadExcel(files = x, sheet = "Table 5")
                          
                        }) %>% 
  
  flatten()

barriers_data2 <- lapply(barriersfiles2,
                         
                         function(x){
                           
                           data <-ReadExcel(files = x, sheet = "Table 5")
                           
                         }) %>% 
  
  flatten()

barriers_data3 <- lapply(barriersfiles3,
                         
                         function(x){
                           
                           data <-ReadExcel(files = x, sheet = "Table 5")
                           
                         }) %>% 
  
  flatten()

barriers_data4 <- lapply(barriersfiles4,
                         
                         function(x){
                           
                           data <-ReadExcel(files = x, sheet = "Table 5")
                           
                         }) %>% 
  
  flatten()

### CHECK THE COLUMNS LOCATIONS ARE CORRECT FOR EXTRACTING RELEVANT DATA FOR THIS SHEET:

barriers_data_a <- lapply(barriers_data,
                          
                          function(x){
                            
                            names(x) <- x[3,]
                            
                            x['date'] <- x[2,4]
                            
                            #data <- x[-c(1:15),]
                            
                            data <- x[58:nrow(x), -1]
                            
                            data <- data %>% janitor::clean_names()
                            
                            return(data)
                            
                          }) %>%
  
  data.table::rbindlist(fill=T)

dim(barriers_data_a)

barriers_data_2a <- lapply(barriers_data2,
                          
                          function(x){
                            
                            names(x) <- x[3,]
                            
                            x['date'] <- x[2,5]
                            
                            #data <- x[-c(1:15),]
                            
                            data <- x[58:176, -c(1:2)]
                            
                            data <- data %>% janitor::clean_names()
                            
                            return(data)
                            
                          }) %>%
  
  data.table::rbindlist(fill=T)

dim(barriers_data_2a)

barriers_data_3a <- lapply(barriers_data3,
                          
                          function(x){
                            
                            names(x) <- x[4,]
                            
                            x['date'] <- x[2,4]
                            
                            #data <- x[-c(1:15),]
                            
                            data <- x[59:nrow(x), -c(1,41)]
                            
                            data <- data %>% janitor::clean_names()
                            
                            return(data)
                            
                          }) %>%
  
  data.table::rbindlist(fill=T)

dim(barriers_data_3a)

barriers_data_4a <- lapply(barriers_data4,
                           
                           function(x){
                             
                             names(x) <- x[4,]
                             
                             x['date'] <- x[2,5]
                             
                             #data <- x[-c(1:15),]
                             
                             data <- x[59:nrow(x), -c(1:2,42)]
                             
                             data <- data %>% janitor::clean_names()
                             
                             return(data)
                             
                           }) %>%
  
  data.table::rbindlist(fill=T)

dim(barriers_data_4a)

#Join the data with the same columns together

barriers_data_a <- rbind(barriers_data_a, barriers_data_2a)
barriers_data_3a <- rbind(barriers_data_3a, barriers_data_4a) 

# data cleaning

barriers_data_a <- barriers_data_a %>%
  
  mutate(date = str_remove(date, fixed(" - Average number of people per day with length of stay 14 days or over who no longer meet the criteria to reside but were not discharged")), 
         
         organisation_code = na,
         
         organisation_name = na_2) %>%
  
  mutate(date = my(date)) 
  
  barriers_data_a <-barriers_data_a[,-c(1,2)] 
  
  barriers_data_3a <- barriers_data_3a %>% 
    
    mutate(date = str_remove(date, fixed(" - Average number of people per day with a length of stay of 14 days or over who no longer meet the criteria to reside but were not discharged")),
    
    organisation_code = na,

    organisation_name = na_2) %>%

    mutate(date = my(date)) 
  
   barriers_data_3a <-barriers_data_3a[,-c(1,2)]
   
# Join data
   
barriers_data_a <- rbind(barriers_data_a, barriers_data_3a, fill = TRUE)
   
  
# Save data

write_rds(barriers_data_a,here('barriers_data.RDS'))

#Zeyad's nice staffing code

GetData <- function() {
  
  # read links
  # Note there is a very annoying amount of cleaning to do here
  # This is primarily due to the nature of what we're trying to get which is the med
  # Updated it to take 2023 data and the trust data that we want (the Group and Organisation workforcedata)
  
  workforce_url <- "https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics"
  
  workforce_links1 <- Rpublic::extract_links(workforce_url, "nhs-workforce-statistics")
  
  workforce_links2 <- purrr::map(
    .x = paste0("https://digital.nhs.uk/", workforce_links1),
    .f = Rpublic::extract_links,
    pattern = ".zip"
  ) |>
    unlist()
  
  
  workforce_links3 <- workforce_links2[grepl(pattern = "%20csv%20files.zip|nhs-work-stat-|nhs_workforce_statistics_", workforce_links2)]
  
  workforce_links <- workforce_links3[!grepl(pattern = "medical|med|nonmed|quart|2013|2014|2015", workforce_links3)]
  
  workforce_links <- workforce_links[1:13] 
  
  CleanWorkforceData <- function(x) {
    x |>
      dplyr::bind_rows() |>
      dplyr::mutate(Date = as.Date(Date)) |>
      dplyr::filter(lubridate::year(Date) >= 2023) |>
      janitor::clean_names() |>
      dplyr::select(date, specialty, specialty_group, total_fte, grade, org_code, grade_sort_order)
  }
  
  CleanWorkforceDataNonMed <- function(x) {
    x |>
      dplyr::bind_rows() |>
      dplyr::mutate(Date = as.Date(Date,format = '%d/%m/%Y')) |>
      dplyr::filter(lubridate::year(Date) >= 2023) |>
      janitor::clean_names() |>
      dplyr::select(dplyr::any_of(c('date', 'org_code', 'org_name', 'main_staff_group','staff_group_1','staff_group_2','care_setting','total_fte')))
  }
  
  CleanWorkforceDataSimple <- function(x) {
    x |>
      dplyr::bind_rows() |>
      dplyr::mutate(Date = as.Date(Date)) |>
      dplyr::filter(lubridate::year(Date) >= 2023) |>
      janitor::clean_names() 
    
  }
  
  # read data
  workforce_data <- purrr::map(
    .x = workforce_links,
    .f = function(.x) {
      Rpublic::extract_zipped(.x, pattern = "medical staff") |>
        CleanWorkforceData()
    }
  ) 
  
  workforce_data_nonmed <- purrr::map(
    .x = workforce_links,
    .f = function(.x) {
      Rpublic::extract_zipped(.x, pattern = "excluding") |> 
        CleanWorkforceDataNonMed()
    }
  ) 
  
  workforce_data_simple <- purrr::map(
    .x = workforce_links,
    .f = function(.x) {
      Rpublic::extract_zipped(.x, pattern = "Group and Organisation") |> 
        CleanWorkforceDataSimple()
    }
  ) 
  
  workforce_data_1 <- workforce_data |> 
    purrr::keep(function(x) nrow(x) <= 20000 & nrow(x) != 0) |> 
    dplyr::bind_rows()
  
  workforce_data_2 <- workforce_data_nonmed |> 
    dplyr::bind_rows()
  
  workforce_data_3 <- workforce_data_simple |> 
    dplyr::bind_rows()
  
  return(list(
    medical_workforce = workforce_data_1,
    non_medical_workforce = workforce_data_2,
    simple_workforce = worforce_data_3
  ))
  
}

workforce_data <- GetData()
rm(GetData)

#Clean the staffing data

workforce_data_2a <- workforce_data_2 %>% 
  filter(org_code %in% c('RXQ','RTX','RAE','RNA','RK5','RM1', 'RBL'))

workforce_data_3a <- workforce_data_3 %>% 
  filter(org_code %in% c('RXQ','RTX','RAE','RNA','RK5','RM1', 'RBL')) %>% 
  .[,c(1, 6:7, 10:13)] %>% 
  filter(data_type == 'FTE') %>% 
  filter(as.Date(date) %in% c('2023-10-31', '2024-09-30')) %>% 
 distinct(.)  

  workforce_data_3a$staff_group[workforce_data_3a$staff_group == 'HCHS doctors'] <- 'HCHS Doctors'

  workforce_data_3a <- workforce_data_3a %>%  
    pivot_wider(., names_from = date, values_from = total) %>% 
  mutate(staffing_diff = `2024-09-30` - `2023-10-31`)
  
  write_rds(workforce_data_3a,here('workforce_data.RDS'))
  