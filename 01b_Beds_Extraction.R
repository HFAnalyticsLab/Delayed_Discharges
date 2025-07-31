library(readxl)
library(curl)


url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202404-April-2024-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
apr_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 2,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  mutate(org_code = str_trim(org_code, side = "left")) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Apr-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)



url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202405-May-2024-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
may_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'May-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)



url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202406-June-2024-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jun_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jun-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)



url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202407-July-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jul_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jul-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)



url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202408-August-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
aug_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Aug-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)



url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202409-September-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
sep_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Sept-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)



url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202410-October-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
oct_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Oct-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)



url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202411-November-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
nov_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Nov-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)



url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202412-December-2024-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
dec_24_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Dec-24') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)



url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202501-January-2025-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
jan_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Jan-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)



url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202502-February-2025-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
feb_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Feb-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)



url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202503-March-2025-beds-sitrep-data-FINAL-revised.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
mar_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Mar-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)


url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/202504-April-2025-beds-sitrep-data-finalversion.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
apr_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'Apr-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)


url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202505-May-2025-beds-sitrep-data-finalversion.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel sheet
may_25_beds <- read_excel(temp_file, sheet = 2,skip=14) %>% 
  rename(Region = 1,
         org_code = 3,
         acute_beds = `G&A beds available`,
         adult_acute_beds = `Adult G&A beds available`) %>% 
  left_join(trust_codes,by='org_code') %>% 
  filter(Flag==1) %>%
  mutate(month = 'May-25') %>% 
  select(month,org_code,acute_beds,adult_acute_beds)


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
                       may_25_beds)
