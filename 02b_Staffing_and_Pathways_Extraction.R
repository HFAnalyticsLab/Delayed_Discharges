#Staffing and Delayed Discharge Pathways.



#Staffing#######################################################################

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
june_24_staffing <- read_excel(temp_file, sheet = 4,skip=5) %>% 
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
july_24_staffing <- read_excel(temp_file, sheet = 4,skip=5) %>% 
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

url <- ""
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










#Pathway Data####################################################################



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
  mutate(month = 'July-24') %>% 
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

url <- ""
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
