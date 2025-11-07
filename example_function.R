## Functionalise

get_data <- function(m_use, file = temp_file){
  
  return(
    read_excel(file, sheet = 2, skip=14) %>% 
    rename(Region = 1,
           org_code = 3,
           acute_beds = `G&A beds available`,
           adult_acute_beds = `Adult G&A beds available`,
           occupied_beds = `G&A beds occupied`,
           occupancy_rate = `G&A occupancy rate`) %>% 
    left_join(trust_codes,by='org_code') %>% 
    filter(Flag==1) %>%
    mutate(month = m_use) %>% 
    select(month, org_code, acute_beds, adult_acute_beds, occupied_beds, occupancy_rate)
  )
  
}

url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/202405-May-2024-sitrep-data-FINAL-revised.xlsx"
GET(url, write_disk(temp_file <- tempfile(fileext = ".xlsx")))

get_data('May-24')
