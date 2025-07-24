
# 24/07/2025 - Discharge Ready Data (DRD) filtering by trust

# 1 Remove small & specialist trusts for each month ############################

Sep_23 <- Sep_23 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Oct_23 <- Oct_23 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Nov_23 <- Nov_23 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Dec_23 <- Dec_23 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Jan_24 <- Jan_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Feb_24 <- Feb_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Mar_24 <- Mar_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Apr_24 <- Apr_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
May_24 <- May_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Jun_24 <- Jun_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Jul_24 <- Jul_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Aug_24 <- Aug_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Sep_24 <- Sep_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Oct_24 <- Oct_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Nov_24 <- Nov_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Dec_24 <- Dec_24 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Jan_25 <- Jan_25 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Feb_25 <- Feb_25 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Mar_25 <- Mar_25 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
Apr_25 <- Apr_25 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))
May_25 <- May_25 %>%
  filter(!grepl('CHILDREN|WOMEN|VICTORIA|PAPWORTH|ORTHOPAEDIC|HEART|CHRISTIE|CANCER|EYE', `Org Name`))

# 3 Combine to create filtered England dataset #################################



# 4 Merge monthly datasets for each variable ###################################

# % of discharges delayed (DOD is 1+ days from DRD)


# Average bed days occupied by delayed discharge










































