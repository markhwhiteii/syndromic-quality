#arrived = GMT and visit = local where the visit occurred
# prep
setwd("~/Desktop")
library(dplyr)
library(lubridate)
library(tidyr)

## pipes and dplyr
2 %>% prod(3)
prod(2, 3)

c(0:10) %>% 
  mean()
mean(c(0:10))

data(starwars)
glimpse(starwars)

sw_base <- starwars
sw_dplyr <- starwars

# in base
sw_base$bmi <- sw_base$mass/((sw_base$height/100)^2) 
sw_base <- sw_base[sw_base$gender %in% c("female", "male"),] 
base_table <- tapply(sw_base$bmi, sw_base$gender, mean, na.rm=TRUE)
base_output <- data.frame(gender=factor(names(base_table)), bmi=unname(base_table))
str(base_output)
base_output

# in dplyr
dplyr_output <- sw_dplyr %>%
  mutate(bmi=mass/((height/100)^2),
         gender=as.factor(gender)) %>% 
  filter(gender %in% c("female", "male")) %>% 
  group_by(gender) %>% 
  summarise(bmi=mean(bmi, na.rm=TRUE)) 

glimpse(dplyr_output)
dplyr_output

## load keys
# usually do this from the mft table
names <- read.csv("facilityKey.csv") %>% 
  mutate(C_Biosense_Facility_ID=as.factor(C_Biosense_Facility_ID)) 
head(names)

## visit-arrival lag
out <- read.csv("valagData.csv") %>% 
  group_by(C_BioSense_ID) %>% 
  slice(which.min(Arrived_Date_Time)) %>% 
  slice(1) %>% 
  mutate(lag=as.numeric(difftime(Arrived_Date_Time, C_Visit_Date_Time, units="hours"))) %>% 
  ungroup() %>%
  group_by(C_Biosense_Facility_ID) %>% 
  summarise(First_Message=round(mean(lag, na.rm=TRUE),2)) %>% 
  mutate(C_Biosense_Facility_ID=factor(C_Biosense_Facility_ID, levels=levels(names$C_Biosense_Facility_ID))) %>% 
  right_join(names, ., by="C_Biosense_Facility_ID") 

# look through output
head(out)

filter(out, Facility_Name=="Bespin") 

out %>% 
  filter(First_Message>24) %>% 
  nrow()

out %>% 
  arrange(desc(First_Message)) %>% 
  head()

slice(out, which.max(First_Message)) 

## record counts
# queried by visit date
out <- read.csv("rcData1.csv") %>%
  left_join(read.csv("facilityKey.csv"), by="C_Biosense_Facility_ID") %>%
  mutate(Date=gsub(" UTC", "", floor_date(ymd_hms(C_Visit_Date_Time), unit="day"))) %>%
  group_by(Facility_Name, Date) %>%
  summarise(Count=n()) %>%
  spread(Facility_Name, Count)

# queried by arrived date
out <- read.csv("rcData2.csv") %>% 
  left_join(read.csv("facilityKey.csv"), by="C_Biosense_Facility_ID") %>% 
  mutate(Date=gsub(" UTC", "", floor_date(ymd_hms(Arrived_Date_Time), unit="day"))) %>% 
  group_by(Facility_Name, Date) %>% 
  summarise(Count=n()) %>% 
  spread(Facility_Name, Count) %>% 
  full_join(out, ., by="Date", suffix=c(" (V)", " (A)"))

out[is.na(out)] <- 0 
out <- out[,c("Date", sort(colnames(out)[-1]))] 

