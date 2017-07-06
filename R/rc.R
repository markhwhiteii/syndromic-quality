rc <- 
  function(channel, month, year) {
  
  suppressPackageStartupMessages(require(RODBC))
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(lubridate))
  suppressPackageStartupMessages(require(tidyr))
  
  out <- channel %>% 
    sqlQuery(paste0("SELECT C_Visit_Date_Time, C_Biosense_Facility_ID FROM KS_PR_PRocessed
                    WHERE MONTH(C_Visit_Date_Time) = ", month, " AND YEAR(C_Visit_Date_Time) = ", year)) %>% 
    left_join(sqlQuery(channel, "SELECT C_Biosense_Facility_ID, Facility_Name FROM KS_MFT"), by="C_Biosense_Facility_ID") %>%
    mutate(Date=gsub(" UTC", "", floor_date(ymd_hms(C_Visit_Date_Time), unit="day"))) %>% 
    group_by(Facility_Name, Date) %>% 
    summarise(Count=n()) %>% 
    spread(Facility_Name, Count) 
  
  out <- channel %>% 
    sqlQuery(paste0("SELECT Arrived_Date_Time, C_Biosense_Facility_ID FROM KS_PR_PRocessed
                    WHERE MONTH(Arrived_Date_Time) = ", month, " AND YEAR(Arrived_Date_Time) = ", year)) %>% 
    left_join(sqlQuery(channel, "SELECT C_Biosense_Facility_ID, Facility_Name FROM KS_MFT"), by="C_Biosense_Facility_ID") %>% 
    mutate(Date=gsub(" UTC", "", floor_date(ymd_hms(Arrived_Date_Time), unit="day"))) %>% 
    group_by(Facility_Name, Date) %>% 
    summarise(Count=n()) %>% 
    spread(Facility_Name, Count) %>% 
    full_join(out, ., by="Date", suffix=c(" (V)", " (A)"))
  
  out[is.na(out)] <- 0
  out <- out[,c("Date", sort(colnames(out)[-1]))]
  
  return(out)
}
