valag <- 
  function(channel, begin, end, table, location="KS") {
  
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(RODBC))
    
  names <- sqlQuery(channel, paste0("SELECT C_Biosense_Facility_ID, Facility_Name FROM ", location, "_MFT")) %>%
    mutate(C_Biosense_Facility_ID=as.factor(C_Biosense_Facility_ID))
    
  out <- channel %>%
    sqlQuery(paste0("SELECT C_Biosense_Facility_ID, C_BioSense_ID, C_Visit_Date_Time, Arrived_Date_Time
                     FROM ", location, "_", table, "_Processed 
                     WHERE C_Visit_Date_Time >= '", begin, "' AND C_Visit_Date_Time <= '", end, "'")) %>%
    group_by(C_BioSense_ID) %>%
    slice(which.min(Arrived_Date_Time)) %>%
    slice(1) %>% 
    mutate(lag=as.numeric(difftime(Arrived_Date_Time, C_Visit_Date_Time, units="hours"))) %>% 
    ungroup() %>% 
    group_by(C_Biosense_Facility_ID) %>% 
    summarise(First_Message=round(mean(lag, na.rm=TRUE),2)) %>% 
    mutate(C_Biosense_Facility_ID=factor(C_Biosense_Facility_ID, levels=levels(names$C_Biosense_Facility_ID))) %>% 
    right_join(names, ., by="C_Biosense_Facility_ID")
  
  return(out)
  }
