prepare_ros_table05_data <- function() {
    #######################################################################################
    ### ROS station radiation, wind speed, air temperature, humidity at 5 min interval
    myDF2 <- download_ros_table05_data()
    
    ### assign data and time information
    myDF2$YEAR <- year(myDF2$Date)
    myDF2$Month <- month(myDF2$Date)
    myDF2$DOY <- yday(myDF2$Date)
    myDF2$Hour <- substr(myDF2$DateTime, start=12, stop=13)
    myDF2$Minute <- substr(myDF2$DateTime, start=15, stop=16)
    
    ## ignore 2020 & 2011
    myDF2 <- subset(myDF2, YEAR != "2020")
    myDF2 <- subset(myDF2, YEAR != "2011")
    
    ## assign half hour 
    myDF2$HalfHour <- ifelse(myDF2$Minute > 30, "30", "00")
    
    ## half hourly data
    outDF1 <- summaryBy(AirTC_Avg+RH+NetSW_Avg+NetLW_Avg+NetRad_Avg~Date+Hour+HalfHour, 
                        FUN=mean,
                        data=myDF2, keep.names=T, na.rm=T)
    
    outDF2 <- summaryBy(PPFD_Avg+WS_ms_Max~Date+Hour+HalfHour, 
                        FUN=max,
                        data=myDF2, keep.names=T, na.rm=T)
    
    outDF <- merge(outDF1, outDF2, by=c("Date", "Hour", "HalfHour"), all=T)
    
    write.csv(outDF, "output/observed/input/ros_table05_data.csv", row.names=F)
    
    return(outDF)
}