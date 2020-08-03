prepare_ros_table15_data <- function() {
    #######################################################################################
    ### ROS station rainfall (and soil temperature, volumetric soil water content) data
    myDF1 <- download_ros_table15_data()
    
    ### assign data and time information
    myDF1$YEAR <- year(myDF1$Date)
    myDF1$Month <- month(myDF1$Date)
    myDF1$DOY <- yday(myDF1$Date)
    myDF1$Hour <- substr(myDF1$DateTime, start=12, stop=13)
    myDF1$Minute <- substr(myDF1$DateTime, start=15, stop=16)
    
    ## ignore 2020 & 2011
    myDF1 <- subset(myDF1, YEAR != "2020")
    myDF1 <- subset(myDF1, YEAR != "2011")
    
    ## assign half hour 
    myDF1$HalfHour <- ifelse(myDF1$Minute > 30, "30", "00")
    
    ### fill SoilTemp missing values
    myDF1$SoilTempROS <- rowMeans(myDF1[c("SoilTemp_Avg.1.", "SoilTemp_Avg.2.")], na.rm=TRUE)
    
    ## half hourly rainfall data
    outDF1 <- summaryBy(Rain_mm_Tot~Date+Hour+HalfHour, FUN=sum,
                        data=myDF1, keep.names=T, na.rm=T)
    
    ## half hourly temperature
    outDF2 <- summaryBy(SoilTempROS+ASoilTemp_Avg~Date+Hour+HalfHour, FUN=mean,
                        data=myDF1, keep.names=T, na.rm=T)
    
    outDF <- merge(outDF1, outDF2, by=c("Date", "Hour", "HalfHour"), all=T)
    
    outDF$SoilTempROS <- NULL
    
    write.csv(outDF, "output/observed/input/ros_table15_data.csv", row.names=F)
    
    return(outDF)
}