prepare_soil_data <- function() {
    ### EucFACE soil temperature data
    soilDF <- download_soil_data()
    
    ### assign data and time information
    soilDF$YEAR <- year(soilDF$Date)
    soilDF$DOY <- yday(soilDF$Date)
    soilDF$Hour <- substr(soilDF$DateTime, start=12, stop=13)
    soilDF$Minute <- substr(soilDF$DateTime, start=15, stop=16)
    
    ## ignore 2020 & 2011
    soilDF <- subset(soilDF, YEAR != "2020")
    soilDF <- subset(soilDF, YEAR != "2011")
    
    ## assign half hour 
    soilDF$HalfHour <- ifelse(soilDF$Minute > 30, "30", "00")
    
    ## average soil temperature data across replicates
    soilDF$SoilTemp <- rowMeans(soilDF[c("T30cm_1_Avg", "T30cm_2_Avg")], na.rm=TRUE)
    
    ## half hourly rainfall data
    outDF10 <- summaryBy(SoilTemp~Date+Hour+HalfHour, FUN=mean,
                         data=soilDF, keep.names=T, na.rm=T)
    
    write.csv(outDF10, "output/observed/input/prepare_soil_data.csv", row.names=F)
    
    return(outDF10)
}