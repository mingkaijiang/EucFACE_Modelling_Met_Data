prepare_met_data <- function() {
    
    #### Download temperature and radiation data
    myDF <- download_temperature_data()
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_T1.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)  
    myDF <- myDF[order(myDF$DateTime),]
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
    myDF$DateHour <- as.POSIXct(paste0(myDF$Date, " ", hour(myDF$DateTime), ":00:00"),format = "%Y-%m-%d %H:%M:%S")
    
    myDF$AirTc_Avg <- as.numeric(myDF$AirTc_Avg)
    #myDF$RH_Avg <- as.numeric(myDF$RH_Avg)
    myDF$Net_SW_Avg <- as.numeric(myDF$Net_SW_Avg)
    myDF$Net_LW_Avg <- as.numeric(myDF$Net_LW_Avg)
    
    
    ### Calculate daily mean
    dDF <- aggregate(myDF[c("AirTc_Avg","Net_SW_Avg", "Net_LW_Avg")], 
                     by=myDF[c("Date")], 
                     FUN=mean, na.rm=T, keep.names=T)
    
    ### Colnames
    colnames(dDF) <- c("Date", "Tair", "Net_SW", "Net_LW")
    
    
    ### download rainfall data
    myDF2 <- download_rainfall_data()
    #### Assign ring information
    myDF2$Ring <- sub("FACE_R", "", myDF2$Source)
    myDF2$Ring <- sub("_T1.*", "", myDF2$Ring)
    myDF2$Ring <- as.numeric(myDF2$Ring)  
    myDF2 <- myDF2[order(myDF2$DateTime),]
    myDF2$Month <- format(as.Date(myDF2$Date), "%Y-%m")
    myDF2$Month <- as.Date(paste0(myDF2$Month,"-1"), format = "%Y-%m-%d") 
    myDF2$DateHour <- as.POSIXct(paste0(myDF2$Date, " ", hour(myDF2$DateTime), ":00:00"),format = "%Y-%m-%d %H:%M:%S")
    
    myDF2$Rain_mm_Tot <- as.numeric(myDF2$Rain_mm_Tot)
    
    ### Calculate daily mean
    dDF2 <- summaryBy(Rain_mm_Tot~Date+Ring, FUN=sum, data=myDF2, keep.names=T, na.rm=T)
    
    dDF3 <- summaryBy(Rain_mm_Tot~Date, FUN=mean, data=dDF2, keep.names=T, na.rm=T)
    
    ### merge
    out <- merge(dDF, dDF3, by=c("Date"))
    
    ### save csv
    write.csv(dDF, "output/EucFACE_met_data_for_LPJ.csv", row.names=F)

    return(dDF)
    
}