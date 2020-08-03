prepare_t1_general_data <- function() {
    
    myDF3 <- download_t1_general_data()
    
    ### assign data and time information
    myDF3$YEAR <- year(myDF3$Date)
    myDF3$DOY <- yday(myDF3$Date)
    myDF3$Hour <- substr(myDF3$DateTime, start=12, stop=13)
    myDF3$HalfHour <- substr(myDF3$DateTime, start=15, stop=16)
    
    ## ignore 2020 & 2011
    myDF3 <- subset(myDF3, YEAR != "2020")
    myDF3 <- subset(myDF3, YEAR != "2011")
    
    ### half hour
    myDF3$HalfHour <- as.numeric(as.character(myDF3$HalfHour))
    myDF3$HalfHour2 <- ifelse(myDF3$HalfHour < 30, 0, 30)
    
    ### summaryBy
    sumDF <- summaryBy(Net_SW_Avg+Net_LW_Avg+SlrW_Avg~YEAR+DOY+Hour+HalfHour2+Date, 
                       FUN=mean, data=myDF3, keep.names=T, na.rm=T)
    
    
    test <- summaryBy(Net_SW_Avg+Net_LW_Avg+SlrW_Avg~YEAR, 
                      FUN=mean, keep.names=T, data=sumDF, na.rm=T)
    
    test$sum <- with(test, Net_SW_Avg+SlrW_Avg)
    
    
    
    #return(outDF)
}