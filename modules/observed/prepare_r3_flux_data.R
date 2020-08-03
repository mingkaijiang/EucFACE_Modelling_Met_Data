prepare_r3_flux_data <- function() {
    
    myDF3 <- download_r3_flux_data()
    
    ### assign data and time information
    myDF3$YEAR <- year(myDF3$Date)
    myDF3$DOY <- yday(myDF3$Date)
    myDF3$Hour <- substr(myDF3$DateTime, start=12, stop=13)
    myDF3$HalfHour <- substr(myDF3$DateTime, start=15, stop=16)
    
    ## ignore 2020 & 2011
    myDF3 <- subset(myDF3, YEAR != "2020")
    myDF3 <- subset(myDF3, YEAR != "2011")
    
    outDF3 <- myDF3[,c("Date", "Hour", "HalfHour",
                       "Ts_mean", 
                       #"LI190SB_PAR_Den_Avg",
                       #"TargTempC_Avg.1.", 
                       "Net_SW_Avg", "Net_LW_Avg",
                       "Net_Rad_Avg", "Pressure_hPa_Avg")]
    
    outDF3$Pressure_kPa <- outDF3$Pressure_hPa_Avg / 10
    outDF3$Pressure_Pa <- outDF3$Pressure_hPa_Avg * 100
    
    outDF3$ID <- paste0(outDF3$Date, "-", outDF3$Hour, "-", outDF3$HalfHour)
    
    outDF <- outDF3[!duplicated(outDF3$ID), ]
    
    outDF$ID <- NULL
    
    ### write
    write.csv(outDF, "output/observed/input/r3_flux_data.csv", row.names=F)    
    
    return(outDF)
}