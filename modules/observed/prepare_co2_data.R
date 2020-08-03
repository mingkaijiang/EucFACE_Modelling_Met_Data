prepare_co2_data <- function() {
    ### CO2 concentration in the rings
    myDF4 <- download_co2_data()
    
    #### Assign ring information
    myDF4$Ring <- sub("FACE_AUTO_R", "", myDF4$Source)
    myDF4$Ring <- sub("_FCPLOGG.*", "", myDF4$Ring)
    myDF4$Ring <- as.numeric(myDF4$Ring)  
    
    ### assign data and time information
    myDF4$YEAR <- year(myDF4$Date)
    myDF4$DOY <- yday(myDF4$Date)
    myDF4$Hour <- substr(myDF4$DateTime, start=12, stop=13)
    myDF4$Minute <- substr(myDF4$DateTime, start=15, stop=16)
    
    ## ignore 2020 & 2011
    myDF4 <- subset(myDF4, YEAR != "2020")
    myDF4 <- subset(myDF4, YEAR != "2011")
    
    ## assign half hour 
    myDF4$HalfHour <- ifelse(myDF4$Minute > 30, "30", "00")
    
    ## quality control
    myDF4$WindSpeed <- as.numeric(as.character(myDF4$WindSpeed))
    
    myDF4$Concentration.1Min[myDF4$Concentration.1Min > 1000] <- 1000
    myDF4$Concentration.1Min[myDF4$Concentration.1Min < 350] <- 350
    
    myDF4$WindSpeed[myDF4$WindSpeed < 0] <- 0.0
    
    myDF4$IRGA.Pressure[myDF4$IRGA.Pressure < 900] <- 900
    myDF4$IRGA.Pressure[myDF4$IRGA.Pressure > 1100] <- 1100
    
    
    ## half hourly data
    outDF1 <- summaryBy(WindSpeed~Date+Hour+HalfHour+Ring, 
                        FUN=max,
                        data=myDF4, keep.names=T, na.rm=T)
    
    outDF2 <- summaryBy(Air.Temp+Concentration.1Min+IRGA.Vapor.Pressure+IRGA.Pressure+PPFD~Date+Hour+HalfHour+Ring, 
              FUN=mean,
              data=myDF4, keep.names=T, na.rm=T)
    
    outDF4 <- merge(outDF1, outDF2, by=c("Date", "Hour", "HalfHour", "Ring"), all=T)
    
    
    outDF4$RH <- outDF4$IRGA.Vapor.Pressure/saturate_vp_func(outDF4$Air.Temp)

    ### quality control
    outDF4$Air.Temp[outDF4$Air.Temp > 55] <- 55
    outDF4$RH[outDF4$RH > 100] <- 100
    
    ### averaging across rings
    outDF5 <- summaryBy(WindSpeed+Air.Temp+IRGA.Pressure+PPFD+RH~Date+Hour+HalfHour,
                        FUN=mean, data=outDF4, keep.names=T)
    
    outDF4$Trt <- "aCO2"
    outDF4$Trt[outDF4$Ring%in%c(1,4,5)] <- "eCO2"
    
    outDF6 <- summaryBy(Concentration.1Min~Date+Hour+HalfHour+Trt,
                        FUN=mean, data=outDF4, keep.names=T)
    
    subDF1 <- subset(outDF6, Trt=="aCO2")
    subDF2 <- subset(outDF6, Trt=="eCO2")
    outDF7 <- merge(subDF1, subDF2, by=c("Date", "Hour", "HalfHour"), all=T)
    
    ### merge the two datasets
    outDF8 <- merge(outDF5, outDF7, by=c("Date", "Hour", "HalfHour"), all=T)
    
    outDF8$Trt.x <- NULL
    outDF8$Trt.y <- NULL
    
    colnames(outDF8) <- c("Date", "Hour", "HalfHour", "WindSpeed", "Air.Temp",
                          "IRGA.Pressure", "PPFD", "RH", "aCO2", "eCO2")
    
    ### write
    write.csv(outDF8, "output/observed/input/prepare_co2_data.csv", row.names=F)
    
    return(outDF8)
}