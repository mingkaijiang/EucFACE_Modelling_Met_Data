prepare_EucFACE_observed_dry_met_data_csv <- function(timestep) {
    #### Note: prepare observed data (2012 - 2019)
 
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
    
    ## half hourly rainfall data
    outDF1 <- summaryBy(Rain_mm_Tot~Date+Hour+HalfHour, FUN=sum,
                        data=myDF1, keep.names=T, na.rm=T)
    #######################################################################################
    
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
    outDF2 <- summaryBy(PPFD_Avg+AirTC_Avg+RH+WS_ms_Avg+NetSW_Avg+NetLW_Avg+NetRad_Avg~Date+Hour+HalfHour, 
                        FUN=mean,
                        data=myDF2, keep.names=T, na.rm=T)
    
    ### merge the two datasets
    outDF <- merge(outDF1, outDF2, by=c("Date", "Hour", "HalfHour"), all=T)
    
    
    #######################################################################################
    ### variables to add: VPD, SWdown, LWdown, PSurf, CO2air, SoilTemp, Ndep
    
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
                        "Ts_mean", "wnd_spd", "LI190SB_PAR_Den_Avg",
                       "TargTempC_Avg.1.", "Net_SW_Avg", "Net_LW_Avg",
                       "Net_Rad_Avg", "Pressure_hPa_Avg")]
    
    outDF3$Pressure_kPa <- outDF3$Pressure_hPa_Avg / 10
    outDF3$Pressure_Pa <- outDF3$Pressure_hPa_Avg * 100
    
    
    ### merge the two datasets
    outDF <- merge(outDF, outDF3, by=c("Date", "Hour", "HalfHour"), all=T)

    
    #######################################################################################
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
    outDF4 <- summaryBy(WindSpeed+Air.Temp+Concentration.1Min+IRGA.Vapor.Pressure+IRGA.Pressure+PPFD~Date+Hour+HalfHour+Ring, 
                        FUN=mean,
                        data=myDF4, keep.names=T, na.rm=T)
    
    
    ### calculate RH
    saturate.vp.func <- function(Tc,a=6.12,m=7.59,Tn=240.73){
        # T = Temperature (°C)
        VPS <- a*10^(m*Tc/(Tc+Tn))
        return(VPS)
    }
    
    outDF4$RH <- outDF4$IRGA.Vapor.Pressure/saturate.vp.func(outDF4$Air.Temp)
    
    
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
    
    ### merge all datasets to fill gaps in time coverage and data variables
    outDF9 <- merge(outDF, outDF8, by=c("Date", "Hour", "HalfHour"), all=T)
    
    ### fill data gaps
    outDF9$RH.y <- ifelse(is.na(outDF9$RH.y), outDF9$RH.x, outDF9$RH.y)
    outDF9$RH.x <- NULL
    
    outDF9$WindSpeed <- ifelse(is.na(outDF9$WindSpeed), outDF9$WS_ms_Avg, outDF9$WindSpeed)
    outDF9$WS_ms_Avg <- NULL
    outDF9$wnd_spd <- NULL

    outDF9$Air.Temp <- ifelse(is.na(outDF9$Air.Temp), outDF9$AirTC_Avg, outDF9$Air.Temp)
    outDF9$AirTC_Avg <- NULL
    outDF9$Ts_mean <- NULL
    outDF9$TargTempC_Avg.1. <- NULL
    
    outDF9$PPFD <- ifelse(is.na(outDF9$PPFD), outDF9$PPFD_Avg, outDF9$PPFD)
    outDF9$PPFD_Avg <- NULL
    outDF9$LI190SB_PAR_Den_Avg <- NULL

    outDF9$Net_SW_Avg <- NULL
    outDF9$Net_LW_Avg <- NULL
    outDF9$Net_Rad_Avg <- NULL
    
    outDF9$IRGA.Pressure <- ifelse(is.na(outDF9$IRGA.Pressure), outDF9$Pressure_hPa_Avg, outDF9$IRGA.Pressure)
    outDF9$Pressure_hPa_Avg <- NULL
    outDF9$Pressure_kPa <- NULL
    outDF9$Pressure_Pa <- NULL
    
    
    ### assign column names
    colnames(outDF9) <- c("Date", "Hour", "HalfHour", "Rain", "SWnet", "LWnet",
                          "Radnet", "Wind", "Tair", "PSurf", "PAR", "RH", "CO2ambient",
                          "CO2elevated")
    
    ### calculate VPD
    outDF9$VPD <- RHtoVPD(outDF9$RH, outDF9$Tair) * 1000
    
    ## add additional variables
    outDF9$YEAR <- year(outDF9$Date)
    outDF9$DOY <- yday(outDF9$Date)
    
    #######################################################################################
    ### read N deposition and CO2 data
    ndepDF <- read.table("tmp_data/EucFACE_forcing_daily_CO2NDEP_1750-2023.dat", header=T)
    colnames(ndepDF) <- c("YEAR", "DOY", "CO2air", "elevatedCO2", "Ndep")
    ndepDF$elevatedCO2 <- NULL
    ndepDF$Ndep <- ndepDF$Ndep / 10
    
    ### assign ndep data onto the outDF
    outDF9 <- merge(outDF9, ndepDF, by=c("YEAR", "DOY"), all.x=T)
    
    outDF9$CO2ambient <- ifelse(is.na(outDF9$CO2ambient), outDF9$CO2air, outDF9$CO2ambient)
    outDF9$CO2elevated <- ifelse(is.na(outDF9$CO2elevated), outDF9$CO2air, outDF9$CO2elevated)
    outDF9$CO2air <- NULL
    
    ### fill missing values
    outDF9$Rain <- ifelse(is.na(outDF9$Rain), 0.0, outDF9$Rain)
    outDF9$PSurf <- ifelse(is.na(outDF9$PSurf), 1015, outDF9$PSurf)
    
    ### shortwave radiation
    outDF9$SWdown <- ifelse(outDF9$SWnet<=0, 0.0, outDF9$SWnet)
    
    ### longwave down 
    outDF9$tairK <- outDF9$Tair + 273.15
    
    outDF9$sat_vapress <- 611.2 * exp(17.67 * ((outDF9$tairK - 273.15) / (outDF9$tairK - 29.65)))
    outDF9$vapress <- max(5.0, outDF9$RH) / 100. * outDF9$sat_vapress
    outDF9$LWdown <- 2.648 * outDF9$tairK + 0.0346 * outDF9$vapress - 474.0

    ### fill missing values    
    outDF9$SWdown <- na.locf(outDF9$SWdown)

    ### delete unneeded variables
    outDF9$sat_vapress <- NULL
    outDF9$vapress <- NULL
    outDF9$Radnet <- NULL
    outDF9$SWnet <- NULL
    outDF9$LWnet <- NULL
    outDF9$tairK <- NULL
    
    ### unit
    outDF9$Tair <- outDF9$Tair + 273.15
    outDF9$PSurf <- outDF9$PSurf * 100
    
    #######################################################################################
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
    
    
    outDF11 <- merge(outDF9, outDF10, by=c("Date","Hour","HalfHour"), all=T)
    
    ### fill SoilTemp missing values
    myDF1$SoilTempROS <- rowMeans(myDF1[c("SoilTemp_Avg.1.", "SoilTemp_Avg.2.")], na.rm=TRUE)
    
    outDF12 <- summaryBy(SoilTempROS+ASoilTemp_Avg~Date+Hour+HalfHour, FUN=mean,
                         data=myDF1, keep.names=T, na.rm=T)
    
    outDF13 <- merge(outDF11, outDF12, by=c("Date","Hour","HalfHour"), all=T)
    
    ## fill missing values
    outDF13$SoilTemp <- ifelse(is.na(outDF13$SoilTemp), outDF13$ASoilTemp_Avg, outDF13$SoilTemp)
    outDF13$SoilTempROS <- NULL
    outDF13$ASoilTemp_Avg <- NULL
    
    ## order
    outDF13 <- outDF13[order(outDF13$Date, outDF13$Hour, outDF13$HalfHour),]

    ## datetime
    outDF13$DateTime <- as.POSIXct(paste0(outDF13$Date, " ", outDF13$Hour, ":",
                                       outDF13$HalfHour, ":00"),
                                format = "%Y-%m-%d %H:%M:%S")
    
    outDF <- unique(outDF13, by="DateTime")
    
    #######################################################################################
    ## create a new outDF to store all data time series
    time.series <- seq(as.Date("2012-01-01"), as.Date("2019-12-31"), by = "day")
    l <- length(time.series)
    hour.series <- seq(0.5, 24, by=0.5)
    
    outDF$HOUR <- rep(hour.series, times=l)

    ## arrange select
    out <- outDF[,c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                      "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                      "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")]
    
    ## soilTemp convert to K
    out$SoilTemp <- outDF$SoilTemp + 273.15
    
    
    #######################################################################################
    
    ### generate variable name and unit list
    var.list <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
    

    ### add unit and name list
    unit.list <- c("year", "day", "hour", "W m-2", "umol m-2 s-1", "W m-2", "K", "mm halfhour-1",
                   "Pa", "%", "m s-1", "Pa", "ppmv", "ppmv", "K", "g N m-2 yr-1")
    
    name.list <- c("year", "day", "hour", "shortwave radiation", 
                   "photosynthetically active radiation", "longwave radiation",
                   "air temperature", "rainfall", "vapor pressure deficit",
                   "relative humidity", "wind speed", "surface pressure",
                   "ambient CO2", "elevated CO2", "soil temperature", "nitrogen deposition")
    
    headDF <- data.frame(rbind(name.list, unit.list))
    colnames(headDF) <- var.list
    rownames(headDF) <- NULL
    
    ### decide what timestep to output
    if(timestep == "half_hourly") {

        write.table(headDF, "output/observed/csv/half_hourly/EUC_met_observed_dry_half_hourly_2012_2019.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(out, "output/observed/csv/half_hourly/EUC_met_observed_dry_half_hourly_2012_2019.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
    } else if(timestep == "daily") {
        
        ### calculate total rainfall of the day
        dDF1 <- summaryBy(Rain~YEAR+DOY, FUN=sum, data=out, keep.names=T)
        
        ### extract daytime DF
        subDF <- subset(out, PAR > 0.0)
        
        dDF2 <- summaryBy(SWdown+PAR+LWdown+Tair+VPD+RH+Wind+PSurf+CO2ambient+CO2elevated+SoilTemp+Ndep~YEAR+DOY,
                          FUN=mean, data=subDF, keep.names=T)
    
        dDF <- merge(dDF1, dDF2, by=c("YEAR", "DOY"), all=T)
        
        ### rearrange variables
        outDF2 <- dDF[,c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                         "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                         "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")]
        
        outDF2 <- outDF2[order(outDF2$YEAR, outDF2$DOY),]
        
        ### add unit and name list
        unit.list <- c("year", "day", "W m-2", "umol m-2 s-1", "W m-2", "K", "mm day-1",
                       "Pa", "%", "m s-1", "Pa", "ppmv", "ppmv", "K", "g N m-2 yr-1")
        
        name.list <- c("year", "day", "shortwave radiation", 
                       "photosynthetically active radiation", "longwave radiation",
                       "air temperature", "rainfall", "vapor pressure deficit",
                       "relative humidity", "wind speed", "surface pressure",
                       "ambient CO2", "elevated CO2", "soil temperature", "nitrogen deposition")
        
        var.list <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                      "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                      "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
        
        headDF <- data.frame(rbind(name.list, unit.list))
        colnames(headDF) <- var.list
        rownames(headDF) <- NULL
        
        write.table(headDF, "output/observed/csv/daily/EUC_met_observed_dry_daily_2012_2019.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(outDF2, "output/observed/csv/daily/EUC_met_observed_dry_daily_2012_2019.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
    }
    
}