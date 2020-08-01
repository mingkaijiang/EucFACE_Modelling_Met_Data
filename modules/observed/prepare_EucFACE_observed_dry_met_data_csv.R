prepare_EucFACE_observed_dry_met_data_csv <- function(timestep) {
    #### Note: prepare observed data (2012 - 2019)
 
    #######################################################################################
    ### ROS station rainfall (and soil temperature, volumetric soil water content) data
    ## half hourly rainfall data
    outDF1 <- prepare_ros_table15_data()
    
    #######################################################################################
    ### ROS station radiation, wind speed, air temperature, humidity at 5 min interval
    ## half hourly data
    outDF2 <- prepare_ros_table05_data()
    
    ### merge the two datasets
    outDF <- merge(outDF1, outDF2, by=c("Date", "Hour", "HalfHour"), all=T)
    
    
    #######################################################################################
    ### variables to add: VPD, SWdown, LWdown, PSurf, CO2air, SoilTemp, Ndep
    outDF3 <- prepare_r3_flux_data()
    
    ### merge the two datasets
    outDF <- merge(outDF, outDF3, by=c("Date", "Hour", "HalfHour"), all=T)

    
    #######################################################################################
    ### CO2 concentration in the rings
    outDF8 <- prepare_co2_data()
    
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
    #outDF9$SWdown <- ifelse(outDF9$SWnet<=0, 0.0, outDF9$SWnet)
    b <- min(outDF9$SWnet, na.rm=T)
    
    outDF9$SWdown <- outDF9$SWnet + abs(b)
    
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