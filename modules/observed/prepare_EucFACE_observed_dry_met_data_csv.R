prepare_EucFACE_observed_dry_met_data_csv <- function(timestep, run.option) {
    #### Note: prepare observed data (2012 - 2019)
 
    #######################################################################################

    if (run.option == "rerun") {
        outDF1 <- read.csv("output/observed/input/ros_table15_data.csv")
        outDF2 <- read.csv("output/observed/input/ros_table05_data.csv")
        outDF3 <- read.csv("output/observed/input/r3_flux_data.csv")
        outDF4 <- read.csv("output/observed/input/prepare_co2_data.csv")
        outDF5 <- read.csv("output/observed/input/prepare_soil_data.csv")
        
    } else if (run.option == "newrun") {
        ### ROS station Rain_mm_Tot, SoilTempROS, ASoilTemp_Avg
        ## half hourly rainfall data
        outDF1 <- prepare_ros_table15_data()
        
        ### ROS station PPFD_Avg, AirTC_Avg, RH, WS_ms_Avg, NetSW_Avg, NetLW_Avg, NetRad_Avg
        ## half hourly data
        outDF2 <- prepare_ros_table05_data()
        
        ### Variables: "Ts_mean", "wnd_spd", "LI190SB_PAR_Den_Avg", "TargTempC_Avg.1.", 
        ### "Net_SW_Avg", "Net_LW_Avg", "Net_Rad_Avg", "Pressure_kPa_Avg"
        outDF3 <- prepare_r3_flux_data()
        
        ### variables:  "WindSpeed", "Air.Temp", "IRGA.Pressure", "PPFD", "RH", "aCO2", "eCO2"
        outDF4 <- prepare_co2_data()
        
        ### EucFACE soil temperature data
        outDF5 <- prepare_soil_data()
    }
    
    
    #######################################################################################
    ### merge the two datasets
    outDF <- merge(outDF1, outDF2, by=c("Date", "Hour", "HalfHour"), all=T)
    outDF <- merge(outDF, outDF3, by=c("Date", "Hour", "HalfHour"), all=T)
    outDF <- merge(outDF, outDF4, by=c("Date", "Hour", "HalfHour"), all=T)
    
    ### fill data gaps
    outDF$RH.y <- ifelse(is.na(outDF$RH.y), outDF$RH.x, outDF$RH.y)
    outDF$RH.x <- NULL
    
    outDF$WindSpeed <- ifelse(is.na(outDF$WindSpeed), outDF$WS_ms_Avg, outDF$WindSpeed)
    outDF$WS_ms_Avg <- NULL
    outDF$wnd_spd <- NULL

    outDF$Air.Temp <- ifelse(is.na(outDF$Air.Temp), outDF$AirTC_Avg, outDF$Air.Temp)
    outDF$AirTC_Avg <- NULL
    outDF$Ts_mean <- NULL
    outDF$TargTempC_Avg.1. <- NULL
    
    outDF$PPFD <- ifelse(is.na(outDF$PPFD), outDF$PPFD_Avg, outDF$PPFD)
    outDF$PPFD_Avg <- NULL
    outDF$LI190SB_PAR_Den_Avg <- NULL

    outDF$Net_SW_Avg <- NULL
    outDF$Net_LW_Avg <- NULL
    outDF$Net_Rad_Avg <- NULL
    
    outDF$IRGA.Pressure <- ifelse(is.na(outDF$IRGA.Pressure), outDF$Pressure_hPa_Avg, outDF$IRGA.Pressure)
    outDF$Pressure_hPa_Avg <- NULL
    outDF$Pressure_kPa <- NULL
    outDF$Pressure_Pa <- NULL
    
    outDF$SoilTemp <- ifelse(is.na(outDF$SoilTemp), outDF$ASoilTemp_Avg, outDF$SoilTemp)
    outDF$SoilTempROS <- NULL
    outDF$ASoilTemp_Avg <- NULL
    
    
    ### assign column names
    colnames(outDF) <- c("Date", "Hour", "HalfHour", "Rain", "SWnet", "LWnet",
                          "Radnet", "Wind", "Tair", "PSurf", "PAR", "RH", "CO2ambient",
                          "CO2elevated")
    
    ### calculate VPD
    outDF$VPD <- RHtoVPD(outDF$RH, outDF$Tair) * 1000
    
    ## add additional variables
    outDF$YEAR <- year(outDF$Date)
    outDF$DOY <- yday(outDF$Date)
    
    #######################################################################################
    ### read N deposition and CO2 data
    ndepDF <- read.table("tmp_data/EucFACE_forcing_daily_CO2NDEP_1750-2023.dat", header=T)
    colnames(ndepDF) <- c("YEAR", "DOY", "CO2air", "elevatedCO2", "Ndep")
    #ndepDF$elevatedCO2 <- NULL
    ndepDF$Ndep <- ndepDF$Ndep / 10
    
    ### assign ndep data onto the outDF
    outDF <- merge(outDF, ndepDF, by=c("YEAR", "DOY"), all.x=T)
    
    #outDF$CO2ambient <- ifelse(is.na(outDF$CO2ambient), outDF$CO2air, outDF$CO2ambient)
    outDF$CO2ambient <- outDF$CO2air
    outDF$CO2elevated <- outDF$elevatedCO2
    outDF$CO2air <- NULL
    outDF$elevatedCO2 <- NULL
    
    ### fill missing values
    outDF$Rain <- ifelse(is.na(outDF$Rain), 0.0, outDF$Rain)
    outDF$PSurf <- ifelse(is.na(outDF$PSurf), 1015, outDF$PSurf)
    
    ### shortwave radiation
    #outDF$SWdown <- ifelse(outDF$SWnet<=0, 0.0, outDF$SWnet)
    b <- min(outDF$SWnet, na.rm=T)
    outDF$SWdown <- outDF$SWnet + abs(b)
    
    ### longwave down 
    outDF$tairK <- outDF$Tair + 273.15
    
    outDF$sat_vapress <- 611.2 * exp(17.67 * ((outDF$tairK - 273.15) / (outDF$tairK - 29.65)))
    outDF$vapress <- max(5.0, outDF$RH) / 100. * outDF$sat_vapress
    outDF$LWdown <- 2.648 * outDF$tairK + 0.0346 * outDF$vapress - 474.0

    ### fill missing values    
    outDF$SWdown <- na.locf(outDF$SWdown)

    ### delete unneeded variables
    outDF$sat_vapress <- NULL
    outDF$vapress <- NULL
    outDF$Radnet <- NULL
    outDF$SWnet <- NULL
    outDF$LWnet <- NULL
    outDF$tairK <- NULL
    
    ### unit
    outDF$Tair <- outDF$Tair + 273.15
    outDF$PSurf <- outDF$PSurf * 100
    
    #######################################################################################
    ### merge
    outDF6 <- merge(outDF, outDF5, by=c("Date","Hour","HalfHour"), all=T)
    
    ## order
    outDF6 <- outDF6[order(outDF6$Date, outDF6$Hour, outDF6$HalfHour),]

    ## datetime
    outDF6$DateTime <- as.POSIXct(paste0(outDF6$Date, " ", outDF6$Hour, ":",
                                       outDF6$HalfHour, ":00"),
                                format = "%Y-%m-%d %H:%M:%S")
    
    outDF7 <- unique(outDF6, by="DateTime")
    
    #######################################################################################
    ## create a new outDF to store all data time series
    time.series <- seq(as.Date("2012-01-01"), as.Date("2019-12-31"), by = "day")
    l <- length(time.series)
    hour.series <- seq(0.5, 24, by=0.5)
    
    outDF7$HOUR <- rep(hour.series, times=l)

    ## arrange select
    out <- outDF7[,c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                      "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                      "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")]
    
    ## soilTemp convert to K
    out$SoilTemp <- outDF7$SoilTemp + 273.15
    
    ### processing in Martin's code
    out$VPD <- ifelse(out$VPD < 0.05, 0.05, out$VPD)
    out$Wind <- ifelse(out$Wind <= 0.0, 0.1, out$Wind)
    
    ### correction
    #out$SWdown <- out$SWdown - 124.72
    #out$PAR <- out$PAR + 323.81
    #out$LWdown <- out$LWdown - 13.35
    #out$Tair <- out$Tair + 1.79
    #out$VPD <- out$VPD + 104.47
    #out$RH <- out$RH - 1.64
    #out$PSurf <- out$PSurf - 16.90
    #out$SoilTemp <- out$SoilTemp + 2.15
    
    
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
        subDF <- subset(out, PAR >= 5.0)
        
        dDF2 <- summaryBy(SWdown+PAR+LWdown+Tair+VPD+RH+Wind+PSurf+CO2ambient+CO2elevated+SoilTemp+Ndep~YEAR+DOY,
                          FUN=mean, data=subDF, keep.names=T)
    
        dDF <- merge(dDF1, dDF2, by=c("YEAR", "DOY"), all=T)
        
        ### rearrange variables
        outDF8 <- dDF[,c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                         "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                         "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")]
        
        outDF8 <- outDF8[order(outDF8$YEAR, outDF8$DOY),]
        
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
        
        write.table(outDF8, "output/observed/csv/daily/EUC_met_observed_dry_daily_2012_2019.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
    }
    
}