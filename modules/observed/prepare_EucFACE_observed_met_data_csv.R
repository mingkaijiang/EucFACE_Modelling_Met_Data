prepare_EucFACE_observed_met_data_csv <- function(timestep) {
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
    
    
    #######################################################################################
    ### calculate VPD
    outDF$VPD <- RHtoVPD(outDF$RH, outDF$AirTC_Avg)
    
    
    #######################################################################################
    ### read N deposition and CO2 data
    ndepDF <- read.table("tmp_data/EucFACE_forcing_daily_CO2NDEP_1750-2023.dat", header=T)
    colnames(ndepDF) <- c("YEAR", "DOY", "CO2air", "elevatedCO2", "Ndep")
    ndepDF$elevatedCO2 <- NULL
    ndepDF$Ndep <- ndepDF$Ndep / 10

    
    ### assign ndep data onto the outDF
    
    #######################################################################################
    
    ### generate variable name and unit list
    var.list <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2air", "SoilTemp", "Ndep")
    
    colnames(inDF2) <- var.list
    
    
    ### SWdown and PAR for some days are all zero, so replace these data with
    ### data from one day earlier
    missing.data.list <- data.frame(c("1992", "1996", "1997", "1998",
                                      "1998", "1999", "2001", "2002",
                                      "2003", "2004", "2005", "2006"),
                                    c("351", "126", "280", "218", "219",
                                      "91", "31", "49", "52", "275",
                                      "181", "349"))
    colnames(missing.data.list) <- c("YEAR", "DOY")
    
    for (i in missing.data.list$YEAR) {
        for (j in missing.data.list$DOY) {
            k <- as.numeric(j) - 1
            inDF2[inDF2$YEAR==i&inDF2$DOY==j, "SWdown"] <- inDF2[inDF2$YEAR==i&inDF2$DOY==k, "SWdown"]
            inDF2[inDF2$YEAR==i&inDF2$DOY==j, "PAR"] <- inDF2[inDF2$YEAR==i&inDF2$DOY==k, "PAR"]
        }
    }

    
    ### add unit and name list
    unit.list <- c("year", "day", "hour", "W m-2", "umol m-2 s-1", "W m-2", "K", "kg m-2 s-1",
                   "Pa", "%", "m s-1", "Pa", "ppmv", "K", "g N m-2 yr-1")
    
    name.list <- c("year", "day", "hour", "shortwave radiation", 
                   "photosynthetically active radiation", "longwave radiation",
                   "air temperature", "rainfall", "vapor pressure deficit",
                   "relative humidity", "wind speed", "surface pressure",
                   "CO2 concentration", "soil temperature", "nitrogen deposition")
    
    headDF <- data.frame(rbind(name.list, unit.list))
    colnames(headDF) <- var.list
    rownames(headDF) <- NULL
    
    ### decide what timestep to output
    if(timestep == "half_hourly") {

        write.table(headDF, "output/historic/csv/half_hourly/EUC_met_historic_half_hourly_1992_2011.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(inDF2, "output/historic/csv/half_hourly/EUC_met_historic_half_hourly_1992_2011.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
    } else if(timestep == "daily") {
        
        ### calculate total rainfall of the day
        dDF1 <- summaryBy(Rain~YEAR+DOY, FUN=sum, data=inDF2, keep.names=T)
        
        ### extract daytime DF
        subDF <- subset(inDF2, PAR > 0.0)
        
        dDF2 <- summaryBy(SWdown+PAR+LWdown+Tair+VPD+RH+Wind+PSurf+CO2air+SoilTemp+Ndep~YEAR+DOY,
                          FUN=mean, data=subDF, keep.names=T)
    
        dDF <- merge(dDF1, dDF2, by=c("YEAR", "DOY"), all=T)
        
        ### rearrange variables
        outDF2 <- dDF[,c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                         "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                         "CO2air", "SoilTemp", "Ndep")]
        
        outDF2 <- outDF2[order(outDF2$YEAR, outDF2$DOY),]
        
        ### add unit and name list
        unit.list <- c("year", "day", "W m-2", "umol m-2 s-1", "W m-2", "K", "kg m-2 s-1",
                       "Pa", "%", "m s-1", "Pa", "ppmv", "K", "g N m-2 yr-1")
        
        name.list <- c("year", "day", "shortwave radiation", 
                       "photosynthetically active radiation", "longwave radiation",
                       "air temperature", "rainfall", "vapor pressure deficit",
                       "relative humidity", "wind speed", "surface pressure",
                       "CO2 concentration", "soil temperature", "nitrogen deposition")
        
        var.list <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                      "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                      "CO2air", "SoilTemp", "Ndep")
        
        headDF <- data.frame(rbind(name.list, unit.list))
        colnames(headDF) <- var.list
        rownames(headDF) <- NULL
        
        write.table(headDF, "output/historic/csv/daily/EUC_met_historic_daily_1992_2011.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(outDF2, "output/historic/csv/daily/EUC_met_historic_daily_1992_2011.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
    }
    
}