prepare_EucFACE_observed_met_data_csv <- function(timestep) {
    #### Note: prepare observed data (2012 - 2019)
 
    #######################################################################################
    ### read EucFACE temperature data
    #### Download temperature and radiation data
    myDF <- download_temperature_data()
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_T1.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)  
    myDF <- myDF[order(myDF$DateTime),]
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
    
    myDF$Minute <- substr(myDF$DateTime, start=15, stop=16)
    myDF$HalfHour <- ifelse(myDF$Minute > 30, "30", "00")
    
    myDF$DateHour <- as.POSIXct(paste0(myDF$Date, " ", hour(myDF$DateTime), ":",
                                       myDF$HalfHour, ":00"),
                                format = "%Y-%m-%d %H:%M:%S")
    
    myDF$AirTc_Avg <- as.numeric(myDF$AirTc_Avg)
    myDF$RH_Avg <- as.numeric(myDF$RH_Avg)
    myDF$Net_SW_Avg <- as.numeric(myDF$Net_SW_Avg)
    myDF$Net_LW_Avg <- as.numeric(myDF$Net_LW_Avg)
    myDF$LI190SB_PAR_Den_Avg <- as.numeric(myDF$LI190SB_PAR_Den_Avg)
    
    ### Calculate half hourly mean
    tempDF <- aggregate(myDF[c("AirTc_Avg","Net_SW_Avg", "Net_LW_Avg", "RH_Avg",
                               "LI190SB_PAR_Den_Avg")], 
                     by=myDF[c("DateHour")], 
                     FUN=mean, na.rm=T, keep.names=T)
    
    ### Colnames
    colnames(tempDF) <- c("DateHour", "Tair", "SWnet", "LWnet", "RH", "PAR")
    #######################################################################################
    
    #######################################################################################
    ### read EucFACE rainfall data
    ### download rainfall data
    myDF2 <- download_rainfall_data()
    
    #### Assign ring information
    myDF2$Ring <- sub("FACE_R", "", myDF2$Source)
    myDF2$Ring <- sub("_T1.*", "", myDF2$Ring)
    myDF2$Ring <- as.numeric(myDF2$Ring)  
    myDF2 <- myDF2[order(myDF2$DateTime),]
    myDF2$Month <- format(as.Date(myDF2$Date), "%Y-%m")
    myDF2$Month <- as.Date(paste0(myDF2$Month,"-1"), format = "%Y-%m-%d") 
    
    myDF2$Minute <- substr(myDF2$DateTime, start=15, stop=16)
    myDF2$HalfHour <- ifelse(myDF2$Minute > 30, "30", "00")
    
    myDF2$DateHour <- as.POSIXct(paste0(myDF2$Date, " ", hour(myDF2$DateTime), ":",
                                        myDF2$HalfHour, ":00"),
                                 format = "%Y-%m-%d %H:%M:%S")
    
    myDF2$Rain_mm_Tot <- as.numeric(myDF2$Rain_mm_Tot)
    
    ### Calculate half hourly total for each ring
    tDF <- summaryBy(Rain_mm_Tot~DateHour+Ring, FUN=sum, data=myDF2, 
                        keep.names=T, na.rm=T)
    
    precDF <- summaryBy(Rain_mm_Tot~DateHour, FUN=mean, data=tDF, keep.names=T, na.rm=T)
    
    ### merge
    outDF1 <- merge(tempDF, precDF, by=c("DateHour"))
    #######################################################################################
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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