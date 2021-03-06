prepare_EucFACE_observed_wet_met_data_csv <- function(timestep) {
    #### Note: prepare observed data (2012 - 2019)
    
    
    ### decide what timestep to output
    if(timestep == "half_hourly") {
        myDF <- read.csv("output/observed/csv/half_hourly/EUC_met_observed_dry_half_hourly_2012_2019.csv", 
                         skip=3, header=F)
        
        
        ### generate variable name and unit list
        var.list <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                      "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                      "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
        
        colnames(myDF) <- var.list
        
        ### which year is wettest?
        subDF <- subset(myDF, YEAR==2015)
        
        outDF <- do.call("rbind", replicate(8, subDF, simplify = FALSE))
        
        ### new year list
        yr.list2 <- rep(c(2012:2019), each=(48*365))
        
        ### assign new year list
        outDF$YEAR <- yr.list2
        
        ### add leap year days
        leapDF <- subset(outDF, YEAR == 2012 & DOY == 365)
        leapDF$DOY <- 366
        
        ### year 2012, 366
        outDF <- rbind(outDF, leapDF)
        
        ### year 2016, 366
        leapDF$YEAR <- 2016
        outDF <- rbind(outDF, leapDF)
        
        ### order by date
        outDF <- outDF[order(outDF$YEAR, outDF$DOY, outDF$HOUR),]
        
        ### assign time-series CO2 and Ndep
        outDF$CO2ambient <- myDF$CO2ambient
        outDF$CO2elevated <- myDF$CO2elevated
        outDF$Ndep <- myDF$Ndep
        
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
        
        write.table(headDF, "output/observed/csv/half_hourly/EUC_met_observed_wet_half_hourly_2012_2019.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(outDF, "output/observed/csv/half_hourly/EUC_met_observed_wet_half_hourly_2012_2019.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
        
    } else if(timestep == "daily") {
        myDF <- read.csv("output/observed/csv/daily/EUC_met_observed_dry_daily_2012_2019.csv", 
                         skip=3, header=F)
        
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
        
        ### new year list
        yr.list2 <- rep(c(2012:2019), each=365)
        
        colnames(myDF) <- var.list
        
        ### prepare output DF
        subDF <- subset(myDF, YEAR==2015)
        
        outDF <- do.call("rbind", replicate(8, subDF, simplify = FALSE))
        
        ### assign new year list
        outDF$YEAR <- yr.list2
        
        ### add leap year days
        leapDF <- subset(outDF, YEAR == 2012 & DOY == 365)
        leapDF$DOY <- 366
        
        ### year 2012, 366
        outDF <- rbind(outDF, leapDF)
        
        ### year 2016, 366
        leapDF$YEAR <- 2016
        outDF <- rbind(outDF, leapDF)
        
        ### order by date
        outDF <- outDF[order(outDF$YEAR, outDF$DOY),]
        
        ### assign CO2 and Ndep
        outDF$CO2ambient <- myDF$CO2ambient
        outDF$CO2elevated <- myDF$CO2elevated
        outDF$Ndep <- myDF$Ndep
        
        
        write.table(headDF, "output/observed/csv/daily/EUC_met_observed_wet_daily_2012_2019.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(outDF, "output/observed/csv/daily/EUC_met_observed_wet_daily_2012_2019.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
    }
    
}