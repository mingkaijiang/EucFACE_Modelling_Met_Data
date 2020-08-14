prepare_EucFACE_predicted_wet_met_data_csv <- function(timestep) {
    #### Note: prepare predicted data (2020 - 2069), based on 2015 data
 
    ### add unit and name list
    unit.list.hour <- c("year", "day", "hour", "W m-2", "umol m-2 s-1", "W m-2", "K", "mm halfhour-1",
                        "Pa", "%", "m s-1", "Pa", "ppmv", "ppmv", "K", "g N m-2 yr-1")
    
    name.list.hour <- c("year", "day", "hour", "shortwave radiation", 
                        "photosynthetically active radiation", "longwave radiation",
                        "air temperature", "rainfall", "vapor pressure deficit",
                        "relative humidity", "wind speed", "surface pressure",
                        "ambient CO2", "elevated CO2", "soil temperature", "nitrogen deposition")
    
    var.list.hour <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                       "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                       "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
    
    unit.list.day <- c("year", "day", "W m-2", "umol m-2 s-1", "W m-2", "K", "mm day-1",
                        "Pa", "%", "m s-1", "Pa", "ppmv", "ppmv", "K", "g N m-2 yr-1")
    
    name.list.day <- c("year", "day", "shortwave radiation", 
                        "photosynthetically active radiation", "longwave radiation",
                        "air temperature", "rainfall", "vapor pressure deficit",
                        "relative humidity", "wind speed", "surface pressure",
                        "ambient CO2", "elevated CO2", "soil temperature", "nitrogen deposition")
    
    var.list.day <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                       "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                       "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
    
    headDF.hour <- data.frame(rbind(name.list.hour, unit.list.hour))
    colnames(headDF.hour) <- var.list.hour
    rownames(headDF.hour) <- NULL
    
    headDF.day <- data.frame(rbind(name.list.day, unit.list.day))
    colnames(headDF.day) <- var.list.day
    rownames(headDF.day) <- NULL
    
    
    
    ### decide what timestep to output
    if(timestep == "half_hourly") {

        ### new year list
        yr.list2 <- rep(c(2020:2069), each=(48*365))
        
        
        ### read input
        myDF <- read.csv("output/observed/csv/half_hourly/EUC_met_observed_dry_half_hourly_2012_2019.csv",
                         skip=3, header=F)
        colnames(myDF) <- var.list.hour
        
        ### prepare output DF
        subDF <- subset(myDF, YEAR==2015)
        
        outDF <- do.call("rbind", replicate(50, subDF, simplify = FALSE))
        
        ### assign new year list
        outDF$YEAR <- yr.list2
        
        #### re-add leap year into the dataframe
        leap.yr <- c(2020:2069)[leap_year(c(2020:2069))]
        
        for (i in leap.yr) {
            tmpDF <- subset(outDF, YEAR == i & DOY == 365)
            tmpDF$DOY <- 366
            outDF <- rbind(outDF, tmpDF)
        }
        
        outDF <- outDF[order(outDF$YEAR, outDF$DOY, outDF$HOUR),]

        ### update fixed N deposition data
        fixed.ndep.value <- unique(myDF[myDF$YEAR==2019, "Ndep"])
        outDF$Ndep <- fixed.ndep.value
        
        ### update fixed N deposition data
        fixed.ndep.value <- unique(myDF[myDF$YEAR==2019, "Ndep"])
        outDF$Ndep <- fixed.ndep.value
        
        ### update CO2 values
        fixed.co2.value <- unique(myDF[myDF$YEAR==2019, "CO2ambient"]) + 3
        fixed.co2.series <- seq(from = fixed.co2.value, to = (fixed.co2.value+3*49), by=3)
        co2DF <- data.frame(c(2020:2069), fixed.co2.series)
        colnames(co2DF) <- c("YEAR", "CO2ambient")
        co2DF$CO2elevated <- co2DF$CO2ambient + 150
        fixed.eCO2.value <- co2DF[co2DF$YEAR == 2029, "CO2elevated"]
        co2DF$CO2elevated <- ifelse(co2DF$YEAR > 2029, fixed.eCO2.value, co2DF$CO2elevated)
        outDF <- merge(outDF, co2DF, by="YEAR")
        outDF$CO2ambient.x <- outDF$CO2ambient.y
        outDF$CO2elevated.x <- outDF$CO2elevated.y
        
        outDF$CO2ambient.y <- NULL
        outDF$CO2elevated.y <- NULL
        

        write.table(headDF.hour, "output/predicted/csv/half_hourly/EUC_predicted_wet_met_half_hourly_2020_2069.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(outDF, "output/predicted/csv/half_hourly/EUC_predicted_wet_met_half_hourly_2020_2069.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
        
    } else if(timestep == "daily") {
        
        ### new year list
        yr.list2 <- rep(c(2020:2069), each=365)
        
        
        ### read input
        myDF <- read.csv("output/observed/csv/daily/EUC_met_observed_dry_daily_2012_2019.csv",
                         skip=3, header=F)
        colnames(myDF) <- var.list.day
        
        ### prepare output DF
        subDF <- subset(myDF, YEAR==2015)
        
        outDF <- do.call("rbind", replicate(50, subDF, simplify = FALSE))
        
        ### assign new year list
        outDF$YEAR <- yr.list2
        
        #### re-add leap year into the dataframe
        leap.yr <- c(2020:2069)[leap_year(c(2020:2069))]
        
        for (i in leap.yr) {
            tmpDF <- subset(outDF, YEAR == i & DOY == 365)
            tmpDF$DOY <- 366
            outDF <- rbind(outDF, tmpDF)
        }
        
        outDF <- outDF[order(outDF$YEAR, outDF$DOY),]
        
        ### update fixed N deposition data
        fixed.ndep.value <- unique(myDF[myDF$YEAR==2019, "Ndep"])
        outDF$Ndep <- fixed.ndep.value
        
        ### update CO2 values
        fixed.co2.value <- unique(myDF[myDF$YEAR==2019, "CO2ambient"]) + 3
        fixed.co2.series <- seq(from = fixed.co2.value, to = (fixed.co2.value+3*49), by=3)
        co2DF <- data.frame(c(2020:2069), fixed.co2.series)
        colnames(co2DF) <- c("YEAR", "CO2ambient")
        co2DF$CO2elevated <- co2DF$CO2ambient + 150
        fixed.eCO2.value <- co2DF[co2DF$YEAR == 2029, "CO2elevated"]
        co2DF$CO2elevated <- ifelse(co2DF$YEAR > 2029, fixed.eCO2.value, co2DF$CO2elevated)
        outDF <- merge(outDF, co2DF, by="YEAR")
        outDF$CO2ambient.x <- outDF$CO2ambient.y
        outDF$CO2elevated.x <- outDF$CO2elevated.y
        
        outDF$CO2ambient.y <- NULL
        outDF$CO2elevated.y <- NULL
        
        
        write.table(headDF.day, "output/predicted/csv/daily/EUC_predicted_wet_met_daily_2020_2069.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(outDF, "output/predicted/csv/daily/EUC_predicted_wet_met_daily_2020_2069.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
        
        
    }
    
}