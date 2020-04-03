prepare_EucFACE_predicted_wet_met_data_csv <- function(timestep) {
    #### Note: prepare predicted data (2020 - 2069), based on 2015 data
 
    ### add unit and name list
    unit.list.hour <- c("year", "day", "hour", "W m-2", "umol m-2 s-1", "W m-2", "K", "kg m-2 s-1",
                        "Pa", "%", "m s-1", "Pa", "ppmv", "ppmv", "K", "g N m-2 yr-1")
    
    name.list.hour <- c("year", "day", "hour", "shortwave radiation", 
                        "photosynthetically active radiation", "longwave radiation",
                        "air temperature", "rainfall", "vapor pressure deficit",
                        "relative humidity", "wind speed", "surface pressure",
                        "ambient CO2", "elevated CO2", "soil temperature", "nitrogen deposition")
    
    var.list.hour <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                       "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                       "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
    
    unit.list.day <- c("year", "day", "W m-2", "umol m-2 s-1", "W m-2", "K", "kg m-2 s-1",
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
        myDF <- read.csv("output/observed/csv/half_hourly/EUC_met_observed_half_hourly_2012_2019.csv",
                         skip=3, header=F)
        colnames(myDF) <- var.list.hour
        
        ### prepare output DF
        subDF <- subset(myDF, YEAR==2015)
        
        outDF <- do.call("rbind", replicate(50, subDF, simplify = FALSE))
        
        ### assign new year list
        outDF$YEAR <- yr.list2

        out <- outDF
        

        write.table(headDF.hour, "output/predicted/csv/half_hourly/EUC_predicted_wet_met_half_hourly_2020_2069.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(out, "output/predicted/csv/half_hourly/EUC_predicted_wet_met_half_hourly_2020_2069.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
        
    } else if(timestep == "daily") {
        
        ### new year list
        yr.list2 <- rep(c(2020:2069), each=365)
        
        
        ### read input
        myDF <- read.csv("output/observed/csv/daily/EUC_met_observed_daily_2012_2019.csv",
                         skip=3, header=F)
        colnames(myDF) <- var.list.day
        
        ### prepare output DF
        subDF <- subset(myDF, YEAR==2015)
        
        outDF <- do.call("rbind", replicate(50, subDF, simplify = FALSE))
        
        ### assign new year list
        outDF$YEAR <- yr.list2
        
        out <- outDF
        
        
        write.table(headDF.day, "output/predicted/csv/daily/EUC_predicted_wet_met_daily_2020_2069.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(out, "output/predicted/csv/daily/EUC_predicted_wet_met_daily_2020_2069.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
        
        
    }
    
}