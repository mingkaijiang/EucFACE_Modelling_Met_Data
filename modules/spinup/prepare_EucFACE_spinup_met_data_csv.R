prepare_EucFACE_spinup_met_data_csv <- function(timestep) {
    #### Note: prepare spinup data
    #### based on Medlyn 2016 paper equilbrium data (50 year)
    
    ### read input 
    inDF <- read.csv("tmp_data/EucFACE_forcing_1992-2011.csv",header=T)
    
    ### ignore snowfall, as there is none
    inDF$Snowf..kg.m.2.s. <- NULL
    ### ignore Qair - near surface specific humidity
    inDF$Qair..kg.kg. <- NULL
    
    ### add pre-industrial N deposition 
    ### 2.25 kg N ha-1 yr-1
    ### equivalent to: 0.225 g N m-2 yr-1
    inDF$Ndep <- 0.225 

    
    ### generate variable name and unit list
    var.list <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2air", "SoilTemp", "Ndep")
    
    colnames(inDF) <- var.list
    
    ### force CO2 concentration to be pre-industrial
    inDF$CO2air <- 276.84
    
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
            inDF[inDF$YEAR==i&inDF$DOY==j, "SWdown"] <- inDF[inDF$YEAR==i&inDF$DOY==k, "SWdown"]
            inDF[inDF$YEAR==i&inDF$DOY==j, "PAR"] <- inDF[inDF$YEAR==i&inDF$DOY==k, "PAR"]
        }
    }
    
    
    ### Rain convert unit from kg/m2/s to mm / half an hour
    inDF$Rain <- inDF$Rain * 1800 
    
    ### generate a random year list
    set.seed(123)
    yr.list <- sample(1992:2011, 50, replace=T)
    
    ### prepare output DF
    outDF <- c()
    
    ### loop through random years to create the equilibrium dataset
    for (i in yr.list) {
        tmpDF <- subset(inDF, YEAR == i)
        
        outDF <- rbind(outDF, tmpDF)
    }

    
    ### add unit and name list
    unit.list <- c("year", "day", "hour", "W m-2", "umol m-2 s-1", "W m-2", "K", "mm halfhour-1",
                   "Pa", "%", "m s-1", "Pa", "ppmv", "K", "g N m-2 yr-1")
    
    name.list <- c("year", "day", "hour", "shortwave radiation", 
                   "photosynthetically active radiation", "longwave radiation",
                   "air temperature", "rainfall", "vapor pressure deficit",
                   "relative humidity", "wind speed", "surface pressure",
                   "CO2 concentration", "soil temperature", "nitrogen deposition")
    
    headDF <- data.frame(rbind(name.list, unit.list))
    colnames(headDF) <- var.list
    rownames(headDF) <- NULL
    
    
    ### remove leap year
    outDF <- outDF[outDF$DOY <= 365, ]
    
    ### reassign year information
    outDF$YEAR <- rep(c(1700:1749), each=(48*365))
    
    ### decide what timestep to output
    if(timestep == "half_hourly") {

        write.table(headDF, "output/spinup/csv/half_hourly/EUC_met_spinup_half_hourly_50yrs.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(outDF, "output/spinup/csv/half_hourly/EUC_met_spinup_half_hourly_50yrs.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
    } else if(timestep == "daily") {
        
        dDF1 <- summaryBy(Rain~YEAR+DOY, FUN=sum, data=outDF, keep.names=T)
        
        
        ### extract daytime DF
        subDF <- subset(outDF, PAR > 0.0)
        
        dDF2 <- summaryBy(SWdown+PAR+LWdown+Tair+VPD+RH+Wind+PSurf+CO2air+SoilTemp+Ndep~YEAR+DOY,
                          FUN=mean, data=subDF, keep.names=T)
    
        dDF <- merge(dDF1, dDF2, by=c("YEAR", "DOY"), all=T)
        
        ### rearrange variables
        outDF2 <- dDF[,c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                         "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                         "CO2air", "SoilTemp", "Ndep")]
        
        outDF2 <- outDF2[order(outDF2$YEAR, outDF2$DOY),]
        

        ### add unit and name list
        unit.list <- c("year", "day", "W m-2", "umol m-2 s-1", "W m-2", "K", "mm day-1",
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
        
        write.table(headDF, "output/spinup/csv/daily/EUC_met_spinup_daily_50yrs.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(outDF2, "output/spinup/csv/daily/EUC_met_spinup_daily_50yrs.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
    }
    
}