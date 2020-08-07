prepare_EucFACE_spinup_met_data_csv <- function() {
    #### Note: prepare spinup data
    
    
    ########################################################################
    #### based on Medlyn 2016 paper equilbrium data (20 year)
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
    var.list1 <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2air", "SoilTemp", "Ndep")
    
    colnames(inDF) <- var.list1
    
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
    
    
    ########################################################################
    ### Observed data over 2013 - 2019 at EucFACE location
    myDF <- read.csv("output/observed/csv/half_hourly/EUC_met_observed_dry_half_hourly_2012_2019.csv", 
                     skip=3, header=F)
    
    
    ### generate variable name and unit list
    var.list2 <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
    
    colnames(myDF) <- var.list2
    
    myDF$CO2elevated <- NULL
    names(myDF)[names(myDF) == 'CO2ambient'] <- "CO2air"
    
    ### set to pre-industrial values
    myDF$CO2air <- 276.84
    myDF$Ndep <- 0.225 
    
    ########################################################################
    ### merge the two dataset
    totDF <- rbind(inDF, myDF)
    
    ### generate a random year list
    set.seed(123)
    yr.list <- sample(1992:2019, (50+262), replace=T)
    
    ### prepare output DF
    outDF <- c()
    
    ### loop through random years to create the equilibrium dataset
    for (i in yr.list) {
        tmpDF <- subset(totDF, YEAR == i)
        
        outDF <- rbind(outDF, tmpDF)
    }

    
    ### remove leap year
    outDF <- outDF[outDF$DOY <= 365, ]
    
    ### re-assign year information
    outDF$YEAR <- rep(c(1700:2011), each=(48*365))

    
    #######################################################################################
    ### read N deposition and CO2 data
    ndepDF <- read.table("tmp_data/EucFACE_forcing_daily_CO2NDEP_1750-2023.dat", header=T)
    colnames(ndepDF) <- c("YEAR", "DOY", "CO2air", "elevatedCO2", "Ndep")
    ndepDF$elevatedCO2 <- NULL
    
    ### convert unit to g N m-2 yr-1
    ndepDF$Ndep <- ndepDF$Ndep / 10 
    

    ### add n deposition  data for the period of 1992 to 2011
    outDF2 <- merge(outDF, ndepDF, by=c("YEAR", "DOY"), all.x=T)
    outDF2$Ndep <- ifelse(is.na(outDF2$Ndep.y), outDF2$Ndep.x, outDF2$Ndep.y)
    outDF2$CO2air <- ifelse(is.na(outDF2$CO2air.y), outDF2$CO2air.x, outDF2$CO2air.y)
    
    outDF2$CO2air.x <- NULL
    outDF2$CO2air.y <- NULL
    outDF2$Ndep.x <- NULL
    outDF2$Ndep.y <- NULL
    
    ### rearrange
    outDF2 <- outDF2[,c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                        "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                        "CO2air", "SoilTemp", "Ndep")]
    
    ### split into spin-up and historic file
    spinDF <- subset(outDF2, YEAR <= 1749)
    histDF <- subset(outDF2, YEAR >= 1750)
    
    ########################################################################
    #### Output - half hourly
    ### add unit and name list
    unit.list <- c("year", "day", "hour", "W m-2", "umol m-2 s-1", "W m-2", "K", "mm halfhour-1",
                   "Pa", "%", "m s-1", "Pa", "ppmv", "K", "g N m-2 yr-1")
    
    name.list <- c("year", "day", "hour", "shortwave radiation", 
                   "photosynthetically active radiation", "longwave radiation",
                   "air temperature", "rainfall", "vapor pressure deficit",
                   "relative humidity", "wind speed", "surface pressure",
                   "CO2 concentration", "soil temperature", "nitrogen deposition")
    
    headDF <- data.frame(rbind(name.list, unit.list))
    colnames(headDF) <- var.list1
    rownames(headDF) <- NULL
    

    #### Spin-up
    write.table(headDF, "output/spinup/csv/half_hourly/EUC_met_spinup_half_hourly_50yrs.csv",
                col.names=T, row.names=F, sep=",", append=F, quote = F)
    
    write.table(spinDF, "output/spinup/csv/half_hourly/EUC_met_spinup_half_hourly_50yrs.csv",
                col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
    #### Historic
    write.table(headDF, "output/historic/csv/half_hourly/EUC_met_historic_half_hourly_1750_2011.csv",
                col.names=T, row.names=F, sep=",", append=F, quote = F)
    
    write.table(histDF, "output/historic/csv/half_hourly/EUC_met_historic_half_hourly_1750_2011.csv",
                col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
    
    ########################################################################
    #### Output - daily
    dDF1 <- summaryBy(Rain~YEAR+DOY, FUN=sum, data=outDF2, keep.names=T)
        
        
    ### extract daytime DF
    subDF <- subset(outDF2, PAR > 0.0)
        
    dDF2 <- summaryBy(SWdown+PAR+LWdown+Tair+VPD+RH+Wind+PSurf+CO2air+SoilTemp+Ndep~YEAR+DOY,
                      FUN=mean, data=subDF, keep.names=T)
    
    dDF <- merge(dDF1, dDF2, by=c("YEAR", "DOY"), all=T)
        
    ### rearrange variables
    dDF <- dDF[,c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2air", "SoilTemp", "Ndep")]
        
    dDF <- dDF[order(dDF$YEAR, dDF$DOY),]
    
    
    ### split
    spinDF <- subset(dDF, YEAR <= 1749)
    histDF <- subset(dDF, YEAR >= 1750)    

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
        
    ### spinup
    write.table(headDF, "output/spinup/csv/daily/EUC_met_spinup_daily_50yrs.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
    write.table(spinDF, "output/spinup/csv/daily/EUC_met_spinup_daily_50yrs.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
    ### historic   
    write.table(headDF, "output/historic/csv/daily/EUC_met_historic_daily_1750_2011.csv",
                col.names=T, row.names=F, sep=",", append=F, quote = F)
    
    write.table(histDF, "output/historic/csv/daily/EUC_met_historic_daily_1750_2011.csv",
                col.names=F, row.names=F, sep=",", append=T, quote = F)
    
}