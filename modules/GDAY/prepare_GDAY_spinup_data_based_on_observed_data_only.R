prepare_GDAY_spinup_data_based_on_observed_data_only <- function() {
    #### read half-hourly data and output daily data 
    #### in the GDAY format and unit
    
    ### prepare output data variable names, units
    outname.list <- cbind("#year", "doy", "tair", "rain", "tsoil", "tam", "tpm",
                          "tmin", "tmax", "tday", "vpd_am", "vpd_pm", "CO2",
                          "ndep", "nfix", "pdep", "pfert", "wind", "pres", "wind_am",
                          "wind_pm", "par_am", "par_pm")
    
    ### add unit and name list
    unit.list <- cbind("#-", "-", "c", "mm", "c", "c", "c", "c", "c", "c",
                       "kPa", "kPa", "ppm", "t/ha/day", "t/ha/day", "t/ha/day", 
                       "t/ha/day", "m/s", "kPa", "m/s", "m/s", "mj/m2/am", "mj/m2/pm")
    
    ### add column headers
    head.list <- rbind("#EUC daily met forcing",
                       "#Data for spin-up",
                       paste0("#Created by Mingkai Jiang: ", Sys.Date()))
    
    
    ### read in data
    myDF <- read.csv("output/observed/csv/half_hourly/EUC_met_observed_dry_half_hourly_2012_2019.csv", 
                     skip=3, header=F)
    
    ### generate variable name and unit list
    var.list <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2air", "CO2elevated", "SoilTemp", "Ndep")
    
    colnames(myDF) <- var.list
    
    myDF$CO2elevated <- NULL
    
    ### convert unit
    ## VPD from Pa to kPa
    myDF$VPD <- myDF$VPD / 1000
    
    ## PSurf from Pa to Kpa
    myDF$PSurf <- myDF$PSurf / 1000
    
    ### revise N deposition
    myDF$Ndep <- 0.225 
    
    ### revise CO2 concentration
    myDF$CO2air <-  276.84
    
    ## remove leap year and keep only 20 years of data
    #myDF <- subset(myDF, DOY <= 365)

    
    ### calculate daily sum of rainfall
    dDF1 <- summaryBy(Rain~YEAR+DOY, FUN=sum, data=myDF, keep.names=T)
    
    ### extract daytime DF
    subDF <- subset(myDF, PAR >= 5.0)
    subDF.am <- subset(subDF, HOUR >= 6. & HOUR <= 11.5)
    subDF.pm <- subset(subDF, HOUR >= 12 & HOUR <= 19.)
    
    ### calculate AM and PM values
    dDF2 <- summaryBy(Tair+VPD+Wind~YEAR+DOY, FUN=mean, 
                      data=subDF.am, keep.names=T, na.rm=T)
    colnames(dDF2) <- c("YEAR", "DOY", "Tam", "VPD_am", "Wind_am")
    
    dDF3 <- summaryBy(Tair+VPD+Wind~YEAR+DOY, FUN=mean, 
                      data=subDF.pm, keep.names=T, na.rm=T)
    colnames(dDF3) <- c("YEAR", "DOY", "Tpm", "VPD_pm", "Wind_pm")
    
    ### calculate daytime means
    dDF4 <- summaryBy(Tair+SoilTemp+CO2air+Ndep+Wind+PSurf~YEAR+DOY, FUN=mean, 
                      data=subDF, keep.names=T, na.rm=T)
    names(dDF4)[names(dDF4) == 'Tair'] <- "Tday"
    
    
    ### calculate whole day means
    dDF5 <- summaryBy(Tair~YEAR+DOY, FUN=mean,
                      data=myDF, keep.names=T, na.rm=T)
    
    ### calculate Tmin and Tmax
    dDF6 <- summaryBy(Tair~YEAR+DOY, FUN=c(min, max),
                     data=subDF, keep.names=T, na.rm=T)
    
    ## PAR: from umol m-2 s-1 to mj m-2 halfhour-1
    ## 1 W m-2 = 4.6 umol m-2 s-1
    ## 1 MJ s-1 = 1e6 W
    subDF.am$PAR <- subDF.am$PAR / 4.6 * 1e-6 * 1800
    subDF.pm$PAR <- subDF.pm$PAR / 4.6 * 1e-6 * 1800
    
    ## PAR for morning and afternoons
    dDF7 <- summaryBy(PAR~YEAR+DOY, FUN=sum,
                      data=subDF.am, keep.names=T, na.rm=T)
    names(dDF7)[names(dDF7) == 'PAR'] <- "PAR_am"
    
    dDF8 <- summaryBy(PAR~YEAR+DOY, FUN=sum,
                      data=subDF.pm, keep.names=T, na.rm=T)
    names(dDF8)[names(dDF8) == 'PAR'] <- "PAR_pm"
    
    
    ### merge
    dDF <- merge(dDF1, dDF2, by=c("YEAR", "DOY"), all=T)
    dDF <- merge(dDF, dDF3, by=c("YEAR", "DOY"), all=T)
    dDF <- merge(dDF, dDF4, by=c("YEAR", "DOY"), all=T)
    dDF <- merge(dDF, dDF5, by=c("YEAR", "DOY"), all=T)
    dDF <- merge(dDF, dDF6, by=c("YEAR", "DOY"), all=T)
    dDF <- merge(dDF, dDF7, by=c("YEAR", "DOY"), all=T)
    dDF <- merge(dDF, dDF8, by=c("YEAR", "DOY"), all=T)
    
    
    ### add nfix and pdep
    dDF$Nfix <- 0.0
    dDF$Pdep <- 0.000093 * 100 # g P m-2 yr-1
    
    ### convert Ndep and Pdep from g P m-2 yr-1 to g P m-2 d-1
    dDF$Ndep <- dDF$Ndep / 365
    dDF$Pdep <- dDF$Pdep / 365
    
    dDF$Pfert <- 0.0
    
    
    dDF <- dDF[,c("YEAR", "DOY", "Tair", "Rain", "SoilTemp",
                  "Tam", "Tpm", "Tair.min", "Tair.max",
                  "Tday", "VPD_am", "VPD_pm", "CO2air",
                  "Ndep", "Nfix", "Pdep", "Pfert", "Wind", "PSurf", "Wind_am",
                  "Wind_pm", "PAR_am", "PAR_pm")]
    
    ### convert units
    dDF$Tair <- dDF$Tair - 273.15
    dDF$SoilTemp <- dDF$SoilTemp - 273.15
    dDF$Tair.min <- dDF$Tair.min - 273.15    
    dDF$Tair.max <- dDF$Tair.max - 273.15
    dDF$Tday <- dDF$Tday - 273.15
    dDF$Tam <- dDF$Tam - 273.15
    dDF$Tpm <- dDF$Tpm - 273.15
    
    ## convert Ndep and Pdep from g m-2 d-1 to t ha-1 d-1
    dDF$Ndep <- dDF$Ndep / 100
    dDF$Pdep <- dDF$Pdep / 100
    
    ## remove leap year and reorder
    dDF <- subset(dDF, DOY <= 365)
    
    dDF <- dDF[order(dDF$YEAR, dDF$DOY),]
    
    ### create 50-years
    ### generate a random year list
    set.seed(123)
    yr.list <- sample(2012:2019, 50, replace=T)
    
    ### prepare output DF
    outDF <- c()
    
    ### loop through random years to create the equilibrium dataset
    for (i in yr.list) {
        tmpDF <- subset(dDF, YEAR == i)
        
        outDF <- rbind(outDF, tmpDF)
    }
    
    ### re-assign year information
    outDF$YEAR <- rep(c(1700:1749), each=365)
    
    ### add leap year data back
    #### re-add leap year into the dataframe
    leap.yr <- c(1700:1749)[leap_year(c(1700:1749))]
    
    
    for (i in leap.yr) {
        tmpDF <- subset(outDF, YEAR == i & DOY == 365)
        tmpDF$DOY <- 366
        outDF <- rbind(outDF, tmpDF)
    }
    
    outDF <- outDF[order(outDF$YEAR, outDF$DOY),]
    
    
    ### output
    write.table(head.list, "output/GDAY/EUC_met_spinup_daily_50yrs.csv",
                col.names=F, row.names=F, sep=",", append=F, quote = F)
    
    write.table(unit.list, "output/GDAY/EUC_met_spinup_daily_50yrs.csv",
                col.names=F, row.names=F, sep=",", append=T, quote = F)
    
    write.table(outname.list, "output/GDAY/EUC_met_spinup_daily_50yrs.csv",
                col.names=F, row.names=F, sep=",", append=T, quote = F)
    
    write.table(outDF, "output/GDAY/EUC_met_spinup_daily_50yrs.csv",
                col.names=F, row.names=F, sep=",", append=T, quote = F)
    
    
}