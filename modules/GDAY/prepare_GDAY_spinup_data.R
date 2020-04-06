prepare_GDAY_spinup_data <- function() {
    #### read half-hourly data and output daily data 
    #### in the GDAY format and unit
    
    ### prepare output data variable names, units
    outname.list <- c("#year", "doy", "tair", "rain", "tsoil", "tam", "tpm",
                       "tmin", "tmax", "tday", "vpd_am", "vpd_pm", "CO2",
                       "ndep", "nfix", "pdep", "wind", "pres", "wind_am",
                       "wind_pm", "par_am", "par_pm")
    
    ### add unit and name list
    unit.list <- c("#-", "-", "c", "mm", "c", "c", "c", "c", "c", "c",
                   "kPa", "kPa", "ppm", "t/ha/year", "t/ha/year", "t/ha/year",
                   "m/s", "kPa", "m/s", "m/s", "mj/m2/am", "mj/m2/pm")
    
    ### add column headers
    head.list <- rbind("#EUC daily met forcing",
                       "#Data for spin-up",
                       paste0("#Created by Mingkai Jiang: ", Sys.Date()))
    
    
    ### read in data
    myDF <- read.csv("output/spinup/csv/half_hourly/EUC_met_spinup_half_hourly_50yrs.csv", skip=3)
    
    ### generate variable name and unit list
    var.list <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2air", "SoilTemp", "Ndep")
    
    colnames(myDF) <- var.list
    
    ### convert unit
    ## VPD from Pa to kPa
    myDF$VPD <- myDF$VPD / 1000
    
    ## PSurf from Pa to Kpa
    myDF$PSurf <- myDF$PSurf / 1000
    
    ### calculate daily sum of rainfall
    dDF1 <- summaryBy(Rain~YEAR+DOY, FUN=sum, data=myDF, keep.names=T)
    
    ### extract daytime DF
    subDF <- subset(inDF, PAR > 0.0)
    
    dDF2 <- summaryBy(SWdown+PAR+LWdown+Tair+VPD+RH+Wind+PSurf+CO2air+SoilTemp+Ndep~YEAR+DOY,
                      FUN=mean, data=subDF, keep.names=T)
    
    dDF <- merge(dDF1, dDF2, by=c("YEAR", "DOY"), all=T)
    
    ### rearrange variables
    outDF2 <- dDF[,c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                     "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                     "CO2air", "SoilTemp", "Ndep")]
    
    outDF2 <- outDF2[order(outDF2$YEAR, outDF2$DOY),]
    
    doutDF <- c()
    
    ### random arranging to create 50 years of data
    for (i in yr.list) {
        tmpDF <- subset(outDF2, YEAR == i)
        doutDF <- rbind(doutDF, tmpDF)
    }
    
    
    
    
    
    
    ### output
    write.table(head.list, "output/GDAY/EUC_met_spinup_daily_50yrs.csv",
                col.names=T, row.names=F, sep=",", append=F, quote = F)
    
    write.table(unit.list, "output/GDAY/EUC_met_spinup_daily_50yrs.csv",
                col.names=F, row.names=F, sep=",", append=T, quote = F)
    
    write.table(outname.list, "output/GDAY/EUC_met_spinup_daily_50yrs.csv",
                col.names=F, row.names=F, sep=",", append=T, quote = F)
    
    write.table(outDF, "output/GDAY/EUC_met_spinup_daily_50yrs.csv",
                col.names=F, row.names=F, sep=",", append=T, quote = F)
    
    
}