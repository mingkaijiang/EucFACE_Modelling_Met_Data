check_GDAY_met_data_quality <- function()  {
    ### read in files
    spinDF <- read.csv("output/GDAY/EUC_met_spinup_daily_50yrs.csv", skip=4)
    names(spinDF)[names(spinDF) == 'X.year'] <- "year"
    
    histDF <- read.csv("output/GDAY/EUC_met_historic_daily_1750_2011.csv", skip=4)
    names(histDF)[names(histDF) == 'X.year'] <- "year"
    
    obsDF.aCO2 <- read.csv("output/GDAY/EUC_met_DRY_AMB_daily_2012_2019.csv", skip=4)
    names(obsDF.aCO2)[names(obsDF.aCO2) == 'X.year'] <- "year"
    
    obsDF.eCO2 <- read.csv("output/GDAY/EUC_met_DRY_ELE_daily_2012_2019.csv", skip=4)
    names(obsDF.eCO2)[names(obsDF.eCO2) == 'X.year'] <- "year"
    
    
    ## check annual values, for spinup and hist dataframes
    tDF1 <- rbind(spinDF, histDF)
    tDF2 <- summaryBy(.~year, FUN=mean, keep.names=T, na.rm=T, data=tDF1)
    
    ## number of columns
    n <- dim(tDF2)[2]
    
    pdf("output/GDAY/quality_check/spin_up_historic_check.pdf")
    for (i in 3:n) {
        plot(tDF2[,i]~tDF2$year, xlab="year", ylab=colnames(tDF2)[i])
        title(colnames(tDF2)[i])
    }
    dev.off()

    ### check obs dataset
    tDF3 <- rbind(tDF1, obsDF.aCO2)
    tDF4 <- summaryBy(.~year, FUN=mean, keep.names=T, na.rm=T, data=tDF3)
    
    ## number of columns
    n <- dim(tDF4)[2]
    
    pdf("output/GDAY/quality_check/historic_obseved_check.pdf")
    for (i in 3:n) {
        plot(tDF4[,i]~tDF4$year, xlab="year", ylab=colnames(tDF4)[i])
        title(colnames(tDF4)[i])
    }
    dev.off()
    
    ## problem variable: tam - 2 degree less
    ##                   tpm - 2 degree less
    ##                   tday - 2 degree less
    ##                   vpd_am - small
    ##                   vpd_pm - small
    ##                   wind - small
    ##                   pres: variable
    ##                   wind_am - small
    ##                   wind_pm - small
    ##                   par_am - small
    ##                   par_pm - small
    
    
    ### to check of the original 50-year data makes sense in context of EucFACE data
    obsDF.aCO2 <- read.csv("tmp_data/EUC_met_data_amb_avg_co2.csv", skip=4)
    names(obsDF.aCO2)[1] <- "year"
    names(obsDF.aCO2)[13] <- "CO2"
    
    ### check obs dataset
    tDF3 <- rbind(tDF1, obsDF.aCO2)
    tDF4 <- summaryBy(.~year, FUN=mean, keep.names=T, na.rm=T, data=tDF3)
    
    ## number of columns
    n <- dim(tDF4)[2]
    
    pdf("output/GDAY/quality_check/original_historic_obseved_check.pdf")
    for (i in 3:n) {
        plot(tDF4[,i]~tDF4$year, xlab="year", ylab=colnames(tDF4)[i])
        title(colnames(tDF4)[i])
    }
    dev.off()
    
    ### it seems that problem already exists in Martin's original met data,
    ### next check the original spin-up file
    
    ### read in files
    spinDF <- read.csv("tmp_data/EUC_met_data_equilibrium_50_yrs.csv", skip=4)
    names(spinDF)[names(spinDF) == 'X.year'] <- "year"
    
    spinDF <- spinDF[spinDF$doy <= 365, ]
    spinDF$year <- rep(c(1962:2011), each=365)
    
    
    ### to check of the original 50-year data makes sense in context of EucFACE data
    obsDF.aCO2 <- read.csv("tmp_data/EUC_met_data_amb_avg_co2.csv", skip=4)
    names(obsDF.aCO2)[1] <- "year"
    obsDF.aCO2$pdep <- NULL
    
    ### check obs dataset
    tDF3 <- rbind(spinDF, obsDF.aCO2)
    tDF4 <- summaryBy(.~year, FUN=mean, keep.names=T, na.rm=T, data=tDF3)
    
    ## number of columns
    n <- dim(tDF4)[2]
    
    pdf("output/GDAY/quality_check/very_original_historic_obseved_check.pdf")
    for (i in 3:n) {
        plot(tDF4[,i]~tDF4$year, xlab="year", ylab=colnames(tDF4)[i])
        title(colnames(tDF4)[i])
    }
    dev.off()
    
    ### so, the 50-yr spin-up file that I used is different to those used by Martin
    ### the one used by Martin is called: EUC_met_data_equilibrium_50_yrs.csv
    ### the one I used in my data processing is called: tmp_data/EucFACE_forcing_1992-2011.csv
    
    
    
}