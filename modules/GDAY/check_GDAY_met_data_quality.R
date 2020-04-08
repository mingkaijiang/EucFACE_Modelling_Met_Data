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
    
    #pdf("output/GDAY/quality_check/spin_up_historic_check.pdf")
    #for (i in 3:n) {
    #    plot(tDF2[,i]~tDF2$year)
    #}
    #dev.off()
    
    ### check obs dataset
    tDF2 <- rbind(tDF1, obsDF.aCO2)
    tDF3 <- summaryBy(.~year, FUN=mean, keep.names=T, na.rm=T, data=tDF2)
    
    ## number of columns
    n <- dim(tDF3)[2]
    
    pdf("output/GDAY/quality_check/historic_obseved_check.pdf")
    for (i in 3:n) {
        plot(tDF3[,i]~tDF3$year)
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
    
}