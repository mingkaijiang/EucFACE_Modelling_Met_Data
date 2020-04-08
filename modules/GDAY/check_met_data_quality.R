check_met_data_quality <- function()  {
    
    var.list <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2air", "SoilTemp", "Ndep")
    
    ### read in files
    spinDF <- read.csv("output/spinup/csv/daily/EUC_met_spinup_daily_50yrs.csv", 
                       skip=3, header=FALSE)

    histDF <- read.csv("output/historic/csv/daily/EUC_met_historic_daily_1750_2011.csv", 
                       skip=3, header=FALSE)
    
    colnames(spinDF) <- colnames(histDF) <- var.list

    #obsDF.aCO2 <- read.csv("output/GDAY/EUC_met_DRY_AMB_daily_2012_2019.csv", skip=4)
    #names(obsDF.aCO2)[names(obsDF.aCO2) == 'X.year'] <- "year"
    #
    #obsDF.eCO2 <- read.csv("output/GDAY/EUC_met_DRY_ELE_daily_2012_2019.csv", skip=4)
    #names(obsDF.eCO2)[names(obsDF.eCO2) == 'X.year'] <- "year"
    
    ### remove leap year
    spinDF <- subset(spinDF, DOY <= 365)
    
    ### rename years
    spinDF$YEAR <- rep(c(1700:1749), each=365)
    
    ## check annual values, for spinup and hist dataframes
    tDF1 <- rbind(spinDF, histDF)
    tDF2 <- summaryBy(.~YEAR, FUN=mean, keep.names=T, na.rm=T, data=tDF1)
    
    ## number of columns
    n <- dim(tDF2)[2]
    
    pdf("output/GDAY/quality_check/spin_up_historic_check.pdf")
    for (i in 3:n) {
        plot(tDF2[,i]~tDF2$YEAR)
    }
    dev.off()
    
    ### problem data variable
    ### rain, tsoil, CO2, par_am, par_pm
    
}