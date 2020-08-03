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
    
    ## some small mismatches, which are totally reasonable
    ### because spin-up and historic period are "assuemd" data,
    ### whereas real data are used for 2012 and onward.
    ### also the mis-match should not cause any problem at all.
    
    
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
    
    ### So, the 50-yr spin-up file that I used is different to those used by Martin.
    ### The one used by Martin is called: EUC_met_data_equilibrium_50_yrs.csv,
    ### the one I used in my data processing is called: tmp_data/EucFACE_forcing_1992-2011.csv.
    ### Problem is that, EUC_met_data_equilibrium_50_yrs.csv is a daily file,
    ### whereas EucFACE_forcing_1992-2011.csv is a half-hourly file.
    ### Ideally, we will need to create half-hourly spin-up and historic (1750-2011) period files.
    
    ### So, how do we reconcile the variable value differences between the two files?
    ### Because it seems that we don't have an option but to use EucFACE_forcing_1992-2011.csv.
    
    ### open the two files and check variable match for each individual variable. 
    
    ### read in the two spin-up candidate files
    ### my spin-up file
    spinDF1 <- read.csv("output/GDAY/EUC_met_spinup_daily_50yrs.csv", skip=4)
    names(spinDF1)[names(spinDF1) == 'X.year'] <- "year"
    spinDF1$pdep <- NULL
    names(spinDF1)[names(spinDF1) == 'CO2'] <- "co2"
    
    
    ### Martin's spin-up file
    spinDF2 <- read.csv("tmp_data/EUC_met_data_equilibrium_50_yrs.csv", skip=4)
    names(spinDF2)[names(spinDF2) == 'X.year'] <- "year"
    spinDF2 <- spinDF2[spinDF2$doy <= 365, ]
    spinDF2$year <- rep(c(1750:1799), each=365)
    
    ### merge the two files
    plotDF <- rbind(spinDF1, spinDF2)
    
    ### average
    tDF <- summaryBy(.~year, FUN=mean, keep.names=T, na.rm=T, data=plotDF)
    
    ## number of columns
    n <- dim(tDF)[2]
    
    pdf("output/GDAY/quality_check/spin-up_file_check.pdf")
    for (i in 3:n) {
        plot(tDF[,i]~tDF$year, xlab="year", ylab=colnames(tDF)[i])
        title(colnames(tDF)[i])
    }
    dev.off()
    
    
    ### calculate correction factors
    sumDF1 <- colMeans(spinDF1)
    sumDF2 <- colMeans(spinDF2)
    corrDF <- as.data.frame(sumDF2 - sumDF1)
    colnames(corrDF) <- "value"
    
    write.csv(corrDF, "tmp_data/correction_factor.csv")

    ### correct myDF
    revDF <- spinDF1
    
    revDF$tair <- revDF$tair + corrDF$value[3]
    revDF$rain <- revDF$rain + corrDF$value[4]
    revDF$tsoil <- revDF$tsoil + corrDF$value[5]
    revDF$tam <- revDF$tam + corrDF$value[6]
    revDF$tpm <- revDF$tpm + corrDF$value[7]
    revDF$tmin <- revDF$tmin + corrDF$value[8]
    revDF$tmax <- revDF$tmax + corrDF$value[9]
    revDF$tday <- revDF$tday + corrDF$value[10]
    revDF$vpd_am <- revDF$vpd_am + corrDF$value[11]
    revDF$vpd_pm <- revDF$vpd_pm + corrDF$value[12]
    revDF$wind <- revDF$wind + corrDF$value[16]
    revDF$pres <- revDF$pres + corrDF$value[17]
    revDF$wind_am <- revDF$wind_am + corrDF$value[18]
    revDF$wind_pm <- revDF$wind_pm + corrDF$value[19]
    revDF$par_am <- revDF$par_am + corrDF$value[20]
    revDF$par_pm <- revDF$par_pm + corrDF$value[21]
    
    
    ### merge the two files
    plotDF <- rbind(revDF, spinDF2)
    
    ### average
    tDF <- summaryBy(.~year, FUN=mean, keep.names=T, na.rm=T, data=plotDF)
    
    ## number of columns
    n <- dim(tDF)[2]
    
    pdf("output/GDAY/quality_check/spin-up_file_revised.pdf")
    for (i in 3:n) {
        plot(tDF[,i]~tDF$year, xlab="year", ylab=colnames(tDF)[i])
        title(colnames(tDF)[i])
    }
    dev.off()
    
    
    
    
    ################################################################################
    
    ### check the un-transformed data
    spinDF <- read.csv("output/spinup/csv/daily/EUC_met_spinup_daily_50yrs.csv", skip=3, header=F)
    histDF <- read.csv("output/historic/csv/daily/EUC_met_historic_daily_1750_2011.csv", skip=3, header=F)
    obsDF <- read.csv("output/observed/csv/daily/EUC_met_observed_dry_daily_2012_2019.csv", skip=3, header=F)
    
    var.list <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2air", "SoilTemp", "Ndep")
    
    colnames(spinDF) <- colnames(histDF) <- var.list
    
    #spinDF <- spinDF[spinDF$DOY <= 365, ]
    
    #spinDF$YEAR <- rep(c(1700:1749), each=365)
    
    ### generate variable name and unit list
    var.list2 <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
    
    colnames(obsDF) <- var.list2
    
    
    ### variable consistency
    obsDF$CO2elevated <- NULL
    names(obsDF)[names(obsDF) == 'CO2ambient'] <- "CO2air"
    
    ### check data
    rDF1 <- rbind(spinDF, histDF)
    rDF2 <- rbind(histDF, obsDF)
    
    ### calculate annual mean
    ckDF1 <- summaryBy(.~YEAR, FUN=mean, keep.names=T, na.rm=T, data=rDF1)
    ckDF2 <- summaryBy(.~YEAR, FUN=mean, keep.names=T, na.rm=T, data=rDF2)
    
    n1 <- dim(ckDF1)[2]
    n2 <- dim(ckDF2)[2]
    
    pdf("output/GDAY/quality_check/raw_check1.pdf")
    for (i in 3:n1) {
        plot(ckDF1[,i]~ckDF1$YEAR, xlab="year", ylab=colnames(ckDF1)[i])
        title(colnames(ckDF1)[i])
    }
    dev.off()
    
    pdf("output/GDAY/quality_check/raw_check2.pdf")
    for (i in 3:n2) {
        plot(ckDF2[,i]~ckDF2$YEAR, xlab="year", ylab=colnames(ckDF2)[i])
        title(colnames(ckDF2)[i])
    }
    dev.off()
    
    sumDF1 <- colMeans(histDF, na.rm=T)
    sumDF2 <- colMeans(obsDF, na.rm=T)
    corrDF <- as.data.frame(sumDF2 - sumDF1)
    colnames(corrDF) <- "value"
    
    write.csv(corrDF, "tmp_data/correction_factor.csv")
    
    ### alternatively, can use Martin's Python code to generate mode data for obs period
    ### will that solve the mismatches between historic and obs?
    
}
