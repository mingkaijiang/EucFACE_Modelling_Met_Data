prepare_ndep_data <- function() {
    ### read N deposition and CO2 data
    ndepDF <- read.table("tmp_data/EucFACE_forcing_daily_CO2NDEP_1750-2023.dat", header=T)
    colnames(ndepDF) <- c("YEAR", "DOY", "CO2ambient", "CO2elevated", "Ndep")
    ndepDF$Ndep <- ndepDF$Ndep / 10
    
    ndepDF$YEAR <- as.numeric(ndepDF$YEAR)
    ndepDF$DOY <- as.numeric(ndepDF$DOY)

    return(ndepDF)
}