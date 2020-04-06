prepare_GDAY_spinup_data <- function() {
    ### read half-hourly data and output daily data 
    ### in the GDAY format and unit
    myDF <- read.csv("output/spinup/csv/half_hourly/EUC_met_spinup_half_hourly_50yrs.csv", skip=3)

    ### generate variable name and unit list
    var.list <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2air", "SoilTemp", "Ndep")
    
    colnames(myDF) <- var.list
    
    
}