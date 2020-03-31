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
    ### 
    inDF$Ndep <- 0.225
    
    ### generate a random year list
    set.seed(123)
    yr.list <- sample(1992:2011, 50, replace=T)
    
    ### generate variable name and unit list
    var.list <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2air", "SoilTemp", "Ndep")
    
    colnames(inDF) <- var.list
    
    ### prepare output DF
    outDF <- c()
    
    ### loop through random years to create the equilibrium dataset
    for (i in yr.list) {
        tmpDF <- subset(inDF, YEAR == i)
        
        outDF <- rbind(outDF, tmpDF)
    }
    
    ### force CO2 concentration to be pre-industrial
    outDF$CO2air <- 276.84

    
    ### add unit and name list
    unit.list <- c("year", "day", "hour", "W m-2", "umol m-2 s-1", "W m-2", "K", "kg m-2 s-1",
                   "Pa", "%", "m s-1", "Pa", "ppmv", "K", "g N m-2 yr-1")
    
    name.list <- c("year", "day", "hour", "shortwave radiation", 
                   "photosynthetically active radiation", "longwave radiation",
                   "air temperature", "rainfall", "vapor pressure deficit",
                   "relative humidity", "wind speed", "surface pressure",
                   "CO2 concentration", "soil temperature", "nitrogen deposition")
    
    headDF <- data.frame(rbind(name.list, unit.list))
    colnames(headDF) <- var.list
    rownames(headDF) <- NULL
    
    ### decide what timestep to output
    if(timestep == "half_hourly") {

        write.table(headDF, "output/spinup/csv/half_hourly/EUC_met_spinup_half_hourly_50yrs.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(outDF, "output/spinup/csv/half_hourly/EUC_met_spinup_half_hourly_50yrs.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
    } else if(timestep == "daily") {
        
        ### generate daily means (day time means)
        
        
        
        
        write.table(headDF, "output/spinup/csv/half_hourly/EUC_met_spinup_half_hourly_50yrs.csv",
                    col.names=T, row.names=F, sep=",", append=F, quote = F)
        
        write.table(outDF, "output/spinup/csv/half_hourly/EUC_met_spinup_half_hourly_50yrs.csv",
                    col.names=F, row.names=F, sep=",", append=T, quote = F)
        
        
    }
    
}