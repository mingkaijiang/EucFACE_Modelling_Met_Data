prepare_below_canopy_met_data <- function() {
    
    #### Download the data - takes time to run
    myDF <- download_below_canopy_met_data()
    
    ### data description
    ### general radiation, air T, RH, throughfall and PAR below EucFACE canopy averaged every minute
    
    ### PAR_Den_1_Avg: LiCor Quantum 190SB PAR Sensor 1 (μmol s-1 m-2)
    ### PAR_Den_2_Avg: LiCor Quantum 190SB PAR Sensor 2 (μmol s-1 m-2)
    ### PAR_Den_3_Avg: LiCor Quantum 190SB PAR Sensor 3 (μmol s-1 m-2)
    ### AirTC_1_Avg: Vaisala HMP 155 Air Temperature Probe 1 (degree C)
    ### AirTC_2_Avg: Vaisala HMP 155 Air Temperature Probe 2 (degree C)
    ### RH_1_Avg: Vaisala HMP 155 Relative Humidity Probe 1 (%)
    ### RH_2_Avg: Vaisala HMP 155 Relative Humidity Probe 2 (%)
    ### TrghFlow_mm_Tot: Flow Through Rain Gauge along the pathway on south of the ring | Rainfall through the canopy (mm)
    
    
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_B1_AirVars.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)  
    myDF <- myDF[order(myDF$DateTime),]
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
    myDF$DateHour <- as.POSIXct(paste0(myDF$Date, " ", hour(myDF$DateTime), ":00:00"),format = "%Y-%m-%d %H:%M:%S")
    
    ### as numeric
    myDF$AirTC_1_Avg <- as.numeric(myDF$AirTC_1_Avg)
    myDF$AirTC_2_Avg <- as.numeric(myDF$AirTC_2_Avg)
    
    ### Calculate hourly mean
    hDF <-aggregate(myDF[c("AirTC_1_Avg","AirTC_2_Avg")],
                    by=myDF[c("DateHour", "Ring")], 
                    FUN=mean, na.rm = T, keep.names=T) 
    
    ### Colnames
    colnames(hDF) <- c("DateHour", "Ring", "AirTC_1_Avg", "AirTC_2_Avg")
    
    ### Air temperature from degree C to K
    hDF$AirTC_1_Avg <- hDF$AirTC_1_Avg + 273.15
    hDF$AirTC_2_Avg <- hDF$AirTC_2_Avg + 273.15
    
    ### Save  data
    write.csv(hDF, "output/met_data_hourly_below_canopy.csv", row.names=F)
    
    
    
    return(hDF)
    
    
}