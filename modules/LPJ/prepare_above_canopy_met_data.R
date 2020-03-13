prepare_above_canopy_met_data <- function() {
    
    #### Download the data - takes time to run
    myDF <- download_above_canopy_met_data()
    
    ### data description
    ### general radiation, air T, RH and surface T above EucFACE canopy averaged every minute
    
    ### AirTc_Avg: Vaisala HMP 155 Air T measured on top of tower (degree C)
    ### RH_Avg: Vaisala HMP 155 relative humidity measured on top of tower (%)
    
    ### SBTempC_Avg.1.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 body temperature average (Field of View 18.75 half angle)
    ###                 positioned in North of central tower
    ### SBTempC_Avg.2.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 body temperature average (Field of View 18.75 half angle)
    ###                 positioned in East of central tower
    ### SBTempC_Avg.3.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 body temperature average (Field of View 18.75 half angle)
    ###                 positioned in South of central tower
    ### SBTempC_Avg.4.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 body temperature average (Field of View 18.75 half angle)
    ###                 positioned in West of central tower
    
    ### Net_SW_Avg: Kipp & Zonen CNR2 Short wave (310-2800nm) infrared radiation average (W m-2)
    ### Net_LW_Avg: Kipp & Zonen CNR2 Long wave (310-2800nm) infrared radiation average (W m-2)
    ### LI190SB_PAR_Den_Avg: LiCor Quantum Q45248 PAR sensor (mol s-1 m-2)
    ### SlrW_Avg: Solar short wave Licor Pyranomete LI 200 X (400-1100nm) (W m-2)
    
    ### R1: 
    ### TargTempC_Avg.1.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in North of central tower reading the canopy temp of tree no 112
    ### TargTempC_Avg.2.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in East of central tower reading the canopy temp of tree no 107
    ### TargTempC_Avg.3.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in South of central tower reading the canopy temp of tree no 122
    ### TargTempC_Avg.4.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in West of central tower reading the canopy temp of tree no 117
    
    
    ### R2: 
    ### TargTempC_Avg.1.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in North of central tower reading the canopy temp of tree no 214
    ### TargTempC_Avg.2.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in East of central tower reading the canopy temp of tree no 208
    ### TargTempC_Avg.3.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in South of central tower reading the canopy temp of tree no 205
    ### TargTempC_Avg.4.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in West of central tower reading the canopy temp of tree no 222
    
    
    ### R4: 
    ### TargTempC_Avg.1.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in North of central tower reading the canopy temp of tree no 414
    ### TargTempC_Avg.2.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in East of central tower reading the canopy temp of tree no 408
    ### TargTempC_Avg.3.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in South of central tower reading the canopy temp of tree no 407
    ### TargTempC_Avg.4.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in West of central tower reading the canopy temp of tree no 416
    
    ### R5: 
    ### TargTempC_Avg.1.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in North of central tower 
    ### TargTempC_Avg.2.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in East of central tower 
    ### TargTempC_Avg.3.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in South of central tower 
    ### TargTempC_Avg.4.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in West of central tower 
    
    ### R6: 
    ### TargTempC_Avg.1.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in North of central tower 
    ### TargTempC_Avg.2.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in East of central tower 
    ### TargTempC_Avg.3.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in South of central tower 
    ### TargTempC_Avg.4.: IR thermometer Apogee SI-121-L10 measuring 
    ###                 target temperature average (Field of View 18.75 half angle)
    ###                 positioned in West of central tower 
    
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_T1.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)  
    myDF <- myDF[order(myDF$DateTime),]
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
    myDF$DateHour <- as.POSIXct(paste0(myDF$Date, " ", hour(myDF$DateTime), ":00:00"),format = "%Y-%m-%d %H:%M:%S")
    
    myDF$AirTc_Avg <- as.numeric(myDF$AirTc_Avg)
    #myDF$RH_Avg <- as.numeric(myDF$RH_Avg)
    #myDF$LI190SB_PAR_Den_Avg <- as.numeric(myDF$LI190SB_PAR_Den_Avg)
    
    ### Calculate hourly mean
    hDF <-aggregate(myDF[c("AirTc_Avg")],#,"RH_Avg","LI190SB_PAR_Den_Avg")], 
                    by=myDF[c("DateHour")], 
                    FUN=mean, na.rm = T, keep.names=T) 
    
    ### Calculate daily mean
    #dDF <- aggregate(myDF[c("AirTc_Avg","RH_Avg","LI190SB_PAR_Den_Avg")], 
    #                 by=myDF[c("Date")], 
    #                 FUN=mean, na.rm=T, keep.names=T)
    
    ### Calculate monthly mean
    #mDF <- aggregate(myDF[c("AirTc_Avg","RH_Avg","LI190SB_PAR_Den_Avg")], 
    #                 by=myDF[c("Month")], 
    #                 FUN=mean, na.rm=T, keep.names=T)
    
    ### Colnames
    colnames(hDF) <- c("DateHour", "AAirT")
    
    ### Air temperature from degree C to K
    hDF$AAirT <- hDF$AAirT + 273.15
    
    ### Save  data
    write.csv(hDF, "output/met_data_hourly_above_canopy.csv", row.names=F)

    

    return(hDF)
    
    
}