make_Rsoil_validation_dataset <- function(){
    
    #- read in the data
    RE <- as.data.frame(data.table::fread("tmp_data/FACE_P0031_RA_Rsoil-PROCESSED_20120902-20151030_L2_v1.csv"))
    RE$DateTime <- as.POSIXct(RE$DateTime,format="%Y-%m-%d %T",tz="GMT")
    RE$Date <- as.Date(RE$Date)
    
    #- read in the DAMM parameter estimates from fitting done outside this repo
    params_all_collars <- read.csv("tmp_data/DAMM_parameters_all_collars.csv")
    
    
    #-----
    #- apply the DAMM model given the collar-specific parameters to predict Rsoil for each collar on each day
    
    #- get daily average met drivers for each collar
    RE$ring_collar <-factor(paste(RE$ring,RE$collar,sep="_"))
    RE.m <- data.frame(dplyr::summarize(dplyr::group_by(RE, Date, ctreat,ring,ring_collar), 
                                        Rsoil=mean(Rsoil,na.rm=T),    
                                        theta=mean(VWC,na.rm=T),
                                        Tsoil=mean(Tsoil,na.rm=T)))
    RE.m.collar <- split(RE.m,RE.m$ring_collar)
    
    
    #run the model forward with the optimized parameter set
    for (i in 1:length(RE.m.collar)){
        
        #- find the correct row to use in the parameter dataframe, extract the DAMM model parameters
        row <- which(params_all_collars$ring_collar==RE.m.collar[[i]]$ring_collar[1])
        pars <- unname(as.matrix(params_all_collars[row,c("AlphaSx","EaSx","kMSx","kMO2")]))
        RE.m.collar[[i]]$DAMM <- 0
        RE.m.collar[[i]]$DAMM <- DAMM_optim(par=pars,soilT=RE.m.collar[[i]]$Tsoil,
                                            soilM=RE.m.collar[[i]]$theta,flux=RE.m.collar[[i]]$Rsoil,
                                            #EaSx=EaSx[RE.m.collar[[i]]$ring[1]],
                                            #kMSx=kMSx[RE.m.collar[[i]]$ring[1]],
                                            #kMO2=kMO2[RE.m.collar[[i]]$ring[1]],
                                            type="predict") 
        
    }
    RE.m.collar.all <- do.call(rbind,RE.m.collar)
    RE.m.collar.all$ring <- factor(substr(RE.m.collar.all$ring_collar,1,1))
    #-----
    
    
    #- convert from umol CO2 m-2 s-1 to mg C m-2 day-1
    RE.m.collar.all$soil_respiration_flux <- RE.m.collar.all$DAMM*60*60*24*1e-6*12.01*1000
    RE.m.collar.all$ndays <- 1
    
    #- average across dates and plots
    Rsoil.out <- summaryBy(soil_respiration_flux~Date+ring,data=RE.m.collar.all,FUN=mean,keep.names=T)
    
    #- prepare output df
    names(Rsoil.out) <- c("Start_date","Ring","soil_respiration_flux")
    Rsoil.out$Date <- Rsoil.out$Start_date
    Rsoil.out$End_date <- Rsoil.out$Start_date
    Rsoil.out$Ring <- as.numeric(Rsoil.out$Ring)
    Rsoil.out$ndays <- as.numeric(Rsoil.out$End_date - Rsoil.out$Start_date) + 1
    Rsoil.out <- Rsoil.out[,c("Start_date","End_date","Date","Ring","soil_respiration_flux", "ndays")]
    
    colnames(RE.m.collar.all) <- c("Date","ctreat", "Ring", "Ring_collar", "Rsoil", "theta", "Tsoil", "DAMM", 
                                   "soil_respiration_flux", "ndays")
    
    # Only use data period 2012-2016
    Rsoil.out <- Rsoil.out[Rsoil.out$Date<="2016-12-31",]
    
    ### calculate treatment means for each day
    Rsoil.out$Trt <- "aCO2"
    Rsoil.out$Trt[Rsoil.out$Ring%in%c(1,4,5)] <- "eCO2"
    
    out <- summaryBy(soil_respiration_flux~Trt+Date, FUN=c(mean, sd), keep.names=T, na.rm=T, data=Rsoil.out)
    
    
    colnames(out) <- c("Trt", "Date", "Rsoil_mg_m2_d", "RsoilSD")
    
    out <- out[out$Date >= "2013-01-01",]
    
    write.csv(out, "output/validation_datasets/EucFACE_daily_soil_respiration_flux_2013_2015.csv", row.names=F)
}