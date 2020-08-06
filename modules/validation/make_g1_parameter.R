make_g1_parameter <- function() {
    ### read in eucface aci data
    aciDF <- read.csv("tmp_data/Aci.EucFACE.csv")
    
    ### clean data
    aciDF <- aciDF[aciDF$Number!=290,]
    aciDF <- aciDF[aciDF$Number!=293,]
    aciDF <- aciDF[aciDF$Number!=304,]
    aciDF <- aciDF[aciDF$Number!=305,]
    aciDF <- aciDF[aciDF$Number!=322,]
    aciDF <- aciDF[aciDF$Number!=324,]
    aciDF <- aciDF[aciDF$Number!=341,]
    aciDF <- aciDF[aciDF$Number!=385,]
    aciDF <- aciDF[aciDF$Number!=387,]
    aciDF <- aciDF[aciDF$Number!=478,]
    aciDF <- aciDF[aciDF$Number!=647,]
    aciDF <- aciDF[aciDF$Number!=664,]
    aciDF <- aciDF[aciDF$Number!=670,]
    
    ## id list
    id.list <- unique(aciDF$Number)
    
    ### prepare storage DF
    outDF <- data.frame(id.list, NA, NA, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA,
                        NA)
    colnames(outDF) <- c("Number", "RMSE", "Vcmax", "Vcmax.se", 
                         "Jmax", "Jmax.se", "Rd", "Rd.se",
                         "Ci_transition_Ac_Aj","curve.fitting", 
                         "GammaStar", "Km", "G1")
    
    
    
    ### the for loop
    for (i in id.list) {
        ## subset each data
        test <- subset(aciDF, Number == i)
        
        ### fit aci curve
        fit1 <- fitaci(test, 
                       fitmethod="bilinear",
                       varnames = list(ALEAF="Photo",
                                       Tleaf="Tleaf", 
                                       Ci = "Ci",
                                       PPFD="PARi"),
                       Tcorrect=T, fitTPU=F)
        
        ### fit g1 value
        fit2 <- fitBB(test, 
                      varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL",
                                      Ca = "CO2S", RH = "RH_S"),
                      gsmodel="BBOpti")
        
        ## get information on Number
        outDF[outDF$Number == i, "curve.fitting"] <- fit1$fitmethod
        
        ## assign fitted values
        outDF[outDF$Number == i, "RMSE"] <- fit1$RMSE
        outDF[outDF$Number == i, "Vcmax"] <- fit1$pars[1,1]
        outDF[outDF$Number == i, "Vcmax.se"] <- fit1$pars[1,2]
        outDF[outDF$Number == i, "Jmax"] <- fit1$pars[2,1]
        outDF[outDF$Number == i, "Jmax.se"] <- fit1$pars[2,2]
        outDF[outDF$Number == i, "Rd"] <- fit1$pars[3,1]
        outDF[outDF$Number == i, "Rd.se"] <- fit1$pars[3,2]
        outDF[outDF$Number == i, "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
        outDF[outDF$Number == i, "GammaStar"] <- fit1$GammaStar
        outDF[outDF$Number == i, "Km"] <- fit1$Km
        # G1
        outDF[outDF$Number == i, "G1"] <- coef(fit2)[2]
        
    }
    
    outDF$JVratio <- outDF$Jmax / outDF$Vcmax
    
    
    ### add campagin, ring, tree, height information
    idDF <- unique(aciDF[,c("Number", "Tree", "Leaf", "Species", "Ring",
                            "C.treat", "Age", "Height..m.", "Date",
                            "Campaign")])
    
    outDF <- merge(outDF, idDF, by="Number")
    
    ### subset and summarize
    outDF <- subset(outDF, C.treat == "0C")
    
    out <- summaryBy(Vcmax+Jmax+G1~C.treat, FUN=c(mean, sd), data=outDF, keep.names=T, na.rm=T)
    
    print(out)
}