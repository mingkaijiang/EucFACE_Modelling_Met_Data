merge_above_below_canopy_data <- function(atDF, btDF) {
    
    #### above canopy data has shorter duration, and averaged over all rings
    #### below canopy data has two temperature profile, for each Ring
    
    #### merge
    mgDF <- merge(btDF, atDF, by = c("DateHour"))
    
    ### rename
    colnames(mgDF) <- c("DateHour", "Ring", "BT1", "BT2", "AT")
    
    ### check difference
    mgDF$diff1 <- mgDF$AT - mgDF$BT1
    mgDF$diff2 <- mgDF$AT - mgDF$BT2

    
    return(mgDF)
    
}