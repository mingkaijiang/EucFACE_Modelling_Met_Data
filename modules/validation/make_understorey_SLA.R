make_understorey_SLA <- function() {
    inDF <- read.csv("tmp_data/EucFACE_GrassStrip_Harvest_20170523.csv")
    
    ### subset ambient only
    aDF <- inDF[inDF$Ring%in%c(2,3,6),]
    
    ### average
    summary(aDF$LiveSubsampleSLA)
    
}