make_leaf_nutrient_concentration_parameter <- function() {
    ### new file code
    df <- read.csv("tmp_data/FACE_P0020_RA_NPleaf_2012-2018-L2.csv")
    
    df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
    
    ## correct unit
    df$PercP <- df$Pm / 10
    df$PercN <- df$Nm / 10
    
    ### summarize
    out <- summaryBy(PercP+PercN~CO2.treat+Age, FUN=c(mean, sd), data=df, na.rm=T, keep.names=T)
    
    
    
}