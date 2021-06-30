calculate_annual_forcing <- function() {
    
    file.dir <- list.files(path="output/GDAY/", pattern=".csv")
    
    for (i in file.dir) {
        myDF <- read.csv(paste0("output/GDAY/", i), skip=4)
        colnames(myDF)[1] <- "year"
        
        outDF1 <- myDF[myDF$doy==1,c("year", "CO2")]
        
        outDF2 <- summaryBy(ndep+nfix+pdep+pfert~year, FUN=sum, data=myDF,
                            na.rm=T, keep.names=T)
        
        outDF <- merge(outDF1, outDF2, by=c("year"))
        
        write.csv(outDF, paste0("output/GDAY/ann/ann_", i), row.names=F)
    }
    
    
}