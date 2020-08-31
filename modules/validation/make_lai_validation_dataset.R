make_lai_validation_dataset <- function(){
    
    ### read data from local repo
    res <- readTOA5("tmp_data/FACE_P0037_RA_GAPFRACLAI_OPEN_L2.dat")
    
    res <- subset(res, select=c(Date, Ring, LAI))
    names(res)[3] <- "lai_variable"
    
    ### return a number for ring
    res$Ring <- as.numeric(res$Ring)
    
    ### Only use data period 2012-2016
    res <- res[res$Date<="2016-12-31",]
    
    ### summarize by treatment
    res$Trt <- "aCO2"
    res$Trt[res$Ring%in%c(1,4,5)] <- "eCO2"
    
    out <- summaryBy(lai_variable~Date+Trt, FUN=c(mean,sd), data=res, keep.names=T, na.rm=T)
    
    colnames(out) <- c("Date", "Trt", "lai", "laiSD")
    
    write.csv(out, "output/validation_datasets/EucFACE_LAI_2012_2016.csv", row.names=F)
    
    
    p1 <- ggplot(out, aes(Date, lai, col=Trt)) +
        geom_ribbon(aes(x=Date, ymin=lai-laiSD,
                        ymax=lai+laiSD, fill=Trt), alpha=0.1, lty=0.2)+
        geom_line(lwd = 1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"))+
        ylab("LAI")+
        scale_fill_manual(
                          breaks=c("aCO2", "eCO2"),
                          values=c("blue2", "red3"),
                          labels=c(expression(aC[a]), expression(eC[a])))+
        scale_color_manual(breaks=c("aCO2", "eCO2"),
                           values=c("blue2", "red3"),
                           labels=c(expression(aC[a]), expression(eC[a])))
    
    pdf("output/validation_datasets/LAI.pdf", width=6, height=4)
    
    plot(p1)
    
    dev.off()
}