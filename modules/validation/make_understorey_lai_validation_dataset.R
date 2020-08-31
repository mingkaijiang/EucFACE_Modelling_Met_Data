make_understorey_lai_validation_dataset <- function(){
    
    ### read data from local repo
    res <- read.csv("tmp_data/understorey_lai.csv")
    
    res$Trt <- "aCO2"
    res$Trt[res$Ring%in%c(1,4,5)] <- "eCO2"
    
    plotDF <- summaryBy(LAI~Date+Trt, FUN=c(mean, sd), data=res, keep.names=T)
    
    plotDF$Date <- as.Date(as.character(plotDF$Date))
    
    ## plot
    p1 <- ggplot(plotDF, aes(Date, LAI.mean, col=Trt)) +
        geom_ribbon(aes(x=Date, ymin=LAI.mean-LAI.sd,
                        ymax=LAI.mean+LAI.sd, fill=Trt), alpha=0.1, lty=0.2)+
        geom_point(pch=19) +
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
    
    pdf("output/validation_datasets/understorey_LAI.pdf", width=6, height=4)
    
    plot(p1)
    
    dev.off()
}