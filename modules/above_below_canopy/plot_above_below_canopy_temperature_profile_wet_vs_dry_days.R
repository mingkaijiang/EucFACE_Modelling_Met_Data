plot_above_below_canopy_temperature_profile_wet_vs_dry_days <- function(plotDF) {
    
    ### process the data
    plotDF$HOD <- hour(plotDF$DateHour)
    plotDF$Date <- as.Date(plotDF$DateHour)
    dDF <- summaryBy(AT~Date, FUN=mean, data=plotDF, keep.names=T, na.rm=T)
    colnames(dDF) <- c("Date", "Daily_Mean_Tair")
    plotDF2 <- merge(plotDF, dDF, by="Date")
    
    ### subset days
    #test <- subset(plotDF2, Daily_Mean_Tair > 303)
    plotDF3 <- plotDF2[plotDF2$Date%in%c(as.Date("2016-12-29"),as.Date("2020-01-04")),]
    
    
    
    ### plotting
    p1 <- ggplot(plotDF3, aes(x=HOD, y=diff1, group=as.character(Date),
                              col=as.character(Date))) +
        geom_hline(yintercept=0, col="black", lwd=1.5)+
        geom_line()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0,
              legend.direction="vertical")+
        xlab("HOD") +
        ylab("Tair diff (above - below)")+
        xlim(0, 24)
    
    
    ggsave(filename = "output/T_canopy_wet_vs_dry.pdf", 
           plot = p1,
           width = 89, 
           height = 89,
           units = "mm",
           dpi = 300)


}