plot_canopy_temperature_profile_wet_vs_dry_days <- function(plotDF) {
    
    ### process the data
    plotDF$HOD <- hour(plotDF$DateHour)
    plotDF$Date <- as.Date(plotDF$DateHour)
    
    ### quality control - remove unrealistic temperatures
    plotDF <- plotDF[plotDF$SBTmp1 > -5, ]
    plotDF <- plotDF[plotDF$TargTmp1 > -5, ]
    plotDF <- plotDF[plotDF$TargTmp2 > -5, ]
    
    plotDF <- plotDF[plotDF$AirT_Avg < 50, ]
    plotDF <- plotDF[plotDF$SBTmp2 < 50, ]
    plotDF <- plotDF[plotDF$SBTmp4 < 50, ]
    plotDF <- plotDF[plotDF$TargTmp1 < 50, ]
    plotDF <- plotDF[plotDF$TargTmp2 < 50, ]
    plotDF <- plotDF[plotDF$TargTmp3 < 50, ]
    plotDF <- plotDF[plotDF$TargTmp4 < 50, ]
    
    
    ### calculate temperature difference for each four sensor individually, and for each ring
    plotDF$Tmp1Diff <- plotDF$SBTmp1 - plotDF$AirT_Avg#plotDF$TargTmp1
    plotDF$Tmp2Diff <- plotDF$SBTmp2 - plotDF$AirT_Avg#plotDF$TargTmp2
    plotDF$Tmp3Diff <- plotDF$SBTmp3 - plotDF$AirT_Avg#plotDF$TargTmp3
    plotDF$Tmp4Diff <- plotDF$SBTmp4 - plotDF$AirT_Avg#plotDF$TargTmp4
    
    ### calculate average temperature difference profile
    plotDF$TmpDiff_Avg <- rowMeans(data.frame(plotDF$Tmp1Diff,plotDF$Tmp2Diff,plotDF$Tmp3Diff,plotDF$Tmp4Diff), na.rm=T)

    ### plot 
    p1 <- ggplot(plotDF, aes(x=AirT_Avg, y=TmpDiff_Avg)) +
        geom_bin2d(bins = 500) +
        theme_bw()+
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
              legend.direction="horizontal")+
        xlab("Tair") +
        ylab("Tair - Tcanopy")+
        scale_fill_continuous(type = "viridis")
    
    #plot(p1)
    
    ### create binned Tair profile
    plotDF2 <- data.frame(seq(5, 45, by=10), NA, NA, NA, NA)
    colnames(plotDF2) <- c("Tair", "Tdiff_mean", "Tdiff_sd", "Td_pos", "Td_neg")
    
    for (i in plotDF2$Tair) {
        plotDF2[plotDF2$Tair == i, "Tdiff_mean"] <- mean(plotDF[plotDF$AirT_Avg > (i - 5) & plotDF$AirT_Avg <= (i + 5), "TmpDiff_Avg"], na.rm=T)
        plotDF2[plotDF2$Tair == i, "Tdiff_sd"] <- sd(plotDF[plotDF$AirT_Avg > (i - 5) & plotDF$AirT_Avg <= (i + 5), "TmpDiff_Avg"], na.rm=T)
    }
    
    plotDF2$Td_pos <- plotDF2$Tdiff_mean+plotDF2$Tdiff_sd
    plotDF2$Td_neg <- plotDF2$Tdiff_mean-plotDF2$Tdiff_sd
    
    ### plotting
    p2 <- ggplot(plotDF2, aes(x=Tair, y=Tdiff_mean)) +
        geom_point()+
        geom_errorbar(aes(x=Tair, ymin=Td_neg, ymax=Td_pos))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0,
              legend.direction="vertical")+
        xlab("Tair") +
        ylab("Tair - Tcanopy")+
        scale_fill_continuous(type = "viridis")
    
    
    #plot(p2)
    
    
    ### prepare daily profile
    dDF <- summaryBy(AirT_Avg~Date+Ring, FUN=mean, data=plotDF, keep.names=T, na.rm=T)
    colnames(dDF) <- c("Date", "Ring", "Daily_Mean_Tair")
    plotDF3 <- merge(plotDF, dDF, by=c("Date", "Ring"))
    
    
    ### plotting
    p3 <- ggplot(plotDF3, aes(x=HOD, y=TmpDiff_Avg, group=Date,
                              col=Daily_Mean_Tair, fill=Daily_Mean_Tair), pch=19) +
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
              legend.direction="horizontal")+
        xlab("HOD") +
        ylab("Tair - Tcanopy")+
        scale_fill_continuous(type = "viridis")+
        scale_color_continuous(type = "viridis")+
        xlim(0, 24)
    
    #plot(p3)
    
    
    ################################# Plot daily profile at every 2 degree
    plotDF4 <- data.frame(rep(seq(1, 49, by=2), 24), rep(0:23, each = 25),
                          NA, NA, NA, NA)
    colnames(plotDF4) <- c("Tmean", "HOD", "diff_mean", "diff_sd", "diff_pos", "diff_neg")
    
    for (i in plotDF4$Tmean) {
        for (j in 0:23) {
            plotDF4[plotDF4$Tmean == i & plotDF4$HOD == j, "diff_mean"] <- mean(plotDF3[plotDF3$Daily_Mean_Tair > (i - 1) & plotDF3$Daily_Mean_Tair <= (i + 1) & plotDF3$HOD == j, "TmpDiff_Avg"], na.rm=T)
            plotDF4[plotDF4$Tmean == i & plotDF4$HOD == j, "diff_sd"] <- sd(plotDF3[plotDF3$Daily_Mean_Tair > (i - 1) & plotDF3$Daily_Mean_Tair <= (i + 1) & plotDF3$HOD == j, "TmpDiff_Avg"], na.rm=T)
        }
    }
    
    plotDF4$diff_pos <- plotDF4$diff_mean+plotDF4$diff_sd
    plotDF4$diff_neg <- plotDF4$diff_mean-plotDF4$diff_sd
    
    
    
    ### plotting
    p4 <- ggplot(plotDF4, aes(x=HOD, y=diff_mean, group=Tmean,
                              col=Tmean)) +
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
              legend.direction="horizontal")+
        xlab("HOD") +
        ylab("Tair - Tcanopy")+
        scale_color_continuous(type = "viridis")+
        xlim(0, 24)
    
    #plot(p4)
    
    
    ### subset days, compare dry and wet days
    plotDF5 <- plotDF3[plotDF3$Date%in%c(as.Date("2016-12-29"),as.Date("2020-01-04")),]
    
    
    
    ### plotting
    p5 <- ggplot(plotDF5, aes(x=HOD, y=TmpDiff_Avg, group=as.character(Date),
                              col=as.character(Date))) +
        geom_hline(yintercept=0, col="black", lwd=1.0)+
        geom_line()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=18, family="Helvetica"),
              legend.title=element_text(size=8, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0,
              legend.direction="vertical")+
        xlab("HOD") +
        ylab("Tair - Tcanopy")+
        xlim(0, 24)+
        scale_color_discrete(name="Date")
        
    
    #plot(p5)
    
    multi.panel.plot <-
        ggdraw() +
        draw_plot(p1, x = 0.0, y = .51, width = 0.5, height = .45) +
        draw_plot(p2, x = 0.51, y = .51, width = 0.4, height = .45) +
        #draw_plot(p3, x = 0.0, y = .41, width = 1., height = .2) +
        draw_plot(p4, x = 0.0, y = .0, width = 0.5, height = .45) +
        draw_plot(p5, x = 0.5, y = .0, width = 0.5, height = .45)
    
    
    ggsave(filename = "output/T_canopy_profile.pdf", 
           plot = multi.panel.plot,
           width = 160, 
           height = 160,
           units = "mm",
           dpi = 300)


}