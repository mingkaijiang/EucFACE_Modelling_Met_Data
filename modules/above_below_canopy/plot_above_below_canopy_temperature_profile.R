plot_above_below_canopy_temperature_profile <- function(plotDF) {
    
    ######################### Plot diff vs tair, with all data
    
    p1 <- ggplot(plotDF, aes(x=AT, y=diff1)) +
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
              legend.position="right",
              legend.text.align=0,
              legend.direction="vertical")+
        #geom_smooth(se=FALSE, method="loess", span=0.5)+
        xlab("Tair (above)") +
        ylab("Tair diff (above - below)")+
        scale_fill_continuous(type = "viridis")
    
    p2 <- ggplot(plotDF, aes(x=AT, y=diff2)) +
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
              legend.position="right",
              legend.text.align=0,
              legend.direction="vertical")+
        #geom_smooth(se=FALSE, method="loess", span=0.5)+
        xlab("Tair (above)") +
        ylab("Tair diff (above - below)")+
        scale_fill_continuous(type = "viridis")
    
    
    ######################### Plot diff vs tair, with Tair > 40
    #### subset Tair > 40 
    subDF <- subset(plotDF, AT >= 313.5)
    
    p3 <- ggplot(subDF, aes(x=AT, y=diff1)) +
        geom_bin2d(bins = 5) +
        theme_bw()+
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
        xlab("Tair (above)") +
        ylab("Tair diff (above - below)")+
        scale_fill_continuous(type = "viridis")
    
    #plot(p3)
    
    
    ######################### Plot diff vs tair, with binned data
    plotDF2 <- data.frame(seq(275, 315, by=10), NA, NA, NA, NA)
    colnames(plotDF2) <- c("Tair", "Td_mean", "Td_sd", "Td_pos", "Td_neg")
    
    for (i in plotDF2$Tair) {
        plotDF2[plotDF2$Tair == i, "Td_mean"] <- mean(plotDF[plotDF$AT > (i - 5) & plotDF$AT <= (i + 5), "diff1"], na.rm=T)
        plotDF2[plotDF2$Tair == i, "Td_sd"] <- sd(plotDF[plotDF$AT > (i - 5) & plotDF$AT <= (i + 5), "diff1"], na.rm=T)
    }
    
    plotDF2$Td_pos <- plotDF2$Td_mean+plotDF2$Td_sd
    plotDF2$Td_neg <- plotDF2$Td_mean-plotDF2$Td_sd
    
    ### plotting
    p4 <- ggplot(plotDF2, aes(x=Tair, y=Td_mean)) +
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
        xlab("Tair (above)") +
        ylab("Tair diff (above - below)")+
        scale_fill_continuous(type = "viridis")
    
    #plot(p4)
    
    
    ################################# Plot daily profile
    plotDF$HOD <- hour(plotDF$DateHour)
    plotDF$Date <- as.Date(plotDF$DateHour)
    dDF <- summaryBy(AT~Date, FUN=mean, data=plotDF, keep.names=T, na.rm=T)
    colnames(dDF) <- c("Date", "Daily_Mean_Tair")
    plotDF3 <- merge(plotDF, dDF, by="Date")
    
    ### plotting
    p5 <- ggplot(plotDF3, aes(x=HOD, y=diff1, group=Date,
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
              legend.position="right",
              legend.text.align=0,
              legend.direction="vertical")+
        xlab("HOD") +
        ylab("Tair diff (above - below)")+
        scale_fill_continuous(type = "viridis")+
        scale_color_continuous(type = "viridis")+
        xlim(0, 24)
    
    ################################# Plot daily profile at every 2 degree
    plotDF4 <- data.frame(rep(seq(277, 307, by=2), 24), rep(0:23, each = 16),
                          NA, NA, NA, NA)
    colnames(plotDF4) <- c("Td", "HOD", "diff_mean", "diff_sd", "diff_pos", "diff_neg")
    
    for (i in plotDF4$Td) {
        for (j in 0:23) {
            plotDF4[plotDF4$Td == i & plotDF4$HOD == j, "diff_mean"] <- mean(plotDF3[plotDF3$Daily_Mean_Tair > (i - 1) & plotDF3$Daily_Mean_Tair <= (i + 1) & plotDF3$HOD == j, "diff1"], na.rm=T)
            plotDF4[plotDF4$Td == i & plotDF4$HOD == j, "diff_sd"] <- sd(plotDF3[plotDF3$Daily_Mean_Tair > (i - 1) & plotDF3$Daily_Mean_Tair <= (i + 1) & plotDF3$HOD == j, "diff1"], na.rm=T)
        }
    }
    
    plotDF4$diff_pos <- plotDF4$diff_mean+plotDF4$diff_sd
    plotDF4$diff_neg <- plotDF4$diff_mean-plotDF4$diff_sd
    
    
    
    ### plotting
    p6 <- ggplot(plotDF4, aes(x=HOD, y=diff_mean, group=Td,
                              col=Td)) +
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
              legend.position="right",
              legend.text.align=0,
              legend.direction="vertical")+
        xlab("HOD") +
        ylab("Tair diff (above - below)")+
        #scale_fill_continuous(type = "viridis")+
        scale_color_continuous(type = "viridis")+
        xlim(0, 24)
    
    #plot(p6)
    
    
    ################################## save output
    multi.panel.plot <-
        ggdraw() +
        draw_plot(p1, x = 0.0, y = .7, width = 1., height = .3) +
        draw_plot(p4, x = 0.0, y = .35, width = .8, height = .3) +
        draw_plot(p6, x = 0.0, y = .0, width = 1., height = .3)
    
    
    ggsave(filename = "output/T_canopy.pdf", 
           plot = multi.panel.plot,
           width = 89, 
           height = 160,
           units = "mm",
           dpi = 300)
    
    

}