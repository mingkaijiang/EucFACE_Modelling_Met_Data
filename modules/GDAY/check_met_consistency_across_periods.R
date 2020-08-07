check_met_data_consistency_across_periods <- function()  {
    
    ### read in files
    spinDF <- read.csv("output/spinup/csv/daily/EUC_met_spinup_daily_50yrs.csv", 
                       skip=3, header=FALSE)

    histDF <- read.csv("output/historic/csv/daily/EUC_met_historic_daily_1750_2011.csv", 
                       skip=3, header=FALSE)
    
    var.list1 <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                   "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                   "CO2air", "SoilTemp", "Ndep")
    
    colnames(spinDF) <- colnames(histDF) <- var.list1

    
    obsDF.wet <- read.csv("output/observed/csv/daily/EUC_met_observed_wet_daily_2012_2019.csv", skip=3, header=FALSE)
    obsDF.dry <- read.csv("output/observed/csv/daily/EUC_met_observed_dry_daily_2012_2019.csv", skip=3, header=FALSE)
    prdDF.wet <- read.csv("output/predicted/csv/daily/EUC_predicted_wet_met_daily_2020_2069.csv", skip=3, header=FALSE)
    prdDF.dry <- read.csv("output/predicted/csv/daily/EUC_predicted_dry_met_daily_2020_2069.csv", skip=3, header=FALSE)
    
    ### generate variable name and unit list
    var.list2 <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                  "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                  "CO2air", "CO2elevated", "SoilTemp", "Ndep")
    
    colnames(obsDF.wet) <- colnames(obsDF.dry) <- colnames(prdDF.wet) <- colnames(prdDF.dry) <- var.list2
    
    obsDF.wet$CO2elevated <- NULL
    obsDF.dry$CO2elevated <- NULL
    prdDF.wet$CO2elevated <- NULL
    prdDF.dry$CO2elevated <- NULL
    
    ### assign labels
    spinDF$Label <- "spinup"
    histDF$Label <- "historic"
    obsDF.wet$Label <- "obs_wet"
    obsDF.dry$Label <- "obs_dry"
    prdDF.wet$Label <- "prd_wet"
    prdDF.dry$Label <- "prd_dry"
    
    ### merge
    rDF <- rbind(spinDF, histDF, obsDF.wet, obsDF.dry, prdDF.wet, prdDF.dry)
    
    ################################ Check annual consistency ###########################
    ## check annual values, for spinup and hist dataframes
    plotDF1 <- summaryBy(.~YEAR+Label, FUN=mean, keep.names=T, na.rm=T, data=rDF)
    
    ## number of columns
    n <- dim(plotDF1)[2]
    
    pdf("output/predicted/csv/annual_met_data_validation.pdf", width = 8, height=6)
    for (i in 4:n) {
        p <- ggplot(plotDF1) +
            geom_point(aes(x = YEAR, y = plotDF1[,i], fill = Label, pch = Label), size=4)+
            geom_line(aes(x = YEAR, y = plotDF1[,i], col=Label))+
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_text(size=12),
                  axis.title.x=element_blank(),
                  axis.text.y=element_text(size=12),
                  axis.title.y=element_text(size=14),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=16),
                  panel.grid.major=element_blank(),
                  legend.position="bottom",
                  legend.box = 'horizontal',
                  legend.box.just = 'left',
                  plot.title = element_text(size=16, face="bold.italic", 
                                            hjust = 0.5))+
            ylab(colnames(plotDF1)[i])+
            scale_color_manual(name="",
                               limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               values=c("grey", "black", "blue3", "red2", "purple", "orange"),
                               guide=guide_legend(nrow=1))+
            scale_fill_manual(name="",
                              limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                              labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                              values=c("grey", "black", "blue3", "red2", "purple", "orange"),
                              guide=guide_legend(nrow=1))+
            scale_linetype_manual(name="",
                                  limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                                  labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                                  values=c("dotted", "dotted", "solid", "solid", "dotted", "dotted"),
                                  guide=guide_legend(nrow=1))+
            scale_shape_manual(name="",
                               limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               values=c(24,24,21,21, 22, 22),
                               guide=guide_legend(nrow=1))+
            ggtitle(colnames(plotDF1)[i])+
            xlab("Year")+
            scale_x_continuous(limits=c(1700, 2070),
                               breaks=c(1700, 1750, 1800, 1850, 1900, 1950, 2000, 2050))
        
        
        plot(p)
    }
    
    dev.off()
    
    
    
    ################################ Check daily consistency ###########################
    ## subsets
    plotDF2 <- subset(rDF, YEAR <= 1751 & YEAR >= 1748)
    plotDF3 <- subset(rDF, YEAR <= 2013 & YEAR >= 2010)
    plotDF4 <- subset(rDF, YEAR <= 2021 & YEAR >= 2018)
    
    ## number of columns
    n <- dim(plotDF2)[2]
    
    ### add Date
    plotDF2$Date <- as.Date((plotDF2$DOY-1), origin = paste0(plotDF2$YEAR, "-01-01"))
    plotDF3$Date <- as.Date((plotDF3$DOY-1), origin = paste0(plotDF3$YEAR, "-01-01"))
    plotDF4$Date <- as.Date((plotDF4$DOY-1), origin = paste0(plotDF4$YEAR, "-01-01"))
    
    
    
    pdf("output/predicted/csv/daily_met_data_validation_spinup_historic.pdf", width = 8, height=6)
    for (i in 3:n) {
        p <- ggplot(plotDF2) +
            geom_point(aes(x = Date, y = plotDF2[,i], fill = Label, pch = Label), size=4)+
            geom_line(aes(x = Date, y = plotDF2[,i], col=Label))+
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_text(size=12),
                  axis.title.x=element_blank(),
                  axis.text.y=element_text(size=12),
                  axis.title.y=element_text(size=14),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=16),
                  panel.grid.major=element_blank(),
                  legend.position="bottom",
                  legend.box = 'horizontal',
                  legend.box.just = 'left',
                  plot.title = element_text(size=16, face="bold.italic", 
                                            hjust = 0.5))+
            ylab(colnames(plotDF2)[i])+
            scale_color_manual(name="",
                               limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               values=c("grey", "black", "blue3", "red2", "purple", "orange"),
                               guide=guide_legend(nrow=1))+
            scale_fill_manual(name="",
                              limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                              labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                              values=c("grey", "black", "blue3", "red2", "purple", "orange"),
                              guide=guide_legend(nrow=1))+
            scale_linetype_manual(name="",
                                  limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                                  labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                                  values=c("dotted", "dotted", "solid", "solid", "dotted", "dotted"),
                                  guide=guide_legend(nrow=1))+
            scale_shape_manual(name="",
                               limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               values=c(24,24,21,21, 22, 22),
                               guide=guide_legend(nrow=1))+
            ggtitle(colnames(plotDF2)[i])+
            xlab("Year")
        
        
        plot(p)
    }
    
    dev.off()
    
    
    ### historic to obs
    pdf("output/predicted/csv/daily_met_data_validation_historic_obs.pdf", width = 8, height=6)
    for (i in 3:n) {
        p <- ggplot(plotDF3) +
            geom_point(aes(x = Date, y = plotDF3[,i], fill = Label, pch = Label), size=4)+
            geom_line(aes(x = Date, y = plotDF3[,i], col=Label))+
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_text(size=12),
                  axis.title.x=element_blank(),
                  axis.text.y=element_text(size=12),
                  axis.title.y=element_text(size=14),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=16),
                  panel.grid.major=element_blank(),
                  legend.position="bottom",
                  legend.box = 'horizontal',
                  legend.box.just = 'left',
                  plot.title = element_text(size=16, face="bold.italic", 
                                            hjust = 0.5))+
            ylab(colnames(plotDF3)[i])+
            scale_color_manual(name="",
                               limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               values=c("grey", "black", "blue3", "red2", "purple", "orange"),
                               guide=guide_legend(nrow=1))+
            scale_fill_manual(name="",
                              limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                              labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                              values=c("grey", "black", "blue3", "red2", "purple", "orange"),
                              guide=guide_legend(nrow=1))+
            scale_linetype_manual(name="",
                                  limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                                  labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                                  values=c("dotted", "dotted", "solid", "solid", "dotted", "dotted"),
                                  guide=guide_legend(nrow=1))+
            scale_shape_manual(name="",
                               limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               values=c(24,24,21,21, 22, 22),
                               guide=guide_legend(nrow=1))+
            ggtitle(colnames(plotDF3)[i])+
            xlab("Year")
        
        
        plot(p)
    }
    
    dev.off()
    
    #### obs to future
    pdf("output/predicted/csv/daily_met_data_validation_obs_future.pdf", width = 8, height=6)
    for (i in 3:n) {
        p <- ggplot(plotDF4) +
            geom_point(aes(x = Date, y = plotDF4[,i], fill = Label, pch = Label), size=4)+
            geom_line(aes(x = Date, y = plotDF4[,i], col=Label))+
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_text(size=12),
                  axis.title.x=element_blank(),
                  axis.text.y=element_text(size=12),
                  axis.title.y=element_text(size=14),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=16),
                  panel.grid.major=element_blank(),
                  legend.position="bottom",
                  legend.box = 'horizontal',
                  legend.box.just = 'left',
                  plot.title = element_text(size=16, face="bold.italic", 
                                            hjust = 0.5))+
            ylab(colnames(plotDF4)[i])+
            scale_color_manual(name="",
                               limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               values=c("grey", "black", "blue3", "red2", "purple", "orange"),
                               guide=guide_legend(nrow=1))+
            scale_fill_manual(name="",
                              limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                              labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                              values=c("grey", "black", "blue3", "red2", "purple", "orange"),
                              guide=guide_legend(nrow=1))+
            scale_linetype_manual(name="",
                                  limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                                  labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                                  values=c("dotted", "dotted", "solid", "solid", "dotted", "dotted"),
                                  guide=guide_legend(nrow=1))+
            scale_shape_manual(name="",
                               limits=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               labels=c("spinup", "historic", "obs_wet", "obs_dry", "prd_wet", "prd_dry"),
                               values=c(24,24,21,21, 22, 22),
                               guide=guide_legend(nrow=1))+
            ggtitle(colnames(plotDF4)[i])+
            xlab("Year")
        
        
        plot(p)
    }
    
    dev.off()
}