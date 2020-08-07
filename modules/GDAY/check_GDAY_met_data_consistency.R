check_GDAY_met_data_consistency <- function()  {
    ### read in files
    spinDF <- read.csv("output/GDAY/EUC_met_spinup_daily_50yrs.csv", skip=4)
    names(spinDF)[names(spinDF) == 'X.year'] <- "year"
    
    histDF <- read.csv("output/GDAY/EUC_met_historic_daily_1750_2011.csv", skip=4)
    names(histDF)[names(histDF) == 'X.year'] <- "year"
    
    obsDF.wet <- read.csv("output/GDAY/EUC_met_WET_AMB_daily_2012_2019.csv", skip=4)
    names(obsDF.wet)[names(obsDF.wet) == 'X.year'] <- "year"
    
    obsDF.dry <- read.csv("output/GDAY/EUC_met_DRY_AMB_daily_2012_2019.csv", skip=4)
    names(obsDF.dry)[names(obsDF.dry) == 'X.year'] <- "year"
    
    prdDF.wet <- read.csv("output/GDAY/EUC_met_WET_AMB_NOP_daily_2020_2069.csv", skip=4)
    names(prdDF.wet)[names(prdDF.wet) == 'X.year'] <- "year"
    
    prdDF.dry <- read.csv("output/GDAY/EUC_met_DRY_AMB_NOP_daily_2020_2069.csv", skip=4)
    names(prdDF.dry)[names(prdDF.dry) == 'X.year'] <- "year"
    
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
    plotDF1 <- summaryBy(.~year+Label, FUN=mean, keep.names=T, na.rm=T, data=rDF)
    
    ## number of columns
    n <- dim(plotDF1)[2]
    
    pdf("output/GDAY/quality_check/annual_met_data_validation.pdf", width = 8, height=6)
    for (i in 4:n) {
        p <- ggplot(plotDF1) +
            geom_point(aes(x = year, y = plotDF1[,i], fill = Label, pch = Label), size=4)+
            geom_line(aes(x = year, y = plotDF1[,i], col=Label))+
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
    
    
    
    #### check on Ndep, Pdep, and CO2 concentrations
    
}
