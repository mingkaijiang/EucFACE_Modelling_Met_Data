check_daily_hourly_match <- function(data.period) {
    
    if (data.period == "obs") {
        
        ########################################################################################
        ######################################### DRY ##########################################
        ### read in hourly data
        hDF <- read.csv("output/observed/csv/half_hourly/EUC_met_observed_dry_half_hourly_2012_2019.csv", 
                        skip=3, header=F)
        var.list1 <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                       "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                       "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
        colnames(hDF) <- var.list1
        
        ### read in daily data
        dDF <- read.csv("output/observed/csv/daily/EUC_met_observed_dry_daily_2012_2019.csv", 
                         skip=3, header=F)
        var.list2 <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                      "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                      "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
        colnames(dDF) <- var.list2
        
        
        ######################## process hourly data ############################
        
        ### calculate total rainfall of the day
        tmpDF1 <- summaryBy(Rain~YEAR+DOY, FUN=sum, data=hDF, keep.names=T)
        
        ### extract daytime DF
        subDF <- subset(hDF, PAR >= 5.0)
        
        tmpDF2 <- summaryBy(SWdown+PAR+LWdown+Tair+VPD+RH+Wind+PSurf+CO2ambient+CO2elevated+SoilTemp+Ndep~YEAR+DOY,
                          FUN=mean, data=subDF, keep.names=T)
        
        dDF2 <- merge(tmpDF1, tmpDF2, by=c("YEAR", "DOY"), all=T)
        
        ### rearrange variables
        dDF2 <- dDF2[,c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                        "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                        "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")]
        
        ######################## finish process hourly data ############################
        dDF$Label <- "original"
        dDF2$Label <- "validation"
        
        rDF <- rbind(dDF, dDF2)
        
        ### summarize to annual
        plotDF <- summaryBy(.~YEAR+Label, FUN=mean, keep.names=T, na.rm=T, data=rDF)

        n <- dim(plotDF)[2]

        pdf("output/observed/csv/quality_check_dry.pdf")
        for (i in 4:n) {
            p <- ggplot(plotDF) +
                geom_point(aes(x = YEAR, y = plotDF[,i], fill = Label, pch = Label), size=4)+
                geom_line(aes(x = YEAR, y = plotDF[,i], col=Label))+
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
                ylab(colnames(plotDF)[i])+
                ggtitle(colnames(plotDF)[i])+
                xlab("Year")
            
            plot(p)
        }
        
        dev.off()
        
        ##################################### End DRY ##########################################
        ########################################################################################

        
        ########################################################################################
        ######################################### WET ##########################################
        ### read in hourly data
        hDF <- read.csv("output/observed/csv/half_hourly/EUC_met_observed_wet_half_hourly_2012_2019.csv", 
                        skip=3, header=F)
        var.list1 <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                       "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                       "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
        colnames(hDF) <- var.list1
        
        ### read in daily data
        dDF <- read.csv("output/observed/csv/daily/EUC_met_observed_wet_daily_2012_2019.csv", 
                        skip=3, header=F)
        var.list2 <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                       "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                       "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
        colnames(dDF) <- var.list2
        
        
        ######################## process hourly data ############################
        
        ### calculate total rainfall of the day
        tmpDF1 <- summaryBy(Rain~YEAR+DOY, FUN=sum, data=hDF, keep.names=T)
        
        ### extract daytime DF
        subDF <- subset(hDF, PAR >= 5.0)
        
        tmpDF2 <- summaryBy(SWdown+PAR+LWdown+Tair+VPD+RH+Wind+PSurf+CO2ambient+CO2elevated+SoilTemp+Ndep~YEAR+DOY,
                            FUN=mean, data=subDF, keep.names=T)
        
        dDF2 <- merge(tmpDF1, tmpDF2, by=c("YEAR", "DOY"), all=T)
        
        ### rearrange variables
        dDF2 <- dDF2[,c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                        "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                        "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")]
        
        ######################## finish process hourly data ############################
        dDF$Label <- "original"
        dDF2$Label <- "validation"
        
        rDF <- rbind(dDF, dDF2)
        
        ### summarize to annual
        plotDF <- summaryBy(.~YEAR+Label, FUN=mean, keep.names=T, na.rm=T, data=rDF)
        
        n <- dim(plotDF)[2]
        
        pdf("output/observed/csv/quality_check_wet.pdf")
        for (i in 4:n) {
            p <- ggplot(plotDF) +
                geom_point(aes(x = YEAR, y = plotDF[,i], fill = Label, pch = Label), size=4)+
                geom_line(aes(x = YEAR, y = plotDF[,i], col=Label))+
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
                ylab(colnames(plotDF)[i])+
                ggtitle(colnames(plotDF)[i])+
                xlab("Year")
            
            plot(p)
        }
        
        dev.off()
        
        ##################################### End WET ##########################################
        ########################################################################################
        
    } else if (data.period == "spinup") {
        
        ### read in hourly data
        hDF <- read.csv("output/spinup/csv/half_hourly/EUC_met_spinup_half_hourly_50yrs.csv", 
                        skip=3, header=F)
        var.list1 <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                       "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                       "CO2air", "SoilTemp", "Ndep")
        colnames(hDF) <- var.list1
        
        ### read in daily data
        dDF <- read.csv("output/spinup/csv/daily/EUC_met_spinup_daily_50yrs.csv", 
                        skip=3, header=F)
        var.list2 <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                       "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                       "CO2air", "SoilTemp", "Ndep")
        colnames(dDF) <- var.list2
        
        
        ######################## process hourly data ############################
        
        ### calculate total rainfall of the day
        tmpDF1 <- summaryBy(Rain~YEAR+DOY, FUN=sum, data=hDF, keep.names=T)
        
        ### extract daytime DF
        subDF <- subset(hDF, PAR >= 5.0)
        
        tmpDF2 <- summaryBy(SWdown+PAR+LWdown+Tair+VPD+RH+Wind+PSurf+CO2air+SoilTemp+Ndep~YEAR+DOY,
                            FUN=mean, data=subDF, keep.names=T)
        
        dDF2 <- merge(tmpDF1, tmpDF2, by=c("YEAR", "DOY"), all=T)
        
        ### rearrange variables
        dDF2 <- dDF2[,c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                        "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                        "CO2air", "SoilTemp", "Ndep")]
        
        ######################## finish process hourly data ############################
        dDF$Label <- "original"
        dDF2$Label <- "validation"
        
        rDF <- rbind(dDF, dDF2)
        
        ### summarize to annual
        plotDF <- summaryBy(.~YEAR+Label, FUN=mean, keep.names=T, na.rm=T, data=rDF)
        
        n <- dim(plotDF)[2]
        
        pdf("output/spinup/csv/quality_check.pdf")
        for (i in 4:n) {
            p <- ggplot(plotDF) +
                geom_point(aes(x = YEAR, y = plotDF[,i], fill = Label, pch = Label), size=4)+
                geom_line(aes(x = YEAR, y = plotDF[,i], col=Label))+
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
                ylab(colnames(plotDF)[i])+
                ggtitle(colnames(plotDF)[i])+
                xlab("Year")
            
            plot(p)
        }
        
        dev.off()
        
        
    } else if (data.period == "historic") {
        
        ### read in hourly data
        hDF <- read.csv("output/historic/csv/half_hourly/EUC_met_historic_half_hourly_1750_2011.csv", 
                        skip=3, header=F)
        var.list1 <- c("YEAR", "DOY", "HOUR", "SWdown", "PAR", "LWdown",
                       "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                       "CO2air", "SoilTemp", "Ndep")
        colnames(hDF) <- var.list1
        
        ### read in daily data
        dDF <- read.csv("output/historic/csv/daily/EUC_met_historic_daily_1750_2011.csv", 
                        skip=3, header=F)
        var.list2 <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                       "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                       "CO2air", "SoilTemp", "Ndep")
        colnames(dDF) <- var.list2
        
        
        ######################## process hourly data ############################
        
        ### calculate total rainfall of the day
        tmpDF1 <- summaryBy(Rain~YEAR+DOY, FUN=sum, data=hDF, keep.names=T)
        
        ### extract daytime DF
        subDF <- subset(hDF, PAR >= 5.0)
        
        tmpDF2 <- summaryBy(SWdown+PAR+LWdown+Tair+VPD+RH+Wind+PSurf+CO2air+SoilTemp+Ndep~YEAR+DOY,
                            FUN=mean, data=subDF, keep.names=T)
        
        dDF2 <- merge(tmpDF1, tmpDF2, by=c("YEAR", "DOY"), all=T)
        
        ### rearrange variables
        dDF2 <- dDF2[,c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                        "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                        "CO2air", "SoilTemp", "Ndep")]
        
        ######################## finish process hourly data ############################
        dDF$Label <- "original"
        dDF2$Label <- "validation"
        
        rDF <- rbind(dDF, dDF2)
        
        ### summarize to annual
        plotDF <- summaryBy(.~YEAR+Label, FUN=mean, keep.names=T, na.rm=T, data=rDF)
        
        n <- dim(plotDF)[2]
        
        pdf("output/historic/csv/quality_check.pdf")
        for (i in 4:n) {
            p <- ggplot(plotDF) +
                geom_point(aes(x = YEAR, y = plotDF[,i], fill = Label, pch = Label), size=4)+
                geom_line(aes(x = YEAR, y = plotDF[,i], col=Label))+
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
                ylab(colnames(plotDF)[i])+
                ggtitle(colnames(plotDF)[i])+
                xlab("Year")
            
            plot(p)
        }
        
        dev.off()
        
    }
    else if (data.period == "pred") {
        
    } else {
        print("no data period available")
    }
    
}