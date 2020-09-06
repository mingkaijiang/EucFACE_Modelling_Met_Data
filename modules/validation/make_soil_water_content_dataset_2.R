make_soil_water_content_dataset_2 <- function() {
    
    ### read in data
    myDF <- read.csv("tmp_data/soil_water_content.csv")
    
    myDF$Date <- as.Date(as.character(myDF$Date))
    
    probe <- summaryBy(.~Location+Date+Depth+Probe.ID+Ring,FUN=mean,data=myDF,
                       keep.names=T,na.rm=T)
    ring <- summaryBy(.~Location+Date+Depth+Ring,FUN=mean,data=probe,keep.names=T,na.rm=T)
    treat <- summaryBy(.~Location+Date+Depth,FUN=mean,data=ring,keep.names=T,na.rm=T)
    
    # Use fancy package to do contour map over time - best graphs
    data1 <- subset(treat,Location=="Ambient",!is.na(VWC))
    data1$Depthd <- - data1$Depth
    
    a <- interp(x=data1$Date, y=data1$Depthd, z=data1$VWC,
                xo=seq(min(data1$Date),max(data1$Date),by=1),
                yo=seq(min(data1$Depthd),max(data1$Depthd),by=10), duplicate="mean")
    
    a$x <- as.Date(a$x,origin="1970-01-01")
    
    pdf("output/validation_datasets/swc_ambient.pdf")
    filled.contour(a, color.palette=function(n) topo.colors(n=17,rev=T),ylab="Depth (cm)",
                   main="Ambient Soil Water")

    dev.off()
    
    
    
}