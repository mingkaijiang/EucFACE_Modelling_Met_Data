make_sla_parameter <- function() {
    
    ### Generate ring-specific SLA data per date
    inDF1 <- read.csv("tmp_data/FACE_P0020_RA_LMA_L2_20130213-20131115.csv", stringsAsFactors=F)
    inDF2 <- read.csv("tmp_data/FACE_P0020_RA_LMA_20140130-20141016_L2.csv", stringsAsFactors=F)
    inDF3 <- read.csv("tmp_data/FACE_P0020_RA_LMA_20150129-20150416_L2.csv", stringsAsFactors=F)
    inDF4 <- read.csv("tmp_data/FACE_P0020_RA_LMA_20160201-20161018_L2.csv", stringsAsFactors=F)
    
    lma_raw <- rbind(inDF1, inDF2, inDF3, inDF4)
    lma_raw$Date <- as.Date(lma_raw$Date, format="%d/%m/%Y")
    lma <- droplevels(subset(lma_raw, TREE != "outs R6"))  # outside ring trees
    lma <- mutate(lma, 
                  Ring = as.numeric(substr(TREE,1,1)),
                  LMA = as.numeric(LMA),
                  SLA = 10000 / LMA,
                  AGE = AGE)  # cm2 g-1
    lma <- subset(lma, !is.na(Ring), select =c(Date,Ring, SLA, AGE))  # missing ring is for tree 'outs R6' - ignore
    
    ### treatment
    lma$Trt <- "aCO2"
    lma$Trt[lma$Ring%in%c(1,4,5)] <- "eCO2"
    
    lma_a <- summaryBy(SLA ~ Trt + AGE, FUN=c(mean, sd), na.rm=TRUE, data=lma, keep.names=TRUE)
    
    
    out <- subset(lma_a, Trt=="aCO2")
    print(out)
    
}