make_soil_water_content_dataset <- function() {
    
    ### read in data
    myDF <- read.csv("tmp_data/soil_water_content.csv")
    
    
    myDF$Date <- as.Date(as.character(myDF$Date))
    aDF <- subset(myDF, Location == "Ambient")
    
    ### check raw data
    #with(aDF, plot(VWC~Date))

    #p1 <- ggplot(aDF, aes(Date, Depth, fill = VWC)) +
    #    geom_tile() +
    #    scale_y_reverse() +
    #    scale_fill_gradient2(
    #        midpoint = 20, 
    #        high = scales::muted("blue"), 
    #        low = scales::muted("red")) +
    #    coord_cartesian(expand = F)
    #
    #plot(p1)

    
    aDF %>% 
        summarise(min(Date), max(Date), n_distinct(Date))
    
    
    aDF_yearless <- aDF %>%
        mutate(
            day_since_start = as.numeric(Date - min(Date) + 1, unit = "days")
        )
    
    smooth_fit <- loess(
        VWC ~ day_since_start + Depth, 
        data = aDF_yearless, 
        span = 0.2
    )
    
    newDF <- crossing(
        tibble(Date = seq(ymd("2012-04-30"), ymd("2018-10-15"), by = 1)),
        tibble(Depth = seq(25, 450, by=5))
    ) %>%
        mutate(
            day_since_start = as.numeric(Date - min(Date) + 1, unit = "days"),
            VWC = predict(
                smooth_fit, 
                newdata = tibble(day_since_start = day_since_start, Depth = Depth)
            )
        )
    
    
    p2 <- ggplot(newDF, aes(Date, Depth, fill = VWC)) +
        geom_tile() +
        scale_y_reverse() +
        scale_fill_gradient2(
            midpoint = 20, 
            high = scales::muted("blue"), 
            low = scales::muted("red")) +
        coord_cartesian(expand = F)
    
    pdf("output/validation_datasets/swc_ambient.pdf")
    plot(p2)
    dev.off()
    
    
    write.csv(newDF, "output/validation_datasets/EucFACE_swc_ambient_interpolated.csv", row.names=F)
}