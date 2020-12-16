fit_soil_water_retension <- function() {
    
    
    myDF <- read.csv("tmp_data/soil_water_content.csv")

    ### for pedo transfer function to get soil retension curve
    #require(soilassessment)
    
    ### for calculating soil physics
    #require(soilphysics)
    
    h <- c(0.001, 50.65, 293.77, 790.14, 992.74, 5065, 10130, 15195)
    w <- c(0.5650, 0.4013, 0.2502, 0.2324, 0.2307, 0.1926, 0.1812, 0.1730)
    #fitsoilwater(w, h)
    
    require(devtools)
    install_github("mrke/NicheMapR")
    
    require(githubinstall)
    githubinstall("NicheMapR")

}