download_below_canopy_met_data <- function() {
    s <- searchHIEv("FACE_R[1-6]_B1_AirVars")
    downloadTOA5(hievSearch=s, maxnfiles=10000)
}