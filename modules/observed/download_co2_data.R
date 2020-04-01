download_co2_data <- function() {
    s <- searchHIEv("FACE_AUTO_R[1-6]_FCPLOGG_R")
    downloadTOA5(hievSearch=s, maxnfiles=20000)
}

