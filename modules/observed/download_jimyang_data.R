download_jimyang_data <- function() {
    s <- searchHIEv("FACE_AUTO_RA_MET_L2_")
    downloadTOA5(hievSearch=s, maxnfiles=20000)
}