download_r3_flux_data <- function() {
    s <- searchHIEv("FACE_R3_T1_flux_")
    downloadTOA5(hievSearch=s, maxnfiles=20000)
}