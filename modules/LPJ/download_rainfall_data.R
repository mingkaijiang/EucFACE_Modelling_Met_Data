download_rainfall_data <- function() {
    s <- searchHIEv("FACE_R[1-6]_Rain")
    downloadTOA5(hievSearch=s, maxnfiles=10000)
}