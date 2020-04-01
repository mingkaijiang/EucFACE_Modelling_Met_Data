download_ros_table05_data <- function() {
    s <- searchHIEv("ROS_WS_Table05min")
    downloadTOA5(hievSearch=s, maxnfiles=20000)
}