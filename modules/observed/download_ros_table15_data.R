download_ros_table15_data <- function() {
    s <- searchHIEv("ROS_WS_Table15min")
    downloadTOA5(hievSearch=s, maxnfiles=20000)
}