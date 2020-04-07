move_all_met_data_into_GDAY_simulation_folders <- function() {
    
    ### source path
    source.path <- "output/GDAY/"
    
    ### list all csv in source path
    filelist <- list.files(path = source.path, pattern="*.csv$")
    
    ### destination path
    dest.path <- "~/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/met_data/"    
    
    for (i in filelist) {
        system(paste0("cp ", source.path, i, " ", dest.path, i))
    }
    
}