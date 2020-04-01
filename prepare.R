#### prepare


#### Create data folder
if(!dir.exists("data"))dir.create("data")

output.folders <- c("output",
                    "output/spinup",
                    "output/spinup/csv",
                    "output/spinup/netcdf",
                    "output/spinup/csv/daily",
                    "output/spinup/csv/half_hourly",
                    "output/spinup/netcdf/daily",
                    "output/spinup/netcdf/half_hourly",
                    "output/historic",
                    "output/historic/csv",
                    "output/historic/netcdf",
                    "output/historic/csv/daily",
                    "output/historic/csv/half_hourly",
                    "output/historic/netcdf/daily",
                    "output/historic/netcdf/half_hourly",
                    "output/observed",
                    "output/observed/csv",
                    "output/observed/netcdf",
                    "output/observed/csv/daily",
                    "output/observed/csv/half_hourly",
                    "output/observed/netcdf/daily",
                    "output/observed/netcdf/half_hourly",
                    "output/predicted",
                    "output/predicted/csv",
                    "output/predicted/netcdf",
                    "output/predicted/csv/daily",
                    "output/predicted/csv/half_hourly",
                    "output/predicted/netcdf/daily",
                    "output/predicted/netcdf/half_hourly")

#### Create output folder
for (y in output.folders) {
    if(!dir.exists(y)) {
        dir.create(y, showWarnings = FALSE)
    }
}




#### Install HIEv
if(!require(HIEv)){
    stop("Install the HIEv package first from bitbucket.org/remkoduursma/HIEv")
}

#### Set token
setToken(tokenfile="tokenfile.txt", quiet=TRUE)

### Set data path
setToPath("data")

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, 
               doBy, 
               readxl,
               lubridate,
               ggplot2,
               knitr,
               cowplot,
               viridis,
               sciplot,
               RColorBrewer,
               ncdf4,
               plantecophys)    

#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("modules", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)

