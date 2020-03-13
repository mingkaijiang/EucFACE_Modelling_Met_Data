#### prepare


#### Create data folder
if(!dir.exists("data"))dir.create("data")

#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
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
               RColorBrewer)    

#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("modules", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)

