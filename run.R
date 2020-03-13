###########################################################################
###########################################################################
###                                                                     ###
###                   EucFACE met data code repository                  ###
###                                                                     ###
###########################################################################
###########################################################################
###########################################################################
###                Step 1: Set up the basics                            ###
###                                                                     ###
###########################################################################
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("prepare.R")


###########################################################################
###            Step 2: above and below canopy T data                    ###
###                                                                     ###
###########################################################################
#### prepare above and below canopy air T profile at hourly timestep
### above canopy air T
atDF <- prepare_above_canopy_met_data()

### below canopy air T
btDF <- prepare_below_canopy_met_data()


#### read data from saved files
atDF <- read.csv("output/met_data_hourly_above_canopy.csv")
btDF <- read.csv("output/met_data_hourly_below_canopy.csv")

#### merge two datasets
airTDF <- merge_above_below_canopy_data(atDF, btDF)


#### make basic plots
plot_above_below_canopy_temperature_profile(airTDF)
plot_above_below_canopy_temperature_profile_wet_vs_dry_days(airTDF)


###########################################################################
###                      Step 3: canopy T data                          ###
###                                                                     ###
###########################################################################
#### prepare canopy air T profile at hourly timestep
cnpDF <- prepare_canopy_met_data()

### read in from output file
cnpDF <- read.csv("output/met_data_hourly_canopy_temperature_profile.csv")

plot_canopy_temperature_profile_wet_vs_dry_days(plotDF=cnpDF)