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
###                          Step 2: met data                           ###
###                                                                     ###
###########################################################################
#### output met data in two formats:
#### csv and netcdf
#### output met data in two timesteps:
#### half hourly, and daily

###########################################################################
#### Step 2.1. prepare spin-up data
#### weather data: recycle Martin's 50/20 year equilibrium weather data
#### CO2, N and P deposition: realistic data over 1750 - 2012

#### half hourly:
###  csv:

###  nc:

#### daily:
###  csv

###  nc

###########################################################################
#### Step 2.2. prepare 7-year simulation data (2013 - 2019)
#### 4 scenarios: 
####             OBS_WET_AMB_NOP: aCO2, wet (recycle wet year)
####             OBS_WET_ELE_NOP: eCO2, wet (recycle wet year)
####             OBS_DRY_AMB_NOP: aCO2, dry (realistic weather)
####             OBS_DRY_ELE_NOP: eCO2, dry (realsitic weather)

### half hourly
prepare_EucFACE_OBS_met_data_half_hourly_csv()

prepare_EucFACE_OBS_met_data_half_hourly_nc()

### daily
prepare_EucFACE_OBS_met_data_daily_csv()

prepare_EucFACE_OBS_met_data_daily_nc()

###########################################################################
#### Step 2.3. Prepare 50-year future prediction data

#### 12 scenarios: 
####             PRD_WET_AMB_NOP: aCO2, wet, no P added
####             PRD_WET_ELE_NOP: eCO2, wet, no P added
####             PRD_WET_AMB_MDP: aCO2, wet, medium P added
####             PRD_WET_AMB_MDP: eCO2, wet, medium P added
####             PRD_WET_AMB_HIP: aCO2, wet, high P added
####             PRD_WET_AMB_HIP: aCO2, wet, high P added

####             PRD_DRY_AMB_NOP: aCO2, dry, no P added
####             PRD_DRY_ELE_NOP: eCO2, dry, no P added
####             PRD_DRY_AMB_MDP: aCO2, dry, medium P added
####             PRD_DRY_AMB_MDP: eCO2, dry, medium P added
####             PRD_DRY_AMB_HIP: aCO2, dry, high P added
####             PRD_DRY_AMB_HIP: aCO2, dry, high P added

### half hourly
prepare_EucFACE_PRD_met_data_half_hourly_csv()

prepare_EucFACE_PRD_met_data_half_hourly_nc()

### daily
prepare_EucFACE_PRD_met_data_daily_csv()

prepare_EucFACE_PRD_met_data_daily_nc()




###########################################################################
