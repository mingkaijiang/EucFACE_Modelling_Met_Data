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
#### Notes:
#### a: output met data in two formats:
####                                   csv and netcdf
#### b: output met data in two timesteps:
####                                   half hourly, and daily
#### c: four periods: 
####                 spin-up: 20-years of data
####                 historic: recycle 20 years of data, but with 1750 - 2011 CO2 forcing
####                 observed - OBS (2013 - 2019)
####                 prdicted - PRD (2020 - 2069)

###########################################################################
#### Step 2.1. prepare spin-up data
#### weather data: recycle Martin's 50 year equilibrium weather data

### csv
prepare_EucFACE_spinup_met_data_csv(timestep="half_hourly")

prepare_EucFACE_spinup_met_data_csv(timestep="daily")

#### nc

###########################################################################
#### Step 2.2. prepare historic data (1992 - 2011)
#### weather data: recycle Martin's 1992 - 2011 weather data
#### CO2, N and P deposition: realistic data over 1750 - 2012

### csv
prepare_EucFACE_historic_met_data_csv(timestep="half_hourly")

prepare_EucFACE_historic_met_data_csv(timestep="daily")


###########################################################################
#### Step 2.2. prepare 7-year simulation data (2013 - 2019)
#### 4 scenarios: 
####             OBS_WET_AMB_NOP: aCO2, wet (recycle wet year)
####             OBS_WET_ELE_NOP: eCO2, wet (recycle wet year)
####             OBS_DRY_AMB_NOP: aCO2, dry (realistic weather)
####             OBS_DRY_ELE_NOP: eCO2, dry (realsitic weather)

### half hourly
prepare_EucFACE_OBS_met_data_csv(timestep="half_hourly")
prepare_EucFACE_OBS_met_data_csv(timestep="daily")

prepare_EucFACE_OBS_met_data_half_hourly_nc()


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
