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
#### which we name as period 1700-1749
### note that leap days are removed.

### csv
prepare_EucFACE_spinup_met_data_csv(timestep="half_hourly")

prepare_EucFACE_spinup_met_data_csv(timestep="daily")


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

#### dry
prepare_EucFACE_observed_dry_met_data_csv(timestep="half_hourly", run.option="rerun")
prepare_EucFACE_observed_dry_met_data_csv(timestep="daily", run.option="rerun")

#### wet
prepare_EucFACE_observed_wet_met_data_csv(timestep="half_hourly")
prepare_EucFACE_observed_wet_met_data_csv(timestep="daily")

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

### dry
prepare_EucFACE_predicted_dry_met_data_csv(timestep="half_hourly")
prepare_EucFACE_predicted_dry_met_data_csv(timestep="daily")

### wet
prepare_EucFACE_predicted_wet_met_data_csv(timestep="half_hourly")
prepare_EucFACE_predicted_wet_met_data_csv(timestep="daily")


###########################################################################
###                     Step 3: GDAY input format                       ###
###                                                                     ###
###########################################################################
##### Prepare all half-hourly data in GDAY input format
##### Will do it for each period of data separately
### all output at daily timestep
###########################################################################
#### prepare GDAY spinup data
prepare_GDAY_spinup_data()

#### prepare GDAY historic data (1750 to 2011)
prepare_GDAY_historic_data()

#### prepare GDAY observed data (2012 - 2019)
#### under 4 scenarios
### AMB: ambient CO2; DRY: realistic climate
### ELE: elevated CO2; DRY: realistic climate
### AMB: ambient CO2; WET: repeated wet year climate
### ELE: elevated CO2; WET: repeated wet year climate
prepare_GDAY_observed_dry_data()
prepare_GDAY_observed_wet_data()


#### prepare GDAY predicted data (2020 - 2069)
#### under 12 scenarios
### AMB: ambient CO2; DRY: realistic climate, NOP: no P addition
### ELE: elevated CO2; DRY: realistic climate, NOP: no P addition
### AMB: ambient CO2; WET: repeated wet year climate, NOP: no P addition
### ELE: elevated CO2; WET: repeated wet year climate, NOP: no P addition

### AMB: ambient CO2; DRY: realistic climate, MDP: medium P addition
### ELE: elevated CO2; DRY: realistic climate, MDP: medium P addition
### AMB: ambient CO2; WET: repeated wet year climate, MDP: medium P addition
### ELE: elevated CO2; WET: repeated wet year climate, MDP: medium P addition

### AMB: ambient CO2; DRY: realistic climate, HIP: high P addition
### ELE: elevated CO2; DRY: realistic climate, HIP: high P addition
### AMB: ambient CO2; WET: repeated wet year climate, HIP: high P addition
### ELE: elevated CO2; WET: repeated wet year climate, HIP: high P addition
prepare_GDAY_predicted_dry_data()
prepare_GDAY_predicted_wet_data()

###########################################################################
###                     Step 3: check met data quality                  ###
###                                                                     ###
###########################################################################
#### check data quality
check_met_data_quality()

check_GDAY_met_data_quality()


#### move all GDAY met files to the simulation folder
move_all_met_data_into_GDAY_simulation_folders()


###########################################################################