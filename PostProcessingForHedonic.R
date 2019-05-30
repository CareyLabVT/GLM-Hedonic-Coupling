#### Load packages ####
# If you don't have GLM 2.2.0, run installation lines below. If already installed,
# skip to line 12

#install.packages('sp')
#install.packages('devtools') 
#library(sp) 
#library(devtools)
#devtools::install_github("CareyLabVT/GLMr", force = TRUE) # version 2.2.0rc5
#devtools::install_github("CareyLabVT/glmtools", force = TRUE) 

pacman::p_load(cowplot, GLMr, glmtools, tidyverse, lubridate)

sim_folder <- './Hedonic_sims' #or wherever you need to go in terms of directory 
  #structure as to where your output.nc files are!

#BE SURE TO CHANGE YOUR OUTPUT.NC FILE NAMES!

# PART 1: GET THE DATA FROM YOUR GLM OUTPUT FILES! ##############
### Baseline (no change; sim 1) ####
### BMP scenario (90% fertilizer reduction) ####

nc_file_baseline <- file.path(sim_folder, 'output_baseline.nc') #this defines the BASELINE output.nc file 
nc_file_BMP <- file.path(sim_folder, 'output_BMP.nc') #this defines the BMP output.nc file 

# Extract variables of interest from GLM baseline output
chla_baseline   <- get_var(nc_file_baseline, "PHY_TCHLA", reference='surface', z_out=c(1))
lec_baseline    <- get_var(nc_file_baseline, "extc_coef", reference='surface', z_out=c(1))
secchi_baseline <- data.frame(DateTime=as.POSIXct(lec$DateTime), secchi = 1.7/lec[,2])
TP_baseline     <- get_var(nc_file_baseline, "TOT_tp", reference='surface',z_out=c(1))

sim_folder <- './' #or wherever you need to go!

baseline <- left_join(chla_baseline, secchi_baseline) %>% left_join(., TP_baseline) %>%
  rename(Chla_baseline = PHY_TCHLA_1, Secchi_baseline = secchi, TP_baseline = TOT_tp_1) %>%
  write.csv(baseline, "BaselineScenario_output.csv")

# Extract variables of interest from GLM BMP output
chla_BMP   <- get_var(nc_file_BMP, "PHY_TCHLA", reference='surface', z_out=c(1))
lec_BMP    <- get_var(nc_file_BMP, "extc_coef", reference='surface', z_out=c(1))
secchi_BMP <- data.frame(DateTime=as.POSIXct(lec$DateTime), secchi = 1.7/lec[,2])
TP_BMP     <- get_var(nc_file_BMP, "TOT_tp", reference='surface',z_out=c(1))

BMP <- left_join(chla_BMP, secchi_BMP) %>% left_join(., TP_BMP) %>%
  rename(chla_BMP = PHY_TCHLA_1, secchi_BMP = secchi, TP_BMP = TOT_tp_1) %>%
  write.csv(BMP, "BMPScenario_output.csv")

#then, send these files to Weizhe/Kelly for analysis in the hedonic model

# PART 2: Plots of different scenarios ####
pacman::p_load(cowplot, tidyverse, lubridate)

plot(baseline$DateTime, baseline$Chla_baseline, col="black", type="l",
     ylab="Chla (ug/L)", xlab="Date")
points(BMP$DateTime, BMP$chla_BMP, col="red", type='l')
legend("topleft", legend=c("Baseline","BMP"), fill=c("black","red"))

plot(baseline$DateTime, baseline$Secchi_baseline, col="black", type="l",
     ylab="Secchi (m)", xlab="Date")
points(BMP$DateTime, BMP$secchi_BMP, col="red", type='l')
legend("topleft", legend=c("Baseline","BMP"), fill=c("black","red"))
