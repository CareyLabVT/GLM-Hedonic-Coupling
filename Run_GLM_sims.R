# Load packages ####
pacman::p_load(cowplot, GLMr, glmtools, tidyverse, lubridate)

sim_folder <- './Hedonic_sims'
nml_file <- paste0(sim_folder,"/glm2.nml")

# PART 1: RUN DIFFERENT SIMS! ##############
### Baseline (no change; sim 1) ####
sim <- 1
nml <- read_nml(nml_file) # Change inflow files and save glm2.nml!
nml$inflow$inflow_fl     #check that sim inflows match what's below

run_glm(sim_folder, verbose=TRUE) # "Run complete" if everything worked ok.
nc_file <- file.path(sim_folder, 'output.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
chla   <- get_var(nc_file, "PHY_TCHLA", reference='surface', z_out=c(1))
lec    <- get_var(nc_file, "extc_coef", reference='surface', z_out=c(1))
secchi <- data.frame(DateTime=as.POSIXct(lec$DateTime), secchi = 1.7/lec[,2])
TP     <- get_var(nc_file, "TOT_tp", reference='surface',z_out=c(1))

all <- left_join(chla, secchi) %>% left_join(., TP) %>%
  rename(Chla = PHY_TCHLA_1, Secchi = secchi, TP = TOT_tp_1) %>%
  mutate(sim = sim) %>% select(sim, DateTime:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Wide.csv', sep=""), append=F) %>%
  group_by(DateTime, sim) %>% gather("variable", "value", Chla:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Long.csv', sep=""), append=F)

### 100% reduction (sim 0) ####
sim <- 0
nml <- read_nml(nml_file) # Change inflow files and save glm2.nml!
#nml <- set_nml(nml, 'inflow_fl', c('yahara_in.0.csv','pheasant_in.0.csv'))
nml$inflow$inflow_fl     #check that sim inflows match what's below
#write_path <- file.path(sim_folder,'glm2.nml')
#write_nml(nml, file = write_path)

run_glm(sim_folder, verbose=TRUE) # "Run complete" if everything worked ok.
nc_file <- file.path(sim_folder, 'output.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
chla   <- get_var(nc_file, "PHY_TCHLA", reference='surface', z_out=c(1))
lec    <- get_var(nc_file, "extc_coef", reference='surface', z_out=c(1))
secchi <- data.frame(DateTime=as.POSIXct(lec$DateTime), secchi = 1.7/lec[,2])
TP     <- get_var(nc_file, "TOT_tp", reference='surface',z_out=c(1))

all <- left_join(chla, secchi) %>% left_join(., TP) %>%
  rename(Chla = PHY_TCHLA_1, Secchi = secchi, TP = TOT_tp_1) %>%
  mutate(sim = sim) %>% select(sim, DateTime:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Wide.csv', sep=""), append=F) %>%
  group_by(DateTime, sim) %>% gather("variable", "value", Chla:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Long.csv', sep=""), append=F)

### 75% reduction (sim 0.25) ####
sim <- 0.25
nml <- read_nml(nml_file) # Change inflow files and save glm2.nml!
#nml <- set_nml(nml, 'inflow_fl', c('yahara_in.0.25.csv','pheasant_in.0.25.csv'))
nml$inflow$inflow_fl     #check that sim inflows match what's below

run_glm(sim_folder, verbose=TRUE) # "Run complete" if everything worked ok.
nc_file <- file.path(sim_folder, 'output.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
chla   <- get_var(nc_file, "PHY_TCHLA", reference='surface', z_out=c(1))
lec    <- get_var(nc_file, "extc_coef", reference='surface', z_out=c(1))
secchi <- data.frame(DateTime=as.POSIXct(lec$DateTime), secchi = 1.7/lec[,2])
TP     <- get_var(nc_file, "TOT_tp", reference='surface',z_out=c(1))

all <- left_join(chla, secchi) %>% left_join(., TP) %>%
  rename(Chla = PHY_TCHLA_1, Secchi = secchi, TP = TOT_tp_1) %>%
  mutate(sim = sim) %>% select(sim, DateTime:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Wide.csv', sep=""), append=F) %>%
  group_by(DateTime, sim) %>% gather("variable", "value", Chla:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Long.csv', sep=""), append=F)

### 50% reduction (sim 0.5) ####
sim <- 0.5
nml <- read_nml(nml_file) # Change inflow files and save glm2.nml!
#nml <- set_nml(nml, 'inflow_fl', c('yahara_in.0.5.csv','pheasant_in.0.5.csv'))
nml$inflow$inflow_fl     #check that sim inflows match what's below

run_glm(sim_folder, verbose=TRUE) # "Run complete" if everything worked ok.
nc_file <- file.path(sim_folder, 'output.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
chla   <- get_var(nc_file, "PHY_TCHLA", reference='surface', z_out=c(1))
lec    <- get_var(nc_file, "extc_coef", reference='surface', z_out=c(1))
secchi <- data.frame(DateTime=as.POSIXct(lec$DateTime), secchi = 1.7/lec[,2])
TP     <- get_var(nc_file, "TOT_tp", reference='surface',z_out=c(1))

all <- left_join(chla, secchi) %>% left_join(., TP) %>%
  rename(Chla = PHY_TCHLA_1, Secchi = secchi, TP = TOT_tp_1) %>%
  mutate(sim = sim) %>% select(sim, DateTime:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Wide.csv', sep=""), append=F) %>%
  group_by(DateTime, sim) %>% gather("variable", "value", Chla:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Long.csv', sep=""), append=F)

### 25% reduction (sim 0.75) ####
sim <- 0.75
nml <- read_nml(nml_file) # Change inflow files and save glm2.nml!
#nml <- set_nml(nml, 'inflow_fl', c('yahara_in.0.75.csv','pheasant_in.0.75.csv'))
nml$inflow$inflow_fl     #check that sim inflows match what's below

run_glm(sim_folder, verbose=TRUE) # "Run complete" if everything worked ok.
nc_file <- file.path(sim_folder, 'output.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
chla   <- get_var(nc_file, "PHY_TCHLA", reference='surface', z_out=c(1))
lec    <- get_var(nc_file, "extc_coef", reference='surface', z_out=c(1))
secchi <- data.frame(DateTime=as.POSIXct(lec$DateTime), secchi = 1.7/lec[,2])
TP     <- get_var(nc_file, "TOT_tp", reference='surface',z_out=c(1))

all <- left_join(chla, secchi) %>% left_join(., TP) %>%
  rename(Chla = PHY_TCHLA_1, Secchi = secchi, TP = TOT_tp_1) %>%
  mutate(sim = sim) %>% select(sim, DateTime:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Wide.csv', sep=""), append=F) %>%
  group_by(DateTime, sim) %>% gather("variable", "value", Chla:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Long.csv', sep=""), append=F)

### 25% increase (sim 1.25) ####
sim <- 1.25
nml <- read_nml(nml_file) # Change inflow files and save glm2.nml!
#nml <- set_nml(nml, 'inflow_fl', c('yahara_in.1.25.csv','pheasant_in.1.25.csv'))
nml$inflow$inflow_fl     #check that sim inflows match what's below

run_glm(sim_folder, verbose=TRUE) # "Run complete" if everything worked ok.
nc_file <- file.path(sim_folder, 'output.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
chla   <- get_var(nc_file, "PHY_TCHLA", reference='surface', z_out=c(1))
lec    <- get_var(nc_file, "extc_coef", reference='surface', z_out=c(1))
secchi <- data.frame(DateTime=as.POSIXct(lec$DateTime), secchi = 1.7/lec[,2])
TP     <- get_var(nc_file, "TOT_tp", reference='surface',z_out=c(1))

all <- left_join(chla, secchi) %>% left_join(., TP) %>%
  rename(Chla = PHY_TCHLA_1, Secchi = secchi, TP = TOT_tp_1) %>%
  mutate(sim = sim) %>% select(sim, DateTime:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Wide.csv', sep=""), append=F) %>%
  group_by(DateTime, sim) %>% gather("variable", "value", Chla:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Long.csv', sep=""), append=F)

### 50% increase (sim 1.5) ####
sim <- 1.5
nml <- read_nml(nml_file) # Change inflow files and save glm2.nml!
#nml <- set_nml(nml, 'inflow_fl', c('yahara_in.1.5.csv','pheasant_in.1.5.csv'))
nml$inflow$inflow_fl     #check that sim inflows match what's below

run_glm(sim_folder, verbose=TRUE) # "Run complete" if everything worked ok.
nc_file <- file.path(sim_folder, 'output.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
chla   <- get_var(nc_file, "PHY_TCHLA", reference='surface', z_out=c(1))
lec    <- get_var(nc_file, "extc_coef", reference='surface', z_out=c(1))
secchi <- data.frame(DateTime=as.POSIXct(lec$DateTime), secchi = 1.7/lec[,2])
TP     <- get_var(nc_file, "TOT_tp", reference='surface',z_out=c(1))

all <- left_join(chla, secchi) %>% left_join(., TP) %>%
  rename(Chla = PHY_TCHLA_1, Secchi = secchi, TP = TOT_tp_1) %>%
  mutate(sim = sim) %>% select(sim, DateTime:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Wide.csv', sep=""), append=F) %>%
  group_by(DateTime, sim) %>% gather("variable", "value", Chla:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Long.csv', sep=""), append=F)

### 75% increase (sim 1.75) ####
sim <- 1.75
nml <- read_nml(nml_file) # Change inflow files and save glm2.nml!
#nml <- set_nml(nml, 'inflow_fl', c('yahara_in.1.75.csv','pheasant_in.1.75.csv'))
nml$inflow$inflow_fl     #check that sim inflows match what's below

run_glm(sim_folder, verbose=TRUE) # "Run complete" if everything worked ok.
nc_file <- file.path(sim_folder, 'output.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
chla   <- get_var(nc_file, "PHY_TCHLA", reference='surface', z_out=c(1))
lec    <- get_var(nc_file, "extc_coef", reference='surface', z_out=c(1))
secchi <- data.frame(DateTime=as.POSIXct(lec$DateTime), secchi = 1.7/lec[,2])
TP     <- get_var(nc_file, "TOT_tp", reference='surface',z_out=c(1))

all <- left_join(chla, secchi) %>% left_join(., TP) %>%
  rename(Chla = PHY_TCHLA_1, Secchi = secchi, TP = TOT_tp_1) %>%
  mutate(sim = sim) %>% select(sim, DateTime:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Wide.csv', sep=""), append=F) %>%
  group_by(DateTime, sim) %>% gather("variable", "value", Chla:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Long.csv', sep=""), append=F)

### 100% increase (sim 2) ####
sim <- 2
nml <- read_nml(nml_file) # Change inflow files and save glm2.nml!
#nml <- set_nml(nml, 'inflow_fl', c('yahara_in.2.csv','pheasant_in.2.csv'))
nml$inflow$inflow_fl     #check that sim inflows match what's below

run_glm(sim_folder, verbose=TRUE) # "Run complete" if everything worked ok.
nc_file <- file.path(sim_folder, 'output.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
chla   <- get_var(nc_file, "PHY_TCHLA", reference='surface', z_out=c(1))
lec    <- get_var(nc_file, "extc_coef", reference='surface', z_out=c(1))
secchi <- data.frame(DateTime=as.POSIXct(lec$DateTime), secchi = 1.7/lec[,2])
TP     <- get_var(nc_file, "TOT_tp", reference='surface',z_out=c(1))

all <- left_join(chla, secchi) %>% left_join(., TP) %>%
  rename(Chla = PHY_TCHLA_1, Secchi = secchi, TP = TOT_tp_1) %>%
  mutate(sim = sim) %>% select(sim, DateTime:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Wide.csv', sep=""), append=F) %>%
  group_by(DateTime, sim) %>% gather("variable", "value", Chla:TP) %>%
  write_csv(paste(sim_folder, '/output/Sim', sim, '_', format(Sys.Date(), "%d%b%Y"),'_Long.csv', sep=""), append=F)

# PART 2: Merge sim output into single dataframe ####
#convertP = 30.97/1000 # P conversion factor: mmol/m3  to mg/L (P molar mass 30.97 g/mol)
convertP.ug = 30.97 # P conversion factor: mmol/m3 to ug/L

dataLong <- list.files(path = "./Hedonic_sims/output/", pattern="*Long.csv", full.names = T) %>% 
  map_df(~read_csv(.)) %>%
  mutate(sim = as.factor(sim)) %>%
  mutate(value = ifelse(variable == 'TP', value*convertP.ug, value)) %>%
  write_csv(paste('./Sim Output/Sim_TimeSeries_long_', format(Sys.Date(), "%d%b%Y"),'.csv', sep=""))

dataWide <- list.files(path = "./Hedonic_sims/output/", pattern="*Wide.csv", full.names = T) %>% 
  map_df(~read_csv(.)) %>%
  mutate(sim = as.factor(sim), year = year(DateTime), month = month(DateTime), TP = TP*convertP.ug) %>%
  write_csv(paste('./Sim Output/Sim_TimeSeries_wide_', format(Sys.Date(), "%d%b%Y"),'.csv', sep=""))

# Summary by year: Calculate mean, max, min Secchi and Chla by summer or annual
summer <- dataWide %>% filter(month > 5 & month < 9) %>% group_by(year, sim) %>%
  summarise(mean_Secchi = mean(Secchi), max_Secchi = max(Secchi), 
            mean_Chla = mean(Chla), min_Chla = min(Chla)) %>%
  gather(metric, value, mean_Secchi:min_Chla) %>% mutate(Time = "summer")

annual <- dataWide %>% group_by(year, sim) %>%
  summarise(mean_Secchi = mean(Secchi), max_Secchi = max(Secchi), 
            mean_Chla = mean(Chla), min_Chla = min(Chla)) %>%
  gather(metric, value, mean_Secchi:min_Chla) %>% mutate(Time = "annual")

bind_rows(summer, annual) %>% spread(sim, value) %>% 
  write_csv(paste('./Sim Output/Yearly_summary_', format(Sys.Date(), "%d%b%Y"),'.csv', sep=""))

# Summary overall: Calculate mean, max, min Secchi and Chla
summer2 <- dataWide %>% filter(month > 5 & month < 9) %>% group_by(sim) %>%
  summarise(mean_Secchi = mean(Secchi), max_Secchi = max(Secchi), 
            mean_Chla = mean(Chla), min_Chla = min(Chla)) %>%
  gather(metric, value, mean_Secchi:min_Chla) %>% mutate(Time = "summer")

annual2 <- dataWide %>% group_by(sim) %>%
  summarise(mean_Secchi = mean(Secchi), max_Secchi = max(Secchi), 
            mean_Chla = mean(Chla), min_Chla = min(Chla)) %>%
  gather(metric, value, mean_Secchi:min_Chla) %>% mutate(Time = "annual")

bind_rows(summer2, annual2) %>% spread(sim, value) %>% 
  write_csv(paste('./Sim Output/Overall_summary_', format(Sys.Date(), "%d%b%Y"),'.csv', sep=""))
