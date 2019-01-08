# Script to make different inflow N & P files ####
pacman::p_load(tidyverse)

# Load data
yahara <- read_csv('./Mendota_1979_2015_AED2_Paul(14Dec2017)/Mendota_yahara_30year2xP.csv')
pheasant <- read_csv('./Mendota_1979_2015_AED2_Paul(14Dec2017)/Mendota_pheasant_branch_30year2xP.csv')

# Set model start/end dates
startDate = as.Date('2000-04-01')# start date
endDate = as.Date('2013-12-31')# start date

# Create sequence of nutrient sims relative to baseline (1)
sims <- seq(0,2,0.25)

for (i in unique(sims)) {
  yahara %>% 
    filter(time >= startDate & time <= endDate) %>%
    mutate_at(vars(PHS_frp:NIT_nit), funs(. * i)) %>%
    write.csv(paste('./Hedonic_sims/yahara_in', i, 'csv', sep = '.'), row.names = FALSE, quote = FALSE)
  
}

for (i in unique(sims)) {
  pheasant %>% 
    filter(time >= startDate & time <= endDate) %>%
    mutate_at(vars(PHS_frp:NIT_nit), funs(. * i)) %>%
    write.csv(paste('./Hedonic_sims/pheasant_in', i, 'csv', sep = '.'), row.names = FALSE, quote = FALSE)
  
}

# Subset NLDAS for shorter sims 
met_data <- read_csv('./Mendota_1979_2015_AED2_Paul(14Dec2017)/NLDAS2_Mendota_1979_2016_cell_5_GLMReady.csv')

met_data %>% filter(time >= startDate & time <= endDate) %>%
  write.csv('./Hedonic_sims/met_hourly.csv', row.names = FALSE, quote = FALSE)

# Subset outflow file for shorter sims 
outflow <- read_csv('./Mendota_1979_2015_AED2_Paul(14Dec2017)/Mendota_outflow_30year.csv')

outflow %>% filter(time >= startDate & time <= endDate) %>%
  write.csv('./Hedonic_sims/outflow.csv', row.names = FALSE, quote = FALSE)
