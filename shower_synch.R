# shower_synch.R
# script to synchronize hot water time to total water time using the ends of showers
# then possibly output synchronized interval data for showers with both total and hot 
# into data files by slk 
# Jim Lutz "Tue Feb  6 17:46:36 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_interval4.RData
load(file = paste0(wd_data,"DT_shower_interval4.RData"))

see plot_shower for ideas on how to retrieve data

