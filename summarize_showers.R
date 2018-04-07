# summarize_showers.R
# script to summarize data, including clearing/showering draws for all showers.
# Jim Lutz "Fri Apr  6 18:19:01 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_Flows.RData, this is the shower only interval data
# for plot_shower_only()
load(file = paste0(wd_data,"DT_shower_Flows.RData"))
str(DT_shower_Flows)

# how many showers?
DT_shower_Flows[,list(nshowers=length(unique(EventID))), 
                by = c("study", "KEYCODE", "logging", "meter")
                ][,list(totshowers=sum(nshowers)), by = meter]
#          meter totshowers
# 1: total water       1255
# 2:   hot water       1252

DT_shower_Flows[,list(EventID = unique(EventID)), 
                by = c("study", "KEYCODE", "logging", "meter")
                ]
