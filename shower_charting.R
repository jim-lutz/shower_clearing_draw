# shower_charting.R
# script to chart hot and total water for showers
# Jim Lutz "Mon Jan  8 15:53:17 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_interval4.RData
load(file = paste0(wd_data,"DT_shower_interval4.RData"))

DT_shower_interval4
names(DT_shower_interval4)

# now see how many hot/total pairs
DT_shower_meter <- DT_shower_interval4[, list(count=max(nshower)), by = c("study", "logging", "meter", "KEYCODE")][order(KEYCODE)]

# this is easier to see
dcast(DT_shower_meter, study + KEYCODE + logging ~ meter )[order(KEYCODE)]

# find some temporary values for testing plotting function
DT_shower_interval4[study == "Seattle" &
                      logging == 1 &
                      KEYCODE == 13197, list(meter, nshower, START, END, VOLUME, ncoincid) ][order(START)]

# try this
s='Seattle' 
l=1 
k=13197 
DT=DT_shower_interval4 
t1="1999-11-02 19:25:00"
t2="1999-11-02 19:41:00"
save.charts=FALSE

plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)
