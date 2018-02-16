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
DT_shower_interval4

# now see how many hot/total pairs
DT_shower_meter <- DT_shower_interval4[, list(count=max(nshower)), by = c("study", "logging", "meter", "KEYCODE")][order(KEYCODE)]
dcast(DT_shower_meter, study + logging + KEYCODE ~ meter )[order(KEYCODE)]

# try this
s='Seattle' 
l=2 
k=13197 
DT=DT_shower_interval4 

  # s = study               - Seattle | EBMUD
  # l = logging             - 1 | 2 | 3 , the phase of the study
  # k = KEYCODE             - 5 digit integer that identifies site
  # DT=DT_shower_interval4  - information about shower interval data
  
# DT_1slk.END is a data.table of just meter START END for  one slk.
DT_1slk.END <- DT[ study==s & logging == l & KEYCODE == k, list(meter, START, END)]

# count of showers in total water
nshowers <- nrow(DT_1slk.END[meter == 'total water'])

# test the find.closest.hots function
DT_ENDs <- data.table(ldply(.data=1:nshowers, .fun =find.closest.hots, .progress= "text", .inform=TRUE))


# plot some to see what's going on
t1="2000-03-26 18:23:00"
t2="2000-03-26 18:40:00"
save.charts=FALSE

plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)

