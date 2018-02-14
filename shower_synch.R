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
# they're close, but still not identical

# this is easier to see
dcast(DT_shower_meter, study + logging + KEYCODE ~ meter )[order(KEYCODE)]

# try this
s='Seattle' 
l=2 
k=13197 
DT=DT_shower_interval4 

# find offsets
<- function (s=study, l=logging, k=KEYCODE, DT=DT_shower_interval4) {
  # function to plot power and water flow for one siteID
  # s = study               - Seattle | EBMUD
  # l = logging             - 1 | 2 | 3 , the phase of the study
  # k = KEYCODE             - 5 digit integer that identifies site
  # DT=DT_shower_interval4  - information about shower interval data
  
  # for each total shower END find the next and prev closest hot shower END
  # DT1 just one slk.
  DT1 <- DT[ study==s & logging == l & KEYCODE == k]
  
  # keep just the ends
  # for total showers
  DT_total.END <- DT1[meter == 'total water', list(total.END=END)]
  # setkey
  setkey(DT_total.END)
  
  # for hot showers
  DT_hot.END <- DT1[meter == 'hot water', list(hot.END=END)]
  setkey(DT_hot.END)
  
  # count of showers in total water
  nshowers <- nrow(DT_total.END)
  
  # for each shower in DT_total.END find the prev and next shower in DT_hot.END
  nshower = 2
  
  # find end of that shower in total water.
  this.total.END = DT_total.END[nshower,]$total.END
  
  # find the next closest hot.END, could be NA
  next.hot.END <- min(DT_hot.END[hot.END >= this.total.END]$hot.END)
  
  # find the prev closest hot.END, could be NA  
  prev.hot.END <- max(DT_hot.END[hot.END < this.total.END]$hot.END)
  
  # add to DT_total.END
  DT_total.END[nshower, c("next.hot.END", "prev.hot.END") := list(next.hot.END, prev.hot.END)]
  
  
  see plot_shower for ideas on how to retrieve data

