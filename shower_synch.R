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

  # s = study               - Seattle | EBMUD
  # l = logging             - 1 | 2 | 3 , the phase of the study
  # k = KEYCODE             - 5 digit integer that identifies site
  # DT=DT_shower_interval4  - information about shower interval data
  
# DT_1slk.END is a data.table of just meter and END for  one slk.
DT_1slk.END <- DT[ study==s & logging == l & KEYCODE == k, list(meter, END)]

# count of showers in total water
nshowers <- nrow(DT_1slk.END[meter == 'total water'])

# shower number in total 
nshower = 2

# variables to work with while building function
DT_END=DT_1slk.END 
n = nshower

find.closest.hots <- function (n = nshower, DT_END=DT_1slk.END) {
  # function to find the closest next and previous hot shower ENDs 
  # to the END of one total shower for one slk
  # DT_END  is a data.table of just meter and END for  one slk.
  # n       is the total shower in question
  
  # for total showers
  DT_total.END <- DT_END[meter == 'total water', list(total.END=END)]
  # setkey
  setkey(DT_total.END)
  
  # for hot showers
  DT_hot.END <- DT_END[meter == 'hot water', list(hot.END=END)]
  setkey(DT_hot.END)
  
  # find end of shower n in total water.
  this.total.END = DT_total.END[n,]$total.END
  
  # find the next closest hot.END, could be NA
  next.hot.END <- min(DT_hot.END[hot.END >= this.total.END]$hot.END)
  
  # find the prev closest hot.END, could be NA  
  prev.hot.END <- max(DT_hot.END[hot.END < this.total.END]$hot.END)
  
  # make a 1 row data.table
  DT_ENDs <- data.table(total.END        = this.total.END,
                        prev.hot.END     = prev.hot.END,
                        next.hot.END     = next.hot.END
                        )
  return(DT_ENDs)
  
}
  
# test the find.closest.hots function
DT_ENDs <- data.table(ldply(.data=1:nshowers, .fun =find.closest.hots, .progress= "text", .inform=TRUE))


# plot some to see what's going on
t1="2000-03-25 17:00:00"
t2="2000-03-25 17:20:00"
save.charts=FALSE

plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)

