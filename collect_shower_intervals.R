# collect_shower_intervals.R
# script to extract shower interval data and save to one .Rdata file for later processing
# Jim Lutz "Thu Mar  1 09:20:29 2018"

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

# total number of showers
nrow(DT_shower_interval4)
# 2245
# not too bad 
2245*8*10
# [1] 179600
# should be ~ 200k records, actually it's
DT_shower_interval4[,list(nrec = sum(DURATION)/10)]
# nrec
# 1: 99426

# want to have the following fields
# study logging meter KEYCODE  

# look for some sample data with coincident draws
DT_shower_interval4[study   == 'Seattle'  &
                    logging == 1          &
                    KEYCODE == 13197      ,
                    list(START, END, ncoincid)
                    ]

# plot some events
s='Seattle' 
l=1 
k=13197 
DT=DT_shower_interval4 
t1="1999-10-28 05:14:00"
t2="1999-10-28 05:30:00"
save.charts=FALSE

plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)


p

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
DT_1slk.END <- DT[ study==s & logging == l & KEYCODE == k, list(meter, START, END, ncoincid)]

# count of showers in total water
nshowers <- nrow(DT_1slk.END[meter == 'total water'])

# test the find.closest.hots function
DT_ENDs <- data.table(ldply(.data=1:nshowers, .fun =find.closest.hots, .progress= "text", .inform=TRUE))


# plot some to see what's going on
t1="2000-03-26 18:21:00"
t2="2000-03-26 18:50:00"
save.charts=FALSE

plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)

# that one looks like it has a lot of coincident draws
# Aquacraft sees only 5 coincident draws same total & hot
# also looks like this showerhead is single lever, temperature select only



