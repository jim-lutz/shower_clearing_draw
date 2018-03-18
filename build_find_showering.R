# build_find_showering.R
# script to function to find showering time from shower-only interval data
# Jim Lutz # view_shower_intervals.R
# script to plot interval data and shower-only interval data
# initially used to build a plot_shower_only graph
# Jim Lutz "Fri Mar 16 19:35:44 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_Flows.RData, for plot_shower_only()
load(file = paste0(wd_data,"DT_shower_Flows.RData"))
str(DT_shower_Flows)

# try one skl while building graphing function plot_shower_only
s='Seattle'; k=13431; l=1; m='total water'

# from view_shower_intervals.R develop on a good shower
t1="1999-10-27 17:40:00"
t2="1999-10-27 17:47:00"

# take a look
plot_shower_only(s, l, k, DT=DT_shower_Flows, t1, t2) 

# get data for 'total water' for this shower
DT_1shower <-
DT_shower_Flows[study==s & KEYCODE==k & logging==l & meter==m &
                  t1 <= StartTime & StartTime <= t2,]

# only StartTime & Rate
DT_1shower <- DT_1shower[,list(StartTime,Rate)]

# get seconds from start as numbers

# set timezone, all these Aquacraft sites are in the Pacific time zone
tz="America/Los_Angeles"

# convert StartTime to posix times
DT_1shower[,date.time:=ymd_hms(StartTime, tz=tz)]

# initial time
start.time <- DT_1shower$date.time[1]

# duration, in seconds since start of shower
DT_1shower[, dsec:=as.numeric(as.duration(interval(start.time, date.time)))]

# drop times now
DT_1shower[, `:=`(StartTime = NULL,
                  date.time = NULL)
           ]

# create new column for every row
# clearing draw at the begining, showering draw at the end

# size of data.table
ndt = nrow(DT_1shower)

# this for the clearing draw
for (r in 1:ndt) {  # do this for each row
  set(DT_1shower,                          # modify data.table DT_1shower
      i = 1:r,                             # apply to the first r rows
      j = paste0('Y',r),                   # make the column names
      value = mean(DT_1shower$Rate[1:r])   # average of Rate for the first r rows
      )
}
head(DT_1shower)
tail(DT_1shower)
# seems to have worked

# now for the showering draw
for (r in 1:ndt) {  # do this for each row
  set(DT_1shower,                                 # modify data.table DT_1shower
      i = (r+1):ndt,                              # apply to the last r-1 rows
      j = paste0('Y',r),                          # make the column names
      value = mean(DT_1shower$Rate[((r+1):ndt)])  # average of Rate for the last r-1 rows
  )
}
head(DT_1shower[, paste0('Y',1:5)])
tail(DT_1shower[, paste0('Y',31:35)])
# seems to have worked
