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

# calculate Y, test case i==4, that's probably best for this shower
DT_1shower[1:4, Y4:= mean(Rate[1:4])]
DT_1shower[5:.N, Y4:= mean(Rate[4:.N])]
