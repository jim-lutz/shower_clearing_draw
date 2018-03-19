# view_shower_intervals.R
# script to plot interval data and shower-only interval data
# initially used to build a plot_shower_only graph
# Jim Lutz "Fri Mar 16 19:35:44 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_interval4.RData, for plot_shower()
load(file = paste0(wd_data,"DT_shower_interval4.RData"))
names(DT_shower_interval4)

# load DT_shower_Flows.RData, for plot_shower_only()
load(file = paste0(wd_data,"DT_shower_Flows.RData"))
str(DT_shower_Flows)

# try one skl while building graphing function plot_shower_only
s='Seattle'; k=13431; l=1

# bracket the start and end of that skl
bracket <- DT_shower_Flows[study==s & KEYCODE==k & logging==l, 
                           list(start=min(StartTime),end=max(StartTime))]

# find how many coincident shower events for that skl
DT_shower_Flows[study==s & KEYCODE==k & logging==l, 
                list(nrec     = length(StartTime),
                     mtr      = unique(meter),
                     start    = min(StartTime),
                     end      = max(StartTime)
                     ), 
                by=EventID][order(start)][1:20] 
# some really short and some really long showers?

# look to see what's happening
t1="1999-10-27 17:40:00"
t2="1999-10-27 17:47:00"

DT_shower_Flows[study==s & KEYCODE==k & logging==l & 
                t1 <= StartTime & StartTime <= t2,] [order(StartTime,EventID)]

plot_water(s, l, k, DT=DT_shower_interval4, t1, t2)
plot_shower_only(s, l, k, DT=DT_shower_Flows, t1, t2) 
# it's a clean looking shower.
# find some messy ones for comparison

# look to see what's happening somewhen else
t1="1999-10-31 09:13:00"
t2="1999-10-31 09:25:00"

DT_shower_Flows[study==s & KEYCODE==k & logging==l &  
                t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_water(s, l, k, DT=DT_shower_interval4, t1, t2)
# not your typical shower
plot_shower_only(s, l, k, DT=DT_shower_Flows, t1, t2, save.charts=TRUE) 
# shower defined differently for hot water 
# this will be a good test for extracting showering time

# try a different shower
# ============================
test.eID = 2801
DT_shower_Flows[EventID==test.eID,]

# bracket the start and end of that event
bracket <- DT_shower_Flows[EventID==test.eID, 
                           list(start=min(StartTime),end=max(StartTime))]
bracket
#                  start                 end
# 1: 1999-11-08 21:15:14 1999-11-08 21:24:24

# find how many coincident events
DT_shower_Flows[bracket$start <= StartTime & StartTime <= bracket$end,][order(StartTime)] 

# look to see what's happening
t1="1999-11-08 21:10:00"
t2="1999-11-08 21:25:00"

DT_shower_Flows[study==s & KEYCODE==k & logging==l &  
                t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_water(s, l, k, DT=DT_shower_interval4, t1, t2)
plot_shower_only(s, l, k, DT=DT_shower_Flows, t1, t2, save.charts=TRUE) 
# hot offset (delayed) by ~15 secs?
# probably one shower, shower starts hot only, then adds cold?
# with something else at 21:21?

# now try a different shower
# ============================
test.eID = 2474
DT_shower_Flows[EventID==test.eID,]

# bracket the start and end of that event
bracket <- DT_shower_Flows[EventID==test.eID, list(start=min(StartTime),end=max(StartTime))]
bracket
#                  start                 end
# 1: 1999-11-07 10:43:34 1999-11-07 10:49:54

# find how many coincident events
DT_shower_Flows[study==s & KEYCODE==k & logging==l &  
                bracket$start <= StartTime & StartTime <= bracket$end,][order(StartTime)] 
# just one shower

# look to see what's happening
t1="1999-11-07 10:43:00"
t2="1999-11-07 10:52:00"

DT_shower_Flows[study==s & KEYCODE==k & logging==l &  
                t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_water(s, l, k, DT=DT_shower_interval4, t1, t2)
plot_shower_only(s, l, k, DT=DT_shower_Flows, t1, t2, save.charts=TRUE) 
# shower starts cold only, then adds hot?
# hot delayed by about 30 seconds?
# funny way to turn it off.

# look at data from very start 
DT_shower_Flows[study==s & KEYCODE==k & logging==l &  
                "1999-11-07 10:43:00" <= StartTime & StartTime <= "1999-11-07 10:45:00",
                 list(StartTime,Rate,meter)][order(StartTime)]

# look at starts and ends of showers for this slk
DT_shower_Flows[study==s & KEYCODE==k & logging==l  
                  ,list(start = min(StartTime),
                        end   = max(StartTime),
                        mtr   = unique(meter)), 
                by=EventID][order(start)][,list(start,end,mtr)]

# at beginning looks like hot's starting with ~6 sec lag, ~ 16-26 sec lag
t1="1999-10-26 10:00:50"
t2="1999-10-26 10:02:55"
plot_water(s, l, k, DT=DT_shower_interval4, t1, t2)
plot_shower_only(s, l, k, DT=DT_shower_Flows, t1, t2) 

# at end looks like hot's starting with ~6 sec lag, ~ 16-26 sec lag
t1="1999-11-09 19:39:50"
t2="1999-11-09 19:43:25"
plot_water(s, l, k, DT=DT_shower_interval4, t1, t2)
plot_shower_only(s, l, k, DT=DT_shower_Flows, t1, t2, save.charts=TRUE) 
# the tail of total water shower was classified as something else.
# would line up better if it was recognized as tail of shower.


