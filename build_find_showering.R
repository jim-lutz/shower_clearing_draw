# build_find_showering.R
# script to function to find showering time from shower-only interval data
# Jim Lutz "Fri Mar 16 19:35:44 2018"

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

# try one sklm while building function find_showering()
s='Seattle'; k=13431; l=1; m='total water'

# from view_shower_intervals.R develop on a good shower
t1="1999-10-27 17:40:00"
t2="1999-10-27 17:47:00"

# get data for 'total water' for this shower
DT_1shower <-
DT_shower_Flows[study==s & KEYCODE==k & logging==l & meter==m &
                  t1 <= StartTime & StartTime <= t2,]

# only one EventID, see if missed any of that EventID
identical(DT_shower_Flows[EventID==332],DT_1shower)
# [1] TRUE

# having trouble after find_showering
DT_1shower.copy <- copy(DT_1shower)

# take a look at the plot total water only
plot1 <- plot_shower_only(s, l, k, DT=DT_1shower, t1, t2) 
plot1

# for testing function
#DT = DT_1shower.copy

# test the find_showering function
start <- find_showering(DT=DT_1shower.copy)

str(DT_1shower)
# set timezone, all these Aquacraft sites are in the Pacific time zone
tz="America/Los_Angeles"

# convert StartTime to posix times
DT_1shower[,date.time:=ymd_hms(StartTime, tz=tz)]

# find Rclearing, predicted average Rate for clearing draw
Rclearing <- mean(DT_1shower[date.time<=start$time,Rate])

# find Rshowering, predicted Rate for showering draw
Rshowering <- mean(DT_1shower[date.time>start$time,Rate])

# schematic times
s1 <- DT_1shower[1,date.time] # this is the start of clearing draw
s2 <- start$time # this is the transition from clearing to showering, at end of interval
s3 <- DT_1shower[.N,date.time] # this is the end of the showering

x <- c(s1,s1,s2,s2,s3,s3)
y <- c(0,Rclearing,Rclearing,Rshowering,Rshowering,0)
l <- data.frame(x,y)

# remove date.time?
DT_1shower[,date.time:=NULL]

# draw black lines on plot1
plot2 <- plot1 + geom_vline(xintercept = start$time)
plot2

# save demo plot
ggsave(plot2,path=wd_charts,file=paste0("shower__demo2.png"),
       width=10,height=7)





