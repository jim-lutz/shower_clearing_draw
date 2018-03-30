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

# load DT_shower_Flows.RData, this is the shower only interval data
# for plot_shower_only()
load(file = paste0(wd_data,"DT_shower_Flows.RData"))
str(DT_shower_Flows)

# try one skl while building graphing function plot_shower_only
s='Seattle'; k=13431; l=1; m='total water'

# from view_shower_intervals.R develop on a good shower
t1="1999-10-27 17:40:00"
t2="1999-10-27 17:47:00"

# take a look and save plot 
plot1 <- plot_shower_only(s, l, k, DT=DT_shower_Flows, t1, t2, save.charts = TRUE) 

# look at the plot
plot1
# it's an archetypical shower

# get data for 'total water' for this shower
DT_1shower <-
DT_shower_Flows[study==s & KEYCODE==k & logging==l & meter==m &
                  t1 <= StartTime & StartTime <= t2,]

# only StartTime & Rate, drop sklm and Name
DT_1shower <- DT_1shower[,list(StartTime,Rate)]

# get seconds from start as numbers

# set timezone, all these Aquacraft sites are in the Pacific time zone
tz="America/Los_Angeles"

# convert StartTime to posix times
DT_1shower[,date.time:=ymd_hms(StartTime, tz=tz)]

# initial time
start.time <- DT_1shower$date.time[1]

# seconds since start of shower at start of each interval
DT_1shower[, dsec:=as.numeric(as.duration(interval(start.time, date.time)))]

# drop times now
# DT_1shower[, `:=`(StartTime = NULL,
#                   date.time = NULL)
#            ]

# create a new column, Yxx, for every row with value of
# average of Rate up to that time as clearing draw , 
# average of Rate after that time as showering draw 

# number of intervals in data.table
nint = nrow(DT_1shower)
# [1] 35

# build a new column for every interval
# each column will consist of two parts
# the average flow rate up to and including that interval (clearing draw)
# then the average flow rate during all subsequent intervals (showering draw)
# this is used to test which interval best starts the showering draw

# this for the <= intervals (clearing draw)
for (r in 1:nint) {  # do this for each row
  set(DT_1shower,                          # modify data.table DT_1shower
      i = 1:r,                             # apply to the first r rows
      j = paste0('Y',r),                   # make the column names
      value = mean(DT_1shower$Rate[1:r])   # average of Rate for the first r rows
      )
}
str(DT_1shower)
ncol(DT_1shower)
# [1] 39

DT_1shower[1:5, 1:10]
DT_1shower[(nint-5):nint, (nint-5):(nint+4)]
# seems to have worked

# now for the subsequent intervals (showering draw)
for (r in 2:nint) {  # do this for each row, except the first
  set(DT_1shower,                            # modify data.table DT_1shower
      i = r:nint,                            # apply to the last r-1 rows
      j = paste0('Y',(r-1)),                 # make the column names, only Y1 - Y34
      value = mean(DT_1shower$Rate[r:nint])  # average of Rate for remaining rows after r
  )
}
DT_1shower[1:5, 1:10]
DT_1shower[(nint-5):nint, (nint-5):(nint+4)]
# seems to have worked

# initialize mae
mae <- rep(NA, nint)

# build an array of maes between each Yxx and Rate
for (r in 1:nint) {  # do this for each row
  yn = paste0('Y',r)  # name of Y column to use
  mae[r] <- DT_1shower[, mean(abs(Rate-get(yn)))]  # mean absolute err to reduce impact of outliers?
}

# look at maes
mae

# the index of the best fit
i <- which.min(mae)
# [1] 3

# the mae of the best fit
mae[i]
# [1] 0.3175952

# find R1, predicted Rate for clearing draw
R1 <- DT_1shower[1,get(paste0('Y',i))]

# find R2, predicted Rate for showering draw
R2 <- DT_1shower[.N,get(paste0('Y',i))]

# schematic times
s1 <- DT_1shower[1,date.time] # this is the start of clearing draw
s2 <- DT_1shower[i,date.time] + dseconds(10) # this is the transition from clearing to showering, at end of interval
s3 <- DT_1shower[.N,date.time] # this is the end of the showering

plot1

x <- c(s1,s1,s2,s2,s3,s3)
y <- c(0,R1,R1,R2,R2,0)
l <- data.frame(x,y)

# draw black lines on plot1
plot3 <- plot1 + geom_line(data = l, aes(x=x,y=y))
# it's not as good a predictor in this case?


# save demo plot
ggsave(plot3,path=wd_charts,file=paste0("shower__demo3.png"),
       width=10,height=7)





