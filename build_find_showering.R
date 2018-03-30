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

# take a look and save plot 
plot1 <- plot_shower_only(s, l, k, DT=DT_shower_Flows, t1, t2, save.charts = TRUE) 

# look at the plot
plot1
# it's an archetypical shower

# get data for 'total water' for this shower
DT_1shower <-
DT_shower_Flows[study==s & KEYCODE==k & logging==l & meter==m &
                  t1 <= StartTime & StartTime <= t2,]

# only one EventID, see if missed any of that EventID
identical(DT_shower_Flows[EventID==332],DT_1shower)
# [1] TRUE

# temporary data.table used to build function
DT <- DT_1shower

find_showering <- function(DT) {
  # function to find the begining of a showering draw 
  # given the interval data for shower
  # DT = one shower extracted from DT_shower_Flows.RData
  
  # set timezone, all these Aquacraft sites are in the Pacific time zone
  tz="America/Los_Angeles"
  
  # convert StartTime to posix times
  DT[,date.time:=ymd_hms(StartTime, tz=tz)]
  
  # build a new column for every interval
  # each column will consist of two parts
  # the average flow rate up to and including that interval (clearing draw)
  # then the average flow rate during all subsequent intervals (showering draw)
  # this is used to test which interval best starts the showering draw
  nint = nrow(DT)
  
  # this for the <= intervals (clearing draw)
  for (r in 1:nint) {  # do this for each row
    set(DT,                          # modify data.table DT_1shower
        i = 1:r,                     # apply to the first r rows
        j = paste0('Y',r),           # make the column names
        value = mean(DT$Rate[1:r])   # average of Rate for the first r rows
    )
  }
  
  # now for the subsequent intervals (showering draw)
  for (r in 2:nint) {  # do this for each row, except the first
    set(DT,                            # modify data.table DT_1shower
        i = r:nint,                    # apply to the last r rows
        j = paste0('Y',(r-1)),         # make the column names
        value = mean(DT$Rate[r:nint])  # average of Rate for remaining rows after r
    )
  }
  
  # initialize rmse
  rmse <- rep(NA, nint)
  
  # build an array of rmses between each Yxx and Rate
  for (r in 1:nint) {  # do this for each row
    yn = paste0('Y',r)  # name of Y column to use
    rmse[r] <- DT[, sqrt(mean((Rate-get(yn))^2))] 
  }
  
  # the index of the best fit
  i <- which.min(rmse)
  
  # transition from clearing to showering, at end of interval
  start.shower <- DT[i,date.time] + dseconds(10)
  # as POSIXct

  return(start.shower)  

}


start <- find_showering(DT_1shower)

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
plot2 <- plot1 + geom_line(data = l, aes(x=x,y=y))

# save demo plot
ggsave(plot2,path=wd_charts,file=paste0("shower__demo.png"),
       width=10,height=7)





