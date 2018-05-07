# build_find_showering2.R
# script to build 2nd function to find showering time from shower-only interval data
# assumes showers start a beginning of first constant flow minute
# Jim Lutz "Fri May  4 15:25:14 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_Flows.RData, this is the shower only interval data
load(file = paste0(wd_data,"DT_shower_Flows.RData"))
str(DT_shower_Flows)

# load DT_summary.RData, this is the shower summary data
load(file = paste0(wd_data,"DT_summary.RData"))
str(DT_summary)
names(DT_summary)
# use this for consistency with prior work, only use
# "study", "KEYCODE", "logging", "meter", "EventID", "shower.id" 

plot_shower_id(4) # this doesn't look good
plot_shower_id(11)
plot_shower_id(10) # house #13266 total
plot_shower_id(24) #
plot_shower_id(2431)
plot_shower_id(2502)

# try shower.id 2502
i = 2502

# get sklmE for 1 shower as a data.table
DT_sklm <- DT_summary[shower.id == i, 
                      list(shower.id, study, KEYCODE, logging, meter, EventID)]

# retrieve interval Flow data for that 1 shower
DT_1shower <- DT_shower_Flows[DT_sklm, 
                              on = c("study", 
                                     "KEYCODE", 
                                     "logging", 
                                     "meter", 
                                     "EventID")
                              ]
str(DT_1shower)

# for testing function
#DT = DT_1shower.copy

find_showering2 <- function(DT=DT_1shower) {
  # function to find the begining of a showering draw 
  # given the interval data for one shower
  # DT = one shower's interval data extracted from DT_shower_Flows.RData
  # returns the following list:
  # start.shower$time   time showering draw started, as POSIXct
  # start.shower$RMSE   RMSE of fitting 2-step draw pattern to interval data
  
  # make a copy of DT to avoid modifying original data.table?
  DT.copy <- copy(DT)
  
  # set timezone, all these Aquacraft sites are in the Pacific time zone
  tz="America/Los_Angeles"
  
  # add posix time version of StartTime
  DT.copy[,date.time:=ymd_hms(StartTime, tz=tz)]
  
  # find the time of the maximum flow rate
  # within the first 5 minutes?
  max.Rate.time <- DT.copy[1:30,][Rate==max(Rate), date.time]
  
  # run length count of identical Rates
  DT.copy[, nintervals := seq_len(.N), by=rleid(Rate)] 
  
  # find the time of first nintervals == 6 
  # after max.Rate.time 
  start.shower <-
  DT.copy[nintervals == 6 & max.Rate.time<date.time,
          list(start.time = min(date.time))]$start.time

  # then 50 seconds before that
  start.shower <- start.shower - dseconds(50)
  
  DT.copy[1:25,list(date.time, Rate, nintervals)]
  
  # clear up working data.table
  rm(DT.copy)
  
  return(start.shower)  
  
}



find_showering2(DT_1shower)
names(DT_1shower)
DT_1shower[,list(StartTime, Rate)]

stop


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





