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
str(DT_shower_interval4)

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

plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)
# it's a clean looking shower.
# will have to find a messy one for comparison later



plot_shower_only <- function (s=study, l=logging, k=KEYCODE, DT=DT_shower_Flows, 
                         t1, t2, save.charts=FALSE) {
# for constructing function only
DT=DT_shower_Flows # remove when done
  # function to plot total and hot water flow for one slk
  # only for showers
  # s = study               - Seattle | EBMUD
  # l = logging             - 1 | 2 | 3 , the phase of the study
  # k = KEYCODE             - 5 digit integer that identifies site
  # DT=DT_shower_Flows      - information about shower interval data
  # t1                      - string ofYYYY-MM-DD hh:mm:ss for start of chart
  # t2                      - string ofYYYY-MM-DD hh:mm:ss for end of chart
  # save.charts             - logical to save charts
  # this needs to be global for this function to work
  # wd_charts = work directory for charts
  # plotting function originally from /home/jiml/HotWaterResearch/projects/CECHWT24/scripts/functions.R
  
  # restrict data to only the desired slk
  DT <- DT[study==s & KEYCODE==k & logging==l,]
  
  # get the total water Flow data 
  DT_tw_flows <- DT[meter=='total water']
  
  # get the hot water Flow data 
  DT_hw_flows <- DT[meter=='hot water']

  # set timezone, all these Aquacraft sites are in the Pacific time zone
  tz="America/Los_Angeles"
  
  # convert StartTime to posix times
  DT_tw_flows[,date.time:=ymd_hms(StartTime, tz=tz)]
  DT_hw_flows[,date.time:=ymd_hms(StartTime, tz=tz)]
  
  # get posix times from t1 & t2
  t_start = ymd_hms(t1, tz=tz)
  t_end   = ymd_hms(t2, tz=tz)
  
  # restrict the total water and hot water flows for the desired times
  DT_tw_flows <- DT_tw_flows[date.time>=t_start & date.time<=t_end, list(Rate,meter),by="date.time"]  
  DT_hw_flows <- DT_hw_flows[date.time>=t_start & date.time<=t_end, list(Rate,meter),by="date.time"]  
  
  
  
  # make a data.table of a set of seconds with 0 as value.
  DT_set.of.seconds <- data.table(date.time=seq(from=t_start, to=t_end, by=dseconds(1) ), Rate=0, meter='zero' )
  # str(DT_set.of.seconds )
  
  # merge DT_set.of.seconds, DT_tw_flows, and DT_hw_flows
  setkey(DT_set.of.seconds, date.time)
  setkey(DT_tw_flows, date.time)
  setkey(DT_hw_flows, date.time)
  DT_intervals <- merge(merge(DT_set.of.seconds, DT_tw_flows, all=TRUE)
                        , DT_hw_flows, all=TRUE)
  
  # clean up DT_intervals
  setnames(DT_intervals, old = c("Rate.x", "Rate.y", "Rate"), 
           new = c("zero", "total.water", "hot.water" ))
  DT_intervals[, `:=` (meter.x = NULL,
                       meter.y = NULL,
                       meter   = NULL)]
  
  # turn NAs to 0
  DT_intervals[is.na(total.water), total.water:=0]
  DT_intervals[is.na(hot.water)  , hot.water  :=0]
  
  summary(DT_intervals)
  # hot > total?
  
  # lag total.water across itself the subsequent 10 seconds 
  # this may cause problems when the TraceWizard intervals aren't exactly 10 seconds
  DT_intervals[, paste0("v",0:9) := shift(total.water, n = 0:9, fill = 0)]
  DT_intervals[, total.water := rowSums(.SD), .SDcols = paste0("v",0:9) ]
  DT_intervals[, paste0("v",0:9) := NULL]
  
  # lag hot.water across itself the subsequent 10 seconds 
  DT_intervals[, paste0("v",0:9) := shift(hot.water, n = 0:9, fill = 0)]
  DT_intervals[, hot.water := rowSums(.SD), .SDcols = paste0("v",0:9) ]
  DT_intervals[, paste0("v",0:9) := NULL]
  
  DT_intervals[ ymd_hms("1999-11-02 19:25:00", tz=tz)<=date.time & 
                  date.time<=ymd_hms("1999-11-02 19:26:00", tz=tz) 
                ,]
  
  # draw 1 second rectangles whenever there is total water or hot water draw
  # if it's only hot water, draw a red rectangle   
  # if it's only total water, draw a blue rectangle
  # if it's both, 
  # draw a purple rectangle where they overlap
  # draw a blue rectangle where total water is greater than hot water
  # draw a red rectangle where hot water is greater than total water  
  
  # set min and max for hot water rectangles
  DT_intervals[,hot_water.min:=0]
  DT_intervals[,hot_water.max:= hot.water]
  
  # set min and max for total water rectangles
  DT_intervals[,total_water.min:=0]
  DT_intervals[,total_water.max:=total.water]
  
  # check if total.water and hot.water ever occur during the same interval
  DT_intervals[total.water>0 & hot.water>0,list(date.time, total.water, hot.water)]
  
  # set up overlap rectangles
  DT_intervals[,overlap.max:=pmin(hot_water.max,total_water.max)]
  DT_intervals[,overlap.min:=0]
  
  # handle total_water when overlap
  # total_water.max > overlap.max, reset total_water.min 
  DT_intervals[total_water.max > overlap.max, total_water.min:=overlap.max]
  
  # total_water.max <= overlap.max, reset total_water.max 
  DT_intervals[total_water.max <= overlap.max, total_water.max:=0]
  
  # handle hot_water when overlap
  # hot_water.max > overlap.max, reset hot_water.min 
  DT_intervals[hot_water.max > overlap.max, hot_water.min:=overlap.max]
  
  # hot_water.max <= overlap.max, reset hot_water.max 
  DT_intervals[hot_water.max <= overlap.max, hot_water.max:=0]
  
  # turn this into a separate function later
  # configure breaks and labels appropriately for t1 & t2
  # calculates span in minutes
  span = as.numeric(as.duration(interval(t_start, t_end)))/60 # minutes
  # breaks = date_breaks("2 hours"), labels = date_format("%H:%M")
  # looking for approx 8 - 12 breaks across span
  if(span>0)             {dbreaks = "1 min";         dlabels = "%H:%M" ; xlabel="time"}
  if(span>10)            {dbreaks = "2 mins";        dlabels = "%H:%M" }
  if(span>30)            {dbreaks = "5 mins";        dlabels = "%H:%M" }
  if(span>60)            {dbreaks = "20 mins";       dlabels = "%H:%M" }
  if(span>180)           {dbreaks = "30 mins";       dlabels = "%H:%M" }
  if(span>360)           {dbreaks = "60 mins";       dlabels = "%H:%M" }
  if(span>720)           {dbreaks = "2 hours";       dlabels = "%H:%M" }
  if(span>(24*60))       {dbreaks = "3 hours";       dlabels = "%H:%M" }    # 1 day
  if(span>(3*24*60))     {dbreaks = "12 hours";      dlabels = "%e %Hh"; xlabel="date" } # 3 days
  if(span>(7*24*60))     {dbreaks = "1 day";         dlabels = "%e"  }      # 1 week
  if(span>(14*24*60))    {dbreaks = "1 day";         dlabels = "%b-%d" }    # 2 weeks
  if(span>(30*24*60))    {dbreaks = "3 days";        dlabels = "%b-%d" }    # 1 month
  if(span>(90*24*60))    {dbreaks = "1 week";        dlabels = "%b-%d" }    # 3 months
  if(span>(120*24*60))   {dbreaks = "2 weeks";       dlabels = "%b-%d" }    # 6 months
  if(span>(365*24*60))   {dbreaks = "2 months";      dlabels = "%b" }       # 1 year
  if(span>(2*365*24*60)) {dbreaks = "4 months";      dlabels = "%b %y" }    # 2 years
  
  
  
  
  # make blank plot
  p2 <- ggplot(data=DT_intervals ) 
  
  # set axis labels for hours
  p2 <- p2 + scale_x_datetime(limits = c(t_start, t_end), date_breaks =dbreaks, date_labels = dlabels)
  
  # set limits for y-scale
  #p2 <- p2 + scale_y_continuous(limits=c(0,5))
  p2 <- p2 + coord_cartesian(ylim = c(0.01, 5)) 
  
  # plot hot.water using pink rectangles
  p2 <- p2 + geom_rect(aes(xmin = date.time, xmax = date.time + dseconds(1), ymin = hot_water.min, ymax = hot_water.max), color="deeppink", fill="deeppink") 
  
  # plot GPM using blue rectangles
  p2 <- p2 + geom_rect(aes(xmin = date.time, xmax = date.time + dseconds(1), ymin = total_water.min, ymax = total_water.max), color="deepskyblue", fill="deepskyblue") 
  
  # plot overlap using purple rectangles
  p2 <- p2 + geom_rect(aes(xmin = date.time, xmax = date.time + dseconds(1), ymin = overlap.min, ymax = overlap.max), color="purple", fill="purple") 
  
  # labels
  p2 <- p2 + xlab(xlabel) + ylab("total[blue] / hot[pink] (GPM) ") + ggtitle(paste0("shower water flow ",k))
  
  # titles and subtitles
  plot.title = paste0("shower water flows for house ",k)
  pdate = strftime(t_start, "%a %F")
  plot.subtitle = paste0('date = ', pdate)
  p2 <- p2 + ggtitle(bquote(atop(.(plot.title),scriptstyle(.(plot.subtitle))))) 
  
  # center the title
  p2 <- p2 + theme(plot.title = element_text(hjust = 0.5))
  
  p2
  
  if(save.charts) {
    # save to (giant) png file
    ggsave(p2,path=wd_charts,file=paste0('shower_',k,'_',pdate,".png"),width=10,height=7)
    # save to (giant) pdf file
    ggsave(p2,path=wd_charts,file=paste0('shower_',k,'_',pdate,".pdf"),width=20,height=14)
    # the PDF format shows the short interval draws.
  }
  
  return(p2)
}


# look to see what's happening
t1="1999-10-31 09:13:00"
t2="1999-10-31 09:25:00"

DT_Flows[ t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)
# not your typical shower

# try a different shower
# ============================
test.eID = 2801
DT_Flows[EventID==test.eID,]

# bracket the start and end of that event
bracket <- DT_Flows[EventID==test.eID, list(start=min(StartTime),end=max(StartTime))]

# find how many coincident events
DT_Flows[bracket$start <= StartTime & StartTime <= bracket$end,][order(StartTime)] 
# overlapping showers?

# look to see what's happening
t1="1999-11-08 21:10:00"
t2="1999-11-08 21:25:00"

DT_Flows[ t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)
# hot offset (delayed) by ~15 secs?
# shower starts hot only, then adds cold?
# with something else at 21:21?
DT_Flows[ "1999-11-08 21:20:00" <= StartTime & StartTime <= "1999-11-08 21:23:00",][order(EventID,StartTime)]
# probably one shower, starting hot only for ~first minute 
# with something else at ~ 1GPM starting at 21:21:19

# now try a different shower
# ============================
test.eID = 2474
DT_Flows[EventID==test.eID,]

# bracket the start and end of that event
bracket <- DT_Flows[EventID==test.eID, list(start=min(StartTime),end=max(StartTime))]

# find how many coincident events
DT_Flows[bracket$start <= StartTime & StartTime <= bracket$end,][order(StartTime)] 
# just one shower

# look to see what's happening
t1="1999-11-07 10:43:00"
t2="1999-11-07 10:50:00"

DT_Flows[ t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)
# shower starts cold only, then adds hot?


