# shower_charting.R
# script to chart hot and total water for showers
# Jim Lutz "Mon Jan  8 15:53:17 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_interval4.RData
load(file = paste0(wd_data,"DT_shower_interval4.RData"))

DT_shower_interval4
names(DT_shower_interval4)

# now see how many hot/total pairs
DT_shower_meter <- DT_shower_interval4[, list(count=max(nshower)), by = c("study", "logging", "meter", "KEYCODE")][order(KEYCODE)]

# this is easier to see
dcast(DT_shower_meter, study + KEYCODE + logging ~ meter )[order(KEYCODE)]

# find some temporary values for testing plotting function
DT_shower_interval4[study == "Seattle" &
                      logging == 1 &
                      KEYCODE == 13197, list(meter, nshower, START, END, VOLUME, ncoincid) ][order(START)]

# try this
s='Seattle' 
l=1 
k=13197 
DT=DT_shower_interval4 
t1="1999-11-02 19:25:00"
t2="1999-11-02 19:41:00"
save.charts=FALSE




# plotting function originally from /home/jiml/HotWaterResearch/projects/CECHWT24/scripts/functions.R
plot_shower <- function (s=study, l=logging, k=KEYCODE, DT=DT_shower_interval4, 
                        t1, t2, save.charts=FALSE) {
  # function to plot power and water flow for one siteID
  # s = study               - Seattle | EBMUD
  # l = logging             - 1 | 2 | 3 , the phase of the study
  # k = KEYCODE             - 5 digit integer that identifies site
  # DT=DT_shower_interval4  - information about shower interval data
  # DT=DT_siteID            - merged data from monitoring at siteID
  # t1                      - string ofYYYY-MM-DD hh:mm:ss for start of chart
  # t2                      - string ofYYYY-MM-DD hh:mm:ss for end of chart
  # save.charts             - logical to save charts
  # this needs to be global for this function to work
  # wd_charts = work directory for charts
  
  # str(DT)

  # get the filename of the total water interval data
  tw_file <- DT[study==s & KEYCODE==k & logging==l & meter=='total water',list(tdw_file=unique(tdb_file))]
  
  # load the total water Flow data as a data.table
  DT_tw_flows <- get_table(fn_database = tw_file, db_table = 'Flows')
  
  # add meter='total water'
  DT_tw_flows[,meter:='total water']
  
  # check on duplicate interval data
  DT_tw_flows[,list(n.intevals=length(StartTime)),by=StartTime][order(-n.intevals)]
  # OK
  
  # get the filename of the hot water interval data
  hw_file <- DT[study==s & KEYCODE==k & logging==l & meter=='hot water',list(tdw_file=unique(tdb_file))]
  
  # load the total water Flow data as a data.table
  DT_hw_flows <- get_table(fn_database = hw_file, db_table = 'Flows')
  
  # add meter='hot water'
  DT_hw_flows[,meter:='hot water']

  # check on duplicate interval data
  DT_hw_flows[,list(n.intevals=length(StartTime)),by=StartTime][order(-n.intevals)]
  # OK
  
  # concatenate the flows data.tables
  DT_flows <- rbind(DT_tw_flows, DT_hw_flows)
  
  # interim clean up
  # rm(DT_tw_flows, DT_hw_flows)
  
  # set timezone, all these Aquacraft sites are in the Pacific time zone
  tz="America/Los_Angeles"
  
  # convert StartTime to posix times
  DT_flows[,date.time:=ymd_hms(StartTime, tz=tz)]
  DT_tw_flows[,date.time:=ymd_hms(StartTime, tz=tz)]
  DT_hw_flows[,date.time:=ymd_hms(StartTime, tz=tz)]
  
  # get posix times from t1 & t2
  t_start = ymd_hms(t1, tz=tz)
  t_end   = ymd_hms(t2, tz=tz)

  # restrict the total and hot water flows for the desired times
  DT_subset_flows <- DT_flows[date.time>=t_start & date.time<=t_end, list(Rate,meter),by="date.time"]  
  DT_tw_flows <- DT_tw_flows[date.time>=t_start & date.time<=t_end, list(Rate,meter),by="date.time"]  
  DT_hw_flows <- DT_hw_flows[date.time>=t_start & date.time<=t_end, list(Rate,meter),by="date.time"]  
  
  
    
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
  if(span>(3*24*60))     {dbreaks = "12 hours";       dlabels = "%e %Hh"; xlabel="date" } # 3 days
  if(span>(7*24*60))     {dbreaks = "1 day";      dlabels = "%e"  } # 1 week
  if(span>(14*24*60))    {dbreaks = "1 day";         dlabels = "%b-%d" }    # 2 weeks
  if(span>(30*24*60))    {dbreaks = "3 days";        dlabels = "%b-%d" }    # 1 month
  if(span>(90*24*60))    {dbreaks = "1 week";        dlabels = "%b-%d" }    # 3 months
  if(span>(120*24*60))   {dbreaks = "2 weeks";       dlabels = "%b-%d" }    # 6 months
  if(span>(365*24*60))   {dbreaks = "2 months";      dlabels = "%b" }       # 1 year
  if(span>(2*365*24*60)) {dbreaks = "4 months";      dlabels = "%b %y" }    # 2 years
  
  
  # make a data.table of a set of seconds with 0 as value.
  DT_set.of.seconds <- data.table(date.time=seq(from=t_start, to=t_end, by=dseconds(1) ), Rate=0, meter='zero' )
  # str(DT_set.of.seconds )
  # str(DT_subset_flows )

  # combine subset of flows with DT_set.of.seconds
  DT_tw_flows
  str(DT_tw_flows)
  
  # merge DT_set.of.seconds, DT_tw_flows, and DT_hw_flows
  setkey(DT_set.of.seconds, date.time)
  setkey(DT_tw_flows, date.time)
  setkey(DT_hw_flows, date.time)
  DT_intervals <- merge(DT_set.of.seconds, DT_tw_flows, all=TRUE)
  DT_intervals[Rate.y>0]
  DT_intervals <- merge(DT_intervals, DT_hw_flows, all=TRUE)
  
  # clean up DT_intervals
  setnames(DT_intervals, old = c("Rate.x", "Rate.y", "Rate"), 
           new = c("Rate.zero", "Rate.total", "Rate.hot" ))
  DT_intervals[, `:=` (meter.x = NULL,
                       meter.y = NULL,
                       meter   = NULL)]
  
  # turn NAs to 0
  DT_intervals[is.na(Rate.total), Rate.total:=0]
  DT_intervals[is.na(Rate.hot)  , Rate.hot  :=0]
  
  summary(DT_intervals)
  # hot > total?
  
  DT_intervals2 <- DT_intervals
  # shift(DT_intervals2[Rate.total>0], n=1:9, fill=DT_intervals[Rate.total>0]$Rate.total)
  # n.rows <- nrow(DT_intervals2)
  # DT_intervals2[1:n.rows]$Rate.total 
  # DT_intervals2[2:n.rows]$Rate.total 
  # DT_intervals2[,Rate.total1:=Rate.total[(-1)+.I] ]
  # DT_intervals2[,Rate.total1 := shift(Rate.total, n = 1, default = 0)]
  
  DT_intervals2 <- 
    DT_intervals2 %>% 
    mutate(Rate.total1 = dplyr::lag(Rate.total, n = 1, default = 0)) %>% 
    mutate(Rate.total2 = dplyr::lag(Rate.total, n = 2, default = 0)) %>% 
    mutate(Rate.total3 = dplyr::lag(Rate.total, n = 3, default = 0)) %>% 
    mutate(Rate.total4 = dplyr::lag(Rate.total, n = 4, default = 0)) %>% 
    mutate(Rate.total5 = dplyr::lag(Rate.total, n = 5, default = 0)) %>% 
    mutate(Rate.total6 = dplyr::lag(Rate.total, n = 6, default = 0)) %>% 
    mutate(Rate.total7 = dplyr::lag(Rate.total, n = 7, default = 0)) %>% 
    mutate(Rate.total8 = dplyr::lag(Rate.total, n = 8, default = 0)) %>% 
    mutate(Rate.total9 = dplyr::lag(Rate.total, n = 9, default = 0)) 

  DT_intervals3 <- data.table(DT_intervals2)
  
  # # rowSums(cbind(Rt, 
  #               c(0,Rt[1:(n.rows-1)]), 
  #               c(0,0,Rt[1:(n.rows-2)]) ))
  
  
  # DT_intervals2[Rate.total>0]
  DT_intervals3[ ymd_hms("1999-11-02 19:25:00", tz=tz)<=date.time & 
                  date.time<=ymd_hms("1999-11-02 19:26:00", tz=tz) 
                ,list(date.time,Rate.total.new,Rate.total,Rate.total1, Rate.total2, Rate.total3)]

  DT_intervals3[,Rate.total.new := Rate.total + Rate.total1 + Rate.total2 + Rate.total3]

  str(DT_intervals2)
  
  
  
  DT_set.of.seconds[DT_tw_flows, on="date.time", with=FALSE, mult="all", nomatch=NA, roll= TRUE, verbose=TRUE]
  DT_set.of.seconds[DT_tw_flows, list(Rate, meter), nomatch=0, roll= TRUE, keyby=date.time]
  
  DT_set.of.data <- rbind(DT_set.of.seconds,DT_subset_flows)
  
  # str(DT_set.of.data)
  # look at data
  # summary(DT_set.of.data)

  # recast DT_set.of.data so total water, hot water and zero 
  # are listed for each 10 second interval
  DT_intervals <- dcast(DT_set.of.data, date.time ~ meter, value.var = "Rate", 
                        fun = sum, drop = TRUE)
  summary(DT_intervals)
  str(DT_intervals)
  
  # clean up the variable names
  setnames(DT_intervals, old = c('hot water', 'total water'), new = c('hot.water', 'total.water'))
  
# trying to draw 10 second rectangles whenever there is total water or hot water draw
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
  # Empty data.table (0 rows) of 3 cols: date.time,total.water,hot.water
  # something's wrong
  
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
  
  
  # make blank plot
  p2 <- ggplot(data=DT_intervals ) 
  
  # set axis labels for hours
  p2 <- p2 + scale_x_datetime(limits = c(start, end), breaks = date_breaks(width=dbreaks), labels = date_format(dlabels))
  
  # set limits for y-scale
  #p2 <- p2 + scale_y_continuous(limits=c(0,5))
  p2 <- p2 + coord_cartesian(ylim = c(0.01, 5)) 
  
  # plot kW using pink rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = hot_water.min, ymax = hot_water.max), color="deeppink", fill="deeppink") 
  
  # plot GPM using blue rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = total_water.min, ymax = total_water.max), color="deepskyblue", fill="deepskyblue") 
  
  # plot overlap using purple rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = overlap.min, ymax = overlap.max), color="purple", fill="purple") 
  
  # labels
  p2 <- p2 + xlab(xlabel) + ylab("GPM(blue) / kW(pink)") + ggtitle(paste0("Water flow and power for Unit ",s))
  
  # titles and subtitles
  plot.title = paste0("Water flow and power for Unit ",s)
  size = DT_info[siteID==s,]$size
  brand = DT_info[siteID==s,]$brand
  pdate = strftime(start, "%F(%a)")
  plot.subtitle = paste0('date = ', pdate, ' HPWH: size = ',size, ', model = ', brand )
  p2 <- p2 + ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 
  
  p2
  
  if(save.charts) {
    # save to (giant) png file
    ggsave(p2,path=wd_charts,file=paste0(s,'_',pdate,".png"),width=10,height=7)
    # save to (giant) pdf file
    ggsave(p2,path=wd_charts,file=paste0(s,'_',pdate,".pdf"),width=20,height=14)
    # the PDF format shows the short interval draws.
  }
  
  return(p2)
}
