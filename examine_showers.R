# examine_showers.R
# script to examine information in DT_summary.RData
# Jim Lutz "Thu May  3 08:05:40 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_Flows.RData, this is the shower only interval data
load(file = paste0(wd_data,"DT_shower_Flows.RData"))

# load DT_summary.RData, this is the shower summary data
load(file = paste0(wd_data,"DT_summary.RData"))
str(DT_summary)

summary(DT_summary$RMSE)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00000 0.08471 0.16273 0.21492 0.29007 1.62256      28 
# max is > 1 ?
# 28 NA's

summary(DT_summary$start.draw)
#                  Min.               1st Qu.                Median 
# "1999-10-25 13:40:49" "2000-04-01 22:00:02" "2000-08-05 10:17:43" 
#                  Mean               3rd Qu.                  Max. 
# "2001-01-02 21:23:13" "2001-08-19 12:28:27" "2002-06-14 08:30:09" 
#                  NA's 
#                  "28" 

# look at distributions
names(DT_summary)

# drop the ones with missing RMSEs
DT_p.summary <- DT_summary[!is.na(RMSE), 
                           list(logging,vol.clearing, vol.showering,
                                dur.clearing, dur.showering,
                                flow.clearing, flow.showering)]

# look at scatterplot matrix of showering and clearing variables
p.spm_shower_clearing <-
  ggpairs(DT_p.summary[ , L := as.factor(logging)], 
          aes(colour = L,
              alpha = 0.4),
          columns = 2:7,
          axisLabels="internal",
          title = "showering and clearing variables",
          lower = list(continuous = "points", combo = "dot_no_facet")
  )

ggsave(filename = paste0(wd_charts,"/spm_shower_clearing.png"), 
       plot = p.spm_shower_clearing,
       width = 10.5, height = 9.8)

rm(DT_p.summary)


# distribution plots of RMSE
p.RMSE <- ggplot(data = DT_summary[!is.na(RMSE)] )
p.RMSE <- p.RMSE + geom_histogram( aes( x = RMSE ),
                                   binwidth = .02,
                                   center = .01)
p.RMSE <- p.RMSE + 
  ggtitle("RMS error of of fitting 2-step draw pattern to shower interval data")
p.RMSE

ggsave(filename = paste0(wd_charts,"/RMSE_shower_fitting.png"), 
       plot = p.RMSE,
       width = 10.5, height = 9.8)

# look at plots of some of these showers.
DT_summary[RMSE==0, list(shower.id)]
# 21 showers

# build function to plot shower given shower.id
i = 161

plot_shower_id <- function (i=shower.id, 
                            DT_sum=DT_summary, 
                            DT_flows=DT_shower_Flows, 
                            save.charts=FALSE,
                            wdc=wd_charts
                            ) {
  # function to plot water flow for one slkmE 
  # similar to plot_shower() & plot_water(), but plots showers only
  # i                        - shower.id from DT_summary
  # DT_sum=DT_summary        - summary information about each shower
  # DT_flows=DT_shower_Flows - shower interval data
  # save.charts              - logical to save charts
  # wdc=wd_charts            - work directory for charts
  
  # get the sklmE for 1 shower as a data.table
  DT_sklmE <- DT_summary[shower.id == i, 
                        list(shower.id, study, KEYCODE, logging, meter, EventID)]
  
  # retrieve interval Flow data for that 1 shower
  DT_1shower <- DT_shower_Flows[DT_sklmE, 
                                on = c("study", 
                                       "KEYCODE", 
                                       "logging", 
                                       "meter", 
                                       "EventID")
                                ]
  
str(DT_1shower)

  # set timezone, all these Aquacraft sites are in the Pacific time zone
  tz="America/Los_Angeles"
  
  # convert StartTime to posix times
  DT_1shower[,date.time:=ymd_hms(StartTime, tz=tz)]
  
  # get t_start and t_end
  t_start <- DT_1shower[,list(t_start = min(date.time))]$t_start
  t_end   <- DT_1shower[,list(t_end   = max(date.time))]$t_end  
  
  # may not need any of this because only plotting 1 type of water.
  {  
  # # make a data.table of a set of seconds with 0 as value.
  # DT_set.of.seconds <- data.table(date.time=seq(from=t_start, to=t_end, by=dseconds(1) ), Rate=0, meter='zero' )
  # # str(DT_set.of.seconds )
  # 
  # # merge DT_set.of.seconds, DT_tw_flows, and DT_hw_flows
  # setkey(DT_set.of.seconds, date.time)
  # setkey(DT_tw_flows, date.time)
  # setkey(DT_hw_flows, date.time)
  # DT_intervals <- merge(merge(DT_set.of.seconds, DT_tw_flows, all=TRUE)
  #                       , DT_hw_flows, all=TRUE)
  # 
  # # clean up DT_intervals
  # setnames(DT_intervals, old = c("Rate.x", "Rate.y", "Rate"), 
  #          new = c("zero", "total.water", "hot.water" ))
  # DT_intervals[, `:=` (meter.x = NULL,
  #                      meter.y = NULL,
  #                      meter   = NULL)]
  # 
  # # turn NAs to 0
  # DT_intervals[is.na(total.water), total.water:=0]
  # DT_intervals[is.na(hot.water)  , hot.water  :=0]
  # 
  # summary(DT_intervals)
  # # hot > total?
  # 
  # # lag total.water across itself the subsequent 10 seconds 
  # # this may cause problems when the TraceWizard intervals aren't exactly 10 seconds
  # DT_intervals[, paste0("v",0:9) := shift(total.water, n = 0:9, fill = 0)]
  # DT_intervals[, total.water := rowSums(.SD), .SDcols = paste0("v",0:9) ]
  # DT_intervals[, paste0("v",0:9) := NULL]
  # 
  # # lag hot.water across itself the subsequent 10 seconds 
  # DT_intervals[, paste0("v",0:9) := shift(hot.water, n = 0:9, fill = 0)]
  # DT_intervals[, hot.water := rowSums(.SD), .SDcols = paste0("v",0:9) ]
  # DT_intervals[, paste0("v",0:9) := NULL]
  # 
  # DT_intervals[ ymd_hms("1999-11-02 19:25:00", tz=tz)<=date.time & 
  #                 date.time<=ymd_hms("1999-11-02 19:26:00", tz=tz) 
  #               ,]
  # 
  # # draw 1 second rectangles whenever there is total water or hot water draw
  # # if it's only hot water, draw a red rectangle   
  # # if it's only total water, draw a blue rectangle
  # # if it's both, 
  # # draw a purple rectangle where they overlap
  # # draw a blue rectangle where total water is greater than hot water
  # # draw a red rectangle where hot water is greater than total water  
  # 
  # # set min and max for hot water rectangles
  # DT_intervals[,hot_water.min:=0]
  # DT_intervals[,hot_water.max:= hot.water]
  # 
  # # set min and max for total water rectangles
  # DT_intervals[,total_water.min:=0]
  # DT_intervals[,total_water.max:=total.water]
  # 
  # # check if total.water and hot.water ever occur during the same interval
  # DT_intervals[total.water>0 & hot.water>0,list(date.time, total.water, hot.water)]
  # 
  # # set up overlap rectangles
  # DT_intervals[,overlap.max:=pmin(hot_water.max,total_water.max)]
  # DT_intervals[,overlap.min:=0]
  # 
  # # handle total_water when overlap
  # # total_water.max > overlap.max, reset total_water.min 
  # DT_intervals[total_water.max > overlap.max, total_water.min:=overlap.max]
  # 
  # # total_water.max <= overlap.max, reset total_water.max 
  # DT_intervals[total_water.max <= overlap.max, total_water.max:=0]
  # 
  # # handle hot_water when overlap
  # # hot_water.max > overlap.max, reset hot_water.min 
  # DT_intervals[hot_water.max > overlap.max, hot_water.min:=overlap.max]
  # 
  # # hot_water.max <= overlap.max, reset hot_water.max 
  # DT_intervals[hot_water.max <= overlap.max, hot_water.max:=0]
  } 
  
  # get x-axis breaks and labels
  span <- dspan(t_start, t_end)
  
  # make blank plot
  p2 <- ggplot(data=DT_1shower ) 
  
  # set axis labels for hours
  p2 <- p2 + scale_x_datetime(limits = c(t_start, t_end), 
                              date_breaks = span$dbreaks, 
                              date_labels = span$dlabels)
  
  # set limits for y-scale
  #p2 <- p2 + scale_y_continuous(limits=c(0,5))
  p2 <- p2 + coord_cartesian(ylim = c(0.01, 5)) 
  
  # plot water using blue rectangles
  p2 <- p2 + geom_rect(aes(xmin = date.time, 
                           xmax = date.time + dseconds(10), 
                           ymin = 0, 
                           ymax = Rate), 
                       color="deepskyblue", 
                       fill="deepskyblue") 
  
  # labels
  p2 <- p2 + xlab(paste0(span$xlabel," (hh:mm)")) + 
    ylab("water flow (GPM) ") 
  
  # titles and subtitles
  plot.title = paste0("Shower ",DT_sklmE$meter," flow for house #", DT_sklmE$KEYCODE)
  pdate = strftime(t_start, "%a %F")
  plot.subtitle = paste0('date = ', pdate)
  p2 <- p2 + ggtitle(bquote(atop(.(plot.title),scriptstyle(.(plot.subtitle))))) 
  
  # center the title
  p2 <- p2 + theme(plot.title = element_text(hjust = 0.5))
  
  p2
  
  if(save.charts) {
    # save to (giant) png file
    ggsave(p2,path=wd_charts,file=paste0('shower_only',k,'_',pdate,".png"),width=10,height=7)
    # save to (giant) pdf file
    ggsave(p2,path=wd_charts,file=paste0('shower_only',k,'_',pdate,".pdf"),width=20,height=14)
    # the PDF format shows the short interval draws.
  }
  
  return(p2)
}



















# save DT_summary
save(DT_summary, file = paste0(wd_data,"DT_summary.RData"))
