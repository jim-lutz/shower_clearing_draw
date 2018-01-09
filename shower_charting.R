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

# now see how many hot/total pairs
DT_shower_meter <- DT_shower_interval4[, list(count=max(nshower)), by = c("study", "logging", "meter", "KEYCODE")][order(KEYCODE)]

# this is easier to see
dcast(DT_shower_meter, study + logging + KEYCODE ~ meter )[order(KEYCODE)]




# plotting function originally from /home/jiml/HotWaterResearch/projects/CECHWT24/scripts/functions.R
plot_kWGPM <- function (s=study, l=logging, k=KEYCODE, DT=DT_shower_interval4, 
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
  # these need to be global for this function to work
  # wd_charts = work directory for charts
  
  # str(DT_info)
  
  # get timezone of siteID
  tz=as.character(DT_info[siteID==s,]$tz)
  
  # get posix times from unit_date
  start = ymd_hms(t1, tz=tz)
  end   = ymd_hms(t2, tz=tz)
  
  # configure breaks and labels
  span = as.numeric(as.duration(new_interval(start, end)))/60 # minutes
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
  
  # get the kW and GPM data for the desired times
  DT_subset_data <- DT_data[date_time>=start & date_time<=end, list(kW=hpwhW/1000,GPM=Flow),by="date_time"]  
  
  # make a data.table of a set of minutes with 0 as value.
  DT_set.of.minutes <- data.table(date_time=seq(from=start, to=end, by=dminutes(1) ), zero=0 )
  # str(DT_set.of.minutes )
  setkey(DT_set.of.minutes,date_time)
  
  # merge sample with day of minutes
  # to deal with missing records, if any
  DT_set.of.data <- merge(DT_set.of.minutes,DT_subset_data,all.x=TRUE)
  
  # str(DT_set.of.data)
  
  # turn NA GPM & kW to zero
  summary(DT_set.of.data$GPM)
  DT_set.of.data$GPM[is.na(DT_set.of.data$GPM)] <- 0
  summary(DT_set.of.data$GPM)
  
  summary(DT_set.of.data$kW)
  DT_set.of.data$kW[is.na(DT_set.of.data$kW)] <- 0
  summary(DT_set.of.data$kW)
  
  # set max and min for kW rectangles
  DT_set.of.data[,kW.max:=kW]
  DT_set.of.data[,kW.min:=0]
  
  # set max and min for GPM rectangles
  DT_set.of.data[,GPM.max:=GPM]
  DT_set.of.data[,GPM.min:=0]
  
  # set up overlap rectangles
  DT_set.of.data[,overlap.max:=pmin(kW.max,GPM.max)]
  DT_set.of.data[,overlap.min:=0]
  
  # handle GPM when overlap
  # GPM > overlap.max, reset GPM.min 
  DT_set.of.data[GPM > overlap.max, GPM.min:=overlap.max]
  
  # GPM <= overlap.max, reset GPM.max 
  DT_set.of.data[GPM <= overlap.max, GPM.max:=0]
  
  # handle kW when overlap
  # kW > overlap.max, reset kW.min 
  DT_set.of.data[kW > overlap.max, kW.min:=overlap.max]
  
  # kW <= overlap.max, reset kW.max 
  DT_set.of.data[kW <= overlap.max, kW.max:=0]
  
  
  # make blank plot
  p2 <- ggplot(data=DT_set.of.data ) 
  
  # set axis labels for hours
  p2 <- p2 + scale_x_datetime(limits = c(start, end), breaks = date_breaks(width=dbreaks), labels = date_format(dlabels))
  
  # set limits for y-scale
  #p2 <- p2 + scale_y_continuous(limits=c(0,5))
  p2 <- p2 + coord_cartesian(ylim = c(0.01, 5)) 
  
  # plot kW using pink rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = kW.min, ymax = kW.max), color="deeppink", fill="deeppink") 
  
  # plot GPM using blue rectangles
  p2 <- p2 + geom_rect(aes(xmin = date_time, xmax = date_time + dminutes(1), ymin = GPM.min, ymax = GPM.max), color="deepskyblue", fill="deepskyblue") 
  
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
