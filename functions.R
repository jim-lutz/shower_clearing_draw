# functions.R
# R functions to help with shower clearing draw analysis

read_log_line <- function(log_line = this_line){
  # function to turn one line of a logging table in an Aquacraft database into a data.table 
  # content of log_line is:
  # from terminal
  #$ mdb-schema -T 'LOGGING DATA 1' 'EBMUD Retrofit Database.mdb'
  # CREATE TABLE [LOGGING DATA 1]
  # (
  #   [KEYCODE]			Integer, 
  #   [USETYPE]			Text (20), 
  #   [DATE]			DateTime, 
  #   [START]			DateTime, 
  #   [DURATION]			Long Integer, 
  #   [END]			DateTime, 
  #   [PEAK]			Single, 
  #   [VOLUME]			Single, 
  #   [MODE]			Single, 
  #   [MODE NO]			Text (6)
  # );
  # output DT_log_line is a one line data.table
  
  #  turn the line into a list
  this_list <- unlist(strsplit(log_line, ","))
  
  # turn it into a data.table
  DT_log_line <-  data.table( KEYCODE   = this_list[1], 
                              USETYPE   = this_list[2], 
                              START     = this_list[4],
                              DURATION  = this_list[5],
                              PEAK      = this_list[7],
                              VOLUME    = this_list[8],
                              MODE      = this_list[9],
                              MODE_NO   = this_list[10] ) 
  
  return(DT_log_line)
}

get_table <- function(fn_database, db_table) {
  # function to return as a data.table a table from an access database
  # fn_database = full filename and path to database
  # db_table    = the table to be extracted from the database
  # calls mdb-export from the linux package mdbtools
  # uses a temporary file, table.csv, to hold the data
  
  # add single quotes to fn_database and db_table names
  fn_database <- paste0("'",fn_database,"'")
  db_table    <- paste0("'",db_table,"'")
  
  # the build the args list for mdb-export 
  mdb_args <- c('-D "%F %T"',fn_database, db_table)
  
  # call mdb-export, output to table.csv
  system2("mdb-export", args = mdb_args, stdout = "table.csv")
  
  # load the output from the temporary file
  DT_table <- fread("table.csv")
  
  # delete the temporary table file
  unlink("table.csv")
  
  return(DT_table)
}


coincident.events <- function(this_eventID, DT_table) {
  # function to return the number of events coincident to one event in a logging table
  # eventID is the number of the event for which coincidenet events are being counted
  # DT_table is a data.table of LOGGING DATA table from Aquacraft
  #   with an added eventID data field
  # returns a data.table of eventID and number of coincident events
  
  event_start <- DT_table[eventID == this_eventID, START]
  event_end   <- DT_table[eventID == this_eventID, END]
  
  DT_table[, coincident := FALSE] # initialize everything FALSE
  
  # find just the coincident events
  DT_table[KEYCODE == KEYCODE[this_eventID] & END > event_start & START < event_end, coincident := TRUE]             
  
  n.coincident <- nrow(DT_table[coincident==TRUE]) - 1 # don't count self event
  
  DT_ncoincid <- data.table(eventID = this_eventID, ncoincid = n.coincident)
  
  return(DT_ncoincid)
  
}


add_table <- function(this_table, this_database ) {
  # function to get the showers from a table and count number of coincident draws
  # this_table is name of table to retrieve
  # this_database is filename to database
  
  # get the study from this_database
  if(str_detect(this_database, "EBMUD")) {study<-"EBMUD"} 
  if(str_detect(this_database, "Seattle")) {study<-"Seattle"} 
  length(study) # squawk if study not found
  
  # get logging and meter from this_table 
  logging <- str_sub(this_table, start = 14, end = 14)
  if(str_detect(this_table, "Hot Water")) {meter<-"hot water"} 
  if(!str_detect(this_table, "Hot Water")) {meter<-"total water"} 
  
  # get the table
  DT_table <- get_table(this_database, this_table)
  # DT_table
  # str(DT_table)
  
  # modifications to make things easier later
  # add an eventID to make tracking things easier
  DT_table[,eventID:=.I]
  
  # drop time from date
  DT_table[,DATE:=str_sub(DATE,1,10)]
  
  # list of shower eventIDs 
  l_showerID <- DT_table[USETYPE=='SHOWER',eventID]
  
  # the number of events coincident to every shower event in the logging table
  DT_coincid_showers <- data.table(ldply(.data=l_showerID,
                                         .fun =coincident.events, 
                                         DT_table,
                                         .progress= "text", 
                                         .inform=TRUE))
  
  # drop a temporary variable
  DT_table[, coincident:=NULL]
  
  # merge with DT_table, only keep showers 
  setkey(DT_table, eventID)
  setkey(DT_coincid_showers, eventID)
  DT_showers <- merge(DT_coincid_showers,DT_table)
  
  # add study, logging and meter
  DT_showers[, `:=`(study   = study,
                    logging = logging,
                    meter   = meter)
             ]
  
  # remove eventID, only used within one table
  DT_showers[, eventID:=NULL]
  
  # reorder column names
  setcolorder(DT_showers, c("study", "logging", "meter", "KEYCODE",
                            "USETYPE", "ncoincid", "DATE", "START", "DURATION", "END", 
                            "PEAK", "VOLUME", "MODE", "MODE NO"
  ))
  
  # return the data.table
  return(DT_showers)
  
}


get.tdb.info <- function(this_tdb) {
  # returns the filename and first and last dates in a *.tdb file
  # this_tdb is the name of the *.tdb file
  
  # get the Flows table from the database
  DT_Flows <- get_table(this_tdb, db_table = 'Flows')
  
  # get first and last times
  first.time <- first(DT_Flows$StartTime)
  last.time  <- last(DT_Flows$StartTime)
  
  # add KEYCODE, study, logging, meter
  # get the KEYCODE from this_tdb
  KEYCODE <- str_extract(this_tdb,"[0-9]{5}")
  
  # get the study from this_tdb
  if(str_detect(this_tdb, "EBMUD")) {study<-"EBMUD"} 
  if(str_detect(this_tdb, "Seattle")) {study<-"Seattle"} 
  length(study) # squawk if study not found
  
  # get logging from this_tdb
  if(str_detect(this_tdb, "Pre Retrofit")) {logging<-1} 
  if(str_detect(this_tdb, "Post Retrofit 1")) {logging<-2} 
  if(str_detect(this_tdb, "Post Retrofit 2")) {logging<-3} 
  length(logging) # squawk if logging not found
  
  # get the meter from this_tdb
  if(str_detect(this_tdb, "[0-9AB]hwb*(tst)*.tdb")) {meter<-"hot water"} 
  if(str_detect(this_tdb, "[0-9AB]HW.tdb")) {meter<-"hot water"} 
  if(str_detect(this_tdb, "[0-9AB](tst)*.tdb"))   {meter<-"total water"} 
  # not sure what A or B are, additional loggings?
  length(meter) # squawk if meter not found
  
  
  # build data.table
  DT_tdb <- data.table(study, logging, meter, KEYCODE, first.time, last.time, this_tdb)
  
  return(DT_tdb)
  
}


plot_shower <- function (s=study, l=logging, k=KEYCODE, DT=DT_shower_interval4, 
                         t1, t2, save.charts=FALSE) {
  # function to plot power and water flow for one siteID
  # s = study               - Seattle | EBMUD
  # l = logging             - 1 | 2 | 3 , the phase of the study
  # k = KEYCODE             - 5 digit integer that identifies site
  # DT=DT_shower_interval4  - information about shower interval data
  # t1                      - string ofYYYY-MM-DD hh:mm:ss for start of chart
  # t2                      - string ofYYYY-MM-DD hh:mm:ss for end of chart
  # save.charts             - logical to save charts
  # this needs to be global for this function to work
  # wd_charts = work directory for charts
  # plotting function originally from /home/jiml/HotWaterResearch/projects/CECHWT24/scripts/functions.R
  
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


find.closest.hots <- function (n = nshower, DT_END=DT_1slk.END) {
  # function to find the closest next and previous hot shower ENDs 
  # to the END of one total shower for one slk
  # DT_END  is a data.table of just meter and END for  one slk.
  # n       is the total shower in question
  
  # for total showers
  DT_total.END <- DT_END[meter == 'total water', list(total.START=START, total.END=END)]
  # setkey
  setkey(DT_total.END)
  
  # for hot showers
  DT_hot.END <- DT_END[meter == 'hot water', list(hot.END=END)]
  setkey(DT_hot.END)
  
  # find start of shower n in total water.
  this.total.START = DT_total.END[n,]$total.START
  
  # find end of shower n in total water.
  this.total.END = DT_total.END[n,]$total.END
  
  # find the next closest hot.END, could be NA
  next.hot.END <- min(DT_hot.END[hot.END >= this.total.END]$hot.END)
  
  # find the prev closest hot.END, could be NA  
  prev.hot.END <- max(DT_hot.END[hot.END < this.total.END]$hot.END)
  
  # make a 1 row data.table
  DT_ENDs <- data.table(total.START      = this.total.START,
                        total.END        = this.total.END,
                        prev.hot.END     = prev.hot.END,
                        next.hot.END     = next.hot.END
  )
  return(DT_ENDs)
  
}


get.Names <- function (s=study, l=logging, k=KEYCODE, m=meter, DT=DT_shower_interval4){
  # function to get Flows from one sklm.tdb
  # add Name to each interval then
  # return it as a data.table
  # s = study               - Seattle | EBMUD
  # l = logging             - 1 | 2 | 3 , the phase of the study
  # k = KEYCODE             - 5 digit integer that identifies site
  # m = meter               - hot water | total water
  # DT=DT_shower_interval4  - information about shower interval data
  
  # get the .tdb filename
  fn_database = DT_shower_interval4[study   == s & 
                                      KEYCODE == k &
                                      logging == l & 
                                      meter   == m, unique(tdb_file)]
  
  # get only Fixtures, EventFixtures, and Flows as datatables, this is rather ugly but appears to work
  for(tb in c('Fixtures', 'EventFixtures', 'Flows')) {
    eval(
      parse(text=
              paste0('DT_', tb, ' <- get_table(fn_database, db_table = ',"'",tb,"')")
      )
    )
  }
  
  # merge Name from Fixtures into EventFixtures
  DT_EventFixtures <- merge(DT_EventFixtures,DT_Fixtures[,list(ID,Name)], by.x = 'IDFixture', by.y = 'ID')
  
  # drop IDFixture
  DT_EventFixtures[,IDFixture:=NULL]
  
  # merge Name from DT_EventFixtures into Flows
  DT_Flows <- merge(DT_Flows, DT_EventFixtures[,list(IDEvent,Name)], by.x = 'EventID', by.y = 'IDEvent' )
  
  # return the modified Flows data.table
  return(DT_Flows)
}


collect.showers <- function(this_sklm, DT=DT_shower_interval4) {
  # collects shower Flows data for one sklm
  # sklm  = string consisting of study_logging_KEYCODE_meter
  # DT    = data.table DT_shower_interval4 with sklm field already added
  
  # recover the sklm's, there's got to be a more elegant way to do this
  DT_sklm <- DT_shower_interval4[sklm==this_sklm][1,list(study,logging,KEYCODE,meter)]
  s = DT_sklm$study
  l = DT_sklm$logging
  k = DT_sklm$KEYCODE
  m = DT_sklm$meter
  
  # get the Flows as a data.table with Name field added
  DT_Flows <- get.Names(s, l, k, m, DT)
  
  # add identifying fields to every record
  DT_Flows[,`:=`(study   = s,
                 KEYCODE = k,
                 logging = l,
                 meter   = m)
           ]
  
  # keep only shower records 
  DT_shower_Flows <- DT_Flows[grep('Shower',Name),]
  
  # return the shower Flows data.table
  return(DT_shower_Flows)
  
}

