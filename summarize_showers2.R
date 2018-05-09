# summarize_showers2.R
# script to summarize information on clearing/showering draws for all showers.
# uses find_showering2
# Jim Lutz "Mon May  7 12:35:50 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_Flows.RData, this is the shower only interval data
load(file = paste0(wd_data,"DT_shower_Flows.RData"))
str(DT_shower_Flows)

# StartTime is chr in DT_shower_Flows
# add date.time in POSIXct
DT_shower_Flows[,date.time:=ymd_hms(StartTime, tz="America/Los_Angeles")]

# shower summary data, 1 record per shower
DT_summary <-
  DT_shower_Flows[,list(EventID = unique(EventID)), 
                by = c("study", "KEYCODE", "logging", "meter")
                ]

# add an shower index number to showers for ease of looping
DT_summary[,shower.id := 1:nrow(DT_summary)]

# put shower.id into DT_shower_Flows
DT_shower_Flows[DT_summary, shower.id := i.shower.id,
                on = c("study", "KEYCODE", "logging", "meter", "EventID")
                ]

# get vol.total and dur.total
DT_totals <-
DT_shower_Flows[ , list(vol.total = sum(Rate)/6,
                        dur.total = as.numeric(difftime(max(date.time),
                                                        min(date.time),
                                                        units = "mins")
                                               ) + 1/6 # include the full duration of the last interval
                        ), by=shower.id ]

# add totals to DT_summary
DT_summary[ DT_totals, `:=` (vol.total = i.vol.total,
                             dur.total = i.dur.total),
            on= "shower.id"]

# look at distribution of vol.total
p.vol.total <- ggplot(data = DT_summary[] )
p.vol.total <- p.vol.total + geom_histogram( aes( x = vol.total ),
                                             binwidth = 1,
                                             center = .5)

p.vol.total <- p.vol.total + 
  ggtitle("count of showers (both hot and total water) by volume") +
  labs(x = "total volume (gal)")
p.vol.total

ggsave(filename = paste0(wd_charts,"/vol.total.png"), 
       plot = p.vol.total,
       width = 10.5, height = 9.8)

# zoom in on small showers
p.small.vol.total <- ggplot(data = DT_summary[vol.total<5] )
p.small.vol.total <- p.small.vol.total + geom_histogram( aes( x = vol.total ),
                                             binwidth = .25,
                                             center = .125)

p.small.vol.total <- p.small.vol.total + 
  ggtitle("count of small showers (both hot and total water) by volume") +
  labs(x = "total volume (gal)")
p.small.vol.total

ggsave(filename = paste0(wd_charts,"/small.vol.total.png"), 
       plot = p.small.vol.total,
       width = 10.5, height = 9.8)

# examine the small showers, vol.total <1
DT_summary[vol.total<1][order(vol.total)]

DT_shower_Flows[shower.id==413]
plot_shower_id(i=413)

# load DT_shower_interval4.RData, 
load(file = paste0(wd_data,"DT_shower_interval4.RData"))

plot_water_id(i=413)
# not likely a shower

DT_shower_Flows[shower.id==2025]
plot_shower_id(2025)
plot_water_id(2025)
# maybe at 17:12, but not at 17:15

# examine the small showers, vol.total 1 - 2
DT_summary[1<vol.total & vol.total<2][order(vol.total)]

DT_shower_Flows[shower.id==1554]
plot_shower_id(1554)
plot_water_id(1554)
# maybe a shower with something else?

DT_shower_Flows[shower.id==1367, list(StartTime,Rate,meter)]
plot_shower_id(1367)
plot_water_id(1367)
# no there's more hot water than total water for several intervals

DT_shower_Flows[shower.id==1258]
plot_shower_id(1258)
plot_water_id(1258)
# maybe a shower with something else in the middle, not categorized correctly

# examine the small showers, vol.total 2 < 3
DT_summary[2<vol.total & vol.total<3][order(vol.total)]

# sample some of those showers
DT_shower_Flows[shower.id==1179, list(StartTime,Rate,meter)]
plot_shower_id(1179)
plot_water_id(1179)
# maybe, but something else is going on over it

DT_shower_Flows[shower.id==1260, list(StartTime,Rate,meter)]
plot_shower_id(1260)
plot_water_id(1260)
# probably is

DT_shower_Flows[shower.id==262, list(StartTime,Rate,meter)]
plot_shower_id(262)
plot_water_id(262)
# probably, one minute constant flow may not detect showering start though

DT_shower_Flows[shower.id==1633, list(StartTime,Rate,meter)]
plot_shower_id(1633)
plot_water_id(1633)
# hot yes, total has something else

DT_shower_Flows[shower.id==1485, list(StartTime,Rate,meter)]
plot_shower_id(1485)
plot_water_id(1485)
# yes

i=1485
# check Rate on plot_water_id()? it looks too high
# plot_water_id() Rate looks OK out of 
# tw_file = /home/jiml/HotWaterResearch/projects/hwds/shower_clearing_draw/data/Aquacraft/EBMUD/Pre Retrofit/22027.tdb

DT_shower_Flows[shower.id==1485, list(StartTime,Rate)]
DT_tw_flows[ymd_hms("2001-02-20 13:26:35",tz=tz)<= date.time &
            date.time <= ymd_hms("2001-02-20 13:30:45",tz=tz),
            list(date.time,Rate)]
DT_shower_Flows[shower.id==1485]
# something else is flowing then?

DT_shower_Flows[shower.id==1319, list(StartTime,Rate,meter)]
plot_shower_id(1319)
plot_water_id(1319)
# probably OK

# maybe limit to showers vol.total >2.5

# look at scatter plot of vol.total vs dur.total
p.dv.total <- ggplot(data = DT_summary[] )
p.dv.total <- p.dv.total + geom_point( aes( x = vol.total, y=dur.total ))

p.dv.total <- p.dv.total + 
  ggtitle("count of showers (both hot and total water) by volume") +
  labs(x = "total volume (gal)")
p.dv.total

ggsave(filename = paste0(wd_charts,"/durvol.total.png"), 
       plot = p.dv.total,
       width = 10.5, height = 9.8)


# zoomed in look at scatter plot of vol.total vs dur.total
p.zdv.total <- ggplot(data = DT_summary[2.5 < vol.total & 
                                          vol.total < 20 &
                                          dur.total < 10] )
p.zdv.total <- p.zdv.total + geom_jitter( aes( x = vol.total, y=dur.total),
                                              shape = "circle")
p.zdv.total

# examing short showers
DT_summary[vol.total >2.5 & dur.total < 2.5 ][order(dur.total)]

DT_shower_Flows[shower.id==1398 , list(StartTime,Rate,meter)]
plot_shower_id(1398 )
plot_water_id(1398 )
# probably not

DT_shower_Flows[shower.id==356 ]
DT_shower_Flows[shower.id==356, list(StartTime,Rate)]
plot_shower_id(356 )
# maybe
plot_water_id(356 ) # missing hot water?

DT_shower_Flows[shower.id==584, list(StartTime,Rate, meter)]
plot_shower_id(584 )
plot_water_id(584 ) 
# no hot water?

DT_shower_Flows[shower.id==97, list(StartTime,Rate, meter)]
plot_shower_id(97 )
plot_water_id(97 ) 
# something else going on, probably not shower


DT_shower_Flows[shower.id==717, list(StartTime,Rate, meter)]
plot_shower_id(717 )
plot_water_id(717 ) 
# 3.955000 2.0000000 maybe

# 311 10.128333 2.1666667
DT_shower_Flows[shower.id==311, list(StartTime,Rate, meter)]
plot_shower_id(311 )
plot_water_id(311 ) 
# maybe not?

# 2500  3.011667 2.3333333
DT_shower_Flows[shower.id==2500, list(StartTime,Rate, meter)]
plot_shower_id(2500 )
plot_water_id(2500 ) 
# probably

# stick w/ dur.total > 2.5



# examine the long showers, dur.total >30
DT_summary[dur.total >30][order(-dur.total)]


DT_shower_Flows[shower.id==2449, list(StartTime,Rate,meter)]
plot_shower_id(2449)
plot_water_id(2449)
# probably


DT_shower_Flows[shower.id==2504, list(StartTime,Rate,meter)]
plot_shower_id(2504)
plot_water_id(2504)
# hot water side of 2449


# look at scatter plot of trimmed vol.total vs dur.total
p.trmdv.total <- ggplot(data = DT_summary[dur.total > 2.5 & vol.total > 2.5] )
p.trmdv.total <- p.trmdv.total + geom_jitter( aes( x = vol.total, y=dur.total),
                                             shape = "circle")

p.trmdv.total <- p.trmdv.total + 
  ggtitle("count of showers (both hot and total water) by volume",
          subtitle = "volume > 2.5 gal & duration > 2.5 min") +
  labs(x = "total volume (gal)")
p.trmdv.total

ggsave(filename = paste0(wd_charts,"/trim.durvol.total.png"), 
       plot = p.trmdv.total,
       width = 10.5, height = 9.8)

# exclude too short & too small
DT_summary <- DT_summary[dur.total > 2.5 & vol.total > 2.5]
# from 2507 to 2256


# loop through every shower.id
# --- --- --- ---
for(i in DT_summary[,shower.id]) { # actual loop
# for(i in 3:6 ) { #short loop for debugging only 
# i = 6 #  for development only
   
  # remove temporary objects if they exist
  if(exists("DT_sklmE")==TRUE) {
   # data.tables
    rm("DT_sklmE", "DT_1shower")
    
    # values
    rm(".start.draw.time", ".end.draw.time",
       ".vol.total", ".dur.total", ".start.showering.time")
    
    # temporary values that may not exist
    if(exists(".vol.clearing")) {
      rm(".vol.clearing",    ".vol.showering",
         ".dur.clearing",    ".dur.showering",
         ".flow.clearing",   ".flow.showering")
      }
    }

  # get sklmE for 1 shower as a data.table
  DT_sklmE <- DT_summary[shower.id == i, 
                        list(shower.id, study, KEYCODE, logging, meter, EventID)]
  
  # report status 
  cat(sprintf("\rshower.id = %4i, study=%7s, KEYCODE=%5i, logging=%d, meter=%11s, EventID=%4i",
                   DT_sklmE$shower.id,
                   DT_sklmE$study,
                   DT_sklmE$KEYCODE,
                   DT_sklmE$logging,
                   DT_sklmE$meter,
                   DT_sklmE$EventID
          )
  )
  
  # retrieve interval Flow data for that 1 shower
  DT_1shower <- DT_shower_Flows[DT_sklmE, 
                                on = c("study", 
                                       "KEYCODE", 
                                       "logging", 
                                       "meter", 
                                       "EventID")
                                ]
  # calculate total volume 
  .vol.total <- sum(DT_1shower[ ]$Rate)/6
  
  # calculate total duration in minutes
  .dur.total <- as.numeric(difftime(max(DT_1shower$date.time),
                                 min(DT_1shower$date.time),
                                 units = "mins") 
                        ) + 1/6  # include the full duration of the last interval
  
  # calc start.draw and end.draw times for that shower
  .start.draw.time <- min(DT_1shower$date.time)
  .end.draw.time   <- max(DT_1shower$date.time) + dseconds(10)
  
  # add calculated values to DT_summary
  DT_summary[shower.id == i,
             `:=` (start.draw.time      = .start.draw.time,
                   end.draw.time        = .end.draw.time,
                   vol.total            = .vol.total,
                   dur.total            = .dur.total)
             ]
   
  # find the start of showering time
  .start.showering.time <- find_showering2(DT_1shower)

  # DT_shower_Flows[shower.id==6, list(StartTime,Rate,meter)]
  # plot_shower_id(6)
  # plot_water_id(6)
  # 
  
  
  # fill in these if .start.showering.time was calculated
  if(!is.na(.start.showering.time)) {
    
    # get the volumes, (gallons)
    .vol.clearing  <- DT_1shower[date.time < .start.showering.time, sum(Rate)/6]
    .vol.showering <- DT_1shower[date.time >= .start.showering.time, sum(Rate)/6]
    
    # get durations, (minutes)
    .dur.clearing  <- as.numeric(difftime(.start.showering.time, .start.draw.time, units = "mins"),
                              units = "mins")
    .dur.showering <- as.numeric(difftime(.end.draw.time, .start.showering.time, units = "mins"),
                              units = "mins")
    
    # get average flow rates, GPM
    .flow.clearing  <- .vol.clearing / .dur.clearing
    .flow.showering <- .vol.showering / .dur.showering
    
    # add calculated values to DT_summary
    DT_summary[shower.id == i,
             `:=` (start.showering.time = .start.showering.time,
                   vol.clearing         = .vol.clearing,
                   vol.showering        = .vol.showering,
                   dur.clearing         = .dur.clearing,
                   dur.showering        = .dur.showering,
                   flow.clearing        = .flow.clearing,
                   flow.showering        = .flow.showering)
             ]
  } 
}

# save DT_summary
save(DT_summary, file = paste0(wd_data,"DT_summary.RData"))


DT_summary[is.na(RMSE), 
           list(nshowers = length(shower.id)), 
           by=.(study,KEYCODE,logging,meter)] [order(KEYCODE)]
# problems w/ KEYCODE 13219
#       study KEYCODE logging       meter nshowers
# 1:  Seattle   13219       2 total water        4
# 2:  Seattle   13219       2   hot water        9
# 3:  Seattle   13219       3   hot water        4
# 4:  Seattle   13236       1   hot water        1
# 5:  Seattle   13266       3 total water        2
# 6:  Seattle   13431       1   hot water        1
# 7:    EBMUD   22009       3 total water        1
# 8:    EBMUD   22021       2   hot water        1
# 9:    EBMUD   22021       2 total water        1
# 10:   EBMUD   22037       3   hot water        1
# 11:   EBMUD   22070       1 total water        1
# 12:   EBMUD   22070       3 total water        1
# 13:   EBMUD   22075       1   hot water        1
