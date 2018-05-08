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
plot_shower_id(i=413, 
               DT_sum=DT_summary, 
               DT_flows=DT_shower_Flows)


# load DT_shower_interval4.RData, 
load(file = paste0(wd_data,"DT_shower_interval4.RData"))

# look at all shower water
plot_water(s="Seattle", l=2, k=13219, DT=DT_shower_interval4, 
           t1="2000-04-05 19:00:00", t2="2000-04-05 19:15:00" )








# loop through every shower
for(i in 1:nrow(DT_summary)) { # actual loop
# for(i in 1:2 ) { #short loop for debugging only 
# i = 2 #  for development only
   
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
  cat('\r',sprintf("shower.id = %4i, study=%7s, KEYCODE=%5i, logging=%d, meter=%11s, EventID=%4i",
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
  
  # fill in these if .start.showering.time was calculated
  if(!is.na(.start.showering.time)) {
    
    # get the volumes, (gallons)
    .vol.clearing  <- DT_1shower[date.time < shower$time, sum(Rate)/6]
    .vol.showering <- DT_1shower[date.time >= shower$time, sum(Rate)/6]
    
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
                   vol.total            = .vol.total,
                   dur.total            = .dur.total)
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
