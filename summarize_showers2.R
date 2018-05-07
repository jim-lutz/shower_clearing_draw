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

# loop through every shower
# for(i in 1:nrow(DT_summary)) { # actual loop
# for(i in 1:2 ) { #short loop for debugging only 
i = 2 #  for development only
   
  # remove temporary objects if they exist
  if(exists("DT_sklm")==TRUE) {
   # data.tables
    rm("DT_sklm", "DT_1shower")
    # lists
    rm("shower")
    # values
    rm(".start.draw.time", ".end.draw.time",
       ".vol.clearing",    ".vol.showering",
       ".dur.clearing",    ".dur.showering",
       ".flow.clearing",   ".flow.showering")
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
   
  # find the start of showering time
  .start.showering.time <- find_showering2(DT_1shower)
  
  # skip the rest if .start.showering.time not calculated
  if(!is.na(.start.showering.time)) {
  
  # get the volumes, gallons
  .vol.clearing  <- DT_1shower[date.time < shower$time, sum(Rate)/6]
  .vol.showering <- DT_1shower[date.time >= shower$time, sum(Rate)/6]
   
  # get durations, minutes
  .dur.clearing  <- as.numeric(difftime(shower$time, .start.draw.time, units = "mins"),
                              units = "mins")
  .dur.showering <- as.numeric(difftime(.end.draw.time, shower$time, units = "mins"),
                              units = "mins")

  # get average flow rates, GPM
  .flow.clearing  <- .vol.clearing / .dur.clearing
  .flow.showering <- .vol.showering / .dur.showering

  # add calculated values to DT_summary
  DT_summary[shower.id == i,
             `:=` (start.draw.time      = .start.draw.time,
                   start.showering.time = .start.showering.time,
                   end.draw.time        = .end.draw.time,
                   vol.total            = .vol.total,
                   vol.clearing         = .vol.clearing,
                   vol.showering        = .vol.showering,
                   dur.total            = .dur.total,
                   dur.clearing         = .dur.clearing,
                   dur.showering        = .dur.showering,
                   flow.clearing        = .flow.clearing,
                   flow.showering       = .flow.showering)
             ]
  } else {
    # add calculated values to DT_summary
    DT_summary[shower.id == i,
               `:=` (start.draw.time      = .start.draw.time,
                     start.showering.time = .start.showering.time,
                     end.draw.time        = .end.draw.time,
                     vol.total            = .vol.total,
                     vol.clearing         = NA,
                     vol.showering        = NA,
                     dur.total            = .dur.total,
                     dur.clearing         = NA,
                     dur.showering        = NA,
                     flow.clearing        = NA,
                     flow.showering       = NA)
               ]
    
  }

}

# save DT_summary
save(DT_summary, file = paste0(wd_data,"DT_summary.RData"))


# There were 50 or more warnings (use warnings() to see the first 50)
# > warnings()
# Warning messages:
# 1: In rm("shower") : object 'shower' not found
# 2: In rm(".start.draw.time", ".end.draw.time", ".vol.clearing",  ... :
#   object '.start.draw.time' not found
# 3: In rm(".start.draw.time", ".end.draw.time", ".vol.clearing",  ... :
#   object '.end.draw.time' not found

View(DT_summary)

# look at shower.id ==368
DT_summary[shower.id == 368,]
# RMSE:flow.showering NA

# look at flow data
DT_shower_Flows[DT_summary[shower.id == 368,],
                on = c("study", 
                       "KEYCODE", 
                       "logging", 
                       "meter", 
                       "EventID")
                ]
# is only 2 records.

# see how many
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
