# summarize_showers.R
# script to summarize information including clearing/showering draws for all showers.
# Jim Lutz "Fri Apr  6 18:19:01 2018"

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

# StartTime is chr in DT_shower_Flows
# add date.time as StartTime in POSIXct
DT_shower_Flows[,date.time:=ymd_hms(StartTime, tz="America/Los_Angeles")]

# how many showers?
DT_shower_Flows[,list(nshowers=length(unique(EventID))), 
                by = c("study", "KEYCODE", "logging", "meter")
                ][,list(totshowers=sum(nshowers)), by = meter]
#          meter totshowers
# 1: total water       1255
# 2:   hot water       1252

# shower summary data, 1 record per shower
DT_summary <-
  DT_shower_Flows[,list(EventID = unique(EventID)), 
                by = c("study", "KEYCODE", "logging", "meter")
                ]

# are there duplicate shower EventIDs?
sum(duplicated(DT_summary[,list(EventID)]))
# [1] 946

# are there duplicated shower EventIDs within a sklm?
sum(duplicated(DT_summary[,list(EventID),
                          by = c("study", "KEYCODE", "logging", "meter")
                          ]))
# [1] 0
# have to refer to showers by sklm & EventID

# add an shower index number to showers for ease of looping
DT_summary[,shower.id := 1:nrow(DT_summary)]

# loop through every shower
for(i in 1:nrow(DT_summary)) {
  # i in 1:2 #short loop for debugging only              
  # i in 1:nrow(DT_summary) # actual loop
  

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
  
  # get sklm for 1 shower as a data.table
  DT_sklm <- DT_summary[shower.id == i, 
                        list(shower.id, study, KEYCODE, logging, meter, EventID)]
  
  # report status 
  cat('\r',sprintf("shower.id = %4i, study=%7s, KEYCODE=%5i, logging=%d, meter=%11s, EventID=%4i",
          DT_sklm$shower.id,
          DT_sklm$study,
          DT_sklm$KEYCODE,
          DT_sklm$logging,
          DT_sklm$meter,
          DT_sklm$EventID
          )
  )
  
  
  # retrieve interval Flow data for that 1 shower
  DT_1shower <- DT_shower_Flows[DT_sklm, 
                                on = c("study", 
                                       "KEYCODE", 
                                       "logging", 
                                       "meter", 
                                       "EventID")
                                ]
  
  # if it's only 1 or 2 records then go to the next shower
  if( nrow(DT_1shower)<3 ) {next}
  
  # calc start.draw and end.draw times for that shower
  .start.draw.time <- min(DT_1shower$date.time)
  .end.draw.time   <- max(DT_1shower$date.time) + dseconds(10)
  
  # call the find_showering function to get RMSE and start of showering draw
  shower <- find_showering(DT=DT_1shower)
  
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
             `:=` (RMSE                 = shower$RMSE,
                   start.draw.time      = .start.draw.time,
                   start.showering.time = shower$time,
                   end.draw.time        = .end.draw.time,
                   vol.clearing         = .vol.clearing,
                   vol.showering        = .vol.showering,
                   dur.clearing         = .dur.clearing,
                   dur.showering        = .dur.showering,
                   flow.clearing        = .flow.clearing,
                   flow.showering       = .flow.showering)
             ]
  
}

# save DT_summary
save(DT_summary, file = paste0(wd_data,"DT_summary.RData"))
