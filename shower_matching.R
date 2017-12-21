# shower_matching.R
# script to find hot and total water for showers
# Jim Lutz "Thu Dec 21 08:45:49 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_showers.RData
load(file = paste0(wd_data,"DT_shower_interval2.RData"))

# see how many hot/total pairs
DT_shower_meter <- DT_shower_interval2[, list(count=max(nshower)), by = c("study", "logging", "meter", "KEYCODE")][order(KEYCODE)]
# they're close, but not identical

# this is easier to see
dcast(DT_shower_meter, study + logging + KEYCODE ~ meter )[order(KEYCODE)]

# examine showers for Seattle 1 13197
DT_shower_interval2[study == "Seattle" &
                    logging == 1 &
                    KEYCODE == 13197, list(meter, DATE, START, END) ][order(START)]

names(DT_shower_interval2)
DT_shower_interval2[study == "Seattle" &
                      logging == 1 &
                      KEYCODE == 13197, list(meter, first.time, START, END) ][order(START)]
# some of the missing showers are because they started 
# the hot water meter a couple days after they started the total water meter
# on this one, the hot and total showers end within +- a minute of each other.
# may not need to worry about adjusting times

# find latest first.time and earlist last.time by SLK
DT_bracket_times <-
  DT_shower_interval2[,list(latest.first.time = max(first.time),
                            earlist.last.time = min(last.time)),
                      by = list(study, logging, KEYCODE)]

# merge onto shower data
DT_shower_interval3 <-
  merge(DT_shower_interval2,DT_bracket_times, by = c("study", "logging", "KEYCODE") )

# now keep only showers within the bracket times
DT_shower_interval4 <-
  DT_shower_interval3[latest.first.time<=START & END<=earlist.last.time]

# recount the shower number by study, logging, meter, KEYCODE, sorted by START
setkey(DT_shower_interval4, START)
DT_shower_interval4[, nshower := seq_len(.N), by = c("study", "logging", "meter", "KEYCODE")]

# now see how many hot/total pairs
DT_shower_meter <- DT_shower_interval4[, list(count=max(nshower)), by = c("study", "logging", "meter", "KEYCODE")][order(KEYCODE)]
# they're close, but still not identical

# this is easier to see
dcast(DT_shower_meter, study + logging + KEYCODE ~ meter )[order(KEYCODE)]

# examine one case
DT_shower_interval4[study == "Seattle" &
                      logging == 2 &
                      KEYCODE == 13219, list(meter, nshower, START, END, VOLUME) ][order(START)]
# probably going to have to make charts of interval data 
# to see what's happening on these ones
# seems like some of the hot water showers should be strung together
# hot and total clocks off by ~10 minutes?

# save DT_shower_interval4
save(DT_shower_interval4, file = paste0(wd_data,"DT_shower_interval4.RData"))
