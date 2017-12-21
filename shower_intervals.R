# shower_intervals.R
# script to read DT_showers.RData and DT_tdb_info.RData 
# then lookup shower interval data from appropriate *.tdb file(s)
# Jim Lutz "Mon Dec 18 16:33:58 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_showers.RData
load(file = paste0(wd_data,"DT_showers.RData"))

# load DT_tdb_info.RData
load(file = paste0(wd_data,"DT_tdb_info.RData"))

# see how many SLMKs (study, logging, meter, KEYCODE) in DT_showers
DT_showers[,list(KEYCODE = unique(KEYCODE)),by = c("study", "logging", "meter")][order(KEYCODE)]
# 259

# see how many SLMKs (study, logging, meter, KEYCODE) in DT_tdb_info
DT_tdb_info[,list(KEYCODE = unique(KEYCODE)),by = c("study", "logging", "meter")][order(KEYCODE)]
# 115 
# Why don't they match?
# There's 123 *.tdb files, missing some *.tdb files?
DT_tdb_info[,list(KEYCODE = unique(KEYCODE)),by = c("study", "logging", "meter", "tdb_name")][order(KEYCODE)]
# 123

# see what's there
tables()
str(DT_showers[,list(study,logging,meter,KEYCODE)])
str(DT_tdb_info[,list(study,logging,meter,KEYCODE)])

# make column types consistent
DT_showers[,logging:=as.numeric(logging)]
DT_tdb_info[,KEYCODE:=as.integer(KEYCODE)]

# merge showers onto the interval data in *.tdb files by meter & KEYCODE, allow duplicates
DT_shower_interval <- merge(DT_tdb_info,DT_showers, by = c("meter", "KEYCODE"), allow.cartesian=TRUE)
nrow(DT_shower_interval)
# [1] 8105

# keep only showers that end before and start after the interval data does
names(DT_shower_interval)
DT_shower_interval2 <- DT_shower_interval[first.time<=START & END<=last.time]
nrow(DT_shower_interval2)
# [1] 2478, same as before




# check if have hot and total water for all the showers
# add shower number by study, logging, meter, KEYCODE, sorted by START
setkey(DT_shower_interval2, START)
DT_shower_interval2[, nshower := seq_len(.N), by = c("study", "logging", "meter", "KEYCODE")]
str(DT_shower_interval2)
DT_shower_interval2
setkey(DT_shower_interval2, study, logging, KEYCODE)

# look at what's there
DT_shower_interval2[,sort(unique(KEYCODE))]
DT_shower_interval2[KEYCODE == 22048, list(KEYCODE, study, logging, meter, nshower, START, END, DURATION)][order(study, logging, nshower)]
# seems OK

# see how many hot/total pairs
DT_shower_meter <- DT_shower_interval2[, list(count=max(nshower)), by = c("study", "logging", "meter", "KEYCODE")][order(KEYCODE)]
# they're close, but not identical

# this is easier to see
dcast(DT_shower_meter, study + logging + KEYCODE ~ meter )[order(KEYCODE)]

# clean up DT_shower_interval2 and save
names(DT_shower_interval2)
DT_shower_interval2[,`:=` (first.time = NULL,
                           last.time  = NULL,
                           tdb_file   = this_tdb,
                           this_tdb   = NULL)
                    ]

# save DT_shower_interval2
save(DT_shower_interval2, file = paste0(wd_data,"DT_shower_interval2.RData"))
