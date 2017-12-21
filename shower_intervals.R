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
# 113 
# Why don't they match?
# and there's 123 *.tdb files, missing some *.tdb files?

# see what's there
tables()
str(DT_showers[,list(study,logging,meter,KEYCODE)])
str(DT_tdb_info[,list(study,logging,meter,KEYCODE)])

# make column types consistent
DT_showers[,logging:=as.numeric(logging)]
DT_tdb_info[,KEYCODE:=as.integer(KEYCODE)]

# keep only the SLMKs have *.tdb files for
DT_shower_interval <- merge(DT_tdb_info,DT_showers, by = c("study", "logging", "meter", "KEYCODE"))
# only 2779 out of 5726 showers

# check to see if showers within *.tdb times
DT_shower_interval[first.time<=START & END<=last.time,]
# 2479
# so some of the showers aren't in the interval data time frame?
DT_shower_interval[first.time>START, list(study, logging, meter, KEYCODE,
                                          first.time, last.time,
                                          START, END)
                                          ]

DT_shower_interval[END>last.time, list(study, logging, meter, KEYCODE,
                                          first.time, last.time,
                                          START, END)
                   ]
# looks like that's the case, lost another 300 showers.
DT_shower_interval2 <- DT_shower_interval[first.time<=START & END<=last.time,]

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
