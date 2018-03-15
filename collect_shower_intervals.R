# collect_shower_intervals.R
# script to extract shower interval data and save to one .Rdata file for later processing
# Jim Lutz "Thu Mar  1 09:20:29 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_interval4.RData
load(file = paste0(wd_data,"DT_shower_interval4.RData"))
DT_shower_interval4

# want to have the following fields
# study KEYCODE logging meter   

# make sklm identifier
DT_shower_interval4[ , sklm:= paste(study,KEYCODE,logging,meter, sep = '_')]

# how many sklm's
nrow(DT_shower_interval4[,list(unique(sklm))])
# [1] 101

# make
# get shower_intervals for one slkm
s='EBMUD'; l=3; k=22027; m='total water'
DT=DT_shower_interval4



# get the Flows as a data.table with Name field added
DT_Flows <- get.Names(s, l, k, m, DT)

# add identifying fields to every record
DT_Flows[,`:=`(study   = s,
               KEYCODE = k,
               logging = l,
               meter   = m)
         ]

# keep only shower records 
DT_Flows <- DT_Flows[grep('Shower',Name),]
# 560 records out of 6254 in this case
