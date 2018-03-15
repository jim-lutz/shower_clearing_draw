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

# make a list of sklm's
l_sklm <- unique(DT_shower_interval4[,sklm])

# get shower_intervals for one slkm
l_sklm[27]

# make a data.table of all the shower interval data
DT_shower_Flows <- data.table(ldply(.data=l_sklm[22:24],
                                       .fun =collect.showers, DT_shower_interval4,
                                       .progress= "text", 
                                       .inform=TRUE))

# check if it worked
l_sklm[22:24]
# [1] "Seattle_13266_2_hot water"   "Seattle_13197_2_total water"
# [3] "Seattle_13197_2_hot water"  

# study?
DT_shower_Flows[,list(nrec=length(ID)),by=study]
#      study nrec
# 1: Seattle 4029

# logging?
DT_shower_Flows[,list(nrec=length(ID)),by=logging]
#    logging nrec
# 1: Seattle 4744

# KEYCODE?
DT_shower_Flows[,list(nrec=length(ID)),by=KEYCODE]
#    KEYCODE nrec
# 1:   13266 1214
# 2:   13197 3530

# meter
DT_shower_Flows[,list(nrec=length(ID)),by=meter]
#          meter nrec
# 1:   hot water 2913
# 2: total water 1831

# number of showers
DT_shower_Flows[,list(nshowers=length(unique(EventID))),
                by=c("study","logging","KEYCODE","meter")]
#      study logging KEYCODE       meter nshowers
# 1: Seattle       2   13266   hot water       25
# 2: Seattle       2   13197 total water       30
# 3: Seattle       2   13197   hot water       29
