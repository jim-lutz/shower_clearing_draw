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
DT_shower_Flows <- data.table(ldply(.data=l_sklm,
                                       .fun =collect.showers, DT_shower_interval4,
                                       .progress= "text", 
                                       .inform=TRUE))

# check if it worked
l_sklm
# [1] "Seattle_13266_2_hot water"   "Seattle_13197_2_total water"
# [3] "Seattle_13197_2_hot water"  

# study?
DT_shower_Flows[,list(nrec=length(ID)),by=study]
#      study  nrec
# 1: Seattle 51922
# 2:   EBMUD 56891

# logging?
DT_shower_Flows[,list(nrec=length(ID)),by=logging]
#    logging  nrec
# 1:       1 35699
# 2:       2 32423
# 3:       3 40691

# KEYCODE?
DT_shower_Flows[,list(nrec=length(ID)),by=KEYCODE][order(KEYCODE)]
#     KEYCODE  nrec
#  1:   13197  6197
#  2:   13219  3344
#  3:   13220  7975
#  4:   13236  2053
#  5:   13266  7689
#  6:   13281  4699
#  7:   13294  4040
#  8:   13410  8010
#  9:   13431  7915
# 10:   22005  2968
# 11:   22009  2615
# 12:   22021  6929
# 13:   22023  3473
# 14:   22027  4679
# 15:   22035  3951
# 16:   22037  4162
# 17:   22048   652
# 18:   22070 14922
# 19:   22075 12540

# meter
DT_shower_Flows[,list(nrec=length(ID)),by=meter]
#          meter  nrec
# 1: total water 55419
# 2:   hot water 53394

# number of showers
DT_shower_Flows[,list(nshowers=length(unique(EventID))),
                by=c("study","logging","KEYCODE","meter")]
#        study logging KEYCODE       meter nshowers
#   1: Seattle       1   13266 total water       18
#   2: Seattle       1   13266   hot water       15
#   3: Seattle       1   13294   hot water       16
#   4: Seattle       1   13294 total water       18
#   5: Seattle       1   13431 total water       60
#  ---                                             
#  97:   EBMUD       3   22023   hot water       21
#  98:   EBMUD       3   22027 total water       17
#  99:   EBMUD       3   22027   hot water       17
# 100:   EBMUD       3   22070 total water       63
# 101:   EBMUD       3   22070   hot water       55

# Name
DT_shower_Flows[,list(nrec=length(ID)),by=Name]
#        Name   nrec
# 1: Shower 1 108813

# save DT_shower_Flows
save(DT_shower_Flows, file = paste0(wd_data,"DT_shower_Flows.RData"))

