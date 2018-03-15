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
l_sklm <- DT_shower_interval4[,list(sklm=unique(sklm))][order(sklm)][[1]]

# get shower_intervals for one slkm
l_sklm[27]


collect.showers <- function(this_sklm, DT=DT_shower_interval4) {
  # collects shower Flows data for one sklm
  # sklm  = string consisting of study_logging_KEYCODE_meter
  # DT    = data.table DT_shower_interval4 with sklm field already added
  
  # recover the sklm's, there's got to be a more elegant way to do this
  DT_sklm <- DT_shower_interval4[sklm==l_sklm[27]][1,list(study,logging,KEYCODE,meter)]
  s = DT_sklm$study
  l = DT_sklm$logging
  k = DT_sklm$KEYCODE
  m = DT_sklm$meter
  
  # get the Flows as a data.table with Name field added
  DT_Flows <- get.Names(s, l, k, m, DT)
  
  # add identifying fields to every record
  DT_Flows[,`:=`(study   = s,
                 KEYCODE = k,
                 logging = l,
                 meter   = m)
           ]
  
  # keep only shower records 
  DT_shower_Flows <- DT_Flows[grep('Shower',Name),]
  
  # return the shower Flows data.table
  return(DT_shower_Flows)
  
}

# test the collect.showers function
DT_shower_Flows <- collect.showers(l_sklm[27],DT_shower_interval4)
