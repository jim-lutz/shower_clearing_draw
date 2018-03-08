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

# how many
nrow(DT_shower_interval4[,list(unique(sklm))])
# [1] 101

# get shower_intervals for one slkm
s='EBMUD'; l=3; k=22027; m='total water'
DT=DT_shower_interval4

# make a collect_shower_intervals function
collect_shower_intervals <- function(s, k, l, m, DT) {
  # function to collect the shower interval data for one sklm
  # returns data.table of just the shower intervals
  # s = study               - Seattle | EBMUD
  # l = logging             - 1 | 2 | 3 , the phase of the study
  # k = KEYCODE             - 5 digit integer that identifies site
  # m = meter               - 'total water'|'hot water'
  # DT=DT_shower_interval4  - information about shower interval data
  
  # get the filename of the interval data
  interval_filename <- DT_shower_interval4[study==s & KEYCODE==k & logging==l & meter==m,list(tdw_file=unique(tdb_file))]
  
  # load the Flow data as a data.table
  DT_Flow <- get_table(fn_database = interval_filename, db_table = 'Flows')
  
  # add identifying fields and remove unneeded fields
  DT_Flow[,`:=`(study   = s,
                KEYCODE = k,
                logging = l,
                meter   = m,
                ID      = NULL,
                EventID = NULL)
              ]
  
  # drop records without any Rate data
  DT_Flow[is.na(Rate)]
  # these should be dropped
  
  # duplicate records with same time in DT_Flow?
  DT_dup_Flow <- DT_Flow[duplicated(DT_Flow[,list(StartTime)]),list(StartTime)][order(StartTime)]
  merge(DT_Flow, DT_dup_Flow, all.y = TRUE )
  # is this a general problem or only some of the slmks?
  # looked at 3 sklms. all 3 had duplicates
  
  # get the shower edges
  DT_edges <- DT_shower_interval4[tdb_file==interval_filename, list(START,END, all=1)]
  # all is a dummy variable for melt
  
  # melt from wide to long
  DT_edges <- melt(DT_edges, 
                   id.vars = c("all") , measure.vars = c("START","END"),
                   variable.name = "edge", value.name = "StartTime")
  
  # drop all
  DT_edges[,all:=NULL]

  # duplicate records with same time in DT_edges?
  DT_dup_edges <- DT_edges[duplicated(DT_edges[,list(StartTime)]),list(StartTime)][order(StartTime)]
  merge(DT_edges, DT_dup_edges, all.y = TRUE )
  # is this a general problem or only some of the sklms?
  
  
    
  
  
  # sort DTs on StartTime
  setkey(DT_edges, StartTime)
  setkey(DT_Flow, StartTime)
  
  # merge DT_edges onto DT_tw_flows
  DT_flows <- merge(DT_Flow,DT_edges, all=TRUE)
  
  # change missing edge to 'NO'
  DT_flows[is.na(edge), edge:='NO']
  
  # initialize shower flag
  DT_flows[,shower:=FALSE]
  
  # reorder the columns
  setcolorder(DT_flows, c('study', 'KEYCODE', 'logging', 'meter', 
                          'edge', 'shower', 'StartTime', 'Rate'))

  # number of records to check
  nrec = nrow(DT_flows)
    
  # loop through all the records
  for (i in 1:nrec){ 
    # if it's not an edge, go to next record
    if(DT_flows[i,edge]=='NO') {next()}
    
    # if START, change current and all subsequent records to TRUE
    if(DT_flows[i,edge]=='START') {
      DT_flows[i:nrec, shower:=TRUE]
    } 
    
    # if END, change all subsequent records to FALSE
    if(DT_flows[i,edge]=='END') {
      DT_flows[(i+1):nrec, shower:=FALSE]
    } 
  
  } 
  
}


