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
# study logging meter KEYCODE  

# get shower_intervals for one slk
s='Seattle' 
l=2 
k=13197 

# get the filename of the total water interval data
tw_file <- DT_shower_interval4[study==s & KEYCODE==k & logging==l & meter=='total water',list(tdw_file=unique(tdb_file))]

# load the total water Flow data as a data.table
DT_tw_flows <- get_table(fn_database = tw_file, db_table = 'Flows')

# add identifying fields and remove unneeded fields
DT_tw_flows[,`:=`(study   = s,
                  KEYCODE = k,
                  logging = l,
                  meter   = 'total water',
                  ID      = NULL,
                  EventID = NULL)
            ]

# get the shower edges
DT_edges <- DT_shower_interval4[tdb_file==tw_file, list(START,END, all=1)]
# all is a dummy variable for melt

# melt from wide to long
DT_edges <- melt(DT_edges, 
                 id.vars = c("all") , measure.vars = c("START","END"),
                 variable.name = "edge", value.name = "StartTime")

# drop all
DT_edges[,all:=NULL]

# sort DTs on StartTime
setkey(DT_edges, StartTime)
setkey(DT_tw_flows, StartTime)

# merge DT_edges onto DT_tw_flows
DT_flows <- merge(DT_tw_flows,DT_edges, all=TRUE)

# change missing edge to 'NO'
DT_flows[is.na(edge), edge:='NO']

# add initial shower flag
DT_flows[,shower:=FALSE]

# 2nd copy for comparison
DT_flows2 <- DT_flows
identical(DT_flows, DT_flows2)

# time the loop
system.time({
  
  # number of records to check
  nrec = nrow(DT_flows)
  
  # instead of testing, change all subsequent records as appropriate
  for (i in 1:nrec){ #  assume first record is not a shower
    
    # if START, change current and all subsequent records to TRUE
    if(DT_flows[i,edge]=='START') {
      DT_flows[i:nrec, shower:=TRUE]
    } 
    
    # if END, change all subsequent records to FALSE
    if(DT_flows[i,edge]=='END') {
      DT_flows[(i+1):nrec, shower:=FALSE]
    } 
    
  }
  
})
#   user  system elapsed 
# 14.316   0.004  14.319 



# time the loop
system.time({
  
# number of records to check
nrec = nrow(DT_flows2)

# change all subsequent records as appropriate
for (i in 1:nrec){
  # if not edge, go to next record
  if(DT_flows2[i,edge]=='NO') {next()}
  
  # if START, change current and all subsequent records to TRUE
  if(DT_flows2[i,edge]=='START') {
    DT_flows2[i:nrec, shower:=TRUE]
  } 
  
  # if END, change all subsequent records to FALSE
  if(DT_flows2[i,edge]=='END') {
    DT_flows2[(i+1):nrec, shower:=FALSE]
  } 
  
}

})
#  user  system elapsed 
# 7.140   0.000   7.137 
# better, but still slow

identical(DT_flows, DT_flows2)
# [1] TRUE

# sometimes duplicate records with same time in DT_tw_flows?
# some StartTimes without any data?

