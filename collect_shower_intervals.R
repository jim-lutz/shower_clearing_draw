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

# now see how many hot/total pairs
DT_shower_meter <- DT_shower_interval4[, list(count=max(nshower)), by = c("study", "logging", "meter", "KEYCODE")][order(KEYCODE)]
dcast(DT_shower_meter, study + logging + KEYCODE ~ meter )[order(KEYCODE)]

# total number of showers
nrow(DT_shower_interval4)
# 2245
# not too bad 
2245*8*10
# [1] 179600
# should be ~ 200k records, actually it's
DT_shower_interval4[,list(nrec = sum(DURATION)/10)]
# nrec
# 1: 99426

# want to have the following fields
# study logging meter KEYCODE  

# get shower_intervals for one slk
s='Seattle' 
l=2 
k=13197 

# get the filename of the total water interval data
tw_file <- DT[study==s & KEYCODE==k & logging==l & meter=='total water',list(tdw_file=unique(tdb_file))]

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
DT_edges <- DT[tdb_file==tw_file, list(START,END, all=1)]
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

# add shower flag
DT_flows[,shower:=FALSE]

# loop through records one at a time, assume first record is not a shower
for (i in 2:nrow(DT_flows)){
  this_edge = DT_flows[i,edge]
  prev_edge = DT_flows[i-1,edge]
  prev_shower = DT_flows[i-1,shower]

  if(prev_edge=='END') {
    DT_flows[i, shower:=FALSE]
    next()
  }
  if(this_edge=='NO') {
    DT_flows[i, shower:=prev_shower]
    next()
  }
  
  if(this_edge=='START') {
    DT_flows[i, shower:=TRUE]
  }
  
  if(this_edge=='END') {
    DT_flows[i, shower:=TRUE]
  }
  
}

# that was slow!
# see if it worked

DT_flows
DT_flows[shower==TRUE,]
DT_flows[shower==TRUE,][1:100]
DT_flows[edge!='NO']

# sometimes duplicate records with same time in DT_tw_flows?

