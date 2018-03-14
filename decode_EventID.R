# decode_EventID.R
# script to develop function to add Name to Flows
# Jim Lutz "Wed Mar 14 15:03:01 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_interval4.RData
load(file = paste0(wd_data,"DT_shower_interval4.RData"))
DT_shower_interval4
names(DT_shower_interval4)

# get shower_intervals for one slkm
s='Seattle'; k=13431; l=1; m='total water'

# look for some interesting showers
DT_shower_interval4[study   == s &
                    KEYCODE == k &
                    logging == l & 
                    meter   == m, 
                    list(ncoincid,START, END, DURATION)][order(-ncoincid)]
#    ncoincid               START                 END DURATION
# 1:        2 1999-10-30 13:12:14 1999-10-30 13:19:24      430
# 2:        1 1999-10-27 20:09:24 1999-10-27 20:20:14      650
# 3:        1 1999-10-28 18:52:54 1999-10-28 18:57:24      270
# 4:        1 1999-10-28 20:25:14 1999-10-28 20:30:44      330
# 5:        1 1999-11-04 14:49:44 1999-11-04 14:51:24      100
# 6:        1 1999-11-07 16:25:04 1999-11-07 16:30:54      350
# 7:        1 1999-11-08 12:56:04 1999-11-08 12:59:04      180

# # get all the tables in a fn_database
# # call mdb-tables, output to table.csv
# system2("mdb-tables", args = c('-1', paste0("'",fn_database,"'")), stdout = "table.csv")
# 
# # load the output from the temporary file as a list
# l_tables <- fread("table.csv", header = FALSE)[[1]]
# 
# # delete the temporary table file
# unlink("table.csv")
# 
# l_tables
#   # [1] "Fixtures"        "Parameters"      "VirtualFixtures" "Events"         
#   # [5] "Flows"           "EventFixtures"  

get.Names <- function (s=study, l=logging, k=KEYCODE, m=meter, DT=DT_shower_interval4){
  # function to get Flows from one sklm.tdb
  # add Name to each interval then
  # return it as a data.table
  # s = study               - Seattle | EBMUD
  # l = logging             - 1 | 2 | 3 , the phase of the study
  # k = KEYCODE             - 5 digit integer that identifies site
  # m = meter               - hot water | total water
  # DT=DT_shower_interval4  - information about shower interval data

  # get the .tdb filename
  fn_database = DT_shower_interval4[study   == s & 
                                      KEYCODE == k &
                                      logging == l & 
                                      meter   == m, unique(tdb_file)]
  
  # get only Fixtures, EventFixtures, and Flows as datatables, this is rather ugly but appears to work
  for(tb in c('Fixtures', 'EventFixtures', 'Flows')) {
    eval(
      parse(text=
          paste0('DT_', tb, ' <- get_table(fn_database, db_table = ',"'",tb,"')")
      )
    )
  }

  # merge Name from Fixtures into EventFixtures
  DT_EventFixtures <- merge(DT_EventFixtures,DT_Fixtures[,list(ID,Name)], by.x = 'IDFixture', by.y = 'ID')
  
  # drop IDFixture
  DT_EventFixtures[,IDFixture:=NULL]
  
  # merge Name from DT_EventFixtures into Flows
  DT_Flows <- merge(DT_Flows, DT_EventFixtures[,list(IDEvent,Name)], by.x = 'EventID', by.y = 'IDEvent' )

  # return the modified Flows data.table
  return(DT_Flows)
}

# test the function
study='Seattle'
logging=1
KEYCODE=13431 
meter='total water'



DT_Flows <- get.Names(s=study, l=logging, k=KEYCODE, m=meter, DT=DT_shower_interval4)
  
  
# find shower intervals in Flows
DT_Flows[grep('Shower',Name),]
# 1687 shower intervals

DT_Flows[grep('Shower',Name),list(dur=length(ID)/6),by=EventID][order(-dur)]
# 60 showers, lot of short ones here
#     EventID        dur
#  1:     381 10.8333333
#  2:    2802 10.6666667
#  3:    2801  9.3333333
#  4:     769  9.1666667
#  5:    1084  8.6666667
#  6:    1405  8.0000000
#  7:     922  7.1666667
#  8:     891  6.6666667
#  9:    2474  6.5000000
# 10:    1146  6.1666667
# 11:     153  6.0000000
# 12:    1538  6.0000000
# 13:    2018  6.0000000

# number of events and intervals by Name
DT_Flows[, list(nevents = length(unique(EventID)),
                nrec    = length(ID)
                ), 
         by=Name][order(-nevents)]
  #                Name nevents nrec
  #  1:          Leak 1    1273 8778
  #  2:        Faucet 1    1213 5109
  #  3:        Toilet 1     335 1743
  #  4:        Shower 1      60 1687
  #  5: Clotheswasher 2      53  156
  #  6:       Bathtub 1      40  843
  #  7:        Toilet @      37  287
  #  8: Clotheswasher 1      35  624
  #  9: Clotheswasher @      28  598
  # 10:         UNKNOWN       4    5
  # 11:         OUTDOOR       2    8

# check out one of the showers
# ============================
test.eID = 1084
DT_Flows[EventID==test.eID,]

# bracket the start and end of that event
bracket <- DT_Flows[EventID==test.eID, list(start=min(StartTime),end=max(StartTime))]

# find how many coincident events
DT_Flows[bracket$start <= StartTime & StartTime <= bracket$end,][order(StartTime)] 
# overlapping showers?

# look to see what's happening
t1="1999-10-31 09:13:00"
t2="1999-10-31 09:25:00"

DT_Flows[ t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)
# not your typical shower

# try a different shower
# ============================
test.eID = 2801
DT_Flows[EventID==test.eID,]

# bracket the start and end of that event
bracket <- DT_Flows[EventID==test.eID, list(start=min(StartTime),end=max(StartTime))]

# find how many coincident events
DT_Flows[bracket$start <= StartTime & StartTime <= bracket$end,][order(StartTime)] 
# overlapping showers?

# look to see what's happening
t1="1999-11-08 21:10:00"
t2="1999-11-08 21:25:00"

DT_Flows[ t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)
# hot offset (delayed) by ~15 secs?
# shower starts hot only, then adds cold?
# with something else at 21:21?
DT_Flows[ "1999-11-08 21:20:00" <= StartTime & StartTime <= "1999-11-08 21:23:00",][order(EventID,StartTime)]
# probably one shower, starting hot only for ~first minute 
# with something else at ~ 1GPM starting at 21:21:19

# now try a different shower
# ============================
test.eID = 2474
DT_Flows[EventID==test.eID,]

# bracket the start and end of that event
bracket <- DT_Flows[EventID==test.eID, list(start=min(StartTime),end=max(StartTime))]

# find how many coincident events
DT_Flows[bracket$start <= StartTime & StartTime <= bracket$end,][order(StartTime)] 
# just one shower

# look to see what's happening
t1="1999-11-07 10:43:00"
t2="1999-11-07 10:50:00"

DT_Flows[ t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)
# shower starts cold only, then adds hot?

