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

# want to have the following fields
# study KEYCODE logging meter   

# make sklm identifier
DT_shower_interval4[ , sklm:= paste(study,KEYCODE,logging,meter, sep = '_')]

# how many
nrow(DT_shower_interval4[,list(unique(sklm))])
# [1] 101

# find a sklm having showers with a few coincidents
DT_shower_interval4[,list(nshower=length(USETYPE), ncoin=sum(ncoincid)),by=sklm][order(-nshower,-ncoin)][1:30]
# Seattle_13431_1_total water

# get shower_intervals for one slkm
s='Seattle'; k=13431; l=1; m='total water'

names(DT_shower_interval4)
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

# get the .tdb filename
fn_database = DT_shower_interval4[study   == s & 
                                  KEYCODE == k &
                                  logging == l & 
                                  meter   == m, unique(tdb_file)]

# get all the tables in fn_database
# call mdb-tables, output to table.csv
system2("mdb-tables", args = c('-1', paste0("'",fn_database,"'")), stdout = "table.csv")

# load the output from the temporary file as a list
l_tables <- fread("table.csv", header = FALSE)[[1]]

# delete the temporary table file
unlink("table.csv")

l_tables
  # [1] "Fixtures"        "Parameters"      "VirtualFixtures" "Events"         
  # [5] "Flows"           "EventFixtures"  

# get only Fixtures, EventFixtures, and Flows as datatables, this is rather ugly but appears to work
for(tb in c('Fixtures', 'EventFixtures', 'Flows')) {
  eval(
    parse(text=
        paste0('DT_', tb, ' <- get_table(fn_database, db_table = ',"'",tb,"')")
    )
  )
}

tables()$NAME
# [1] "DT_EventFixtures"    "DT_Fixtures"         "DT_Flows"           
# [4] "DT_shower_interval4"

# merge Name from Fixtures into EventFixtures
DT_EventFixtures <- merge(DT_EventFixtures,DT_Fixtures[,list(ID,Name)], by.x = 'IDFixture', by.y = 'ID')

# drop IDFixture
DT_EventFixtures[,IDFixture:=NULL]

# merge Name from DT_EventFixtures into Flows
DT_Flows <- merge(DT_Flows, DT_EventFixtures[,list(IDEvent,Name)], by.x = 'EventID', by.y = 'IDEvent' )

# find shower intervals in Flows
DT_Flows[grep('Shower',Name),]
# 1687 shower intervals

DT_Flows[grep('Shower',Name),list(dur=length(ID)/6),by=EventID][order(-dur)]
# 60 showers, lot of short ones here

# number of EventID by Name
DT_Flows[, list(nevents = length(unique(EventID)),
                nrec    = length(ID)
                ), 
         by=Name][order(-nevents)]
  #               Name nevents nrec
  # 1:          Leak 1    1273 8778
  # 2:        Faucet 1    1213 5109
  # 3:        Toilet 1     335 1743
  # 4:        Shower 1      60 1687
  # 5: Clotheswasher 2      53  156
  # 6:       Bathtub 1      40  843
  # 7:        Toilet @      37  287
  # 8: Clotheswasher 1      35  624
  # 9: Clotheswasher @      28  598
  # 10:         UNKNOWN       4    5
  # 11:         OUTDOOR       2    8


DT_Flows[EventID==1084,]

#    ncoincid               START                 END DURATION
# 1:        2 1999-10-30 13:12:14 1999-10-30 13:19:24      430
# 2:        1 1999-10-27 20:09:24 1999-10-27 20:20:14      650
# 3:        1 1999-10-28 18:52:54 1999-10-28 18:57:24      270
# 4:        1 1999-10-28 20:25:14 1999-10-28 20:30:44      330
# 5:        1 1999-11-04 14:49:44 1999-11-04 14:51:24      100
# 6:        1 1999-11-07 16:25:04 1999-11-07 16:30:54      350
# 7:        1 1999-11-08 12:56:04 1999-11-08 12:59:04      180

# look to see what's happening
t1="1999-10-30 13:12:14"
t2="1999-10-30 13:19:24"

DT_Flows[ t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)
# not your typical shower

# look to see what's happening
t1="1999-10-27 20:09:24"
t2="1999-10-27 20:20:14"

DT_Flows[ t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)
# at least the toilet is visible. What's happening at 20:16:30?

# look to see what's happening
t1="1999-10-28 18:52:00"
t2="1999-10-28 18:58:00"

DT_Flows[ t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]
plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)


