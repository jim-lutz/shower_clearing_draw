# decode_EventID.R
# script to help decode EventID in Flows data
# Jim Lutz "Tue Mar 13 16:27:54 2018"

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

# find a shower with a few coincidents
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
                    list(ncoincid,START)][order(-ncoincid)]
#    ncoincid               START
# 1:        2 1999-10-30 13:12:14
# 2:        1 1999-10-27 20:09:24
# 3:        1 1999-10-28 18:52:54
# 4:        1 1999-10-28 20:25:14
# 5:        1 1999-11-04 14:49:44
# 6:        1 1999-11-07 16:25:04
# 7:        1 1999-11-08 12:56:04

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

# get all the datatables, this is rather ugly but appears to work
for(tb in l_tables) {
  eval(
    parse(text=
        paste0('DT_', tb, ' <- get_table(fn_database, db_table = ',"'",tb,"')")
    )
  )
}

tables()$NAME
# [1] "DT_EventFixtures"    "DT_Events"           "DT_Fixtures"        
# [4] "DT_Flows"            "DT_Parameters"       "DT_shower_interval4"
# [7] "DT_VirtualFixtures" 

tables()[,NAME]

# look at Flows
names(DT_Flows)
# [1] "ID"        "EventID"   "StartTime" "Rate" 
summary(DT_Flows$EventID)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    1     759    1578    1573    2421    3080

# look at Events
names(DT_Events)
# [1] "ID"        "StartTime" "EndTime"   "Duration"  "Class"     "Peak"      "Volume"   
# [8] "Mode"      "ModeFreq" 
nrow(DT_Events)
# [1] 3080
# same number of Events as number of EventID in Flows

# look at EventFixtures
names(DT_EventFixtures)
# [1] "ID"        "IDEvent"   "IDFixture" "Preserved"
length(DT_EventFixtures$IDEvent)
# [1] 3080
# same as EventID in Flows
# every IDEvent has an IDFixture
# how many IDFixtures
DT_EventFixtures[, list(nrecs = length(ID)), by=IDFixture][order(IDFixture)]
 #     IDFixture nrecs
 #  1:         2  1273
 #  2:         3   335
 #  3:         5    35
 #  4:         6    53
 #  5:         7    60
 #  6:         9  1213
 #  7:        10    40
 #  8:        12    37
 #  9:        13     4
 # 10:        14    28
 # 11:        16     2
# what about Preserved
DT_EventFixtures[, list(nrecs = length(ID)), by=Preserved][order(Preserved)]
#    Preserved nrecs
# 1:         0  2784
# 2:         1   296
# what's Preserved
DT_EventFixtures[Preserved==1, 
                 list(nrecs = length(ID)), 
                 by=IDFixture][order(IDFixture)]
#    IDFixture nrecs
# 1:         3    24
# 2:         5    15
# 3:         6     7
# 4:         7    21
# 5:         9   122
# 6:        10    40
# 7:        12    37
# 8:        14    28
# 9:        16     2

identical(DT_EventFixtures$ID,DT_EventFixtures$IDEvent)
# [1] TRUE


# now see about Fixtures
names(DT_Fixtures)
# [1] "ID"               "VirtualFixtureID" "Order"            "Name"            
# [5] "MinVol"           "MaxVol"           "MinPeak"          "MaxPeak"         
# [9] "MinDur"           "MaxDur"           "MinMode"          "MaxMode"         
# [13] "MinModeFreq"      "MaxModeFreq"     
DT_Fixtures
# this looks like mostly parameters to determine what type of event it is
# What about VirtualFixtureID?
# More ID in Fixtures than IDFixture in EventFixtures
nrow(DT_Fixtures)
# [1] 14

names(DT_VirtualFixtures)
[1] "ID"          "Order"       "Name"        "MinVol"      "MaxVol"      "MinPeak"    
[7] "MaxPeak"     "MinDur"      "MaxDur"      "MinMode"     "MaxMode"     "MinModeFreq"
[13] "MaxModeFreq"
DT_VirtualFixtures
nrow(DT_VirtualFixtures)
# [1] 13

DT_Fixtures[,list(ID, Name, VirtualFixtureID)]
DT_VirtualFixtures[,list(ID, Name)]

DT_Fixtures[,mvar:=VirtualFixtureID]
setkey(DT_Fixtures,mvar)
DT_VirtualFixtures[,mvar:=ID]
setkey(DT_VirtualFixtures,mvar)

tables()[NAME %in% c('DT_Fixtures','DT_VirtualFixtures')]
merge(DT_Fixtures, DT_VirtualFixtures)[,list(mvar, Name.x, Name.y)]
#     mvar          Name.x        Name.y
#  1:    1        Toilet 1        Toilet
#  2:    1        Toilet @        Toilet
#  3:    2    Dishwasher 1    Dishwasher
#  4:    2    Dishwasher @    Dishwasher
#  5:    3 Clotheswasher 1 Clotheswasher
#  6:    3 Clotheswasher 2 Clotheswasher
#  7:    3 Clotheswasher @ Clotheswasher
#  8:    4        Shower 1        Shower
#  9:    5       Bathtub 1       Bathtub
# 10:    8          Leak 1          Leak
# 11:    9    Irrigation 1    Irrigation
# 12:   10        Faucet 1        Faucet
# 13:   11         UNKNOWN         Other
# 14:   12         OUTDOOR    Humidifier
 
# seems like VirtualFixtures is more generic
# @ is per load? 
# since 'Shower 1' and 'Shower' are the same, 
# probably don't have to go all the way to VirtualFixtures

# DT_Flows$







DT_Fixtures[grep('Shower',Name),list(ID,VirtualFixtureID) ]
#    ID VirtualFixtureID
# 1:  7                4

names(DT_EventFixtures)
# [1] "ID"        "IDEvent"   "IDFixture" "Preserved"

DT_EventFixtures[,list(n=length(ID)),by=IDFixture][order(IDFixture)]
#     IDFixture    n
#  1:         2 1273
#  2:         3  335
#  3:         5   35
#  4:         6   53
#  5:         7   60
#  6:         9 1213
#  7:        10   40
#  8:        12   37
#  9:        13    4
# 10:        14   28
# 11:        16    2

DT_Flows
# 19838

# find duplicate records with same time in DT_Flow
DT_dup_Flow <- DT_Flows[duplicated(DT_Flows[,list(StartTime)]),][order(StartTime)]
# 675

# Look for something in the middle of the recording window
DT_dup_Flow[300:340,]

DT_Flows[EventID==115,]


# look at to see what's happening
t1="1999-11-03 11:25:00"
t2="1999-11-03 11:30:00"


DT_Flows[ t1 <= StartTime & StartTime <= t2,][order(StartTime,EventID)]

DT_shower_interval4[study   == s & 
                      KEYCODE == k &
                      logging == l & 
                      meter   == m &
                      t1 <= START & START <= t2, ]

plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)




