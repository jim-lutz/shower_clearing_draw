# get_showers.R
# script to get shower data from all the 'LOGGING DATA '{1|2|3}{|Hot Water} tables in
# ./data/Aquacraft/EBMUD/EBMUD Retrofit Database.mdb
# Jim Lutz "Tue Dec  5 06:17:35 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# open 'LOGGING DATA 1' from EBMUD Retrofit Database.mdb
# build the file name to the database
fn_database <- paste0(wd_data,"Aquacraft/EBMUD/EBMUD Retrofit Database.mdb")
# the table to get from the database
db_table <- "LOGGING DATA 1"

# get the table
DT_table <- get_table(fn_database, db_table)
DT_table
str(DT_table)

# some modifications to make things easier later
# add an eventID to make tracking things easier
DT_table[,eventID:=.I]

# drop time from date
DT_table[,DATE:=str_sub(DATE,1,10)]


# look at KEYCODEs (houses)
DT_table[,list(count=length(unique(DATE))),by=KEYCODE][order(-count)]
# 33 KEYCODEs of ~ 2 weeks @, except ~ 4 weeks 22009, 22070, 22021

# look at USETYPEs
DT_table[,list(count=length((DATE))),by=USETYPE][order(-count)]
  #            USETYPE count
  #  1:           LEAK 75761
  #  2:         FAUCET 21633
  #  3:         TOILET  5438
  #  4:  CLOTHESWASHER  1431
  #  5:         SHOWER   775
  #  6:     DISHWASHER   541
  #  7: CLOTHESWASHER@   404
  #  8:        TOILET@   353
  #  9:        OUTDOOR   236
  # 10:           BATH   150
  # 11:    DISHWASHER@   145
  # 12:     IRRIGATION    51
  # 13:        UNKNOWN    31
  # 14:         Toilet     4
  # 15:         HOTTUB     1

# look at SHOWERs
DT_table[USETYPE=='SHOWER',list(nshower=length(USETYPE)),by=KEYCODE][order(-nshower)]
# looks plausible

# count number concurrent events during showers
# try one house for now
DT_table[KEYCODE=='22070' & USETYPE=='SHOWER']
DT_table[KEYCODE=='22070' & USETYPE=='SHOWER',list(nshower=length(USETYPE)),by=DATE]

DT_table[KEYCODE=='22070' & USETYPE=='SHOWER' & DATE=='2001-05-04']

# try one house and one day for now
DT_table_test <- DT_table[KEYCODE=='22070' & DATE=='2001-05-04']

DT_table_test[]
# 268 events that day
DT_table_test[USETYPE=='SHOWER']

# pull out start and end of a shower
shower_start <- DT_table_test[eventID=='81720']$START
  # [1] "2001-05-04 06:33:55"
shower_end   <- DT_table_test[eventID=='81720']$END
  # [1] "2001-05-04 06:53:15"
# ~ 20 minute shower

DT_table_test[eventID != '81720' & END > shower_start]
# 247 events ended after the shower started

DT_table_test[eventID != '81720' & START < shower_end]
# 20 events started before the shower ended

DT_table_test[eventID != '81720' & END > shower_start & START < shower_end]
# but no events overlapped that shower

n_overlap <- nrow(DT_table_test[eventID != '81720' & END > shower_start & START < shower_end])

# try other showers
l_showerID <- DT_table_test[USETYPE=='SHOWER',eventID]
shower_start <- DT_table_test[eventID %in% l_showerID,START]
shower_end   <- DT_table_test[eventID %in% l_showerID]$END

n = 1
nrow(DT_table_test[eventID != l_showerID[n] & END > shower_start[n] & START < shower_end[n]])
  # [1] 0
n = 2
nrow(DT_table_test[eventID != l_showerID[n] & END > shower_start[n] & START < shower_end[n]])
  # [1] 0
n = 3
nrow(DT_table_test[eventID != l_showerID[n] & END > shower_start[n] & START < shower_end[n]])
  # [1] 0
n = 4
nrow(DT_table_test[eventID != l_showerID[n] & END > shower_start[n] & START < shower_end[n]])
  # [1] 1
# found one

DT_table_test[eventID %in% (81865:81870)]
# it was a toilet flush