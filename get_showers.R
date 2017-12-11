# get_showers.R
# script to get shower data from all the 'LOGGING DATA '{1|2|3}{|Hot Water} tables 
# in ./data/Aquacraft/EBMUD/EBMUD Retrofit Database.mdb and
# ./data/Aquacraft/Seattle/Seattle end use data.mdb
# Jim Lutz "Tue Dec  5 06:17:35 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# build the file names to the database
fn_EBMUD_db <- paste0(wd_data,"Aquacraft/EBMUD/EBMUD Retrofit Database.mdb")
fn_Seattle_db <- paste0(wd_data,"Aquacraft/Seattle/Seattle end use data.mdb")

# list of tables to get from the databases
l_tables <- paste0("LOGGING DATA ", c('1','2','3'))
l_tables <- c(l_tables, paste0(l_tables,' Hot Water'))

# open 'LOGGING DATA 1' from EBMUD Retrofit Database.mdb
fn_database <- fn_EBMUD_db
db_table <- l_tables[1]

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

# list of shower eventIDs (775)
l_showerID <- DT_table[USETYPE=='SHOWER',eventID]

shower_start <- DT_table[eventID %in% l_showerID,START]
shower_end   <- DT_table_test[eventID %in% l_showerID]$END

# test showerID
this_showerID <- l_showerID[4]

# number of events that coincide with a shower
DT_table[KEYCODE == KEYCODE[this_showerID]] 
  # 4168
DT_table[KEYCODE == KEYCODE[this_showerID] & eventID != this_showerID,]
  # 4167
DT_table[KEYCODE == KEYCODE[this_showerID] & eventID != this_showerID  & END > START[this_showerID],]
  # 3438
DT_table[KEYCODE == KEYCODE[this_showerID] & eventID != this_showerID  & START < END[this_showerID],]
  # 731
DT_table[KEYCODE == KEYCODE[this_showerID] & eventID != this_showerID  & END > START[this_showerID] & START < END[this_showerID],]
  # 2 faucets
DT_table[this_showerID]

n.events <- nrow(DT_table[KEYCODE == KEYCODE[this_showerID] & eventID != this_showerID  & END > START[this_showerID] & START < END[this_showerID],])
  # [1] 2
