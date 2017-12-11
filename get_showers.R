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

DT <- DT_table
this_eventID <- l_showerID[4]

coincident.events <- function(DT, this_eventID) {
  # function to return the number of events coincident to one event in a logging table
  # DT is a data.table of LOGGING DATA table from Aquacraft
  #   with an added eventID data field
  # eventID is the number of the event for which coincidenet events are being counted
  # returns the number of coincident events
  
  DT[eventID == this_eventID, list(eventID,USETYPE,START,END)]
  
  DT1 <- DT[KEYCODE == KEYCODE[this_eventID]]
  
  DT2 <- DT1[eventID != this_eventID,]
  
  DT3 <- DT2[END > START[this_eventID]]
             
  DT4 <- DT3[START < END[this_eventID]]             
             
  DT4[,list(eventID,USETYPE,START,END)]

  
  
    
}

