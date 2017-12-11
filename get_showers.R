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

# test showerID
this_eventID <- l_showerID[4]

DF <- coincident.events(l_showerID[4])

# call coincident.events on every shower event
DT_coincid_showers <- data.table(ldply(.data=l_showerID,
                                       .fun =coincident.events, 
                                       .progress= "text", 
                                       .inform=TRUE))

# look at frequency of coincident draws with showers
qplot(DT_coincid_showers$ncoincid)
summary(DT_coincid_showers$ncoincid)
# -1? 1102?
DT_coincid_showers[ncoincid==-1,]
  #    eventID ncoincid
  # 1:   66257       -1
  # 2:  101068       -1
DT_table[eventID == 66257]
  # END = 1899-12-30 15:13:46
DT_table[eventID == 101068]
  # END = 1899-12-30 14:23:37

DT_coincid_showers[ncoincid==1102,]
  #    eventID ncoincid
  # 1:   67305     1102
DT_table[eventID == 67305]
  # START = 1899-12-30 16:05:06

# looks like there's a date formatting problem somewhere.
