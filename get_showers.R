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

this_database <- fn_Seattle_db
this_table <- db_table

add_table <- function(this_database, this_table) {
  # function to get the showers from a table and count number of coincident draws
  # this_database is filename to database
  # this_table is name of table to retrieve
  
  # get the study from this_database
  if(str_detect(this_database, "EBMUD")) {study<-"EBMUD"} 
  if(str_detect(this_database, "Seattle")) {study<-"Seattle"} 
  length(study) # squawk if study not found

  # get logging and meter from this_table 
  logging <- str_sub(this_table, start = 14, end = 14)
  if(str_detect(this_table, "Hot Water")) {meter<-"hot water"} 
  if(!str_detect(this_table, "Hot Water")) {meter<-"total water"} 

  # get the table
  DT_table <- get_table(this_database, this_table)
  # DT_table
  # str(DT_table)

  # modifications to make things easier later
  # add an eventID to make tracking things easier
  DT_table[,eventID:=.I]

  # drop time from date
  DT_table[,DATE:=str_sub(DATE,1,10)]

  # list of shower eventIDs 
  l_showerID <- DT_table[USETYPE=='SHOWER',eventID]

  # the number of events coincident to every shower event in the logging table
  DT_coincid_showers <- data.table(ldply(.data=l_showerID,
                                         .fun =coincident.events, 
                                         .progress= "text", 
                                         .inform=TRUE))
  
  # drop a temporary variable
  DT_table[, coincident:=NULL]

  # merge with DT_table, only keep showers 
  setkey(DT_table, eventID)
  setkey(DT_coincid_showers, eventID)
  DT_showers <- merge(DT_coincid_showers,DT_table)

  # add study, logging and meter
  DT_showers[, `:=`(study   = study,
                    logging = logging,
                    meter   = meter)
             ]
  
  # remove eventID, only used within one table
  DT_showers[, eventID:=NULL]
  
  # reorder column names
  setcolorder(DT_showers, c("study", "logging", "meter", "KEYCODE",
                            "USETYPE", "ncoincid", "DATE", "START", "DURATION", "END", 
                            "PEAK", "VOLUME", "MODE", "MODE NO"
                            ))
  
  # return the data.table
  return(DT_showers)

}

# test add_table function
this_database <- fn_Seattle_db
this_table <- db_table


add_table(fn_Seattle_db, db_table)
  
  
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
