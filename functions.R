# functions.R
# R functions to help with shower clearing draw analysis

read_log_line <- function(log_line = this_line){
  # function to turn one line of a logging table in an Aquacraft database into a data.table 
  # content of log_line is:
  # from terminal
  #$ mdb-schema -T 'LOGGING DATA 1' 'EBMUD Retrofit Database.mdb'
  # CREATE TABLE [LOGGING DATA 1]
  # (
  #   [KEYCODE]			Integer, 
  #   [USETYPE]			Text (20), 
  #   [DATE]			DateTime, 
  #   [START]			DateTime, 
  #   [DURATION]			Long Integer, 
  #   [END]			DateTime, 
  #   [PEAK]			Single, 
  #   [VOLUME]			Single, 
  #   [MODE]			Single, 
  #   [MODE NO]			Text (6)
  # );
  # output DT_log_line is a one line data.table
  
  #  turn the line into a list
  this_list <- unlist(strsplit(log_line, ","))
  
  # turn it into a data.table
  DT_log_line <-  data.table( KEYCODE   = this_list[1], 
                              USETYPE   = this_list[2], 
                              START     = this_list[4],
                              DURATION  = this_list[5],
                              PEAK      = this_list[7],
                              VOLUME    = this_list[8],
                              MODE      = this_list[9],
                              MODE_NO   = this_list[10] ) 
  
  return(DT_log_line)
}

get_table <- function(fn_database, db_table) {
  # function to return as a data.table a table from an access database
  # fn_database = full filename and path to database
  # db_table    = the table to be extracted from the database
  # calls mdb-export from the linux package mdbtools
  # uses a temporary file, table.csv, to hold the data
  
  # add single quotes to fn_database and db_table names
  fn_database <- paste0("'",fn_database,"'")
  db_table    <- paste0("'",db_table,"'")
  
  # the build the args list for mdb-export 
  mdb_args <- c('-D "%F %T"',fn_database, db_table)
  
  # call mdb-export, output to table.csv
  system2("mdb-export", args = mdb_args, stdout = "table.csv")
  
  # load the output from the temporary file
  DT_table <- fread("table.csv")
  
  # delete the temporary table file
  unlink("table.csv")
  
  return(DT_table)
}


coincident.events <- function(this_eventID, DT_table) {
  # function to return the number of events coincident to one event in a logging table
  # eventID is the number of the event for which coincidenet events are being counted
  # DT_table is a data.table of LOGGING DATA table from Aquacraft
  #   with an added eventID data field
  # returns a data.table of eventID and number of coincident events
  
  event_start <- DT_table[eventID == this_eventID, START]
  event_end   <- DT_table[eventID == this_eventID, END]
  
  DT_table[, coincident := FALSE] # initialize everything FALSE
  
  # find just the coincident events
  DT_table[KEYCODE == KEYCODE[this_eventID] & END > event_start & START < event_end, coincident := TRUE]             
  
  n.coincident <- nrow(DT_table[coincident==TRUE]) - 1 # don't count self event
  
  DT_ncoincid <- data.table(eventID = this_eventID, ncoincid = n.coincident)
  
  return(DT_ncoincid)
  
}


add_table <- function(this_table, this_database ) {
  # function to get the showers from a table and count number of coincident draws
  # this_table is name of table to retrieve
  # this_database is filename to database
  
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
                                         DT_table,
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


get.tdb.info <- function(this_tdb) {
  # returns the filename and first and last dates in a *.tdb file
  # this_tdb is the name of the *.tdb file
  
  # get the Flows table from the database
  DT_Flows <- get_table(this_tdb, db_table = 'Flows')
  
  # get first and last times
  first.time <- first(DT_Flows$StartTime)
  last.time  <- last(DT_Flows$StartTime)
  
  # add KEYCODE, study, logging, meter
  # get the KEYCODE from this_tdb
  KEYCODE <- str_extract(this_tdb,"[0-9]{5}")
  
  # get the study from this_tdb
  if(str_detect(this_tdb, "EBMUD")) {study<-"EBMUD"} 
  if(str_detect(this_tdb, "Seattle")) {study<-"Seattle"} 
  length(study) # squawk if study not found
  
  # get logging from this_tdb
  if(str_detect(this_tdb, "Pre Retrofit")) {logging<-1} 
  if(str_detect(this_tdb, "Post Retrofit 1")) {logging<-2} 
  if(str_detect(this_tdb, "Post Retrofit 2")) {logging<-3} 
  length(logging) # squawk if logging not found
  
  # get the meter from this_tdb
  if(str_detect(this_tdb, "[0-9AB]hwb*(tst)*.tdb")) {meter<-"hot water"} 
  if(str_detect(this_tdb, "[0-9AB]HW.tdb")) {meter<-"hot water"} 
  if(str_detect(this_tdb, "[0-9AB](tst)*.tdb"))   {meter<-"total water"} 
  # not sure what A or B are, additional loggings?
  length(meter) # squawk if meter not found
  
  
  # build data.table
  DT_tdb <- data.table(study, logging, meter, KEYCODE, first.time, last.time, this_tdb)
  
  return(DT_tdb)
  
}
