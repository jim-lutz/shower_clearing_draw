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



