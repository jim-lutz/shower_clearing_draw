# get_data_table.R
# script to test a function to get data tables from different Aquacraft datafiles
# Jim Lutz "Mon Dec  4 07:25:22 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

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
  mdb_args <- c(fn_database, db_table)
  
  # call mdb-export, output to table.csv
  system2("mdb-export", args = mdb_args, stdout = "table.csv")
  
  # load the output from the temporary file
  DT_table <- fread("table.csv")
  
  # delete the temporary table file
  unlink("table.csv")
  
  return(DT_table)
}

# get the 'LOGGING DATA 1' table from 'EBMUD Retrofit Database.mdb'
# build the file name to the database
fn_database <- paste0(wd_data,"Aquacraft/EBMUD/EBMUD Retrofit Database.mdb")
# the table to get from the database
db_table <- "LOGGING DATA 1"

DT_table <- get_table(fn_database, db_table)

str(DT_table)




