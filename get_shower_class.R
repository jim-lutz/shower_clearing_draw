# get_shower_class.R
# script to find class number for USETYPE = SHOWER from different Aquacraft datafiles
# Jim Lutz "Fri Dec  1 07:19:31 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# EBMUD Retrofit Database.mdb
fn_EBMUD <- paste0("'",wd_data,"Aquacraft/EBMUD/EBMUD Retrofit Database.mdb","'")
# table to get from the database
logging_table <- "'LOGGING DATA 1'"
# mdb-export args
mdb_args <- c(fn_EBMUD, logging_table)

# call mdb-export on the EBMUD Retrofit Database.mdb
# see: Liberating data from Microsoft Access “.mdb” files
# http://mazamascience.com/WorkingWithData/?p=168
EBMUD <- (system2("mdb-export", args = mdb_args, stdout = TRUE))
str(EBMUD)

test_EBMUD <- EBMUD[1:10]


this_line = EBMUD[2]

read_log_line <- function(log_line = this_line){
  # function to turn one line of a logging table into a data.table 
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
  this_list <- unlist(strsplit(this_line, ","))

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

read_log_line(this_line)

