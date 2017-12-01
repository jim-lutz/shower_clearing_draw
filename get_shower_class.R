# get_shower_class.R
# script to find class number for USETYPE = SHOWER from different Aquacraft datafiles
# Jim Lutz "Fri Dec  1 07:19:31 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")


# get the 'LOGGING DATA 1' table from 'EBMUD Retrofit Database.mdb'
# the database
fn_EBMUD <- paste0("'",wd_data,"Aquacraft/EBMUD/EBMUD Retrofit Database.mdb","'")
# the table to get from the database
logging_table <- "'LOGGING DATA 1'"
# the args for mdb-export 
mdb_args <- c(fn_EBMUD, logging_table)

# call mdb-export on the EBMUD Retrofit Database.mdb
# see: Liberating data from Microsoft Access “.mdb” files
# http://mazamascience.com/WorkingWithData/?p=168
EBMUD <- (system2("mdb-export", args = mdb_args, stdout = TRUE))

# loop through all the lines, except the first one, in the list form of the table from the database
DT_EBMUD_LOG1 <- ldply(.data=EBMUD[-1], .fun =read_log_line, .progress= "text", .inform=TRUE)
data.table(DT_EBMUD_LOG1)

