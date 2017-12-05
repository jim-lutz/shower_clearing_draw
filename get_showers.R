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

str(DT_table)



