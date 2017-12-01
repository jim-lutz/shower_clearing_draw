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

str(test_EBMUD)
str(strsplit(test_EBMUD,","))
str(str_split(test_EBMUD, ","))
str(data.table(test_EBMUD))
data.table(str_split(test_EBMUD, ","))
DT_EBMUD <- data.table(strsplit(test_EBMUD, ","))

DT_EBMUD[, c("KEYCODE", "USETYPE", "DATE", "START", "DURATION", "END") := 
           list(strsplit(V1, ","))]



DT_EBMUD <- data.table(strsplit(EBMUD, ","))
                     

EBMUD2 <- read.csv(textConnection(EBMUD))
str(EBMUD2)

