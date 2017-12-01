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
str(this_line)

this_list <- unlist(strsplit(this_line, ","))
str(this_list)

# try a function to turn the lines in the list into data.table one line at a time.

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


data.table( KEYCODE   = this_list[1], 
            USETYPE   = this_list[2], 
            START     = this_list[4],
            DURATION  = this_list[5],
            PEAK      = this_list[7],
            VOLUME    = this_list[8],
            MODE      = this_list[9],
            MODE_NO   = this_list[10] ) 




read_log_line <- function(log_line = , save_dir = wd_uuid_data){
  # function to save a single SMAP data stream a file in wd_uuid_data
  # l_data is one SMAP data stream
  # l_data = List of 3
  # $ time : num [1:x] 
  # $ value: num [1:x]
  # $ uuid : chr 
  # save_dir is name of directory to save the file to
  # file name is uuid.RData
  # data.table name is always just DT_uuid
  
  # get the uuid
  uuid <- l_data$uuid
  
  # make the file name
  uuid_fn = paste0(save_dir,uuid,".RData")
  
  # make the data.table
  DT_uuid <- data.table(time = l_data$time, value = l_data$value)
  
  # save the data.table
  save(DT_uuid, file = uuid_fn)
  
  # clean up the data
  rm(l_data, DT_uuid)
  
}



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

