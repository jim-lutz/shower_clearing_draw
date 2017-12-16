# get_tdb_info.R
# script to build a data.table of information about all the Aquacraft EBMUD and Seattle tdb files
# includes study, logging, meter, first, last, and full filename
# Jim Lutz "Sat Dec 16 05:48:07 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# get a list of all the *.tdb files
l_tdbs <- list.files(path = str_sub(wd_data, 1, -2), # get rid of / at end
                     pattern = "*.tdb", 
                     recursive = TRUE, 
                     full.names = TRUE)

# try one file
this_tdb <- l_tdbs[23]

get.tdb.info <- function(this_tdb) {
  # returns the filename and first and last dates in a *.tdb file
  # this_tdb is the name of the *.tdb file
  
  # get the Flows table from the database
  DT_Flows <- get_table(this_tdb, db_table = 'Flows')

  # get first and last times
  first <- first(DT_Flows$StartTime)
  last  <- last(DT_Flows$StartTime)
  
  # add study, logging, meter
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
  if(str_detect(this_tdb, "[0-9]hw.tdb")) {meter<-"hot water"} 
  if(str_detect(this_tdb, "[0-9].tdb"))   {meter<-"total water"} 
  length(meter) # squawk if meter not found
  
  
  
  # build data.table
  DT_tdb <- data.table(study, logging, meter, first, last, this_tdb)
  
  return(DT_tdb)
  
}

get.tdb.info(this_tdb)

  