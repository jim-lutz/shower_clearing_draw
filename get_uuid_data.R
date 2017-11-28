# get_uuid_data.R
# script to collect all the uuid information in
# /home/jiml/HotWaterResearch/projects/HWDS monitoring/retrieve_field_data/data/by_sensorID
# makes one long data.table of uuid, time, value by houseID
# Jim Lutz "Sat Nov 25 13:06:14 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# call useful functions
source("functions.R")

# path to mote files
wd_mote_data = "/home/jiml/HotWaterResearch/projects/HWDS monitoring/retrieve_field_data/data/by_sensorID/"

# load the DT_field_data_uuid data.table
load(file = paste0(wd_data,"DT_field_data_uuid.RData"))

# see what we got
tables()
# NAME                NROW NCOL MB COLS                                  KEY
# [1,] DT_field_data_uuid 2,278    5  1 uuid,houseID,moteID,sensortype,source    
# Total: 1MB
DT_field_data_uuid

# count of motes by houseID
DT_field_data_uuid[,list(nmotes=length(unique(moteID))),by=houseID][order(houseID)]

# get the moteIDs and uuids for a houseID
DT_field_data_uuid[houseID==3,]
# 106 uuids
sort(unique(DT_field_data_uuid[houseID==3,]$moteID))
  #  [1] "x323f" "x3288" "x32f3" "x32ff" "x331f" "x332c" "x3352" "x3356" "x339e" "x33ed"
  # [11] "x3443" "x3497" "x34c5" "x358e" "x35b1" "x35b4"

# build lists of filenames by houseID
DT_field_data_uuid[,mote_fn := paste0(wd_mote_data,"RSmap.",moteID,".raw.xz.RData")]

# confirm that uuid filenames aren't in moteID filenames.
DT_field_data_uuid[uuid=="5f6b1e82-6227-5b6c-b191-db2b69b428c7",]

# get the moteID and uuid filenames by houseID
fn_motes <- Sys.glob(paste0(wd_mote_data,"RSmap.x*.RData"))



# get data from all the moteIDs
DT_uuids_data <- get_DT_uuids_data(fn_motes)
# Error: cannot allocate vector of size 1.3 Gb
# Error in system(paste(which, shQuote(names[i])), intern = TRUE, ignore.stderr = TRUE) : 
#   cannot popen '/usr/bin/which 'svn' 2>/dev/null', probable reason 'Cannot allocate memory'

# Well that's not going to work!
