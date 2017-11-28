# get_accdb_data.R
# script get the access database tables from 
# '/home/jiml/HotWaterResearch/projects/CECHWT24/hot water calcs/draw patterns/Aquacraft/Cal_Events.accdb'
# Jim Lutz "Mon Nov 27 17:11:02 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# filename of Cal_Events.accdb database 
db <- '/home/jiml/HotWaterResearch/projects/CECHWT24/hot water calcs/draw patterns/Aquacraft/Cal_Events.accdb'

# call access2csv.jar on the Cal_Events.accdb database 
system("cd ./data; java -jar ../access2csv.jar '/home/jiml/HotWaterResearch/projects/CECHWT24/hot water calcs/draw patterns/Aquacraft/Cal_Events.accdb'")

# get the schema
system("cd ./data; java -jar ../access2csv.jar '/home/jiml/HotWaterResearch/projects/CECHWT24/hot water calcs/draw patterns/Aquacraft/Cal_Events.accdb' --schema > schema.txt")

# read the AllCalEvents_2011.csv file
fn_events <- paste0(wd_data, "AllCalEvents_2011.csv")
cols_events <- c("Keycode", "SumAs", "CountAs", "StartTime", "Duration",
                 "Peak", "Volume", "Mode", "ModeFreq")
DT_events <- data.table(read_csv(file = fn_events, col_names = cols_events))
str(DT_events)
tables()

# change StartTime to POSIXct
# put on hold for now. have to change timezone to offset from UTC?
# DT_events[,starttime:=parse_datetime(StartTime,format="abdHMSzY")]

# save the data table
fn_DT_events <- paste0(wd_data,"DT_events.RData")
save(DT_events, file = fn_DT_events)



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
