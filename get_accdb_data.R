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
  # Classes ‘data.table’ and 'data.frame':	2168698 obs. of  9 variables:
  # $ Keycode  : int  18192 18192 18192 18192 18192 18192 18192 18192 18192 18192 ...
  # $ SumAs    : chr  "Leak" "Leak" "Leak" "Leak" ...
  # $ CountAs  : chr  "Leak" "Leak" "Leak" "Leak" ...
  # $ StartTime: chr  "Mon Jul 02 15:03:27 PDT 2007" "Mon Jul 02 15:09:37 PDT 2007" "Mon Jul 02 15:11:37 PDT 2007" "Mon Jul 02 15:31:07 PDT 2007" ...
  # $ Duration : int  360 110 1160 150 520 10 160 190 60 280 ...
  # $ Peak     : num  0.21 0.16 0.26 0.13 0.21 0.16 0.16 0.16 0.07 0.16 ...
  # $ Volume   : num  0.52 0.16 1.72 0.17 0.74 0.03 0.22 0.33 0.06 0.42 ...
  # $ Mode     : num  0.07 0.05 0.1 0.06 0.08 0.16 0.09 0.1 0.07 0.1 ...
  # $ ModeFreq : int  9 6 28 10 17 1 6 11 3 8 ...
  # - attr(*, ".internal.selfref")=<externalptr> 
tables()

# change StartTime to POSIXct
# put on hold for now. have to change timezone to offset from UTC?
# DT_events[,starttime:=parse_datetime(StartTime,format="abdHMSzY")]

# save the data table
fn_DT_events <- paste0(wd_data,"DT_events.RData")
save(DT_events, file = fn_DT_events)

