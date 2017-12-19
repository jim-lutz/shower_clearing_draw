# get_tdb_info.R
# script to build a data.table of information about all the Aquacraft EBMUD and Seattle tdb files
# includes study, logging, meter, first.time, last.time, and full filename
# Jim Lutz "Sat Dec 16 05:48:07 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# get a list of all the *.tdb files
l_tdbs <- list.files(path = str_sub(wd_data, 1, -2), # get rid of / at end
                     pattern = "*.tdb$", 
                     recursive = TRUE, 
                     full.names = TRUE)

str_extract(l_tdbs,"[0-9]{5}")

get.tdb.info(l_tdbs[23])

# build a data.table from all the *.tdb files
DT_tdb_info <- data.table(ldply(.data=l_tdbs,
                                .fun =get.tdb.info,
                                .progress= "text", 
                                .inform=TRUE))

# r in fread("table.csv") : File is empty: table.csv
# Error: with piece 124: 
# [1] "/home/jiml/HotWaterResearch/projects/hwds/shower_clearing_draw/data/DT_tdb_info.RData"
# was trying to read DT_tdb_info.RData

DT_tdb_info[,KEYCODE]


# save the data.table
save(DT_tdb_info, file = paste0(wd_data,"DT_tdb_info.RData"))




  