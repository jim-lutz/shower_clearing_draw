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

# build a data.table from all the *.tdb files
DT_tdb_info <- data.table(ldply(.data=l_tdbs,
                                .fun =get.tdb.info,
                                .progress= "text", 
                                .inform=TRUE))

# save the data.table
save(DT_tdb_info, file = paste0(wd_data,"DT_tdb_info.RData"))




  