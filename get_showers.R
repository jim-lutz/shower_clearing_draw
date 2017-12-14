# get_showers.R
# script to get shower data from all the 'LOGGING DATA '{1|2|3}{|Hot Water} tables 
# in ./data/Aquacraft/EBMUD/EBMUD Retrofit Database.mdb and
# ./data/Aquacraft/Seattle/Seattle end use data.mdb
# Jim Lutz "Tue Dec  5 06:17:35 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# build the file names to the database
fn_EBMUD_db <- paste0(wd_data,"Aquacraft/EBMUD/EBMUD Retrofit Database.mdb")
fn_Seattle_db <- paste0(wd_data,"Aquacraft/Seattle/Seattle end use data.mdb")

# list of tables to get from the databases
l_tables <- paste0("LOGGING DATA ", c('1','2','3'))
l_tables <- c(l_tables, paste0(l_tables,' Hot Water'))

# open 'LOGGING DATA 1' from Seattle end use data.mdb
DT_showers <- add_table(l_tables[1], fn_Seattle_db)
  
# get all the LOGGING tables from Seattle end use data.mdb
DT_showers1 <- data.table(ldply(.data=l_tables,
                                .fun =add_table, 
                                fn_Seattle_db,
                                .progress= "text", 
                                .inform=TRUE))

# get all the LOGGING tables from EBMUD Retrofit Database.mdb
DT_showers2 <- data.table(ldply(.data=l_tables,
                                .fun =add_table, 
                                fn_EBMUD_db,
                                .progress= "text", 
                                .inform=TRUE))

# combine the data.tables and save
DT_showers <- rbindlist(list(DT_showers1,DT_showers2))
save(DT_showers, file = paste0(wd_data,"DT_showers.RData"))
  
  # look at frequency of coincident draws with showers
qplot(DT_showers$ncoincid)
summary(DT_showers$ncoincid)
# 1072.000?
DT_showers[ncoincid==1072.000,]
  #    START 1899-12-30 20:42:19

# looks like there's a date formatting problem somewhere.
