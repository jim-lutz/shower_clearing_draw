# decode_EventID.R
# script to extract shower interval data and save to one .Rdata file for later processing
# Jim Lutz "Thu Mar  1 09:20:29 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_interval4.RData
load(file = paste0(wd_data,"DT_shower_interval4.RData"))
DT_shower_interval4

# want to have the following fields
# study KEYCODE logging meter   

# make sklm identifier
DT_shower_interval4[ , sklm:= paste(study,KEYCODE,logging,meter, sep = '_')]

# how many
nrow(DT_shower_interval4[,list(unique(sklm))])
# [1] 101

# get shower_intervals for one slkm
s='EBMUD'; k=22027; l=3; m='total water'

# look for some interesting events.

# get the .tdb filename
fn_database = DT_shower_interval4[study   == s & 
                                  KEYCODE == k &
                                  logging == l & 
                                  meter   == m, unique(tdb_file)]

# get all the tables in fn_database
# call mdb-tables, output to table.csv
system2("mdb-tables", args = c('-1', paste0("'",fn_database,"'")), stdout = "table.csv")

# load the output from the temporary file
l_tables <- fread("table.csv", header = FALSE)[[1]]

# delete the temporary table file
unlink("table.csv")

l_tables
  # [1] "Fixtures"        "Parameters"      "VirtualFixtures" "Events"         
  # [5] "Flows"           "EventFixtures"  

# get all the datatables
for(tb in l_tables) {
  eval(
    parse(text=
        paste0('DT_', tb, ' <- get_table(fn_database, db_table = ',"'",tb,"')")
    )
  )
}


# load the Flow data as a data.table
DT_Flows <- get_table(fn_database, db_table = 'Flows')

names(DT_Flow)

# find duplicate records with same time in DT_Flow
DT_dup_Flow <- DT_Flow[duplicated(DT_Flow[,list(StartTime)]),][order(StartTime)]


# look at something interesting
t1="2002-06-01 11:58:00"
t2="2002-06-01 17:10:00"
save.charts=FALSE

plot_shower(s, l, k, DT=DT_shower_interval4, t1, t2,)




