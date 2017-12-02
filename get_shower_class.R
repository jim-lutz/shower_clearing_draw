# get_shower_class.R
# script to find class number for USETYPE = SHOWER from different Aquacraft datafiles
# Jim Lutz "Fri Dec  1 07:19:31 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")


# get the 'LOGGING DATA 1' table from 'EBMUD Retrofit Database.mdb'
# the database
fn_EBMUD <- paste0("'",wd_data,"Aquacraft/EBMUD/EBMUD Retrofit Database.mdb","'")
# the table to get from the database
logging_table <- "'LOGGING DATA 1'"
# the args for mdb-export 
mdb_args <- c(fn_EBMUD, logging_table)

# call mdb-export on the EBMUD Retrofit Database.mdb
# see: Liberating data from Microsoft Access “.mdb” files
# http://mazamascience.com/WorkingWithData/?p=168
EBMUD <- (system2("mdb-export", args = mdb_args, stdout = TRUE))

# loop through all the lines, except the first one, in the list form of the table from the database
DT_EBMUD_LOG1 <- data.table(ldply(.data=EBMUD[-1], .fun =read_log_line, .progress= "text", .inform=TRUE))

DT_EBMUD_LOG1
str(DT_EBMUD_LOG1)

# keep a backup for debugging. have to use copy otherwise just makes a new pointer to the object
DT_EBMUD_LOG1.bak <- copy(DT_EBMUD_LOG1)

# clean up the data.table. probably should automate this and do it in the function at some point
# get rid of extra '\"'
DT_EBMUD_LOG1[,USETYPE  := str_replace_all(USETYPE,'\"',"")]
DT_EBMUD_LOG1[,START   := str_replace_all(START,  '\"',"")]
DT_EBMUD_LOG1[,MODE_NO := str_replace_all(MODE_NO,'\"',"")]

# convert to numeric 
DT_EBMUD_LOG1[, `:=` (DURATION = as.numeric(DURATION),
                      PEAK     = as.numeric(PEAK),
                      VOLUME   = as.numeric(VOLUME),
                      MODE     = as.numeric(MODE),
                      MODE_NO  = as.numeric(MODE_NO)
                      ) 
            ]

DT_EBMUD_LOG1
str(DT_EBMUD_LOG1)

# restore backup instead of reading database all over again
# DT_EBMUD_LOG1 <- copy(DT_EBMUD_LOG1.bak)

# list and count of USETYPEs
sort(unique(DT_EBMUD_LOG1[]$USETYPE))
DT_EBMUD_LOG1[,list(n.events=length(KEYCODE)),by=USETYPE][order(-n.events)]

# total volume by USETYPE
DT_EBMUD_LOG1[,list(Vol=sum(VOLUME)),by=USETYPE][order(-Vol)]

# look for KEYCODEs with SHOWERs
DT_EBMUD_LOG1[USETYPE=='SHOWER',list(n.SHOWER=length(USETYPE)),by=KEYCODE][order(-n.SHOWER)][1:10]
  #     KEYCODE n.SHOWER
  #  1:   22062       58
  #  2:   22070       54
  #  3:   22021       52
  #  4:   22069       51
  #  5:   22076       45
  #  6:   22075       35
  #  7:   22074       32
  #  8:   22027       28
  #  9:   22045       28
  # 10:   22022       27

# in terminal
  # $ find . -iname "*.tdb" | grep 22062
  # $ find . -iname "*.tdb" | grep 22070
  # ./Post Retrofit 1/22070hw.tdb
  # ./Post Retrofit 1/22070.tdb
  # ./Post Retrofit 2/22070hw.tdb
  # ./Post Retrofit 2/22070.tdb
  # ./Pre Retrofit/22070HWB.tdb
  # ./Pre Retrofit/22070B.tdb
  # $ find . -iname "*.tdb" | grep 22021
  # ./Post Retrofit 1/22021.tdb
  # ./Post Retrofit 1/22021hw.tdb
  # ./Post Retrofit 2/22021.tdb
  # ./Post Retrofit 2/22021hw.tdb
  # ./Pre Retrofit/22021HW.tdb
  # ./Pre Retrofit/22021hwA.tdb
  # ./Pre Retrofit/22021hwB.tdb

# look at dates in 22070
DT_EBMUD_LOG1[KEYCODE=='22070' & USETYPE=='SHOWER'][order(START)]

# look for these ones
# 21:   22070  SHOWER 04/28/01 12:52:15      640 2.50  19.23 1.78      23
# 22:   22070  SHOWER 04/28/01 14:03:15     1220 1.90  34.97 1.69      45
# 23:   22070  SHOWER 04/29/01 11:38:05      850 1.85  24.89 1.76      30
# 24:   22070  SHOWER 04/29/01 13:34:15      840 5.29  24.31 1.55      70
# 25:   22070  SHOWER 04/30/01 06:06:45      670 5.09  22.67 1.78      20
# 26:   22070  SHOWER 04/30/01 06:32:35     1350 3.20  39.14 1.72      45
# 27:   22070  SHOWER 04/30/01 18:14:45      570 1.90  16.53 1.77      16


# hand export of table Flows from /data/Aquacraft/EBMUD/Pre Retrofit/22070B.tdb
X22070B_Flows
DT_X22070B_Flows <- data.table(X22070B_Flows)
str(DT_X22070B_Flows)
DT_X22070B_Flows[str_detect(StartTime, "04/29/") & str_detect(StartTime, " 11:38")]
  #      ID EventID              StartTime Rate
  # 1: 4357     467 04/29/2001 11:38:05 AM 1.85
  # 2: 4358     467 04/29/2001 11:38:15 AM 1.85
  # 3: 4359     467 04/29/2001 11:38:25 AM 1.85
  # 4: 4360     467 04/29/2001 11:38:35 AM 1.85
  # 5: 4361     467 04/29/2001 11:38:45 AM 1.85
  # 6: 4362     467 04/29/2001 11:38:55 AM 1.85
DT_X22070B_Flows[EventID==467]
# that's shower line 23: above

# doesn't appear class has a unique correspondence to SHOWER.
# it's probably some temporary variable in Aquacraft's analysis. 




