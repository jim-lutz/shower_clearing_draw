# calc_clearing_draw.R
# script calculate the clearing draw for showers
# based on:
# Lutz, James D. “Estimating Energy and Water Losses in Residential Hot Water Distribution Systems.” 
# ASHRAE Transactions 111, no. 2 (2005). https://escholarship.org/uc/item/4nj7m0q6.pdf.
# Jim Lutz "Wed Nov 29 09:06:18 2017"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get the database DT_events
fn_DT_events <- paste0(wd_data,"DT_events.RData")
load(file = fn_DT_events)
str(DT_events)

# count events by type (SumAs or CountAs)
DT_events[,list(nevents=length(Keycode)),by=SumAs]
  #             SumAs nevents
  # 1:           Leak 1412714
  # 2:        Bathtub    1628
  # 3:  Clotheswasher   36564
  # 4:     Dishwasher    9025
  # 5:         Faucet  541179
  # 6:          Other    6575
  # 7:         Shower   17204
  # 8:         Toilet  121987
  # 9:     Irrigation   14250
  # 10:             NA    7551
  # 11: Clotheswasher@      16
  # 12:    Dishwasher@       5

DT_events[,list(nevents=length(Keycode)),by=CountAs]
  #          CountAs nevents
  # 1:          Leak 1412714
  # 2:       Bathtub    1628
  # 3:            NA   42409
  # 4: Clotheswasher    7876
  # 5:    Dishwasher    1930
  # 6:        Faucet  541179
  # 7:         Other    6575
  # 8:        Shower   17204
  # 9:        Toilet  121987
  # 10:    Irrigation   14250
  # 11:  Clotheswashe     946

# check that same showers are labeled both ways
with(DT_events, table(CountAs=='Shower',SumAs=='Shower'))
  #         FALSE    TRUE
  # FALSE 2109085       0
  # TRUE        0   17204

# work only with showers
DT_shower_events <- DT_events[CountAs=='Shower', 
          list(Keycode,
               StartTime,
               Duration,
               Peak,
               Volume,
               Mode,
               ModeFreq
               )
          ]

# How many Keycodes?
length(unique(DT_events$Keycode))
  # [1] 730
length(unique(DT_shower_events$Keycode))
  # [1] 711
# No showers identified at 19 sites? Oh well

summary(DT_shower_events$Volume)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.11   10.14   15.33   18.02   22.96  108.51 

# look at the distribution of shower volumes 
p <- ggplot(data = DT_shower_events )
p <- p + geom_histogram(aes(x=Volume), binwidth=1)
p <- p + ggtitle("Shower Volume") + labs(x = "Volume (gallons)", y = "count")
p 

ggsave(filename = paste0(wd_charts,"/shower_volumes1.png"), plot = p)

# Look at ratio of peak/mode for all the shower events
DT_shower_events[,peak_mode:= Peak/Mode]

# exam results
summary(DT_shower_events$peak_mode)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 1.000   1.020   1.143   1.768   1.766 640.000 

# what's the outliers?
DT_shower_events[peak_mode>20,][order(-peak_mode)]
# these are showers with a mode of <= 0.05 GPM?

# get rid of those
DT_shower_events <- DT_shower_events[Mode>0.05]

# now see what's there
summary(DT_shower_events$peak_mode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.020   1.143   1.554   1.762  75.642 
DT_shower_events[peak_mode>15,][order(-peak_mode)]

# get rid of the 2 remaining really strange ones 
DT_shower_events <- DT_shower_events[peak_mode<20]

summary(DT_shower_events$peak_mode)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 1.000   1.020   1.143   1.548   1.762  18.556 

DT_shower_events[peak_mode>10,][order(-peak_mode)]
DT_shower_events[peak_mode==1,][order(-peak_mode)]
# how many have peak as the mode
nrow(DT_shower_events[peak_mode==1,])/nrow(DT_shower_events)
  # [1] 0.1224252

# look at the distribution of peak_mode
p <- ggplot(data = DT_shower_events[peak_mode>1] )
p <- p + geom_histogram(aes(x=peak_mode), binwidth = .05)
p <- p + ggtitle("Shower Peak/Mode (cleaned data)") + labs(x = "ratio (GPM/GPM)", y = "count")
p 

ggsave(filename = paste0(wd_charts,"/peak_mode.png"), plot = p)



# look at the distribution of remaining shower volumes to make sure it's still OK
p <- ggplot(data = DT_shower_events[peak_mode>1] )
p <- p + geom_histogram(aes(x=Volume), binwidth = 1)
p <- p + ggtitle("Shower Volume (cleaned data)") + labs(x = "Volume (gallons)", y = "count")
p 

ggsave(filename = paste0(wd_charts,"/shower_volumes2.png"), plot = p)

# compare the duration at Mode (ModeFreq*10?) to total Duration
DT_shower_events[peak_mode>1, shower_duration:= ModeFreq * 10]
DT_shower_events[peak_mode>1, duration_ratio := shower_duration/Duration]

summary(DT_shower_events$duration_ratio)
   #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   # 0.0750  0.2692  0.3659  0.3943  0.4935  0.9792    2104 

# look at the distribution of duration_ratio to see if it makes sense
p <- ggplot(data = DT_shower_events[peak_mode>1] )
p <- p + geom_histogram(aes(x=duration_ratio))
p <- p + ggtitle("Shower duration ratio") + labs(x = "shower time (s) / total duration (s)", y = "count")
p 

# look at the shower duration
summary(DT_shower_events$shower_duration)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   # 20.0   110.0   170.0   212.1   270.0  1940.0    2104 

# look at the distribution of duration_ratio to see if it makes sense
p <- ggplot(data = DT_shower_events[peak_mode>1] )
p <- p + geom_histogram(aes(x=shower_duration/60), binwidth = .5)
p <- p + ggtitle("Shower duration") + labs(x = "shower time (m)", y = "count")
p 

# the mode selection may be too picky. 
# find the resolution of mode
head(DT_shower_events[,list(nMode=length(Keycode)),by=Mode][order(Mode)], n=100)

# look at the distribution of Mode flow rates
summary(DT_shower_events$Mode)
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.090   1.580   1.910   2.087   2.340   9.770 
p <- ggplot(data = DT_shower_events[] )
p <- p + geom_histogram(aes(x=Mode), binwidth = .25)
p <- p + ggtitle("Mode") + labs(x = "flow rate (GPM)", y = "count")
p 



# calc the clearing draw and actual shower volumes
DT_shower_events[peak_mode>1,clearing:= Peak * (Volume - Mode*Duration )/(Peak-Mode)]

