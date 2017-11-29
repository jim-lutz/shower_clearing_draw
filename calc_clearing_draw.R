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

# look at the distribution of shower volumes 
p <- ggplot(data = DT_heavy )
p <- p + geom_line(aes(x=Hr, y=GPH), color="blue", size=2)
p <- p + geom_line(aes(x=Hr,y=ER), color="red")
p <- p + geom_line(aes(x=Hr,y=HPWH), color="green")
p <- p + ggtitle("Hot Water and Electricity Use (Mon, 5/18, 6 people)") + labs(x = "hour", y = "Hourly Use")
p <- p + scale_x_continuous(breaks=1:24,labels=1:24)
p

ggsave(filename = paste0(wd_charts,"/HW_elec_heavy.png"), plot = p)



# Look at ratio of peak/mode for all the shower events

