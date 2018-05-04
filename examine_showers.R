# examine_showers.R
# script to examine information in DT_summary.RData
# Jim Lutz "Thu May  3 08:05:40 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get some useful functions
source("functions.R")

# load DT_shower_Flows.RData, this is the shower only interval data
load(file = paste0(wd_data,"DT_shower_Flows.RData"))

# load DT_summary.RData, this is the shower summary data
load(file = paste0(wd_data,"DT_summary.RData"))
str(DT_summary)

summary(DT_summary$RMSE)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00000 0.08471 0.16273 0.21492 0.29007 1.62256      28 
# max is > 1 ?
# 28 NA's

summary(DT_summary$start.draw)
#                  Min.               1st Qu.                Median 
# "1999-10-25 13:40:49" "2000-04-01 22:00:02" "2000-08-05 10:17:43" 
#                  Mean               3rd Qu.                  Max. 
# "2001-01-02 21:23:13" "2001-08-19 12:28:27" "2002-06-14 08:30:09" 
#                  NA's 
#                  "28" 

# look at distributions
names(DT_summary)

# drop the ones with missing RMSEs
DT_p.summary <- DT_summary[!is.na(RMSE), 
                           list(logging,vol.clearing, vol.showering,
                                dur.clearing, dur.showering,
                                flow.clearing, flow.showering)]

# look at scatterplot matrix of showering and clearing variables
p.spm_shower_clearing <-
  ggpairs(DT_p.summary[ , L := as.factor(logging)], 
          aes(colour = L,
              alpha = 0.4),
          columns = 2:7,
          axisLabels="internal",
          title = "showering and clearing variables",
          lower = list(continuous = "points", combo = "dot_no_facet")
  )

ggsave(filename = paste0(wd_charts,"/spm_shower_clearing.png"), 
       plot = p.spm_shower_clearing,
       width = 10.5, height = 9.8)

rm(DT_p.summary)


# distribution plots of RMSE
p.RMSE <- ggplot(data = DT_summary[!is.na(RMSE)] )
p.RMSE <- p.RMSE + geom_histogram( aes( x = RMSE ),
                                   binwidth = .02,
                                   center = .01)
p.RMSE <- p.RMSE + 
  ggtitle("RMS error of of fitting 2-step draw pattern to shower interval data") +
  labs(y = "count of showers")
p.RMSE

ggsave(filename = paste0(wd_charts,"/RMSE_shower_fitting.png"), 
       plot = p.RMSE,
       width = 10.5, height = 9.8)

# look at plots of some of these showers.
DT_summary[RMSE==0, list(shower.id)]
# 21 showers

# histogram of total duration of shower
p.Tdur <- ggplot(data = DT_summary[!is.na(RMSE), 
                                        list(Tdur= dur.clearing + dur.showering)
                                        ] 
                      )
p.Tdur <- p.Tdur + geom_histogram( aes(x=Tdur),
                                   binwidth = .5,
                                   center = .25)
p.Tdur <- p.Tdur + ggtitle("distribution of total shower duration") +
                    labs( x="total shower duration (mins)",
                          y = "count of showers")
p.Tdur

# how many really short showerings
summary(DT_summary[!is.na(RMSE)]$dur.showering)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
 # 0.1667  0.5000  4.1667  4.7633  7.1667 31.3333

DT_summary[!is.na(RMSE) & dur.showering < 2,
           list(nshowerings = length(shower.id)),
           by=dur.showering][order(dur.showering)]
#     dur.showering nshowerings
#  1:     0.1666667         516
#  2:     0.3333333          92
#  3:     0.5000000          24
#  4:     0.6666667          46
#  5:     0.8333333          18
#  6:     1.0000000          22
#  7:     1.1666667          16
#  8:     1.3333333          19
#  9:     1.5000000          16
# 10:     1.6666667          13
# 11:     1.8333333          20

# this doesn't look good, about 1/5 showerings < 10 secs?
DT_summary[!is.na(RMSE) & dur.showering <= 1/6, 
           list(study, KEYCODE, logging, meter, EventID, shower.id,
                dur.showering, start.showering.time)]

plot_shower_id(4) # this doesn't look good
plot_shower_id(10)
plot_shower_id(11)
plot_shower_id(10)
plot_shower_id(24)
plot_shower_id(2431)
plot_shower_id(2502)



# histogram of showering duration by logging
# add treat {pre|post}
DT_summary.dur.s <- DT_summary[!is.na(RMSE)] # drop 2 or less intervals
DT_summary.dur.s[logging==1, treat := "pre"]
DT_summary.dur.s[logging>1,  treat := "post"]
DT_summary.dur.s <- DT_summary.dur.s[ , list(treat, dur.showering)]

p.dur.s <- ggplot(data = DT_summary.dur.s[])
p.dur.s <- p.dur.s + geom_freqpoly(data = DT_summary.dur.s[treat=="pre"],
                                    aes(x=dur.showering, ..density..),
                                    binwidth = .5, center = .25,
                                    color = "red", alpha = 0.2,
                                    position="identity")
p.dur.s <- p.dur.s + geom_freqpoly(data = DT_summary.dur.s[treat=="post"],
                                    aes(x=dur.showering, ..density..),
                                    binwidth = .5, center = .25,
                                   color = "green", alpha = 0.2,
                                    position="identity")
p.dur.s <- p.dur.s + ggtitle("distribution of showering duration") +
  labs( x="showering duration (mins)",
        y = "count of showers")
p.dur.s






# scatterplot of RMSE vs total duration of shower
p.RMSEvTdur <- ggplot(data = DT_summary[!is.na(RMSE), 
                                        list(RMSE, 
                                             Tdur= dur.clearing + dur.showering)
                                        ]
                      )

p.RMSEvTdur <- p.RMSEvTdur + 
                    geom_point(aes(x=Tdur, y=RMSE)) +
                    labs( x="total shower duration (mins)")
p.RMSEvTdur




plot_shower_id(1631)

