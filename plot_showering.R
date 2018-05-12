# plot_showering.R
# script to generate presentation plots from DT_summary.RData
# Jim Lutz "Sat May 12 05:41:54 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
source("setup_wd.R")

# get the project functions
source("functions.R")

# load DT_summary.RData, this is the shower summary data
load(file = paste0(wd_data,"DT_summary.RData"))
str(DT_summary)
names(DT_summary)

# add 'install' field to rename logging and combine 2 & 3 as post
DT_summary[logging==1, install:='base']
DT_summary[logging==2 | logging==3, install:= 'post']

# overall project stats
# number of houses
DT_summary[,list(n=length(EventID)), by=KEYCODE]
# 19

# total number of showers (cleaned)
nrow(DT_summary[meter=='total water'])
# [1] 1138

# average showering volume
DT_summary[meter=='total water',
           list(n=length(vol.showering), 
                ave.vol.showering = mean(vol.showering, na.rm=TRUE),
                med.vol.showering = median(vol.showering, na.rm=TRUE),
                ave.vol.clearing  = mean(vol.clearing, na.rm=TRUE),
                med.vol.clearing  = median(vol.clearing, na.rm=TRUE)
                ), 
           by=install]


# update theme to center the titles on all the plots
theme_update(plot.title = element_text(hjust = 0.5))

# which plots to run
# set to FALSE if working on later plots
# ----
# density plot of showering volume by pre and post install
run.dens.vol.showering = TRUE  
# density plot of showering duration by pre and post install
run.dens.dur.showering = TRUE  
# density plot of showering flow by pre and post install
run.dens.flow.showering = TRUE  
# density plot of clearing volume by pre and post install
run.dens.vol.clearing = TRUE  
# histogram of total volume of all showers
run.hist.vol.shower = TRUE  


# histogram of total volume of all showers
if(run.hist.vol.shower) {
  p.hist.vol.shower <- ggplot(data = DT_summary[meter=='total water'] )
  
  p.hist.vol.shower <- p.hist.vol.shower + 
    geom_histogram( aes(x=vol.total),
                    binwidth = 1,
                    center = .5)
  
  p.hist.vol.shower <- p.hist.vol.shower + 
    ggtitle("shower data (cleaned)") +
    labs( x="total shower volume (gal)",
          y = "count of showers")
  
  p.hist.vol.shower

  ggsave(filename = paste0(wd_charts,"/all.hist.volshower.png"), 
         plot = p.hist.vol.shower)
  
  
  } # end of histogram of total volume of all showers


# density plot of clearing volume by pre and post install
if(run.dens.vol.clearing) {
  p.dens.volclearing <- ggplot(data = DT_summary[meter=='total water'] )
  p.dens.volclearing <- p.dens.volclearing + 
    geom_density( aes( x = vol.clearing,
                       fill = install
                       ),
                  adjust = 1/2,
                  alpha = 0.2
                  )
  
  p.dens.volclearing <- p.dens.volclearing + 
    # ggtitle("clearing draw volume by installation") +
    labs(x = "clearing draw volume (gal)",
         y = "smoothed probability density") 
  
  p.dens.volclearing
  
  ggsave(filename = paste0(wd_charts,"/all.dens.volclearing.png"), 
         plot = p.dens.volclearing,
         width = 4.75, height = 4 )
  
} # end of density plot of clearing volume by pre and post install


# density plot of showering volume by pre and post install
if(run.dens.vol.showering) {
  p.dens.volshowering <- ggplot(data = DT_summary[meter=='total water'] )
  p.dens.volshowering <- p.dens.volshowering + 
    geom_density( aes( x = vol.showering,
                       fill = install
                       ),
                  adjust = 1/2,
                  alpha = 0.2
                  )
  
  p.dens.volshowering <- p.dens.volshowering + 
    # ggtitle("showering draw volume by installation") +
    labs(x = "showering draw volume (gal)",
         y = "smoothed probability density")
  
  p.dens.volshowering
  
  ggsave(filename = paste0(wd_charts,"/all.dens.volshowering.png"), 
         plot = p.dens.volshowering,
         width = 4.75, height = 4 )
  
} # end of density plots of showering volume by pre and post install


# density plot of showering duration by pre and post install
if(run.dens.dur.showering) {
  p.dens.durshowering <- ggplot(data = DT_summary[meter=='total water'] )
  p.dens.durshowering <- p.dens.durshowering + 
    geom_density( aes( x = dur.showering,
                       fill = install
    ),
    adjust = 1/2,
    alpha = 0.2
    )
  
  p.dens.durshowering <- p.dens.durshowering + 
    # ggtitle("showering draw duration by installation") +
    labs(x = "showering draw duration (min)",
         y = "smoothed probability density") 
  
  p.dens.durshowering
  
  ggsave(filename = paste0(wd_charts,"/all.dens.durshowering.png"), 
         plot = p.dens.durshowering,
         width = 4.75, height = 4 )
  
} # end of density plot of showering duration by pre and post install


# density plot of showering flow by pre and post install
if(run.dens.flow.showering) {
  p.dens.flowshowering <- ggplot(data = DT_summary[meter=='total water'] )
  p.dens.flowshowering <- p.dens.flowshowering + 
    geom_density( aes( x = flow.showering,
                       fill = install
                       ),
                  adjust = 1/2,
                  alpha = 0.2
                  )
  
  p.dens.flowshowering <- p.dens.flowshowering + 
    # ggtitle("showering draw flow by installation") +
    labs(x = "showering draw flow (GPM)",
         y = "smoothed probability density") 
  
  p.dens.flowshowering
  
  ggsave(filename = paste0(wd_charts,"/all.dens.flowshowering.png"), 
         plot = p.dens.flowshowering,
         width = 4.75, height = 4 )
  
} # end of density plot of showering flow by pre and post install



