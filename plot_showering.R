# plot_showering.R
# script to generate initial plots from DT_summary.RData
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
    ggtitle("clearing draw volume by installation") +
    labs(x = "clearing draw volume (gal)") 
  
  p.dens.volclearing
  
  ggsave(filename = paste0(wd_charts,"/all.dens.volclearing.png"), 
         plot = p.dens.volclearing,
         width = 10.5, height = 9.8)
  
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
    ggtitle("showering draw volume by installation") +
    labs(x = "showering draw volume (gal)")
  
  p.dens.volshowering
  
  ggsave(filename = paste0(wd_charts,"/all.dens.volshowering.png"), 
         plot = p.dens.volshowering,
         width = 10.5, height = 9.8)
  
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
    ggtitle("showering draw duration by installation") +
    labs(x = "showering draw duration (min)") 
  
  p.dens.durshowering
  
  ggsave(filename = paste0(wd_charts,"/all.dens.durshowering.png"), 
         plot = p.dens.durshowering,
         width = 10.5, height = 9.8)
  
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
    ggtitle("showering draw flow by installation") +
    labs(x = "showering draw flow (GPM)") 
  
  p.dens.flowshowering
  
  ggsave(filename = paste0(wd_charts,"/all.dens.flowshowering.png"), 
         plot = p.dens.flowshowering,
         width = 10.5, height = 9.8)
  
} # end of density plot of showering flow by pre and post install



