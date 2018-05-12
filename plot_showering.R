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
# density plots of showering volumme by pre and post install
run.dens.vol.showering = TRUE  


# density plots of showering volumme by pre and post install
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
    ggtitle("showering draws by installation") +
    labs(x = "showering draw volume (gal)") # ,
         #x = "before (base) and after showerhead installation (post)")
  
  p.dens.volshowering
  
  ggsave(filename = paste0(wd_charts,"/all.dens.volshowering.png"), 
         plot = p.dens.volshowering,
         width = 10.5, height = 9.8)
  
} # end of density plots of showering volumme by pre and post install



