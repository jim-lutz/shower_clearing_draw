# examine_showering.R
# script to generate initial plots from DT_summary.RData
# Jim Lutz "Wed May  9 07:41:58 2018"

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

# center the titles
theme_update(plot.title = element_text(hjust = 0.5))

# which plots to run
# ----
# set to FALSE if working on later plots
# scatterplot matrix of showering and clearing variables
  run.spm = FALSE  
# boxplots of volume of clearing draws by logging, KEYCODE, and meter
  run.box.clearing = FALSE 
# boxplots of volume of showering draws by logging, KEYCODE, and meter
  run.box.showering  = FALSE 
# boxplots of duration of showering draws by logging, KEYCODE, and meter
  run.box.dur.showering  = TRUE 
# boxplots of duration of clearing draws by logging, KEYCODE, and meter
  run.box.dur.clearing  = TRUE 
  
  # ----
  
  
# blocks of code to generate plots
# ----
  
# scatterplot matrix of showering and clearing variables
if(run.spm){
  p.spm_showering_clearing2 <-
  ggpairs(DT_summary[ , L := as.factor(logging)], 
          aes(colour = L,
              alpha = 0.4),
          columns = 12:17,
          axisLabels="internal",
          title = "showering and clearing variables",
          lower = list(continuous = "points", combo = "dot_no_facet")
  )

ggsave(filename = paste0(wd_charts,"/spm_shower_clearing2.png"), 
       plot = p.spm_shower_clearing2,
       width = 10.5, height = 9.8)

} # end of scatterplot matrix of showering and clearing variables

# boxplots of volume of clearing draws by logging, KEYCODE, and meter
if(run.box.clearing) {
p.box.volclearing <- ggplot(data = DT_summary[] )
p.box.volclearing <- p.box.volclearing + 
                        geom_boxplot( aes( x = as.factor(logging), 
                                           y = vol.clearing,
                                           fill = as.factor(logging)
                                           ),
                                      notch = TRUE,
                                      varwidth = TRUE,
                                      show.legend = FALSE
                                      )

p.box.volclearing <- p.box.volclearing + 
  ggtitle("shower clearing draws by house") +
  labs(y = "clearing draw volume (gal)",
       x = "before (1) and after showerhead installation (2,3)")

p.box.volclearing

ggsave(filename = paste0(wd_charts,"/all.box.volclearing.png"), 
       plot = p.box.volclearing,
       width = 10.5, height = 9.8)

# by KEYCODE
p.box.volclearing.KEYCODE <- p.box.volclearing + 
  facet_wrap(~KEYCODE, nrow = 4)

p.box.volclearing.KEYCODE

ggsave(filename = paste0(wd_charts,"/box.volclearing.KEYCODE.png"), 
       plot = p.box.volclearing.KEYCODE,
       width = 10.5, height = 9.8)

# by meter
p.box.volclearing.meter <- p.box.volclearing + 
  facet_wrap(~meter, nrow = 4)

p.box.volclearing.meter

ggsave(filename = paste0(wd_charts,"/box.volclearing.meter.png"), 
       plot = p.box.volclearing.meter,
       width = 10.5, height = 9.8)

} # end of boxplots of volume of clearing draws by logging, KEYCODE, and meter

# boxplots of volume of showering draws by logging, KEYCODE, and meter
if(run.box.showering) {  
p.box.volshowering <- ggplot(data = DT_summary[] )

p.box.volshowering <- p.box.volshowering + 
  geom_boxplot( aes( x = as.factor(logging), 
                     y = vol.showering,
                     fill = as.factor(logging)
                     ),
                notch = TRUE,
                varwidth = TRUE,
                show.legend = FALSE
              )

p.box.volshowering <- p.box.volshowering + 
  ggtitle("shower showering draws by house") +
  labs(y = "showering draw volume (gal)",
       x = "before (1) and after showerhead installation (2,3)")

p.box.volshowering

ggsave(filename = paste0(wd_charts,"/all.box.volshowering.png"), 
       plot = p.box.volshowering,
       width = 10.5, height = 9.8)

# by KEYCODE
p.box.volshowering.KEYCODE <- p.box.volshowering + 
  facet_wrap(~KEYCODE, nrow = 4)

p.box.volshowering.KEYCODE

ggsave(filename = paste0(wd_charts,"/box.volshowering.KEYCODE.png"), 
       plot = p.box.volshowering.KEYCODE,
       width = 10.5, height = 9.8)


# by meter
p.box.volshowering.meter <- p.box.volshowering + 
  facet_wrap(~meter, nrow = 2)

p.box.volshowering.meter

ggsave(filename = paste0(wd_charts,"/box.volshowering.meter.png"), 
       plot = p.box.volshowering.meter,
       width = 10.5, height = 9.8)

} # end of boxplots of volume of clearing draws by logging, KEYCODE, and meter

# boxplots of duration of showering draws by logging, KEYCODE, and meter
if(run.box.dur.showering) {  
    p.box.durshowering <- ggplot(data = DT_summary[] )
    
    p.box.durshowering <- p.box.durshowering + 
      geom_boxplot( aes( x = as.factor(logging), 
                         y = dur.showering,
                         fill = as.factor(logging)),
                    notch = TRUE,
                    varwidth = TRUE,
                    show.legend = FALSE
                    )
    
    p.box.durshowering <- p.box.durshowering + 
      ggtitle("shower showering draws by house") +
      labs(y = "showering draw duration (min)",
           x = "before (1) and after showerhead installation (2,3)")
    
    p.box.durshowering
    
    ggsave(filename = paste0(wd_charts,"/all.box.durshowering.png"), 
           plot = p.box.durshowering,
           width = 10.5, height = 9.8)
    
    # by KEYCODE
    p.box.durshowering.KEYCODE <- p.box.durshowering + 
      facet_wrap(~KEYCODE, nrow = 4)
    
    p.box.durshowering.KEYCODE
    
    ggsave(filename = paste0(wd_charts,"/box.durshowering.KEYCODE.png"), 
           plot = p.box.durshowering.KEYCODE,
           width = 10.5, height = 9.8)
    
    
    # by meter
    p.box.durshowering.meter <- p.box.durshowering + 
      facet_wrap(~meter, nrow = 2)
    
    p.box.durshowering.meter
    
    ggsave(filename = paste0(wd_charts,"/box.durshowering.meter.png"), 
           plot = p.box.durshowering.meter,
           width = 10.5, height = 9.8)
    
  } # end of boxplots of duration of showering draws by logging, KEYCODE, and meter

# boxplots of duration of clearing draws by logging, KEYCODE, and meter
if(run.box.dur.clearing) {  
    p.box.durclearing <- ggplot(data = DT_summary[] )
    
    p.box.durclearing <- p.box.durclearing + 
      geom_boxplot( aes( x = as.factor(logging), 
                         y = dur.clearing,
                         fill = as.factor(logging)),
                    notch = TRUE,
                    varwidth = TRUE,
                    show.legend = FALSE
      )
    
    p.box.durclearing <- p.box.durclearing + 
      ggtitle("shower clearing draws by house") +
      labs(y = "clearing draw duration (min)",
           x = "before (1) and after showerhead installation (2,3)")
    
    p.box.durclearing
    
    ggsave(filename = paste0(wd_charts,"/all.box.durclearing.png"), 
           plot = p.box.durclearing,
           width = 10.5, height = 9.8)
    
    # by KEYCODE
    p.box.durclearing.KEYCODE <- p.box.durclearing + 
      facet_wrap(~KEYCODE, nrow = 4)
    
    p.box.durclearing.KEYCODE
    
    ggsave(filename = paste0(wd_charts,"/box.durclearing.KEYCODE.png"), 
           plot = p.box.durclearing.KEYCODE,
           width = 10.5, height = 9.8)
    
    
    # by meter
    p.box.durclearing.meter <- p.box.durclearing + 
      facet_wrap(~meter, nrow = 2)
    
    p.box.durclearing.meter
    
    ggsave(filename = paste0(wd_charts,"/box.durclearing.meter.png"), 
           plot = p.box.durclearing.meter,
           width = 10.5, height = 9.8)
    
  } # end of boxplots of duration of clearing draws by logging, KEYCODE, and meter
  
  
  


