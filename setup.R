# setup.R
# make sure any needed packages are loaded
# Jim Lutz  "Tue Sep 12 15:46:11 2017"

# clean up leftovers before starting
# this is to prevent unintentionally using old data
l_obj=ls(all=TRUE)
l_obj = c(l_obj, "l_obj") # be sure to include l_obj
rm(list = l_obj)
# clear the plots
if(!is.null(dev.list())){
  dev.off(dev.list()["RStudioGD"])
}
# clear history
cat("", file = "nohistory")
loadhistory("nohistory")
# clear the console
cat("\014")

# only works if have internet access
update.packages(checkBuilt=TRUE)

sessionInfo() 
  # R version 3.4.3 (2017-11-30)
  # Platform: x86_64-pc-linux-gnu (64-bit)
  # Running under: Ubuntu 16.04.3 LTS

# work with plyr 
if(!require(plyr)){install.packages("plyr")}
library(plyr)

# work with data.tables
#https://github.com/Rdatatable/data.table/wiki
#https://www.datacamp.com/courses/data-analysis-the-data-table-way
if(!require(data.table)){install.packages("data.table")}
library(data.table)


# work with tidyverse
# http://tidyverse.org/
# needed libxml2-dev installed
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)

# work with lubridate 
if(!require(lubridate)){install.packages("lubridate")}
library(lubridate)
# part of tidyverse but wasn't seeing it


# work with stringr 
# if(!require(stringr)){install.packages("stringr")}
# library(stringr)
# part of tidyverse

# work with ggplot2
# if(!require(ggplot2)){install.packages("ggplot2")}
# library(ggplot2)
# part of tidyverse

# work with GGally
if(!require(GGally)){install.packages("GGally")}
library(GGally)


# change the default background for ggplot2 to white, not gray
theme_set( theme_bw() )



