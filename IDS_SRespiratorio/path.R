
#libraries
library(shiny)
library(shinycssloaders)
#internet
library(httr)

#fonts
library(extrafont)

#carpintery
library(glue)
library(scales)
library(lubridate)
library(forcats)

#maps
#library(sf)



#ggplot
library(grid)
library(ggplot2)
library(ggrepel)
library(ggfittext)
library(patchwork)
library(magick)
library(cowplot)


#tidyverse: 
library(tidyr)
library(stringr)
library(dplyr)


#other
library(rio)
library(cli)
library(zoo)


#shiny
library(DT)



#paths for dashboard




#define references -------------------------------------------------------------------

#dash_path <- dirname(getwd())
#project_path <- dirname(dash_path)
if(Sys.info()["sysname"] == "Windows"){
  
  dash_path <- dirname(getwd())
  project_path <- dirname(dash_path)
  
} else {
  
  project_path <- "~/INS-dashboard"
  dash_path <- file.path(project_path, "dashboard")
  #project_path <- dirname(dash_path)
  
}


dir_modules <- file.path(project_path, 'modules')
dir_styles <- file.path(project_path, 'styles')
dir_functions <- file.path(project_path, 'functions')
#dir_data <- file.path(project_path, 'data')




#logo
logo <- "https://ins.gov.mz/wp-content/uploads/2020/08/cropped-logo.jpg"


