# Based on https://github.com/Sarah-2510/R-Shiny-Project---AIR-QUALITY-INDEX/blob/main/Rshiny%20final.R
# Last update: 2024-08-28
# attention to tz; # local path to files


#>>> locally run:
#read.dcf("DESCRIPTION")
#install.packages("remotes")
#remotes::install_deps(dependencies = TRUE)
#>>> and in Terminal
#cat DESCRIPTION



library(jsonlite)
library(DT)
library(data.table)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(ggplot2)
library(data.table)
library(tidyverse)
library(devtools)
library(openair)
library(dplyr)
library(zoo)
library(httr)

#  --------------------------------------------------------------------------------------------------------
#                                              READING THE FILES
#  --------------------------------------------------------------------------------------------------------

source("./scripts/01-AQI_calculation_thermo_data.R")

