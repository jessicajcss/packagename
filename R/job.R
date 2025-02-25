# Based on https://github.com/Sarah-2510/R-Shiny-Project---AIR-QUALITY-INDEX/blob/main/Rshiny%20final.R
# Last update: 2024-08-28
# attention to tz; # local path to files


#>>> locally run:
#read.dcf("DESCRIPTION")
#install.packages("remotes")
#remotes::install_deps(dependencies = TRUE)
#>>> and in Terminal
#cat DESCRIPTION



library(DT)
library(ggplot2)
library(data.table)
library(tidyverse)
library(devtools)
library(openair)
library(dplyr)
library(zoo)
library(httr)
library(usethis)
library(conflicted)


#  --------------------------------------------------------------------------------------------------------
#                                              CHECKING CONFLICTED FUNCTIONS
#  --------------------------------------------------------------------------------------------------------

#devtools::install_github("r-lib/conflicted")
#library(conflicted)

#conflict_scout(
 # source("R/job.R")
#)
conflicts_prefer(dplyr::filter)
conflicts_prefer(lubridate::hour)
conflicted::conflicts_prefer(dplyr::summarize)

#conflict_scout(
 # source("./scripts/01-AQI_calculation_thermo_data.R")
#)

#tidyverse_conflicts(
 # source("./scripts/00-preprocessing_thermo_GitHub_data.R")
#)

#  --------------------------------------------------------------------------------------------------------
#                                              READING THE FILES
#  --------------------------------------------------------------------------------------------------------

source("./scripts/01-AQI_calculation_thermo_data.R")

