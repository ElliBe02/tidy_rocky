## ---------------------------
##
## Script name: tidy_project_rocky.R
##
#' Purpose of script: To collate the project rockey work in a coherent way 
#' -> more easily described and altered 
#'
## Author: Benjamin Elliot
##
## Date Created: 2023-01-04
##
## ## Email: Benjamin.Elliot@LexisNexisRisk.com
##
## ---------------------------
##
#' Notes:
#'   
##
## ---------------------------
##
## GitHub:
##
# usethis::create_github_token() 
# gitcreds::gitcreds_set() 
##
## ---------------------------

require("tidyverse")
require("reticulate")
require("sp")
require("rworldmap")

source("to_use_markdown.R")       # loads up all the packages we need

## ---------------------------


# Experiment 1 ------------------------------------------------------------

#'  Coverage of sanctioned vessels (IMO) across '2022-05-21 00:00:00' to '2022-06-21 23:59:59'**   


# data importing:
{
# sanctioned vessels:

    # OFAC 
    readxl::excel_sheets('~/projectRocky/Data/OFAC Vessel IMOs.xlsx')
    interest_Vessels <- readxl::read_xlsx('~/projectRocky/Data/OFAC Vessel IMOs.xlsx', sheet = "Sheet3", col_names = FALSE) %>%
      distinct() %>%
      na.omit() %>%
      select(2) %>%
      list() %>%
      unlist() %>%
      unname() %>%
      unique()
    
    # ShipData 
    shipData <- read_table('~/projectRocky/Data/ShipData.txt')

interest_Vessels <- c(interest_Vessels, shipData$LRIMOShipNo) %>%
  unique() %>%
  sort() # interest_Vessels refers to the known sanctioned vessels


# the MT and IHS data: 
py_run_file('~/projectRocky/Experiment 1/01_Experiment_data.py')

ihsData <- py$ihs_01 %>%
  as_tibble()
mtData <- py$mt_01 %>%
  as_tibble()}

# data wrangling
{
  # unique IMO count
    
    ihs_imo_count <- ihsData$imo %>%
      unique() %>%
      length()
    
    mt_imo_count <- mtData$imo %>%
      unique() %>%
      length()
  
    ihs_interest_count <- interest_Vessels %>%
      unique() %>%
      length()
  
  # Coverage
  
  # IHS
    
    ihs_interest_count <- sum(interest_Vessels %in% ihs_imo) %>%
      to_use_markdown(name = "ihs_interest_count")
    
    ihs_vessel_coverage <- (sum(interest_Vessels %in% ihs_imo) / length(interest_Vessels)) %>%
      scales::percent(accuracy = 0.01)
    
    interest_ihs_only <- sum(interest_Vessels %in% ihs_imo) - sum(interest_Vessels %in% sharedIMO)
    
  # MT
    
    mt_interest_count <- sum(interest_Vessels %in% mt_imo) %>%
      to_use_markdown(name = "mt_interest_count")
    
    mt_vessel_coverage <- (sum(interest_Vessels %in% mt_imo) / length(interest_Vessels)) %>%
      scales::percent(accuracy = 0.01)
    
    interest_mt_only <- sum(interest_Vessels %in% mt_imo) - sum(interest_Vessels %in% sharedIMO)
    
    
  # Shared
  
    shared_imo_count <- c(mt_imo[mt_imo %in% ihs_imo], 
                   ihs_imo[ihs_imo %in% mt_imo]) %>%
      unique() %>%
      length()
    
    
    shared_vessel_coverage <- (sum(interest_Vessels %in% sharedIMO) / length(interest_Vessels)) %>%
      scales::percent(accuracy = 0.01)
}

# data export:
{
  # unique IMO count
  saveRDS(interest_count, '~/projectRocky/Data/interest_count.rds')
  saveRDS(ihs_imo_count, '~/projectRocky/Data/markdown data/ihs_imo_count.rds')
  saveRDS(mt_imo_count, '~/projectRocky/Data/markdown data/mt_imo_count.rds')

  # sanctioned coverage
  saveRDS(ihs_vessel_coverage, '~/projectRocky/Data/markdown data/ihs_vessel_coverage.rds')
  saveRDS(mt_vessel_coverage, '~/projectRocky/Data/markdown data/mt_vessel_coverage.rds')
  
  saveRDS(shared_imo_count, '~/projectRocky/Data/markdown data/shared_imo__count.rds')
  
  saveRDS(shared_vessel_coverage, '~/projectRocky/Data/markdown data/shared_vessel_coverage.rds')
  
  saveRDS(interest_ihs_only, '~/projectRocky/Data/markdown data/interest_ihs_only.rds')
  saveRDS(interest_mt_only, '~/projectRocky/Data/markdown data/interest_mt_only.rds')
}  





# Experiment 2 ------------------------------------------------------------

#' Coverage of sanctioned port list across '2022-05-21 00:00:00' to '2022-06-21 23:59:59'.
#' the IHS data is facility ID, and the MT data is port ID


# data import:

# IHS
py_run_file('~/projectRocky/Experiment 2/ihsMovement.py')

ihsMovement <- py$ihsMovement %>%
  as_tibble()

names(ihsMovement) <- c("imo", "movementdate.ihs", "lat.ihs", "lon.ihs", "facilityid")

# first 1,000 of MT data
py_run_file('~/projectRocky/Experiment 2/mtMovement.py')

mtMovement <- py$mtMovement %>%
  as_tibble()

names(mtMovement) <- c("imo", "lon.mt", "lat.mt", "movementdate.mt")

mtMovement[,c(-1, -4)] <- mtMovement[,c(-1, -4)] %>%
  mutate_all(as.numeric)

mtMovement$movementdate.mt <- mtMovement$movementdate.mt %>%
  as.POSIXct()

# additional MT data

reticulate::py_run_file('~/projectRocky/Experiment 2/countingMT.py')

numRows <- 10000

py_rolling <- r_to_py(numRows)

interations <- as.numeric(ceiling((py$mtCount - nrow(mtMovement)) / numRows))



# **********************
  # make sure to grab myFunctions off remote desktop!
# **********************


# formatting merged table

movementOverlap <- myFunctions::format_overlap_table(mtMovement)




