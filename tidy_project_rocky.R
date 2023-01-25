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
 gitcreds::gitcreds_set() 
##
## ---------------------------

require("tidyverse")
require("reticulate")
require("sp")
require("rworldmap")
library("progress")

source("~/to_use_markdown.R")
source("~/coords2country.R")


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
py_run_file('~/projectRocky/Experiment 1/01_grab.py')

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


# Experiments 2 & 5 -------------------------------------------------------

# 2) 
#' Coverage of sanctioned port list across '2022-05-21 00:00:00' to '2022-06-21 23:59:59'.
#' the IHS data is facility ID, and the MT data is port ID


# 5) 
#' Map port data against regions / countries


# data import:
  
  # sanctioned port list

sanctioned_ports <- read.csv('~/projectRocky/Experiment 2/Sanctioned_port_list.csv') %>%
  select('port_id') %>%
  unlist() %>%
  unname() %>%
  as.numeric() %>%
  sort() %>%
  na.omit() %>%
  unique()

  # port ID look up 

py_run_file('~/projectRocky/Experiment 2/02_port_id_lookup.py')

  # world map
world_map <- map_data("world")

  # IHS port calls
py_run_file('~/projectRocky/Experiment 2/02_grab_ihs_port.py')

  # MT port calls
py_run_file('~/projectRocky/Experiment 2/02_grab_mt_port.py')

  # IHS mapping facility 
py_run_file('~/projectRocky/Experiment 2/02_mapping_ihs.py')






# data wrangling

  # IHS port calls
{
ihs_port_calls <- py$simplified_ihs_port_calls %>%
  as_tibble()

    # sort out country names in IHS port data 
    ihs_port_calls$countryname <- ihs_port_calls$countryname %>%
      str_replace("United States of America", "USA") %>%
      str_replace("United States", "USA") %>%
      str_replace("United Kingdom", "UK") %>%
      str_replace("United Republic of Tanzania", "Tanzania") %>%
      str_replace("Hong Kong S.A.R.", "China") %>%
      str_replace("Hong Kong SAR China", "China") %>%
      str_replace("Hong Kong", "China") %>%
      str_replace("Gibraltar", "Spain") %>%
      str_replace(fixed("Korea (North)"), "North Korea") %>%
      str_replace(fixed("Korea (South)"), "South Korea") %>%
      str_replace("Cape Verde Islands", "Cape Verde") %>%
      str_replace(fixed("Congo (Republic)"), "Republic of Congo") %>%
      str_replace(fixed("Congo (Democratic Republic)"), "Democratic Republic of the Congo") %>%
      str_replace("Cote d'Ivoire", "Ivory Coast") %>%
      str_replace("Faeroe Islands", "Faroe Islands") %>%
      str_replace("St ", "Saint ") %>%
      str_replace("St-", "Saint ") %>%
      str_replace(fixed("Saint Kitts & Nevis"), "Saint Kitts") %>%
      str_replace(fixed("Virgin Islands (US)"), "Virgin Islands") %>%
      str_replace(fixed("Virgin Islands (British)"), "Virgin Islands") %>%
      str_replace("Macao", "China") %>%
      str_replace(fixed("Chinese Taipei (Taiwan)"), "Taiwan") %>%
      str_replace("Saint Helena Island", "Saint Helena") %>%
      str_replace("Madeira", "Madeira Islands") %>%
      str_replace("Antigua & Barbuda", "Antigua") %>%
      str_replace("Sao Tome et Principe", "Sao Tome and Principe") %>%
      str_replace("Trinidad & Tobago", "Trinidad") %>%
      str_replace("Wallis & Futuna Islands", "Wallis and Futuna") %>%
      str_replace("Madeira Islands Islands", "Madeira Islands") %>%
      str_replace(fixed("Turks & Caicos Islands"), "Turks and Caicos Islands") %>%
      str_replace("Saint Barthélemy", "Saint Barthelemy") 

ihs_port_calls <- ihs_port_calls %>%
  select(imolrorihsnumber, countryname, movementdate, facilityid)
}
  # IHS port call summary
{
ihs_port_call_summary <- ihs_port_calls %>%
  distinct() %>%
  group_by(countryname) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

names(ihs_port_call_summary)[1] <- "region"
}
  # MT port calls
{
mt_port_calls <- py$mt_port_calls %>% 
  as_tibble()

  # adding region field
  mt_port_calls <- mt_port_calls %>%
    mutate("tempRegion" = 
             (countrycode::countrycode(mt_port_calls$country_code, origin = "iso2c", destination = "country.name")%>%
                str_replace("United States of America", "USA") %>%
                str_replace("United States", "USA") %>%
                str_replace("United Kingdom", "UK") %>%
                str_replace("United Republic of Tanzania", "Tanzania") %>%
                str_replace("Hong Kong S.A.R.", "China") %>%
                str_replace("Hong Kong SAR China", "China")
             )
    )
  
  mt_port_calls <- mt_port_calls %>% 
    mutate("region" = case_when(!is.na(tempRegion) ~ tempRegion, 
                                country_code == "XZ" ~ "International Waters"
    ))
  
  mt_port_calls$region <- mt_port_calls$region %>%
    str_replace(fixed("Congo - Brazzaville"), "Republic of Congo") %>%
    str_replace(fixed("Congo - Kinshasa"), "Democratic Republic of the Congo") %>%
    str_replace("St. ", "Saint ") %>%
    str_replace("Gibraltar", "Spain") %>%
    str_replace("Cote d'Ivoire", "Ivory Coast") %>%
    str_replace(fixed("Myanmar (Burma)"), "Myanmar") %>%
    str_replace(fixed("Saint Martin (French part)"), "Saint Martin") %>%
    str_replace("Wallis & Futuna", "Wallis and Futuna") %>%
    str_replace("Trinidad & Tobago", "Trinidad") %>%
    str_replace(fixed("Saint Kitts & Nevis"), "Saint Kitts") %>%
    str_replace(fixed("Macao SAR China"), "China") %>%
    str_replace(fixed("Côte d’Ivoire"), "Ivory Coast") %>%
    str_replace(fixed("São Tomé & Príncipe"), "Sao Tome and Principe") %>%
    str_replace(fixed("U.S. Virgin Islands"), "Virgin Islands") %>%
    str_replace(fixed("British Virgin Islands"), "Virgin Islands") %>%
    str_replace(fixed("Antigua & Barbuda"), "Antigua") %>%
    str_replace(fixed("Saint Vincent & Grenadines"), "Saint Vincent") %>%
    str_replace(fixed("Curaçao"), "Curacao") %>%
    str_replace(fixed("Turks & Caicos Islands"), "Turks and Caicos Islands") %>%
    str_replace("Saint Barthélemy", "Saint Barthelemy") %>%
    str_replace(fixed("Réunion"), "Reunion") %>%
    str_replace(fixed("Micronesia (Federated States of)"), "Micronesia")

mt_port_calls <- mt_port_calls %>%
  select(imo, region, timestamp_utc, port_id)


}
  # MT port calls summary
{
mt_port_calls_summary <- mt_port_calls %>%
  distinct() %>%
  group_by(region) %>%
  summarise(n=n())
}
  # IHS mapping ports
{
  mapping_ihs_ports_facilityid <- py$mapping_ihs_ports_facilityid %>%
    as_tibble() %>%
    distinct() 
  
  mapping_ihs_ports_facilityid_ii <- py$mapping_ihs_ports_facilityidii %>%
    as_tibble() %>%
    distinct() 
  
  ihs_port_locations <- mapping_ihs_ports_facilityid_ii %>%
    filter(facilityid %in% mapping_ihs_ports_facilityid$facilityid)
  
  ihs_port_locations$countryname <- ihs_port_locations$countryname %>%
    str_replace("United States of America", "USA") %>%
    str_replace("United States", "USA") %>%
    str_replace("United Kingdom", "UK") %>%
    str_replace("United Republic of Tanzania", "Tanzania") %>%
    str_replace("Hong Kong S.A.R.", "China") %>%
    str_replace("Hong Kong SAR China", "China") %>%
    str_replace("Hong Kong", "China") %>%
    str_replace("Gibraltar", "Spain") %>%
    str_replace(fixed("Korea (North)"), "North Korea") %>%
    str_replace(fixed("Korea (South)"), "South Korea") %>%
    str_replace("Cape Verde Islands", "Cape Verde") %>%
    str_replace(fixed("Congo (Republic)"), "Republic of Congo") %>%
    str_replace(fixed("Congo (Democratic Republic)"), "Democratic Republic of the Congo") %>%
    str_replace("Cote d'Ivoire", "Ivory Coast") %>%
    str_replace("Faeroe Islands", "Faroe Islands") %>%
    str_replace("St ", "Saint ") %>%
    str_replace("St-", "Saint ") %>%
    str_replace(fixed("Saint Kitts & Nevis"), "Saint Kitts") %>%
    str_replace(fixed("Virgin Islands (US)"), "Virgin Islands") %>%
    str_replace(fixed("Virgin Islands (British)"), "Virgin Islands") %>%
    str_replace("Macao", "China") %>%
    str_replace(fixed("Chinese Taipei (Taiwan)"), "Taiwan") %>%
    str_replace("Saint Helena Island", "Saint Helena") %>%
    str_replace("Madeira", "Madeira Islands") %>%
    str_replace("Antigua & Barbuda", "Antigua") %>%
    str_replace("Sao Tome et Principe", "Sao Tome and Principe") %>%
    str_replace("Trinidad & Tobago", "Trinidad") %>%
    str_replace("Wallis & Futuna Islands", "Wallis and Futuna") %>%
    str_replace("Madeira Islands Islands", "Madeira Islands") %>%
    str_replace(fixed("Turks & Caicos Islands"), "Turks and Caicos Islands") %>%
    str_replace("Saint Barthélemy", "Saint Barthelemy") 
  
  names(ihs_port_locations)[1] <- "region"
}
  # IHS mapping summary
{
  mapping_ihs_ports_summary <- mapping_ihs_ports_facilityid %>%
    group_by(region) %>%
    summarise(n=n())
}
  # sanctioned port count:
{
sanctioned_port_count <- sanctioned_ports %>%
  length()
saveRDS(sanctioned_port_count, "/sanctioned_port_count.rds")
}
  # port ID look up 
{
port_id_lookup <- py$port_id_lookup %>%
  as_tibble()
}
  # IHS facility IDs
{
ihs_facilityid <- ihs_port_calls$facilityid %>%
    unlist() %>%
    unname() %>%
    unique() %>%
    na.omit() %>%
    sort()
}
# IHS facility ID count
{
  ihs_facilityid_count <- ihs_port_calls$facilityid %>%
    unlist() %>%
    unname() %>%
    unique() %>%
    na.omit() %>%
    length() 
  
  saveRDS(ihs_facilityid_count, "~/ihs_facilityid_count.rds")
}


# The work: 
# common port ID:
{
    # UNLOCODE for sanctioned ports and IHS
    
    sanctioned_ports <- sanctioned_ports %>%
      as_tibble()
    names(sanctioned_ports) <- "port_id"
    
    sanctioned_ports <- left_join(sanctioned_ports, port_id_lookup, "port_id") %>%
      distinct()
    
    ihs_facilityid <- ihs_port_calls %>%
      select(facilityid) %>%
      dplyr::rename("port_id" = facilityid)
    
    ihs_facilityid <- left_join(ihs_facilityid, port_id_lookup, "port_id") %>%
      distinct()
    
      # sanctioned UNLOCODE count
    sanctioned_unlocode_count <- sanctioned_ports$unlocode %>%
      unique() %>%
      na.omit() %>%
      length()
    saveRDS(sanctioned_unlocode_count, "~/sanctioned_unlocode_count.rds")
    
          # sanctioned unmatched count:
    sanctioned_unlocode_na <- sanctioned_ports$unlocode %>%
      is.na() %>%
      sum()
    saveRDS(sanctioned_unlocode_na, "~/sanctioned_unlocode_na.rds")
    
      # IHS UNLOCODE count
    IHS_unlocode_count <- ihs_facilityid$unlocode %>%
      unique() %>%
      length()
    
    saveRDS(IHS_unlocode_count, "~/IHS_unlocode_count.rds")
    
          # IHS unmatched count:
    IHS_unlocode_na <- IHS_unlocode_count$unlocode %>%
      is.na() %>%
      sum()
    saveRDS(IHS_unlocode_na, "~/IHS_unlocode_na.rds")
    
    
    # IHS UNLOCODE count
    IHS_unlocode_count <- ihs_facilityid$unlocode %>%
      unique() %>%
      length()
    
    saveRDS(IHS_unlocode_count, "~/IHS_unlocode_count.rds")
    
    # IHS unmatched count:
    IHS_unlocode_na <- IHS_unlocode_count$unlocode %>%
      is.na() %>%
      sum()
    saveRDS(IHS_unlocode_na, "~/IHS_unlocode_na.rds")
    
    
    # MT unlocode to facility ID look up 
    MT_unlocode <- mt_port_calls$unlocode %>%
      distinct() %>%
      na.omit() %>%
      sort()
    
    
    MT_unlocode_count <- MT_unlocode %>%
      unique() %>%
      length()
    saveRDS(MT_unlocode_count, "~/MT_unlocode_count.rds")
    
    
    MT_unlocode <- left_join(MT_unlocode, port_id_lookup, "unlocode") %>%
      distinct()
    
        # MT facility ID 
    MT_facilityid_count <- MT_unlocode$port_id %>%
      na.omit() %>%
      length()
    saveRDS(MT_facilityid_count, "~/MT_facilityid_count.rds")
    
      # MT not matched
    MT_facility_na <- MT_unlocode$port_id %>%
      unique() %>%
      length()
    
}


# Mapping IHS port plot
mapdata_ports_ihs <- left_join(world_map, mapping_ihs_ports_summary, "region") 
mapdata1_ports_ihs <- mapdata_ports_ihs %>%
  filter(!is.na(mapdata_ports_ihs$n))

saveRDS(mapdata1_ports_ihs, '~/projectRocky/Experiment 2/mapdata1_ports_ihs.rds')

# Plotting MT port calls

      # I must tidy up countries list for plot, as seen above ^
      # I should also have a look at the location of the two in international waters
      # The port in international waters ("XZ"), is at an oil rig

mapdata_mt <- left_join(world_map, mt_port_calls_summary, "region") 
mapdata1_mt <- mapdata_mt %>%
  filter(!is.na(mapdata_mt$n))

saveRDS(mapdata1_mt, '~/projectRocky/Experiment 2/mapdata1_mt.rds')

# Plotting IHS port calls

mapdata_ihs <- left_join(world_map, ihs_port_call_summary, "region") 
mapdata1_ihs <- mapdata_ihs %>%
  filter(!is.na(mapdata_ihs$n))

saveRDS(mapdata1_ihs, '~/projectRocky/Experiment 2/mapdata1_ihs.rds')


# Experiment 3 ------------------------------------------------------------
#' Global positional data overlap

# Data import:




# Experiment 7 ------------------------------------------------------------

#' Map other vessel events such as STS and AIS against movement data

# Grabbing the data
py_run_file('~/projectRocky/07_grab.py')

# data wrangling & export
mt_stsOperations <- py$mt_stsOperations %>%
  as_tibble() %>%
  distinct()

mt_dark_activity <- py$mt_dark_activity %>%
  as_tibble() %>%
  distinct()

ihs_stsOperations <- py$ihs_stsOperations %>%
  as_tibble() %>%
  distinct()

ihs_darkActivity <- py$ihs_darkActivity %>%
  as_tibble() %>%
  distinct()


# STS 
sts_mt <- mt_stsOperations$imo %>%
  unique()
sts_mt_count <- sts_mt %>%
  length()

sts_ihs <- ihs_stsOperations$imolrorihsnumber %>%
  unique()
sts_ihs_count <- sts_ihs %>%
  length()

sts_total <- c(sts_mt, sts_ihs) %>%
  unique()

n <- length(sts_total)

i <- sum(sts_total %in% sts_mt)
sts_mt_prop <- (i/n)*100

i <- sum(sts_total %in% sts_ihs)
sts_ihs_prop <- (i/n)*100

sts_total_count <- n 

remove(n)
remove(i)

saveRDS(sts_mt_count, file = "/projectRocky/rmd_data/ex7/sts_mt_count.rds")
saveRDS(sts_ihs_count, file = "/projectRocky/rmd_data/ex7/sts_ihs_count.rds")

saveRDS(sts_total_count, file = "/projectRocky/rmd_data/ex7/sts_total_count.rds")

saveRDS(sts_mt_prop, file = "/projectRocky/rmd_data/ex7/sts_mt_prop.rds")
saveRDS(sts_ihs_prop, file = "/projectRocky/rmd_data/ex7/sts_ihs_prop.rds")

remove(sts_mt_count)
remove(sts_ihs_count)
remove(sts_total)
remove(sts_total_count)
remove(sts_mt_prop)
remove(sts_ihs_prop)


# Dark activity

dark_mt <- mt_dark_activity$imo %>%
  unique()
dark_mt_count <- dark_mt %>%
  length()


dark_ihs <- ihs_darkActivity$imolrorihsnumber %>%
  unique()
dark_ihs_count <- dark_ihs %>%
  length()


dark_total <- c(dark_mt, dark_ihs) %>%
  unique()
dark_total_count <- dark_total %>%
  length()

n <- dark_total_count

i <- sum(dark_total %in% dark_mt)
dark_mt_prop <- (i/n)*100

i <- sum(dark_total %in% dark_mt)
dark_ihs_prop <- (i/n)*100

remove(n)
remove(i)

saveRDS(dark_mt_count, file = "/projectRocky/rmd_data/ex7/dark_mt_count.rds")
saveRDS(dark_ihs_count, file = "/projectRocky/rmd_data/ex7/dark_ihs_count.rds")

saveRDS(dark_total_count, file = "/projectRocky/rmd_data/ex7/dark_total_count.rds")

saveRDS(dark_mt_prop, file = "/projectRocky/rmd_data/ex7/dark_mt_prop.rds")
saveRDS(dark_ihs_prop, file = "/projectRocky/rmd_data/ex7/dark_ihs_prop.rds")

remove(dark_mt_count)
remove(dark_ihs_count)
remove(dark_total)
remove(dark_total_count)
remove(dark_mt_prop)
remove(dark_ihs_prop)






# Experiment 8 ------------------------------------------------------------

#' Undeclared port calls



# Future Work -------------------------------------------------------------

rstudioapi::restartSession()

require("tidyverse")

# To potentially explore: 
require("spatial")
require("spsurvey")


#' Notes:
#' 
#' A place to pull together exciting next steps.
#' https://bookdown.org/ejvanholm/Textbook/spatial-models.html
#' https://ourcodingclub.github.io/tutorials/spatial/index.html#:~:text=The%20sp%20package%20is%20central%20for%20spatial%20data,is%20commonly%20used%20to%20represent%20spatially%20continuous%20data.
#' 
#' The above URLs shows some potential examples
#' 
#' I'm particularly interested in grid analysis. 






