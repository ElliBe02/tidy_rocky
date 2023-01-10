## ---------------------------
##
## Script name: tidy_map.R
##
#' Purpose of script:
#' To present the mapping functions of R
#'
## Author: Benjamin Elliot
##
## Date Created: 2023-01-10
##
## ## Email: Benjamin.Elliot@LexisNexisRisk.com
##
## ---------------------------
##
#' Notes:
#'   This work was originally done as part of project rocky 🚢
##
## ---------------------------
##
## GitHub:
##
usethis::create_github_token() 
gitcreds::gitcreds_set() 

##
## ---------------------------

require("tidyverse")
require("sp")
require("rworldmap")

# Regional heat map --------------------------------------------------------

world_map <- map_data("world")

    # test data
    regions <- unique(world_map$region) %>%
      sort()
    
    selected <- NULL
    for (i in 1:2000) {
      
      anon_func <- function(x){
        temp <- regions[x]
        return(temp)
      }
      
      temp <- sample(1:length(regions), size = 1) %>%
        anon_func()
      
      selected <- append(selected, temp) 
    }
    
    data_summary <- c(regions, selected)%>%
      as_tibble()
    names(data_summary) <- "region"
    data_summary <- data_summary %>%
      group_by(region) %>%
      summarise(n=n())
    

    mapdata <- left_join(world_map, data_summary, "region") 
    mapdata <- mapdata %>%
      filter(!is.na(mapdata$n))
    
    glimpse(mapdata)

# plotting 

map1 <- ggplot(mapdata, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = n), color = "black") +
  theme_minimal()


# lat long points plotted -------------------------------------------------

    # test data:
    df <- matrix(data = c(
      174.828553, -56.559953, 
      -142.887771, 66.463444, 
      111.943066, -47.279667
      ), 
                 byrow = T, ncol = 2) %>%
      as_tibble()
    names(df) <- c("LON", "LAT")


world_map <- map_data("world")


#Creat a base plot with gpplot2
p <- ggplot() + 
  coord_fixed() + 
  xlab("") + 
  ylab("")


#Add map to base plot
base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="grey", fill=" blue")

#Strip the map down so it looks super clean (and beautiful!)
cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup

#Add simple data points to map
map_data <- 
  base_world +
  geom_point(data=df, 
             aes(x=LON, y=LAT), colour="Grey", 
             fill="#ed1c24",pch=21, size=5, alpha=I(0.7))
map_data 




# UK example --------------------------------------------------------------

# the following code outputs to the "Viewer" (local web content)

require("sp")
require("raster")
require("leaflet")

  # download uk data level 2 from GADM  
  uk <- getData('GADM', country='GBR', level = 2)  

  # creating a colour palette that provides a diff colour for regions
  # in different country i.e., Scotland, Ireland, Wales, etc.  
  pal <- colorFactor("Reds", uk$GID_2)

# plotting:
leaflet(uk) %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(GID_2),
              highlightOptions = highlightOptions(color = "white", weight =2,
                                                  bringToFront = TRUE),
              label = ~paste0(NAME_2, ", ", NAME_1)) 

