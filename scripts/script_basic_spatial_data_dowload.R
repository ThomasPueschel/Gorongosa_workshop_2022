# Species occurrence maps
# The package rgbif offers an interface to the Web Service methods provided by GBIF. 
# It includes functions for searching for taxonomic names, retrieving information on data providers, 
# getting species occurrence records and getting counts of occurrence records.
library("rgbif")

Mozambique_code<- isocodes[grep("Mozambique", isocodes$name), "code"]
occur <- occ_search(scientificName = "Kobus ellipsiprymnus", country = Mozambique_code, hasCoordinate = TRUE, limit = 3000, year = '2006,2021')

# This will return a dataset of all the occurrences of waterbucks recorded in the Mozambique
# between 2006 and 2021 that have geographic coordinates.
str(occur)

# plot
library(ggplot2)
library(maps)
library(ggthemes)

(map <- ggplot(occur$data, aes(x = decimalLongitude, y = decimalLatitude)) + 
    # Specify to only present the UK region of the world in the map 
    # Also change the colour, size of map country borders
    borders(database = "world", regions = "Mozambique", colour = "gray40", size = 0.3) +  
    theme_map() + 
    # Change the colour and transparency of the plotted occurrence points 
    geom_point(alpha = 0.4, colour = "red")) 

