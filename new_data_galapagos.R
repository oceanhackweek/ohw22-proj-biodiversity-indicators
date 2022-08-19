rm(list=ls())

# Packages employed ----
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(sf)
library(ggspatial) #annotation_north_arrow(), annotation_scale()
#remotes::install_github("marinebon/obisindicators")
library(obisindicators)
#remotes::install_github("ropensci/rerddap")
library(rerddap) #en el caso 
library(tidyverse) 
library(readxl)

# Data recently collected ----
data <-     
  
  #load data
  read_excel("OHW_ECOM_FCD.xlsx",.name_repair = "universal")%>% 
  
  #remove blank lines
  filter(!is.na(Year)) 

# Retrieve number of species for each site to compare with OBIS data
sp_gal <- 
  
  data %>% 
  
  #remove rows with empty latuitude or longitude data
  filter(!is.na(Lat_Dec) | !is.na(Long_dec)) %>% 
  
  #remove rows with longitude 0 (probably a mistake while passing data)
  filter(Long_dec!=0) %>% 
  
  #select columns
  select(Year, Dive.Date, Lat_Dec, Long_dec, ScientificName) %>% 
  
  #rename variable
  rename(species = ScientificName) %>% 
  
  #group data according to diving site
  group_by(Lat_Dec, Long_dec, species) %>% 
  
  #count the number of records in that position
  mutate(records = n()) %>% 

  #remove duplicate rows for species
  distinct(Lat_Dec, Long_dec, species, .keep_all = TRUE) %>% 

  #generate new variable (to match obis dataset)
  #mutate(date_year = paste(Year, Dive.Date, sep = "_")) %>% 
  
  as_tibble()


# Create a discrete global grid using the dggridR package:
dggs_gal <- dgconstruct(projection = "ISEA", topology = "HEXAGON", res = 11)


# Then assign cell numbers to the occurrence data:
sp_gal$cell <- dgGEO_to_SEQNUM(dggs_gal, sp_gal$Long_dec, sp_gal$Lat_Dec)[["seqnum"]]

# Calculate indicators
  # Perform the calculation on species level data:
idx_gal <- calc_indicators(sp_gal)

  # missing data generated that max(idx_gal$cell) was > than dgmaxcell(dggs) 
  # so cell geometries could not be added unless this was fixed

# Add cell geometries to the indicators table (idx):
grid_gal <- 
  dgcellstogrid(dggs_gal, idx_gal$cell) %>% 
  st_wrap_dateline() %>%
  rename(cell = seqnum) %>%
  left_join(
    idx_gal,
    by = "cell")

# Plot maps of indicators
  # We will personalize the gmap_indicator function.

gmap_indicator_gal <- function(
  grid_gal, column = "shannon", label = "Shannon index", trans = "identity",
  crs="+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){
  
  world <- rnaturalearth::ne_countries(scale = "large",type = "countries", returnclass = "sf")
  bb <- sf::st_bbox(
    st_transform(grid_gal, crs))
  
  ggplot() +
    geom_sf(
      data = grid_gal, aes_string(
        fill = column, geometry = "geometry"), lwd = 0) +
    viridis::scale_color_viridis(
      option = "viridis", na.value = "white",
      limits = c(0,5),
      name = label, trans = trans) +
    viridis::scale_fill_viridis(
      option = "viridis", na.value = "white",
      limits = c(0,5),
      name = label, trans = trans, alpha=0.75) +
    geom_sf(
      data = world, fill = "grey30", 
      color = "white",lwd=0.5) +
    theme_bw() +
    xlab("") + ylab("") +
    coord_sf(
      crs  = crs,
      xlim = bb[c("xmin","xmax")],
      ylim = bb[c("ymin","ymax")])
}

# Resulting indicator in map form.

# Shannon index
gmap_indicator_gal(grid_gal, "shannon", label = "Shannon index")
