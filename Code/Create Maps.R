## Author: Luke Ozsanlav-Harris
## Created: 01/02/2023

## Create range map of YBW and Chiffchaffs for manuscript

## Packages required
pacman::p_load(tidyverse, data.table, sf, raster, rnaturalearth, patchwork,
               ggspatial, marmap, ggnewscale, cowplot, ggpubr, png, ggpattern)




#-----------------------------------#
#### 1. Read in the Spatial data ####
#-----------------------------------#

## BirdLife data ##

## Citation for BirdLife data: BirdLife International and Handbook of the Birds of the World (2022) Bird species distribution maps of the world. Version 2022.2. Available at http://datazone.birdlife.org/species/requestdis.
## NOTE:
## Either an electronic or two paper copies of all products published using data supplied 
## by BirdLife International will be sent, free of charge, to the BirdLife Global Secretariat at:
## BirdLife International, The David Attenborough Building, Pembroke Street, 
## Cambridge, CB2 3QZ, UK or via email to science@birdlife.org 

## Read in the birdlife range maps
## Each row refers to the following distributions
## Chiff 1- Coll Non-breeding
## Chiff 2- Coll Native resident
## Chiff 3- Coll Breeding
## Chiff 4- Coll Passage

## Chiff 5- Tris Breeding
## Chiff 6- Tris Non-breeding

## Yellow Brow 7- Non-breeding
## Yellow Brow 8- Breeding
## Yellow Brow 9- Passage
BL <- st_read(dsn = "Spatial/Birdlife Data/", layer = "SppDataRequest")
st_crs(BL)

## plot some of the maps
# plot(BL$geometry[6]) 

## Remove the rows I do not need, i.e. the passage ones
BL_sub <- BL %>%  filter(!row_number() %in% c(4, 9))

## write out this object
# st_write(BL, "Spatial/Birdlife Data/SppDataRequest_cropped.shp")

## Create a separate object for each species
Col <-  BL_sub %>%  filter(sci_name == "Phylloscopus collybita")
Tris <-  BL_sub %>%  filter(sci_name == "Phylloscopus tristis")
YB <-  BL_sub %>%  filter(sci_name == "Phylloscopus inornatus")


## Precip istotope data ##

## Read in the global prcip dataset
Precip <- raster("Spatial/Global_precip/d2h_GS.tif")
# plot(Precip)




#-----------------------------------#
#### 2. Download global basemaps ####
#-----------------------------------#

## get coast outlines
countries <- ne_coastline(scale = "medium", returnclass = "sf")
Russia <- ne_countries(scale = "medium", returnclass = "sf", country = "russia")
countries2 <- ne_countries(scale = "medium", returnclass = "sf")




#----------------------------------------------#
#### 3. Create range map for Chiffs and YBW ####
#----------------------------------------------#

# Create seasonal categories
Col$seasonal_type <- factor(
  Col$seasonal,
  levels = c(1, 2, 3),
  labels = c("Res", "Breed", "Non-breed")
)

YB$seasonal_type <- factor(
  YB$seasonal,
  levels = c(1, 2, 3),
  labels = c("Res", "Breed", "Non-breed")
)

Tris$seasonal_type <- factor(
  Tris$seasonal,
  levels = c(1, 2, 3),
  labels = c("Res", "Breed", "Non-breed")
)


# Create plot
m <- ggplot() +
  
  # Countries background
  geom_sf(
    data = countries2,
    aes(geometry = geometry),
    fill = "#BDC3C7",
    colour = "#BDC3C7",
    alpha = 0.6
  ) +
  
  new_scale_fill() +
  
  # P. c. collybita
  geom_sf(data = Col,
          aes( geometry = geometry, fill = seasonal_type), colour = NA, alpha = 1
  ) +
  
  scale_fill_manual(
    name = expression("P. c. collybita"),
    values = c(
      "Res" = "#6f9969",
      "Breed" = "#9fc28f",
      "Non-breed" = "#3f7040"
    )) +
  
  new_scale_fill() +
  
  # P. inornatus
  geom_sf(
    data = YB,
    aes(geometry = geometry,fill = seasonal_type),
    colour = NA, alpha = 1
  ) +
  
  scale_fill_manual(
    name = expression("P. inornatus"),
    values = c(
      "Res" = "#808fe1",
      "Breed" = "#aeb8f3",
      "Non-breed" = "#5365c4"
    )) +
  
  new_scale_fill() +
  
  # P. c. tristis - striped transparent overlay
  geom_sf_pattern(
    data = Tris,
    aes(geometry = geometry, pattern_fill = seasonal_type
    ),
    fill = NA,
    colour = NA,
    pattern = "stripe",
    pattern_angle = 45,
    pattern_colour = NA,
    pattern_density = 0.6,
    pattern_spacing = 0.01,
    pattern_size = 0.08,
    pattern_alpha = 0.9,
  ) +
  
  scale_pattern_fill_manual(
    name = expression("P. c. tristis"),
    values = c(
      "Res" = "#efc86e",
      "Breed" = "#f6dda0",
      "Non-breed" = "#d89f2e"
    )
  ) +
  
  # Map scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.1,
    pad_y = unit(0.25, "in")
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.5, "in"),
    style = north_arrow_orienteering,
    height = unit(0.8, "cm"),
    width = unit(0.5, "cm")
  ) +
  
  # Map limits
  coord_sf(xlim = c(-20, 180), ylim = c(-5, 80),
           crs = 4326, expand = FALSE  ) +
  
  # Legends
  guides(fill = guide_legend(order = 1), pattern_fill = guide_legend(order = 3) ) +
  
  # Styling
  theme_light() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 10,face = "italic", margin = margin(l = 2)),
    legend.position = "bottom",
    axis.title = element_blank(),
    legend.title = element_text(size = 12, face = "bold"),
    legend.box.background = element_rect(colour = "#BDC3C7" ),
    legend.box.margin = margin(0.5, 0.5, 0.5, 0.5),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.spacing.x = unit(0.05, "cm"))



## Save the map
ggsave(plot = m, 
       filename = "Outputs/Fig2_Range_map.png",
       units = "mm", width = 230, height = 150, dpi = 360,   
)




#------------------------------#
#### 4. Create Isoscape map ####
#------------------------------#

## re porject raster to the same as the BirdLife data
## I think it was already the same anyway
Precip <- projectRaster(Precip, crs = crs(BL))

## crop the raster
b <- as(extent(-20, 180, 28, 80), 'SpatialPolygons')
crs(b) <- crs(Precip)
Precip_cr <- crop(Precip, b)

## fortify for plotting
Precip_pts <- rasterToPoints(Precip_cr, spatial = TRUE)
# Then to a 'conventional' dataframe
Precip_df  <- data.frame(Precip_pts)
#glimpse(Precip_df)



## Create shapefile of just breeding areas
BL_sub2 <- BL %>%  filter(row_number() %in% c(2,3,5,8))
Col2 <-  BL_sub2 %>%  filter(sci_name == "Phylloscopus collybita")
Tris2 <-  BL_sub2 %>%  filter(sci_name == "Phylloscopus tristis")
YB2 <-  BL_sub2 %>%  filter(sci_name == "Phylloscopus inornatus")
# glimpse(Col2)

## merge and simplfy the collybita range
Col3 <- Col2 %>% st_union() %>% st_simplify(dTolerance = 15000)

## create the plot of the istopes and ranges
Im <- ggplot() + 
  
  # Render the isotope map
  geom_raster(data = Precip_df, aes(x=x, y=y, fill=d2h_GS)) +
  scale_fill_viridis_c(option = "magma", name = expression(delta^2*H*"  "*("‰")), alpha = 0.7) +
  new_scale_fill() +
  
  
  # add the range areas
  # geom_sf(data = Col3, aes(geometry = geometry, colour = "#6f9969"), fill = NA, alpha = 1, linetype = "solid", size = 0.8) +
  # geom_sf(data = YB2, aes(geometry = geometry, colour = "#808fe1"), fill = NA, alpha = 1, linetype = "solid", size = 0.8) +
  # geom_sf(data = Tris2, aes(geometry = geometry, colour = "#efc86e"), fill = NA, alpha = 1, linetype = "solid", size = 0.8) +
  # scale_colour_manual(name = expression("Taxa"*":"),
  #                   values =c("#6f9969"="#6f9969", "#808fe1"="#808fe1", "#efc86e"="#efc86e"),
  #                   labels = c("P. c. collybita", "P. inornatus", "P. c. trisits")) +
  
  # add map extras
  annotation_scale(location = "br", width_hint = 0.1, pad_y = unit(0.15, "in")) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_orienteering,
                         height = unit(1, "cm"), width = unit(0.7, "cm"),) +
  
  # set amp limits
  coord_sf(xlim = c(-12, 180),
           ylim = c(28, 80), crs = 4326, expand = F) +
  
  # Plot styling
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text=element_text(size=10, face="italic"), legend.position = "bottom",
        axis.title = element_blank(), legend.title= element_text(size=12, face="bold"),
        legend.box="horizontal")



## save the combined plot
ggsave(plot = Im, 
       filename = "Outputs/Fig1_Isotope_map.png",
       units = "mm", width = 200, height = 130, dpi = 300,   
)

