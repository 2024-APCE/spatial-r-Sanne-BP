# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
#setwd("G:/Shared drives/_Org OlffLab/Teaching/APCE/APCE2024/APCE2024GIS")
setwd("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/Advanced Population and Community Ecology/APCE2024/apce2024gis")


# restore the libraries of the project 
renv::restore()

install.packages("remotes")
remotes::install_github("rspatial/terra")
install.packages("terra")

# load the different libraries
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot

# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,10), col = grey.colors(10))
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = rev(terrain.colors(10)))
library(RColorBrewer) 
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))

barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis)
#for reversing color: rev
barplot(rep(1,10), col = rev(viridis::viridis(10)))
barplot(rep(1,10), col = viridis::plasma(10))

viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data for the whole ecosystem
protected_areas
plot(protected_areas)
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
            layer="protected_areas_2022") # read protected area boundaries)
sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/studyarea.gpkg",
                              layer="my_study_area")


# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)
class(elevation)
plot(protected_areas)
plot(elevation)
plot(protected_areas, add=T)

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

# plot the woody biomass map that you want to predict
#class:
Sys.setenv(PROJ_LIB = "/opt/homebrew/Cellar/proj/9.5.0/share/proj")

woody_map <- ggplot() + 
  tidyterra::geom_spatraster(data=woodybiom) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77, 6.55),
                       oob=squish,
                       name = "TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red",linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Woody Biomass")+
  coord_sf(xlim=xlimits,ylim=ylimits,datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
woody_map
                             
ggsave("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/GITHUB/spatial-r-Sanne-BP/figures/woody_map.png", woody_map, width=10, height=10, dpi=300)

# plot the elevation map
elevation_map<-ggplot() +
  tidyterra::geom_spatraster(data=elevation) +
  scale_fill_gradientn(colours=terrain.colors(10),
                       limits=c(1500,3000),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=1,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="deepskyblue2",linewidth=0.5) +
  labs(title="Elevation") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
elevation_map  

#plot the rainfall map 
rain <- rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rain <- terra::resample(rainfall, rain, method = "bilinear")  

rain_map<-ggplot() +
  tidyterra::geom_spatraster(data=rain) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(600,1000),
                       oob=squish,
                       name="mm/yr") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="deepskyblue2",linewidth=0.5) +
  labs(title="Rainfall") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rain_map 



# combine the different maps  into one composite map using the patchwork library
# and save it to a high resolution png
all_maps<-woody_map +elevation_map + rain_map+
  patchwork::plot_layout(ncol=2)
all_maps
ggsave("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/GITHUB/spatial-r-Sanne-BP/figures/all_maps.png", width = 18, height = 18, units = "cm",dpi=300)



############################
### explore your study area
# set the limits of your study area
xlimits<-sf::st_bbox(studyarea)[c(1,3)]
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)
saExt

# crop the woody biomass to the extent of the studyarea
woodybiom_sa <- terra::crop(woodybiom, saExt)

# plot the woody biomass
woody_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=woodybiom_sa) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77, 6.55),
                       oob=squish,
                       name = "TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red",linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Woody Biomass")+
  coord_sf(xlim=xlimits,ylim=ylimits,expand=F,
           datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
woody_map_sa

#elevation data
elevation_sa<-terra::crop(elevation,saExt) # crop to study area
elevation_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=elevation_sa) +
  scale_fill_gradientn(colours=terrain.colors(6),
                       limits=c(1500,3000),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="deepskyblue2",linewidth=0.5) +
  labs(title="Elevation") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
elevation_map_sa  

# make distance to river map
dist2river100_sa<-terra::rast("./2022_rivers/DistanceToRiver_100km.tif")
map_dist2river100_sa<-ggplot() +
  tidyterra::geom_spatraster(data=dist2river100_sa) +
  scale_fill_gradientn(colours=topo.colors(6),
                       limits=c(0,30000),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="deepskyblue2",linewidth=0.5) +
  labs(title="Distance to river") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
map_dist2river100_sa

#rainfall Han
rainfall_30m <- rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rainfall_30m <- terra::resample(rainfall, rainfall_30m, method = "bilinear")  
rainfall_sa<-terra::crop(rainfall_30m,saExt) # crop to study area

pal_blues <- RColorBrewer::brewer.pal(9, "Blues")

rainfall_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(600,1000),
                       oob=squish,
                       name="mm/yr") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="deepskyblue2",linewidth=0.5) +
  labs(title="Rainfall") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rainfall_map_sa  

#Copernicus tree cover
tree_cover_sa<-terra::rast("./2019_copernicus_treecover/copernicus_tree_coverOWN.tif")

pal_greens <- brewer.pal(9, "Greens")

map_tree_cover_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=tree_cover_sa) +
  scale_fill_gradientn(colours=pal_greens,
                       limits=c(0, 100),
                       oob=squish,
                       name = "cover fraction") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red",linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Copernicus tree cover")+
  coord_sf(xlim=xlimits,ylim=ylimits,expand=F,
           datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
map_tree_cover_sa

#Core protected areas
r<-terra::rast("./_MyData/CoreProtectedAreas.tif") 
CoreProtectedAreas_sa <- r |> #  replace NA by 0
  is.na() |>
  terra::ifel(0,r) 

CoreProtectedAreas_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(CoreProtectedAreas_sa)) +
  scale_fill_manual(values=c("grey","lightgreen"),
                    labels=c("no","yes")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="deepskyblue2",linewidth=0.5) +
  labs(title="Core protected areas") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
CoreProtectedAreas_map_sa

# landform valleys and plains (CEC)
landform_sa<-terra::rast("./_MyData/landforms.tif")
landform_sa <- terra::as.factor(landform_sa)
landform_map_sa<-ggplot() + 
  tidyterra::geom_spatraster(data=landform_sa) +
  scale_fill_manual(
    values = c(
      "11" = "#141414", "12" = "#383838", "13" = "#808080", 
      "14" = "#ebeb8f", "15" = "#f7d311", "21" = "#aa0000", 
      "22" = "#d89382", "23" = "#ddc9c9", "24" = "#dccdce", 
      "31" = "#1c6330", "32" = "#68aa63", "33" = "#b5c98e", 
      "34" = "#e1f0e5", "41" = "#a975ba", "42" = "#6f198c"
    ),
    breaks = c("11", "12", "13", "14", "15", "21", "22", "23", "24", "31", "32", "33", "34", "41", "42"),  # Define the range explicitly
    na.value = "grey",             # Set a color for NA values
    labels = c("Peak/ridge (warm)", "Peak/ridge", "Peak/ridge (cool)", "Mountain/divide", "Cliff",
               "Upper slope (warm)", "Upper slope", "Upper slope (cool)", "Upper slope (flat)",
               "Lowerslope (warm)", "Lower slope", "Lower slope (cool)", "Lower slope (flat)", "Valley",
               "Valley (narrow)"),
    name = "landform types") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3") +
  tidyterra::geom_spatvector(data=rivers,
                             color="deepskyblue2") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, color="#F11B00", linewidth=1) +
  labs(title="Landform") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.2)  
landform_map_sa

ggsave("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/GITHUB/spatial-r-Sanne-BP/figures/landforms.png", landform_map_sa, width=8, height=8, dpi=300)

#Add Hills
landform_hills_sa<-terra::rast("./_MyData/hills.tif")
landform_hills_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(landform_hills_sa)) +
  scale_fill_manual(values=c("black","orange"),
                    labels=c("valleys\nand\nplains","hills")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.7) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=1,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="deepskyblue2",linewidth=0.5) +
  labs(title="Landform hills") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
landform_hills_map_sa

#Add ValleyPlains
landform_vp_sa<-terra::rast("./_MyData/valleysPlains.tif")
landform_vp_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(landform_vp_sa)) +
  scale_fill_manual(values=c("black","orange"),
                    labels=c("hills","valleys\nand\nplains")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.7) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=1,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="deepskyblue2",linewidth=0.5) +
  labs(title="Landform Valleys Plains") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
landform_vp_map_sa

# soil cation exchange capacity (CEC)
cec_sa<-terra::rast("./_MyData/CEC_5_15cm.tif")
hist(cec_sa)
cec_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=cec_sa) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(100,310),
                       oob=squish,
                       name="Soil CEC\n5-15cm") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="transparent",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Soil CEC") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
cec_map_sa

#ADD MORE VARIABLES::

#Year last burned
lastburn_sa<-terra::rast("./_MyData/YearLastBurned.tif")
pal_reds <- RColorBrewer::brewer.pal(9, "Reds")

lastburn_sa_map <- ggplot() + 
  tidyterra::geom_spatraster(data=lastburn_sa) +
  scale_fill_gradientn(colours=pal_reds,
                       limits=c(2000, 2016),
                       oob=squish,
                       name = "Year") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red",linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Year since last burn")+
  coord_sf(xlim=xlimits,ylim=ylimits,expand=F,
           datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

lastburn_sa_map <- ggplot() + 
  tidyterra::geom_spatraster(data = lastburn_sa) +
  scale_fill_gradientn(colours = pal_reds,               
                       limits = c(2000, 2016),          
                       oob = squish,                    
                       name = "Year",                   
                       breaks = c(2000, 2004, 2008, 2012, 2016), 
                       labels = c("≤2000", "2004", "2008", "2012", "2016")) +
  tidyterra::geom_spatvector(data = protected_areas, fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, colour = "deepskyblue2", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, fill = NA, colour = "red", linewidth = 1) +
  tidyterra::geom_spatvector(data = lakes, fill = "royalblue3", linewidth = 0.5) +
  labs(title = "Year since last burn") +
  coord_sf(xlim = xlimits, 
           ylim = ylimits, 
           expand = FALSE,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

lastburn_sa_map

# burning frequency map from 2001 - 2016
burnfreq_sa<-terra::rast("./_MyData/BurnFreq.tif")
burnfreq_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=burnfreq_sa) +
  scale_fill_gradientn(colours=pal_reds,
                       limits=c(0,5),
                       oob=squish,
                       name="years\nburned") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=1,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="deepskyblue2",linewidth=0.5) +
  labs(title="Number of years burned") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
burnfreq_map_sa

#Population density
popdens_sa<-terra::rast("./_MyData/population_density.tif")
popdens_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=popdens_sa) +
  scale_fill_gradientn(colours=pal_reds,
                       limits=c(0, 70),
                       oob=squish,
                       name = "density") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red",linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Population density")+
  coord_sf(xlim=xlimits,ylim=ylimits,expand=F,
           datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
popdens_map_sa

# add evaporation
evaporation_sa<-terra::rast("./_MyData/actual_evapotranspiration.tif")

evaporation_30m <- rast(terra::ext(evaporation_sa), resolution = 30, crs = crs(evaporation_sa))
# Resample the raster to 30m resolution
evaporation_30m <- terra::resample(evaporation_sa, evaporation_30m, method = "bilinear")  
evaporation_30m <-terra::crop(evaporation_30m,saExt) # crop to study area

map_evaporation_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=evaporation_sa) +
  scale_fill_gradientn(colours=pal_greens,
                       limits=c(200, 700),
                       oob=squish,
                       name = "mm") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5)+
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5)+
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red",linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Actual Evapotranspiration")+
  coord_sf(xlim=xlimits,ylim=ylimits,expand=F,
           datum = sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
map_evaporation_sa


### put all maps together
all_maps_sa <- woody_map_sa +elevation_map_sa +map_dist2river100_sa + rainfall_map + map_tree_cover_sa + CoreProtectedAreas_map_sa  + landform_hills_map_sa  +cec_map_sa+ burnfreq_map_sa+ + popdens_map_sa + map_evaporation_sa +
  patchwork::plot_layout(ncol=3)
all_maps_sa
ggsave("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/GITHUB/spatial-r-Sanne-BP/figures/all_maps_sa.png", width = 27, height = 27, units = "cm",dpi=300)

ggsave("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/GITHUB/spatial-r-Sanne-BP/figures/landforms.png", landform_map_sa, width=10, height=10, dpi=300)

# make maps also for the other layers that you found

# create 250 random points in your study area
set.seed(123)
rpoints <- terra::spatSample(studyarea, size = 250, 
                             method = "random")
# plot the points
rpoints_map_sa<-ggplot() +
  tidyterra::geom_spatvector(data=rpoints, size=0.5) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="deepskyblue2",linewidth=0.5) +
  labs(title="250 random points") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rpoints_map_sa


# and add them to the previous map
all_maps_sa<-woody_map_sa +elevation_map_sa +map_dist2river100_sa +rainfall_map_sa  +map_tree_cover_sa +
  CoreProtectedAreas_map_sa  + landform_hills_map_sa  +cec_map_sa+ burnfreq_map_sa+ popdens_map_sa+map_evaporation_sa +
  rpoints_map_sa +
  patchwork::plot_layout(ncol=3)
all_maps_sa
ggsave("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/GITHUB/spatial-r-Sanne-BP/figures/all_maps_sa.png", width = 27, height = 27, units = "cm",dpi=300)

# extract your the values of the different raster layers to the points
#########################
# extract your the values of the different raster layers to the points
# Extract raster values at the points
woody_points <- terra::extract(woodybiom_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(woody=TBA_gam_utm36s)
woody_points

dist2river_points <- terra::extract(dist2river100_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(dist2river=distance)
dist2river_points

elevation_points <- terra::extract(elevation, rpoints) |> 
  as_tibble() 
elevation_points

CorProtAr_points <- terra::extract(CoreProtectedAreas_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(CorProtAr=CoreProtectedAreas)
CorProtAr_points

rainfall_points <- terra::extract(rainfall_sa, rpoints) |> 
  as_tibble() |> 
  dplyr::rename(rainfall=CHIRPS_MeanAnnualRainfall)
rainfall_points

cec_points <- terra::extract(cec_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(cec='cec_5-15cm_mean')
cec_points
cec_points <- cec_points[cec_points$cec != 0, ] #filtering out cec=0, as these are in the lake

burnfreq_points <- terra::extract(burnfreq_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(burnfreq=burned_sum)
burnfreq_points

landform_points <- terra::extract(landform_hills_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(hills=remapped)
landform_points

copernicus_points <- terra::extract(tree_cover_sa, rpoints) |> 
  as_tibble()|>
  dplyr::rename(treecover="tree-coverfraction")
copernicus_points

population_points <- terra::extract(popdens_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(popdensity=population)
population_points

evaporation_points <- terra::extract(evaporation_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(evaporation=aet)
evaporation_points

# make long format

# plot how woody cover is predicted by different variables
pointdata<-cbind(dist2river_points[,2],elevation_points[,2],rainfall_points[,2], 
                 landform_points[,2],cec_points[,2], burnfreq_points[,2], copernicus_points[,2],
                 population_points[,2], evaporation_points[,2], woody_points[,2]) |>
  as_tibble()
pointdata
pointdata <- pointdata[complete.cases(pointdata),]
pointdata

getwd()
readr::write_csv(pointdata, "pointdata.csv")

# plot how woody cover is predicted by different variables
# Create a correlation panel plot
library(psych)
psych::pairs.panels(
  pointdata ,
  method = "pearson",     # Correlation method (use "spearman" for rank correlation)
  hist.col = "lightblue",  # Color for histograms
  density = TRUE,          # Add density plots
  ellipses = F,         # Add correlation ellipses
  lm = TRUE,                # Add linear regression lines
  stars=T
)

ggsave("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/GITHUB/spatial-r-Sanne-BP/figures/correlation_panel_plot.png", width = 10, height = 10, units = "cm",dpi=300)

# make long format
names(pointdata)
pointdata_long<-pivot_longer(data=pointdata,
                             cols = dist2river:evaporation, # all except woody
                             names_to ="pred_var",
                             values_to = "pred_val")
pointdata_long

# panel plot
ggplot(data=pointdata_long, mapping=aes(x=pred_val,y=woody,group=pred_var)) +
  geom_point() +
  geom_smooth() +
  ylim(0,40) +
  facet_wrap(~pred_var,scales="free") 

# do a pca
# Load the vegan package
library(vegan)
# Perform PCA using the rda() function
pca_result <- vegan::rda(pointdata,
                         scale = TRUE)
# Display a summary of the PCA
summary(pca_result)

# Plot the PCA
plot(pca_result, scaling = 2, type="n", xlab="",ylab="")  # Use scaling = 1 for distance preservation, scaling = 2 for correlations
# Add points for samples
points(pca_result, display = "sites",  col = pointdata$hills+1, bg = "blue", cex = 1)
# Add arrows for variables
arrows(0, 0, scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
       length = 0.1, col = "red")
# Label the variables with arrows
text(scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
     labels = colnames(pointdata), col = "red", cex = 0.8, pos = 4)
# Add axis labels and a title
title(main = "PCA Biplot")
xlabel <- paste("PC1 (", round(pca_result$CA$eig[1] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
ylabel <- paste("PC2 (", round(pca_result$CA$eig[2] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
title(xlab=xlabel)
title(ylab=ylabel)
# add contours for woody cover
vegan::ordisurf(pca_result, pointdata$woody, add = TRUE, col = "green4")

#First axis PCA1 explains 32.7% of the variation, PCA2 15.9% --> that's good, so ...% in total is explained by the chosen variables 
#first axis: the slope of the regression is the score on the axis, sites as a point, variable scores as a vector

readr::write_csv(pointdata_long, "pointdata_long.csv")



