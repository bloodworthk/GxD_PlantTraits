##########################################################################################################
#Project: Dissertation: Community Weighted Plant Traits in MGP for Drought x Grazing 

#Coder: Kathryn Bloodworth

#Created: 12/14/2021
##########################################################################################################


#### Load Libraries ####

# lots of libraries that mostly get the base map shapes and colors
#devtools::install_github("ropenscilabs/rnaturalearth")
library("rnaturalearth") #https://cran.r-project.org/web/packages/rnaturalearth/rnaturalearth.pdf
library(sp)
#install.packages("rnaturalearthdata")
library("rnaturalearthdata") #https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-physical-labels/
library("rnaturalearthhires")
#install.packages("maps")
library(maps)
#install.packages("reshape")
library(reshape)
#install.packages("mapproj")
library(mapproj)
#install.packages("sf")
library(sf)
#install.packages("raster")
library(raster)
#install.packages("rgdal")
#library(rgdal)
#install.packages("tmap")
library(tmap) 
library(ggplot2)
library(tidyverse)
# setting the color palatte
# install.packagesTR("devtools")
#devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)
GeneralGrantpal<-park_palette("GeneralGrant", 7)
CraterLakepal <- park_palette("CraterLake", 7)


#### Set Working Directory ####
#Bloodworth - Mac
setwd("/Users/kathrynbloodworth/Dropbox (Smithsonian)/Projects/Dissertation/Data")

#### Set ggplot base ####
#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(axis.title.x=element_text(size=30, vjust=-0.35, margin=margin(t=15)),
             axis.text.x=element_text(size=30), axis.title.y=element_text(size=30, angle=90, vjust=0.5,
                                                                          margin=margin(r=15)), axis.text.y=element_text(size=30), plot.title =
               element_text(size=30, vjust=2), panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(), legend.title=element_blank(),
             legend.text=element_text(size=30))

#### Read in Data ####

#### Create Map of Grasslands in North America ####

#Map information

#get data for the US map
US <- ne_countries(scale = "medium", country = "United States of America",returnclass = "sf")

US_States<-ne_states(country = "United States of America")

#load in raster from website https://www.epa.gov/eco-research/ecoregions-north-america that contains physical feature areas 

#Geographic_Locations <- (readOGR("na_cec_eco_l1/NA_CEC_Eco_Level1.shp"))
Ecosystem_shp<- shapefile("na_cec_eco_l1/NA_CEC_Eco_Level1.shp")

summary(Ecosystem_shp@data)

state_shp <- shapefile("ne_50m_admin_1_states_provinces/ne_50m_admin_1_states_provinces.shp")

summary(state_shp@data)

#make data frame with just names of locations of interest
Geographic_Locations_df <- broom::tidy(Ecosystem_shp, region = "NA_L1NAME")

#create a data frame that has the names of the geographic locations 
cnames <- aggregate(cbind(long, lat) ~ id, data=Geographic_Locations_df, FUN=median)

#download state line data from website
ne_download(scale = 'medium', type = 'states',category = c("cultural", "physical","raster"))
#load in state line data
State_lines<-ne_load(scale = 'medium', type = 'states')

#want state lines but not working with geographic locations yet
#Map<-US %>% 
# ggplot()+
#geom_sf(color="black",fill=NA)+
# geom_polygon(data = State_lines, aes(x=long, y = lat, group = group), fill=NA,colour="black", alpha=0.3)+
#coord_sf(xlim = c(-130, -70), ylim =  c(25,60), expand = FALSE)

#map of geographic locations in NA
Map_Geography <-US %>% 
  ggplot()+
  geom_sf(color="black",fill=NA)+
  geom_polygon(data = filter(Geographic_Locations_df, id=="GREAT PLAINS"), aes(x = long, y = lat, group = group,fill=id), colour = "black") +
  geom_polygon(data = State_lines, aes(x=long, y = lat, group = group), fill=NA,colour="black", alpha=0.3)+
  coord_sf(xlim = c(-130, -70), ylim =  c(25,60), expand = FALSE)
  
  #geom_text(data = filter(cnames, id=="GREAT PLAINS"), aes(x = long, y = lat, label = id), size = 4)

Map_Geography



#making a map with tmap package - https://geocompr.robinlovelace.net/adv-map.html

# load data
data(World)

# get current options
str(tmap_options())
# get current style
tmap_style()
tmap_options(check.and.fix = TRUE)
tmap_options(max.categories = 286)


# Add fill and border layers to US shape
MAP_US<-tm_shape(US) +
  tm_polygons()

MAP_US

#add layer of state outlines to map
MAP_US_States<-MAP_US+
  qtm(state_shp,alpha = 0.7)

MAP_US_States

#tm_shape(state_shp)
  #tm_polygons(col="woe_name")

#adding ecosystem layer 

MAP_US_Ecosystem<-MAP_US+
  qtm(Ecosystem_shp)+
  tm_polygons(col="NA_L1NAME")

MAP_US_Ecosystem

####working 
MAP_Ecosystem1<-tm_shape(Ecosystem_shp) +
  tm_polygons()
  
MAP_Ecosystem1

MAP_Ecosystem_States<-MAP_Ecosystem1+
  qtm(state_shp,alpha = 0.7)

MAP_Ecosystem_States



###
  qtm(state_shp)+
  tm_polygons(col="woe_label")
  tm_shape(state_shp)+
  tm_rgb()+
  tm_borders()
  
MAP_US_States


state_shp$woe_label <- as.vector(state_shp$woe_label)
Geographic_Locations$NA_L1NAME <- as.vector(Geographic_Locations$NA_L1NAME)










