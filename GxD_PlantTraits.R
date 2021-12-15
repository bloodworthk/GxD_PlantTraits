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
library(rgdal)
library(ggplot2)
library(tidyverse)
# setting the color palatte
# install.packagesTR("devtools")
#devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)
GeneralGrantpal<-park_palette("GeneralGrant", 7)
CraterLakepal <- park_palette("CraterLake", 7)


#### Set Working Directory ####

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

#load in raster from website https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-physical-labels/ that contains physical feature areas 

Geographic_Locations <- (system.file("NA_CEC_Eco_Level1.shp", package="raster"))
s <- shapefile(Geographic_Locations)

#download state line data from website
#ne_download(scale = 'medium', type = 'states',category = c("cultural", "physical","raster"))
#load in state line data
State_lines<-ne_load(scale = 'medium', type = 'states')

ne_10m_geography_regions_polys.zip

US %>% 
  ggplot()+
  geom_sf(color="black",fill="white")+
  geom_polygon(data = State_lines, aes(x=long, y = lat, group = group), fill="white",colour="black", alpha=0.3)+
  geom_polygon(data = Geographic_Locations, aes(x=long, y = lat, group = group), fill="white",colour="darkgray", alpha=0.3)+
  coord_sf(xlim = c(-130, -70), ylim =  c(25,60), expand = FALSE)
  
#create dataframe with just NA map data
NA_MapData<-map_data("world") %>% 
  filter(region==c("USA","Canada"))

#create dataframe with just map data
MapData<-map_data("state",boundary=TRUE) %>% 
  filter()

#map of locations of meta-analysis studies
ggplot()+
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="white",colour="darkgray", alpha=0.3) +
  #geom_polygon(data = TGP_MapData, aes(x=long, y=lat, group = region, fill = region),fill="gray")+
  #geom_polygon(data = TGP_MapData_Canada, aes(x=long, y=lat, group = country.etc, fill = country.etc),fill="gray")+
  borders("state",colour="black") +
  xlim(-180,-50)+
  #geom_point(data=Map_ResponseVariables_LatLong, mapping=aes(x=Long,y=Lat,fill=Response_Variable),size=3.5,shape=21) +  #this is the dataframe of lat/long, and the points are being colored by num_codominants, with the point shape and size specified at the end fill=response variable
  #scale_colour_manual(values=GeneralGrantpal)+
  theme(text=element_text(size=20, colour="black"),axis.text.x=element_text(size=20, colour="black"),axis.text.y=element_text(size=20, colour="black")) + #formatting the text
  ylab(expression("Latitude "*degree*""))+ #labels for the map x and y axes
  xlab(expression("Longitude "*degree*"")) +
  labs(fill="Response Variable") + #legend label
  theme(legend.position=c(0.15,0.2))  #legend position
#export at 1500 x 1000

