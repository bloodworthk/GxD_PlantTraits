##########################################################################################################
#Project: Dissertation: Community Weighted Plant Traits in MGP for Drought x Grazing 

#Coder: Kathryn Bloodworth

#Created: 12/14/2021
##########################################################################################################

#### Load Libraries ####

library(lme4)
library(ggplot2)
#install.packages("visreg")
library(visreg)
#install.packages("lattice")
library(lattice)
library(tidyverse) 


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

#Trait Data
All_Traits_2019<-read.csv("DxG_Plant_Traits/2019_DxG_leaf_traits.csv")
Field_Traits_2020<-read.csv("DxG_Plant_Traits/2020_DxG_FieldTraits.csv")
Leaf_Traits_2020<-read.csv("DxG_Plant_Traits/2020_DxG_leaf_traits.csv")
Field_Traits_2021<-read.csv("DxG_Plant_Traits/2021_DxG_FieldTraits.csv")
Leaf_Traits_2021<-read.csv("DxG_Plant_Traits/2021_DxG_leaf_traits.csv")

#Species Comp Data
FK_SpComp_2018<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2018.csv")
FK_SpComp_2018$plot<-as.factor(FK_SpComp_2018$plot)
FK_SpComp_2019<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2019.csv")
FK_SpComp_2020<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2020.csv")
FK_SpComp_2021<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2021.csv")
TB_SpComp_2018<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2018.csv")
TB_SpComp_2019<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2019.csv")
TB_SpComp_2020<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2020.csv")
TB_SpComp_2021<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2021.csv")


#Plot Data
plot_layoutK<-read.csv("DxG_Plant_Traits/GMDR_site_plot_metadata.csv")

#Soil moisture data  - bring in and keep only what we need for this study and take average SM data for all months
SM_data<-read.csv("DxG_Plant_Traits/SM_FK_TB_2019-2021.csv") %>% 
  filter(Site=="FK") %>% 
  filter(Year==2019) %>%
  group_by(Block,Paddock,Plot,Drought,Grazing) %>% 
  summarise(Avg_SM=mean(Soil_Moisture)) %>% 
  rename(plot="Plot")

#### Clean Up Species Comp Data and Calculate Relative Cover ####

#get dataframe with just total cover per plot for each year
#2018
Aerial_Cover_2018_FK<-FK_SpComp_2018 %>% 
  filter(aerial_basal!="Basal")

Long_Cov_2018_FK<-gather(Aerial_Cover_2018_FK,key="species","cover",18:117) %>% 
  select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("Oenotherea.suffrutescens.1","STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii","CRCE.LELU.Penstemon.sp","Oenothera.","Unknown..7.baby.guara.","UNKN8.basal.rosette.lancroiati","Unk..3.Basal.Rosette","Unk..3.soft.point.leaf.KW.photo","Unkn..10.small.linear.leaf.KW.photo","Oneothera.n.","Rock","Moss.Lichen.Bogr.overlap")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)




Relative_Cover_2018_FK<-Aerial_Cover_2018_FK %>% 
  select(year,site,plot) %>% 
  
  
  Relative_Cover_2018_FK$newcolumn <- 0

for (i in 18:100) {
  x <- Aerial_Cover_2018_FK[,i]/Aerial_Cover_2018_FK[,10]
  Relative_Cover_2018_FK <- cbind(Relative_Cover_2018_FK, data.frame(x[1]))
}



