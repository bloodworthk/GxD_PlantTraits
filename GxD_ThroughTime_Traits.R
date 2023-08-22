##########################################################################################################
#Project: Dissertation: Through time Plant Traits in MGP for Drought x Grazing 

##########################################################################################################

#### Load Libraries ####

#install.packages("lme4")
library(lme4)
library(ggplot2)
library(grid)
#install.packages("lattice")
library(lattice)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(tidyverse) 
library(scales)


#### Set Working Directory ####
#Bloodworth - Mac
setwd("~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data")

#Bloodworth - PC
setwd("/Users/kjbloodw/Box/Projects/Dissertation/Data")

#### Set ggplot base ####
#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(axis.title.x=element_text(size=30, vjust=-0.35, margin=margin(t=15)),
             axis.text.x=element_text(size=30), axis.title.y=element_text(size=30, angle=90, vjust=0.5,
                                                                          margin=margin(r=15)), axis.text.y=element_text(size=30), plot.title =
               element_text(size=30, vjust=2), panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(), legend.title=element_blank(),
             legend.text=element_text(size=40))

#### Read in Data ####

#Trait Data
Traits<-read.csv("DxG_Plant_Traits/ThroughTime_Traits.csv") %>% 
  mutate(plant=ifelse(plant=="HG",3,ifelse(plant=="LG",1,ifelse(plant=="MG",2,plant)))) %>% 
  select("Year","site","genus_species","species_code","block","plant","percent_green","emerging_leaves","developed_leaves","height_cm","scenesced_leaves","leaf_area_cm","leaf_thickness_mm","wet_weight_g", "biomass_mg","leaf_mg","comments") %>% 
  mutate(LDMC=as.numeric(leaf_mg)/wet_weight_g)

#### Calculate averages ####
 Traits_avg<-Traits %>% 
  group_by(Year,site,genus_species,species_code)%>%
  summarize(percent_green_Std=sd(percent_green,na.rm = T),percent_green_Mean=mean(percent_green,na.rm = T),percent_green_n=length(percent_green),
        emerging_leaves_Std=sd(emerging_leaves,na.rm = T),emerging_leaves_Mean=mean(emerging_leaves,na.rm = T),emerging_leaves_n=length(emerging_leaves),
        developed_leaves_Std=sd(developed_leaves,na.rm = T),developed_leaves_Mean=mean(developed_leaves,na.rm = T),developed_leaves_n=length(developed_leaves),
        height_cm_Std=sd(height_cm,na.rm = T),height_cm_Mean=mean(height_cm,na.rm = T),height_cm_n=length(height_cm),
        scenesced_leaves_Std=sd(scenesced_leaves,na.rm = T),scenesced_leaves_Mean=mean(scenesced_leaves,na.rm = T),scenesced_leaves_n=length(scenesced_leaves),
        leaf_area_cm_Std=sd(leaf_area_cm,na.rm = T),leaf_area_cm_Mean=mean(leaf_area_cm,na.rm = T),leaf_area_cm_n=length(leaf_area_cm),
        leaf_thickness_mm_Std=sd(leaf_thickness_mm,na.rm = T),leaf_thickness_mm_Mean=mean(leaf_thickness_mm,na.rm = T),leaf_thickness_mm_n=length(leaf_thickness_mm),
        wet_weight_g_Std=sd(wet_weight_g,na.rm = T),wet_weight_g_Mean=mean(wet_weight_g,na.rm = T),wet_weight_g_n=length(leaf_thickness_mm),
        biomass_mg_Std=sd(biomass_mg,na.rm = T),biomass_mg_Mean=mean(biomass_mg,na.rm = T),biomass_mg_n=length(biomass_mg),
        leaf_mg_Std=sd(leaf_mg,na.rm = T),leaf_mg_Mean=mean(leaf_mg,na.rm = T),leaf_mg_n=length(leaf_mg),
        LDMC_Std=sd(LDMC,na.rm = T),LDMC_Mean=mean(LDMC,na.rm = T),LDMC_n=length(LDMC))%>%
  mutate(percent_green_St_Error=percent_green_Std/sqrt(percent_green_n),
         emerging_leaves_St_Error=emerging_leaves_Std/sqrt(emerging_leaves_n),
         developed_leaves_St_Error=developed_leaves_Std/sqrt(developed_leaves_n),
         height_cm_St_Error=height_cm_Std/sqrt(height_cm_n),
         scenesced_leaves_St_Error=scenesced_leaves_Std/sqrt(scenesced_leaves_n),
         leaf_area_cm_St_Error=leaf_area_cm_Std/sqrt(leaf_area_cm_n),
         leaf_thickness_mm_St_Error=leaf_thickness_mm_Std/sqrt(leaf_thickness_mm_n),
         wet_weight_g_St_Error=wet_weight_g_Std/sqrt(wet_weight_g_n),
         biomass_mg_St_Error=biomass_mg_Std/sqrt(biomass_mg_n),
         leaf_mg_St_Error=leaf_mg_Std/sqrt(leaf_mg_n),
         LDMC_St_Error=LDMC_Std/sqrt(LDMC_n)) %>% 
  ungroup()

Traits_avg<-Traits_avg[!is.na(Traits_avg$Year),]
Traits_avg$Year=as.numeric(Traits_avg$Year)


#### Percent Green Figure ####
ggplot(Traits_avg,aes(x=Year,y=percent_green_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=percent_green_Mean-percent_green_St_Error,ymax=percent_green_Mean+percent_green_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Percent Green (%)")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)

#### Emerging Leaves Figure ####
ggplot(Traits_avg,aes(x=Year,y=emerging_leaves_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=emerging_leaves_Mean-emerging_leaves_St_Error,ymax=emerging_leaves_Mean+emerging_leaves_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Emerging Leaves")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)

#### developed_leaves Figure ####
ggplot(Traits_avg,aes(x=Year,y=developed_leaves_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=developed_leaves_Mean-developed_leaves_St_Error,ymax=developed_leaves_Mean+developed_leaves_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Developed Leaves")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)

#### Senesced Leaves Figure ####
ggplot(Traits_avg,aes(x=Year,y=scenesced_leaves_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=scenesced_leaves_Mean-scenesced_leaves_St_Error,ymax=scenesced_leaves_Mean+scenesced_leaves_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Senesced Leaves")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)

#### Height Figure ####
ggplot(Traits_avg,aes(x=Year,y=height_cm_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=height_cm_Mean-height_cm_St_Error,ymax=height_cm_Mean+height_cm_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Height (cm)")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)

#### Leaf Area Figure ####
ggplot(Traits_avg,aes(x=Year,y=leaf_area_cm_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_area_cm_Mean-leaf_area_cm_St_Error,ymax=leaf_area_cm_Mean+leaf_area_cm_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Leaf Area (cm)")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)


#### Leaf Thickness Figure ####
ggplot(Traits_avg,aes(x=Year,y=leaf_thickness_mm_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_thickness_mm_Mean-leaf_thickness_mm_St_Error,ymax=leaf_thickness_mm_Mean+leaf_thickness_mm_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Leaf Thickness (mm)")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)


#### Wet Weight Figure ####
ggplot(Traits_avg,aes(x=Year,y=wet_weight_g_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=wet_weight_g_Mean-wet_weight_g_St_Error,ymax=wet_weight_g_Mean+wet_weight_g_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Leaf Wet Weight (g)")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)

#### Biomass  Figure ####
ggplot(Traits_avg,aes(x=Year,y=biomass_mg_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=biomass_mg_Mean-biomass_mg_St_Error,ymax=biomass_mg_Mean+biomass_mg_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Biomass (g)")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)

#### Dry Leaf Weight Figure ####
ggplot(Traits_avg,aes(x=Year,y=leaf_mg_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_mg_Mean-leaf_mg_St_Error,ymax=leaf_mg_Mean+leaf_mg_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Dry Leaf Weight (g)")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)


#### LDMC Figure ####
ggplot(Traits_avg,aes(x=Year,y=LDMC_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=LDMC_Mean-LDMC_St_Error,ymax=LDMC_Mean+LDMC_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Dry Leaf Weight (g)")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)











