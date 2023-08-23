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
library(olsrr)
library(multcomp)
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
  dplyr::select("Year","site","genus_species","species_code","block","plant","percent_green","emerging_leaves","developed_leaves","height_cm","scenesced_leaves","leaf_area_cm","leaf_thickness_mm","wet_weight_g", "biomass_mg","leaf_mg","comments") %>% 
  mutate(LDMC=as.numeric(leaf_mg)/wet_weight_g)

Traits$Year=as.character(Traits$Year)

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
  ylab("LDMC")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)

#### Histogram of all variables ####
ggplot(Traits,aes(x=percent_green,color=species_code, fill=species_code))+
  geom_histogram(alpha=0.6, binwidth = 20)+
  facet_grid(site~species_code)

#### Normality: FK - Percent Green: ####

#BRAR
#non transformed data
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(percent_green)), ((percent_green))  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #normalish

#HECO
#non transformed data
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(percent_green)), ((percent_green))  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) ##normalish

#KOMA
#non transformed data
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(percent_green)), ((percent_green))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) ##normalish

#SPCO
#non transformed data
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(percent_green)), ((percent_green))  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) ##normalish

#TRDU
#non transformed data
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(percent_green)), ((percent_green))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #normal


#### Normality: TB - Percent Green: ####

#BOGR
#non transformed data
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(percent_green)), percent_green  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) ##normalish

#KOMA
#non transformed data
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(percent_green)), ((percent_green))  ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) ##normalish

#LOAR
#non transformed data
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(percent_green)), ((percent_green))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #normal

#PASM
#non transformed data
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(percent_green)), ((percent_green))  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #not normal

#VIAM
#non transformed data
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(percent_green)), ((percent_green))  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #not normal


#### Normality: FK - Emerging Leaves: ####

#BRAR
#non transformed data
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(emerging_leaves)), ((emerging_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #not normal

#HECO
#non transformed data
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(emerging_leaves)), (emerging_leaves)  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #normalish

#KOMA
#non transformed data
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(emerging_leaves)), ((emerging_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) #not normal

#SPCO
#non transformed data
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(emerging_leaves)), ((emerging_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #not normal

#TRDU
#non transformed data
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(emerging_leaves)), (log(emerging_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #not normal


#### Normality: TB - Emerging Leaves: ####

#BOGR
#non transformed data
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(emerging_leaves)), emerging_leaves  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) #not normal

#KOMA
#non transformed data
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(emerging_leaves)), (sqrt/(emerging_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #not normal

#LOAR
#non transformed data
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(emerging_leaves)), ((emerging_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #not normal

#PASM
#non transformed data
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(emerging_leaves)), (sqrt(emerging_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #not normal

#VIAM
#non transformed data
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(emerging_leaves)), (sqrt(emerging_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #not normal

#### Normality: FK - developed_leaves: ####

#BRAR
#non transformed data
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(developed_leaves)), developed_leaves  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #not normal

#HECO
#non transformed data
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(developed_leaves)), (sqrt(developed_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #not normal

#KOMA
#non transformed data
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(developed_leaves)), ((developed_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) #not normal

#SPCO
#non transformed data
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(developed_leaves)), ((developed_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #not normal

#TRDU
#non transformed data
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(developed_leaves)), ((developed_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #not normal


#### Normality: TB - developed_leaves: ####

#BOGR
#non transformed data
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(developed_leaves)), developed_leaves  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) #not normal

#KOMA
#non transformed data
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(developed_leaves)), (sqrt/(developed_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #not normal

#LOAR
#non transformed data
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(developed_leaves)), ((developed_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #not normal

#PASM
#non transformed data
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(developed_leaves)), (sqrt(developed_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #not normal

#VIAM
#non transformed data
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(developed_leaves)), (sqrt(developed_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #not normal

#### Normality: FK - scenesced_leaves: ####

#BRAR
#non transformed data
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #not normal

#HECO
#non transformed data
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(scenesced_leaves)), (sqrt(scenesced_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #not normal

#KOMA
#non transformed data
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(scenesced_leaves)), ((scenesced_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) #not normal

#SPCO
#non transformed data
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(scenesced_leaves)), ((scenesced_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #not normal

#TRDU
#non transformed data
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(scenesced_leaves)), ((scenesced_leaves))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #not normal


#### Normality: TB - scenesced_leaves: ####

#BOGR
#non transformed data
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) #not normal

#KOMA
#non transformed data
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(scenesced_leaves)), (sqrt/(scenesced_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #not normal

#LOAR
#non transformed data
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(scenesced_leaves)), ((scenesced_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #not normal

#PASM
#non transformed data
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(scenesced_leaves)), (sqrt(scenesced_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #not normal

#VIAM
#non transformed data
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(scenesced_leaves)), (sqrt(scenesced_leaves))  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #not normal

#### Normality: FK - height_cm: ####

#BRAR
#non transformed data
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(height_cm)), height_cm  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #not normal

#HECO
#non transformed data
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(height_cm)), (sqrt(height_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #not normal

#KOMA
#non transformed data
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(height_cm)), ((height_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) #not normal

#SPCO
#non transformed data
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(height_cm)), ((height_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #not normal

#TRDU
#non transformed data
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(height_cm)), ((height_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #not normal


#### Normality: TB - height_cm: ####

#BOGR
#non transformed data
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(height_cm)), height_cm  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) #not normal

#KOMA
#non transformed data
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(height_cm)), (sqrt/(height_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #not normal

#LOAR
#non transformed data
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(height_cm)), ((height_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #not normal

#PASM
#non transformed data
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(height_cm)), (sqrt(height_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #not normal

#VIAM
#non transformed data
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(height_cm)), (sqrt(height_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #not normal

#### Normality: FK - leaf_area_cm: ####

#BRAR
#non transformed data
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #not normal

#HECO
#non transformed data
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #not normal

#KOMA
#non transformed data
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(leaf_area_cm)), ((leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) #not normal

#SPCO
#non transformed data
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(leaf_area_cm)), ((leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #not normal

#TRDU
#non transformed data
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(leaf_area_cm)), ((leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #not normal


#### Normality: TB - leaf_area_cm: ####

#BOGR
#non transformed data
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) #not normal

#KOMA
#non transformed data
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(leaf_area_cm)), (sqrt/(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #not normal

#LOAR
#non transformed data
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(leaf_area_cm)), ((leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #not normal

#PASM
#non transformed data
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #not normal

#VIAM
#non transformed data
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #not normal

#### Normality: FK - leaf_thickness_mm: ####

#BRAR
#non transformed data
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #not normal

#HECO
#non transformed data
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(leaf_thickness_mm)), (sqrt(leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #not normal

#KOMA
#non transformed data
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) #not normal

#SPCO
#non transformed data
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #not normal

#TRDU
#non transformed data
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #not normal


#### Normality: TB - leaf_thickness_mm: ####

#BOGR
#non transformed data
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) #normal

#KOMA
#non transformed data
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(leaf_thickness_mm)), (sqrt/(leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #not normal

#LOAR
#non transformed data
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #not normal

#PASM
#non transformed data
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(leaf_thickness_mm)), (sqrt(leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #not normal

#VIAM
#non transformed data
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(leaf_thickness_mm)), (sqrt(leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #normal


#### Stats: FK - percent green ####

#BRAR
FK_BRAR_G <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(percent_green)), percent_green  ~ Year + (1|block))
anova(FK_BRAR_G, type = 3) #posthoc
summary(glht(FK_BRAR_G, linfct = mcp(Year = "Tukey"), test = adjusted(type = "BH"))) #NS

#HECO
FK_HECO_G <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(percent_green)), percent_green  ~ Year + (1|block))
anova(FK_HECO_G, type = 3) #NS

#KOMA
FK_KOMA_G <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(percent_green)), percent_green  ~ Year + (1|block))
anova(FK_KOMA_G, type = 3) #0.01

#SPCO
FK_SPCO_G <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(percent_green)), percent_green  ~ Year + (1|block))
anova(FK_SPCO_G, type = 3) #NS

#TRDU
FK_TRDU_G <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(percent_green)), percent_green  ~ Year + (1|block))
anova(FK_TRDU_G, type = 3) #0.01

#### Stats: TB - percent green ####

#BOGR
TB_BOGR_G <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(percent_green)), percent_green  ~ Year + (1|block))
anova(TB_BOGR_G, type = 3) #0.0002

#KOMA
TB_KOMA_G <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(percent_green)), percent_green  ~ Year + (1|block))
anova(TB_KOMA_G, type = 3) #0.0006

#LOAR
TB_LOAR_G <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(percent_green)), percent_green  ~ Year + (1|block))
anova(TB_LOAR_G, type = 3) #0.001

#PASM
TB_PASM_G <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(percent_green)), percent_green  ~ Year + (1|block))
anova(TB_PASM_G, type = 3) #NS

#VIAM
TB_VIAM_G <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(percent_green)), percent_green  ~ Year + (1|block))
anova(TB_VIAM_G, type = 3) #0.02

#### Stats: FK - emerging_leaves ####

#BRAR
FK_BRAR_EL <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(emerging_leaves)), emerging_leaves  ~ Year + (1|block))
anova(FK_BRAR_EL, type = 3) #NS

#HECO
FK_HECO_EL <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(emerging_leaves)), emerging_leaves  ~ Year + (1|block))
anova(FK_HECO_EL, type = 3) #NS

#KOMA
FK_KOMA_EL <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(emerging_leaves)), emerging_leaves  ~ Year + (1|block))
anova(FK_KOMA_EL, type = 3) #1.9E05

#SPCO
FK_SPCO_EL <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(emerging_leaves)), emerging_leaves  ~ Year + (1|block))
anova(FK_SPCO_EL, type = 3) #0.005
summary(glht(FK_SPCO_EL, linfct = mcp(Year = "Tukey"), test = adjusted(type = "BH"))) #NS


#TRDU
FK_TRDU_EL <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(emerging_leaves)), emerging_leaves  ~ Year + (1|block))
anova(FK_TRDU_EL, type = 3) #NS

#### Stats: TB - emerging_leaves ####

#BOGR
TB_BOGR_EL <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(emerging_leaves)), emerging_leaves  ~ Year + (1|block))
anova(TB_BOGR_EL, type = 3) #0.04

#KOMA
TB_KOMA_EL <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(emerging_leaves)), emerging_leaves  ~ Year + (1|block))
anova(TB_KOMA_EL, type = 3) #NS

#LOAR
TB_LOAR_EL <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(emerging_leaves)), emerging_leaves  ~ Year + (1|block))
anova(TB_LOAR_EL, type = 3) #NS

#PASM
TB_PASM_EL <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(emerging_leaves)), emerging_leaves  ~ Year + (1|block))
anova(TB_PASM_EL, type = 3) #0.001

#VIAM
TB_VIAM_EL <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(emerging_leaves)), emerging_leaves  ~ Year + (1|block))
anova(TB_VIAM_EL, type = 3) #NS

#### Stats: FK - developed_leaves ####

#BRAR
FK_BRAR_DL <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(developed_leaves)), developed_leaves  ~ Year + (1|block))
anova(FK_BRAR_DL, type = 3) #1.4E-13
summary(glht(FK_BRAR_DL, linfct = mcp(Year = "Tukey")), test = adjusted(type = "BH")) #NS

#HECO
FK_HECO_DL <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(developed_leaves)), developed_leaves  ~ Year + (1|block))
anova(FK_HECO_DL, type = 3) #0.007

#KOMA
FK_KOMA_DL <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(developed_leaves)), developed_leaves  ~ Year + (1|block))
anova(FK_KOMA_DL, type = 3) #NS

#SPCO
FK_SPCO_DL <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(developed_leaves)), developed_leaves  ~ Year + (1|block))
anova(FK_SPCO_DL, type = 3) #NS

#TRDU
FK_TRDU_DL <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(developed_leaves)), developed_leaves  ~ Year + (1|block))
anova(FK_TRDU_DL, type = 3) #0.02

#### Stats: TB - developed_leaves ####

#BOGR
TB_BOGR_DL <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(developed_leaves)), developed_leaves  ~ Year + (1|block))
anova(TB_BOGR_DL, type = 3) #NS

#KOMA
TB_KOMA_DL <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(developed_leaves)), developed_leaves  ~ Year + (1|block))
anova(TB_KOMA_DL, type = 3) #NS

#LOAR
TB_LOAR_DL <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(developed_leaves)), developed_leaves  ~ Year + (1|block))
anova(TB_LOAR_DL, type = 3) #NS

#PASM
TB_PASM_DL <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(developed_leaves)), developed_leaves  ~ Year + (1|block))
anova(TB_PASM_DL, type = 3) #0.03

#VIAM
TB_VIAM_DL <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(developed_leaves)), developed_leaves  ~ Year + (1|block))
anova(TB_VIAM_DL, type = 3) #0.03


#### Stats: FK - scenesced_leaves ####

#BRAR
FK_BRAR_SL <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year + (1|block))
anova(FK_BRAR_SL, type = 3) #NS

#HECO
FK_HECO_SL <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year + (1|block))
anova(FK_HECO_SL, type = 3) #0.03

#KOMA
FK_KOMA_SL <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year + (1|block))
anova(FK_KOMA_SL, type = 3) #NS

#SPCO
FK_SPCO_SL <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year + (1|block))
anova(FK_SPCO_SL, type = 3) #NS

#TRDU
FK_TRDU_SL <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year + (1|block))
anova(FK_TRDU_SL, type = 3) #NS

#### Stats: TB - scenesced_leaves ####

#BOGR
TB_BOGR_SL <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year + (1|block))
anova(TB_BOGR_SL, type = 3) #NS

#KOMA
TB_KOMA_SL <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year + (1|block))
anova(TB_KOMA_SL, type = 3) #NS

#LOAR
TB_LOAR_SL <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year + (1|block))
anova(TB_LOAR_SL, type = 3) #0.008

#PASM
TB_PASM_SL <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year + (1|block))
anova(TB_PASM_SL, type = 3) #NS

#VIAM
TB_VIAM_SL <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(scenesced_leaves)), scenesced_leaves  ~ Year + (1|block))
anova(TB_VIAM_SL, type = 3) #0.001


#### Stats: FK - height_cm ####

#BRAR
FK_BRAR_H <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(FK_BRAR_H, type = 3) #0.006

#HECO
FK_HECO_H <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(FK_HECO_H, type = 3) #1.5E-06

#KOMA
FK_KOMA_H <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(FK_KOMA_H, type = 3) #4.995e-07

#SPCO
FK_SPCO_H <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(FK_SPCO_H, type = 3) #0.0009543

#TRDU
FK_TRDU_H <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(FK_TRDU_H, type = 3) # 0.0008992

#### Stats: TB - height_cm ####

#BOGR
TB_BOGR_H <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(TB_BOGR_H, type = 3) #NS

#KOMA
TB_KOMA_H <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(TB_KOMA_H, type = 3) #0.02

#LOAR
TB_LOAR_H <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(TB_LOAR_H, type = 3) #NS

#PASM
TB_PASM_H <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(TB_PASM_H, type = 3) #NS

#VIAM
TB_VIAM_H <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(TB_VIAM_H, type = 3) #0.0001


#### Stats: FK - leaf_area_cm ####

#BRAR
FK_BRAR_LA <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year + (1|block))
anova(FK_BRAR_LA, type = 3) #NS

#HECO
FK_HECO_LA <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year + (1|block))
anova(FK_HECO_LA, type = 3) #NS

#KOMA
FK_KOMA_LA <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year + (1|block))
anova(FK_KOMA_LA, type = 3) #NS

#SPCO
FK_SPCO_LA <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year + (1|block))
anova(FK_SPCO_LA, type = 3) #NS

#TRDU
FK_TRDU_LA <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year + (1|block))
anova(FK_TRDU_LA, type = 3) #NS

#### Stats: TB - leaf_area_cm ####

#BOGR
TB_BOGR_LA <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year + (1|block))
anova(TB_BOGR_LA, type = 3) #NS

#KOMA
TB_KOMA_LA <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year + (1|block))
anova(TB_KOMA_LA, type = 3) #NS

#LOAR
TB_LOAR_LA <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year + (1|block))
anova(TB_LOAR_LA, type = 3) #NS

#PASM
TB_PASM_LA <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year + (1|block))
anova(TB_PASM_LA, type = 3) #NS

#VIAM
TB_VIAM_LA <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year + (1|block))
anova(TB_VIAM_LA, type = 3) #NS


#### Stats: FK - wet_weight_g ####

#BRAR
FK_BRAR_WW <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(wet_weight_g)), wet_weight_g  ~ Year + (1|block))
anova(FK_BRAR_WW, type = 3) #0.01

#HECO
FK_HECO_WW <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(wet_weight_g)), wet_weight_g  ~ Year + (1|block))
anova(FK_HECO_WW, type = 3) #3.96e-05

#KOMA
FK_KOMA_WW <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(wet_weight_g)), wet_weight_g  ~ Year + (1|block))
anova(FK_KOMA_WW, type = 3) #0.03736

#SPCO
FK_SPCO_WW <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(wet_weight_g)), wet_weight_g  ~ Year + (1|block))
anova(FK_SPCO_WW, type = 3) #0.0003633

#TRDU
FK_TRDU_WW <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(wet_weight_g)), wet_weight_g  ~ Year + (1|block))
anova(FK_TRDU_WW, type = 3) #NS

#### Stats: TB - wet_weight_g ####

#BOGR
TB_BOGR_WW <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(wet_weight_g)), wet_weight_g  ~ Year + (1|block))
anova(TB_BOGR_WW, type = 3) #0.02

#KOMA
TB_KOMA_WW <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(wet_weight_g)), wet_weight_g  ~ Year + (1|block))
anova(TB_KOMA_WW, type = 3) #NS

#LOAR
TB_LOAR_WW <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(wet_weight_g)), wet_weight_g  ~ Year + (1|block))
anova(TB_LOAR_WW, type = 3) #NS

#PASM
TB_PASM_WW <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(wet_weight_g)), wet_weight_g  ~ Year + (1|block))
anova(TB_PASM_WW, type = 3) #NS

#VIAM
TB_VIAM_WW <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(wet_weight_g)), wet_weight_g  ~ Year + (1|block))
anova(TB_VIAM_WW, type = 3) #NS

#### Stats: FK - leaf_thickness_mm ####

#BRAR
FK_BRAR_LT <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(FK_BRAR_LT, type = 3) #0.000293

#HECO
FK_HECO_LT <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(FK_HECO_LT, type = 3) #NS

#KOMA
FK_KOMA_LT <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(FK_KOMA_LT, type = 3) #0.001024

#SPCO
FK_SPCO_LT <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(FK_SPCO_LT, type = 3) #0.006678

#TRDU
FK_TRDU_LT <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(FK_TRDU_LT, type = 3) #0.0001141

#### Stats: TB - leaf_thickness_mm ####

#BOGR
TB_BOGR_LT <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(TB_BOGR_LT, type = 3) #0.0009858

#KOMA
TB_KOMA_LT <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(TB_KOMA_LT, type = 3) #0.00173

#LOAR
TB_LOAR_LT <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(TB_LOAR_LT, type = 3) #9.081e-08 

#PASM
TB_PASM_LT <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(TB_PASM_LT, type = 3) #6.756e-05

#VIAM
TB_VIAM_LT <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(TB_VIAM_LT, type = 3) #0.0001121


#### Stats: FK - biomass_mg ####

#BRAR
FK_BRAR_BM <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(biomass_mg)), biomass_mg  ~ Year + (1|block))
anova(FK_BRAR_BM, type = 3) #NS

#HECO
FK_HECO_BM <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(biomass_mg)), biomass_mg  ~ Year + (1|block))
anova(FK_HECO_BM, type = 3) #NS

#KOMA
FK_KOMA_BM <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(biomass_mg)), biomass_mg  ~ Year + (1|block))
anova(FK_KOMA_BM, type = 3) #NS

#SPCO
FK_SPCO_BM <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(biomass_mg)), biomass_mg  ~ Year + (1|block))
anova(FK_SPCO_BM, type = 3) #NS

#TRDU
FK_TRDU_BM <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(biomass_mg)), biomass_mg  ~ Year + (1|block))
anova(FK_TRDU_BM, type = 3) #NS

#### Stats: TB - biomass_mg ####

#BOGR
TB_BOGR_BM <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(biomass_mg)), biomass_mg  ~ Year + (1|block))
anova(TB_BOGR_BM, type = 3) #NS

#KOMA
TB_KOMA_BM <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(biomass_mg)), biomass_mg  ~ Year + (1|block))
anova(TB_KOMA_BM, type = 3) #NS

#LOAR
TB_LOAR_BM <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(biomass_mg)), biomass_mg  ~ Year + (1|block))
anova(TB_LOAR_BM, type = 3) #NS

#PASM
TB_PASM_BM <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(biomass_mg)), biomass_mg  ~ Year + (1|block))
anova(TB_PASM_BM, type = 3) #0.01

#VIAM
TB_VIAM_BM <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(biomass_mg)), biomass_mg  ~ Year + (1|block))
anova(TB_VIAM_BM, type = 3) #0.0001

#### Stats: FK - leaf_mg ####

#BRAR
FK_BRAR_DL <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(leaf_mg)), leaf_mg  ~ Year + (1|block))
anova(FK_BRAR_DL, type = 3) #1.371e-13

#HECO
FK_HECO_DL <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(leaf_mg)), leaf_mg  ~ Year + (1|block))
anova(FK_HECO_DL, type = 3) #NS

#KOMA
FK_KOMA_DL <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(leaf_mg)), leaf_mg  ~ Year + (1|block))
anova(FK_KOMA_DL, type = 3) #NS

#SPCO
FK_SPCO_DL <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(leaf_mg)), leaf_mg  ~ Year + (1|block))
anova(FK_SPCO_DL, type = 3) #NS

#TRDU
FK_TRDU_DL <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(leaf_mg)), leaf_mg  ~ Year + (1|block))
anova(FK_TRDU_DL, type = 3) #0.02871

#### Stats: TB - leaf_mg ####

#BOGR
TB_BOGR_DL <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(leaf_mg)), leaf_mg  ~ Year + (1|block))
anova(TB_BOGR_DL, type = 3) #NS

#KOMA
TB_KOMA_DL <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(leaf_mg)), leaf_mg  ~ Year + (1|block))
anova(TB_KOMA_DL, type = 3) #NS

#LOAR
TB_LOAR_DL <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(leaf_mg)), leaf_mg  ~ Year + (1|block))
anova(TB_LOAR_DL, type = 3) #0.00503

#PASM
TB_PASM_DL <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(leaf_mg)), leaf_mg  ~ Year + (1|block))
anova(TB_PASM_DL, type = 3) #NS

#VIAM
TB_VIAM_DL <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(leaf_mg)), leaf_mg  ~ Year + (1|block))
anova(TB_VIAM_DL, type = 3) #0.004652

#### Stats: FK - LDMC ####

#BRAR
FK_BRAR_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(LDMC)), LDMC  ~ Year + (1|block))
anova(FK_BRAR_LDMC, type = 3) #

#HECO
FK_HECO_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(LDMC)), LDMC  ~ Year + (1|block))
anova(FK_HECO_LDMC, type = 3) #NS

#KOMA
FK_KOMA_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(LDMC)), LDMC  ~ Year + (1|block))
anova(FK_KOMA_LDMC, type = 3) #NS

#SPCO
FK_SPCO_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(LDMC)), LDMC  ~ Year + (1|block))
anova(FK_SPCO_LDMC, type = 3) #NS

#TRDU
FK_TRDU_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(LDMC)), LDMC  ~ Year + (1|block))
anova(FK_TRDU_LDMC, type = 3) #NS

#### Stats: TB - LDMC ####

#BOGR
TB_BOGR_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(LDMC)), LDMC  ~ Year + (1|block))
anova(TB_BOGR_LDMC, type = 3) #NS

#KOMA
TB_KOMA_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(LDMC)), LDMC  ~ Year + (1|block))
anova(TB_KOMA_LDMC, type = 3) #NS

#LOAR
TB_LOAR_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(LDMC)), LDMC  ~ Year + (1|block))
anova(TB_LOAR_LDMC, type = 3) #0.00078

#PASM
TB_PASM_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(LDMC)), LDMC  ~ Year + (1|block))
anova(TB_PASM_LDMC, type = 3) #NS

#VIAM
TB_VIAM_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(LDMC)), LDMC  ~ Year + (1|block))
anova(TB_VIAM_LDMC, type = 3) #NS












