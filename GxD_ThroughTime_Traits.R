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
  dplyr::select("Year","site","genus_species","species_code","block","plant","height_cm","leaf_area_cm","leaf_thickness_mm","wet_weight_g", "biomass_mg","leaf_mg","comments") %>% 
  mutate(LDMC=as.numeric(leaf_mg)/wet_weight_g) %>% 
  mutate(SLA=leaf_area_cm/leaf_mg) %>%
  mutate(Plant_Biomass=as.numeric(leaf_mg)+as.numeric(biomass_mg))

Traits$Year=as.character(Traits$Year)

#### Calculate averages ####
 Traits_avg<-Traits %>% 
  group_by(Year,site,genus_species,species_code)%>% 
  summarize(height_cm_Std=sd(height_cm,na.rm = T),height_cm_Mean=mean(height_cm,na.rm = T),height_cm_n=length(height_cm),
        leaf_area_cm_Std=sd(leaf_area_cm,na.rm = T),leaf_area_cm_Mean=mean(leaf_area_cm,na.rm = T),leaf_area_cm_n=length(leaf_area_cm),
        leaf_thickness_mm_Std=sd(leaf_thickness_mm,na.rm = T),leaf_thickness_mm_Mean=mean(leaf_thickness_mm,na.rm = T),leaf_thickness_mm_n=length(leaf_thickness_mm),
        Plant_Biomass_Std=sd(Plant_Biomass,na.rm = T),Plant_Biomass_Mean=mean(Plant_Biomass,na.rm = T),Plant_Biomass_n=length(Plant_Biomass),
        LDMC_Std=sd(LDMC,na.rm = T),LDMC_Mean=mean(LDMC,na.rm = T),LDMC_n=length(LDMC),
        SLA_Std=sd(SLA,na.rm = T),SLA_Mean=mean(SLA,na.rm = T),SLA_n=length(SLA)) %>%
  mutate(height_cm_St_Error=height_cm_Std/sqrt(height_cm_n),
         leaf_area_cm_St_Error=leaf_area_cm_Std/sqrt(leaf_area_cm_n),
         leaf_thickness_mm_St_Error=leaf_thickness_mm_Std/sqrt(leaf_thickness_mm_n),
         Plant_Biomass_St_Error=Plant_Biomass_Std/sqrt(Plant_Biomass_n),
         LDMC_St_Error=LDMC_Std/sqrt(LDMC_n),
         SLA_St_Error=SLA_Std/sqrt(SLA_n))%>% 
  ungroup()

Traits_avg<-Traits_avg[!is.na(Traits_avg$Year),]
Traits_avg$Year=as.numeric(Traits_avg$Year)



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

#### Biomass  Figure ####
ggplot(Traits_avg,aes(x=Year,y=Plant_Biomass_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=Plant_Biomass_Mean-Plant_Biomass_St_Error,ymax=Plant_Biomass_Mean+Plant_Biomass_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Biomass (g)")+
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

#### SLA Figure ####
ggplot(Traits_avg,aes(x=Year,y=SLA_Mean,fill=species_code,color=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=SLA_Mean-SLA_St_Error,ymax=SLA_Mean+SLA_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("SLA")+
  #expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55))+
  facet_wrap(~site)


#### Normality: FK - height_cm: ####

#BRAR
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(height_cm)), height_cm  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #normal

#HECO
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(height_cm)), ((height_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #normal

#KOMA
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(height_cm)), (log(height_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA)
ols_test_normality(Norm_FK_KOMA) #normalish

#SPCO
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(height_cm)), ((height_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #normal

#TRDU
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(height_cm)), ((height_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #normal


#### Normality: TB - height_cm: ####

#BOGR
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(height_cm)), height_cm  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) #normal

#KOMA
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(height_cm)), (log(height_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #normal

#LOAR
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(height_cm)), (log(height_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #normal

#PASM
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(height_cm)), (sqrt(height_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #normal

#VIAM
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(height_cm)), (log(height_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #normal

#### Normality: FK - leaf_area_cm: ####

#BRAR
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #normal

#HECO
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #normal

#KOMA
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(leaf_area_cm)), (1/exp(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) #normal

#SPCO
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #normal

#TRDU
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #normal


#### Normality: TB - leaf_area_cm: ####

#BOGR
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) #normal

#KOMA
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(leaf_area_cm)), (1/exp(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #normal

#LOAR
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #normal

#PASM
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #normal

#VIAM
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #normal

#### Normality: FK - leaf_thickness_mm: ####

#BRAR
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #normal

#HECO
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #normal

#KOMA
#
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) #normal

#SPCO
#
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #normalish

#TRDU
#
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #normal


#### Normality: TB - leaf_thickness_mm: ####

#BOGR
#
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) #normal

#KOMA
#
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(leaf_thickness_mm)), (log1p(leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #normal

#LOAR
#
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #normal

#PASM
#
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(leaf_thickness_mm)), (sqrt(leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #normal

#VIAM
#
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(leaf_thickness_mm)), (sqrt(leaf_thickness_mm))  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #normal


#### Normality: FK - Plant Biomass: ####

#BRAR
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(Plant_Biomass)), Plant_Biomass  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #normal

#HECO
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #normal

#KOMA
#
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(Plant_Biomass)), (log(Plant_Biomass))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) #normal

#SPCO
#
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(Plant_Biomass)), (Plant_Biomass)  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #normal

#TRDU
#
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(Plant_Biomass)), ((Plant_Biomass))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #normal


#### Normality: TB - Plant_Biomass ####

#BOGR
#
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) #normal

#KOMA
#
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass) ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #normalish

#LOAR
#
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(Plant_Biomass)), (log(Plant_Biomass))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #normal

#PASM
#
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #normal

#VIAM
#
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(Plant_Biomass)), 1/sqrt(Plant_Biomass)  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #normalish


#### Normality: FK - LDMC: ####

#BRAR
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #normal

#HECO
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(LDMC)), 1/sqrt(LDMC)  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #normalish

#KOMA
#
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) #not normal but looks okay

#SPCO
#
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(LDMC)), 1/log(LDMC)  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #normal

#TRDU
#
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #not normal but looks okay


#### Normality: TB - LDMC ####

#BOGR
#
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR)  #not normal but looks okay

#KOMA
#
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(LDMC)), 1/log(LDMC) ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #normalish

#LOAR
#
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(LDMC)), (log(LDMC))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #normal

#PASM
#
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(LDMC)),log(LDMC)  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #not normal but looks okay

#VIAM
#
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(LDMC)), sqrt(LDMC)  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #normalish

#### Normality: FK - SLA: ####

#BRAR
Norm_FK_BRAR <- lm(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Year)
ols_plot_resid_hist(Norm_FK_BRAR) 
ols_test_normality(Norm_FK_BRAR) #normal

#HEC
Norm_FK_HECO <- lm(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Year)
ols_plot_resid_hist(Norm_FK_HECO) 
ols_test_normality(Norm_FK_HECO) #normalish

#KOMA
#
Norm_FK_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ Year)
ols_plot_resid_hist(Norm_FK_KOMA) 
ols_test_normality(Norm_FK_KOMA) #normalish

#SPCO
#
Norm_FK_SPCO <- lm(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Year)
ols_plot_resid_hist(Norm_FK_SPCO) 
ols_test_normality(Norm_FK_SPCO) #normalish

#TRDU
#
Norm_FK_TRDU <- lm(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ Year)
ols_plot_resid_hist(Norm_FK_TRDU) 
ols_test_normality(Norm_FK_TRDU) #normalish


#### Normality: TB - SLA ####

#BOGR
#
Norm_TB_BOGR <- lm(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(SLA)), 1/sqrt(SLA)  ~ Year)
ols_plot_resid_hist(Norm_TB_BOGR) 
ols_test_normality(Norm_TB_BOGR) #normal

#KOMA
#
Norm_TB_KOMA <- lm(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(SLA)), 1/sqrt(SLA) ~ Year)
ols_plot_resid_hist(Norm_TB_KOMA) 
ols_test_normality(Norm_TB_KOMA) #normal

#LOAR
#
Norm_TB_LOAR <- lm(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(SLA)), (1/(SLA))  ~ Year)
ols_plot_resid_hist(Norm_TB_LOAR) 
ols_test_normality(Norm_TB_LOAR) #normalish

#PASM
#
Norm_TB_PASM <- lm(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(SLA)), log(SLA)  ~ Year)
ols_plot_resid_hist(Norm_TB_PASM) 
ols_test_normality(Norm_TB_PASM) #not normal but looks okay

#VIAM
#
Norm_TB_VIAM <- lm(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(SLA)), 1/log(SLA)  ~ Year)
ols_plot_resid_hist(Norm_TB_VIAM) 
ols_test_normality(Norm_TB_VIAM) #normal





#### Stats: FK - height_cm ####

#BRAR
FK_BRAR_H <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(FK_BRAR_H, type = 3) #0.006

#HECO
FK_HECO_H <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(FK_HECO_H, type = 3) #1.5E-06

#KOMA
FK_KOMA_H <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(height_cm)), log(height_cm)  ~ Year + (1|block))
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
TB_KOMA_H <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(height_cm)), log(height_cm)  ~ Year + (1|block))
anova(TB_KOMA_H, type = 3) #0.02

#LOAR
TB_LOAR_H <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(height_cm)),log(height_cm)  ~ Year + (1|block))
anova(TB_LOAR_H, type = 3) #NS

#PASM
TB_PASM_H <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(height_cm)), height_cm  ~ Year + (1|block))
anova(TB_PASM_H, type = 3) #NS

#VIAM
TB_VIAM_H <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(height_cm)),log(height_cm)  ~ Year + (1|block))
anova(TB_VIAM_H, type = 3) #0.0001


#### Stats: FK - leaf_area_cm ####

#BRAR
FK_BRAR_LA <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Year + (1|block))
anova(FK_BRAR_LA, type = 3) #NS

#HECO
FK_HECO_LA <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(leaf_area_cm)), sqrt(leaf_area_cm)  ~ Year + (1|block))
anova(FK_HECO_LA, type = 3) #NS

#KOMA
FK_KOMA_LA <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(leaf_area_cm)), (1/exp(leaf_area_cm))   ~ Year + (1|block))
anova(FK_KOMA_LA, type = 3) #NS

#SPCO
FK_SPCO_LA <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Year + (1|block))
anova(FK_SPCO_LA, type = 3) #NS

#TRDU
FK_TRDU_LA <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Year + (1|block))
anova(FK_TRDU_LA, type = 3) #NS

#### Stats: TB - leaf_area_cm ####

#BOGR
TB_BOGR_LA <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Year + (1|block))
anova(TB_BOGR_LA, type = 3) #NS

#KOMA
TB_KOMA_LA <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(leaf_area_cm)), (1/exp(leaf_area_cm))   ~ Year + (1|block))
anova(TB_KOMA_LA, type = 3) #NS

#LOAR
TB_LOAR_LA <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(leaf_area_cm)), log(leaf_area_cm)  ~ Year + (1|block))
anova(TB_LOAR_LA, type = 3) #NS

#PASM
TB_PASM_LA <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(leaf_area_cm)), sqrt(leaf_area_cm)  ~ Year + (1|block))
anova(TB_PASM_LA, type = 3) #NS

#VIAM
TB_VIAM_LA <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(leaf_area_cm)), sqrt(leaf_area_cm)  ~ Year + (1|block))
anova(TB_VIAM_LA, type = 3) #NS

#### Stats: FK - leaf_thickness_mm ####

#BRAR
FK_BRAR_LT <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(FK_BRAR_LT, type = 3) #0.000293

#HECO
FK_HECO_LT <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ Year + (1|block))
anova(FK_HECO_LT, type = 3) #NS

#KOMA
FK_KOMA_LT <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(FK_KOMA_LT, type = 3) #0.001024

#SPCO
FK_SPCO_LT <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ Year + (1|block))
anova(FK_SPCO_LT, type = 3) #0.006678

#TRDU
FK_TRDU_LT <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(FK_TRDU_LT, type = 3) #0.0001141

#### Stats: TB - leaf_thickness_mm ####

#BOGR
TB_BOGR_LT <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(TB_BOGR_LT, type = 3) #0.0009858

#KOMA
TB_KOMA_LT <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(leaf_thickness_mm)), (log1p(leaf_thickness_mm))  ~ Year + (1|block))
anova(TB_KOMA_LT, type = 3) #0.00173

#LOAR
TB_LOAR_LT <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Year + (1|block))
anova(TB_LOAR_LT, type = 3) #9.081e-08 

#PASM
TB_PASM_LT <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(leaf_thickness_mm)), sqrt(leaf_thickness_mm)  ~ Year + (1|block))
anova(TB_PASM_LT, type = 3) #6.756e-05

#VIAM
TB_VIAM_LT <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(leaf_thickness_mm)), sqrt(leaf_thickness_mm)  ~ Year + (1|block))
anova(TB_VIAM_LT, type = 3) #0.0001121


#### Stats: FK - Plant_Biomass ####

#BRAR
FK_BRAR_BM <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(Plant_Biomass)), Plant_Biomass  ~ Year + (1|block))
anova(FK_BRAR_BM, type = 3) #0.003893

#HECO
FK_HECO_BM <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Year + (1|block))
anova(FK_HECO_BM, type = 3) #0.0003

#KOMA
FK_KOMA_BM <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Year + (1|block))
anova(FK_KOMA_BM, type = 3) #NS

#SPCO
FK_SPCO_BM <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(Plant_Biomass)), (Plant_Biomass)  ~ Year + (1|block))
anova(FK_SPCO_BM, type = 3) #0.1745

#TRDU
FK_TRDU_BM <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(Plant_Biomass)), Plant_Biomass  ~ Year + (1|block))
anova(FK_TRDU_BM, type = 3) #0.01982

#### Stats: TB - Plant_Biomass ####

#BOGR
TB_BOGR_BM <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass) ~ Year + (1|block))
anova(TB_BOGR_BM, type = 3) #NS

#KOMA
TB_KOMA_BM <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ Year + (1|block))
anova(TB_KOMA_BM, type = 3) #0.03815

#LOAR
TB_LOAR_BM <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ Year + (1|block))
anova(TB_LOAR_BM, type = 3) #NS

#PASM
TB_PASM_BM <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ Year + (1|block))
anova(TB_PASM_BM, type = 3) #0.01

#VIAM
TB_VIAM_BM <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(Plant_Biomass)), (1/sqrt(Plant_Biomass))  ~ Year + (1|block))
anova(TB_VIAM_BM, type = 3) #0.0001

#### Stats: FK - LDMC ####

#BRAR
FK_BRAR_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ Year + (1|block))
anova(FK_BRAR_LDMC, type = 3) #0.01218

#HECO
FK_HECO_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(LDMC)), (1/sqrt(LDMC))  ~ Year + (1|block))
anova(FK_HECO_LDMC, type = 3) #NS

#KOMA
FK_KOMA_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ Year + (1|block))
anova(FK_KOMA_LDMC, type = 3) #NS

#SPCO
FK_SPCO_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(LDMC)), (1/log(LDMC))  ~ Year + (1|block))
anova(FK_SPCO_LDMC, type = 3) #NS

#TRDU
FK_TRDU_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ Year + (1|block))
anova(FK_TRDU_LDMC, type = 3) #NS

#### Stats: TB - LDMC ####

#BOGR
TB_BOGR_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ Year + (1|block))
anova(TB_BOGR_LDMC, type = 3) #0.01028

#KOMA
TB_KOMA_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(LDMC)), 1/log(LDMC)  ~ Year + (1|block))
anova(TB_KOMA_LDMC, type = 3) #0.002

#LOAR
TB_LOAR_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ Year + (1|block))
anova(TB_LOAR_LDMC, type = 3) #0.00078

#PASM
TB_PASM_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ Year + (1|block))
anova(TB_PASM_LDMC, type = 3) #NS

#VIAM
TB_VIAM_LDMC <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(LDMC)), sqrt(LDMC)  ~ Year + (1|block))
anova(TB_VIAM_LDMC, type = 3) #NS

#### Stats: FK - SLA ####

#BRAR
FK_BRAR_SLA <- lmerTest::lmer(data = subset(Traits, species_code == "BRAR" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Year + (1|block))
anova(FK_BRAR_SLA, type = 3) #

#HECO
FK_HECO_SLA <- lmerTest::lmer(data = subset(Traits, species_code == "HECO" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ Year + (1|block))
anova(FK_HECO_SLA, type = 3) #NS

#KOMA
FK_KOMA_SLA <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Year + (1|block))
anova(FK_KOMA_SLA, type = 3) #NS

#SPCO
FK_SPCO_SLA <- lmerTest::lmer(data = subset(Traits, species_code == "SPCO" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ Year + (1|block))
anova(FK_SPCO_SLA, type = 3) #NS

#TRDU
FK_TRDU_SLA <- lmerTest::lmer(data = subset(Traits, species_code == "TRDU" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Year + (1|block))
anova(FK_TRDU_SLA, type = 3) #NS

#### Stats: TB - SLA ####

#BOGR
TB_BOGR_SLA <- lmerTest::lmer(data = subset(Traits, species_code == "BOGR" & site== "TB" & !is.na(SLA)), (1/sqrt(SLA))  ~ Year + (1|block))
anova(TB_BOGR_SLA, type = 3) #NS

#KOMA
TB_KOMA_SLA <- lmerTest::lmer(data = subset(Traits, species_code == "KOMA" & site== "TB" & !is.na(SLA)), (1/sqrt(SLA))  ~ Year + (1|block))
anova(TB_KOMA_SLA, type = 3) #NS

#LOAR
TB_LOAR_SLA <- lmerTest::lmer(data = subset(Traits, species_code == "LOAR" & site== "TB" & !is.na(SLA)), 1/(SLA)  ~ Year + (1|block))
anova(TB_LOAR_SLA, type = 3) #0.00078

#PASM
TB_PASM_SLA <- lmerTest::lmer(data = subset(Traits, species_code == "PASM" & site== "TB" & !is.na(SLA)), log(SLA)  ~ Year + (1|block))
anova(TB_PASM_SLA, type = 3) #NS

#VIAM
TB_VIAM_SLA <- lmerTest::lmer(data = subset(Traits, species_code == "VIAM" & site== "TB" & !is.na(SLA)), 1/log(SLA)  ~ Year + (1|block))
anova(TB_VIAM_SLA, type = 3) #NS










