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
library(patchwork)


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
  mutate(Plant_Biomass=as.numeric(leaf_mg)+as.numeric(biomass_mg)) %>% 
  #remove outliers for SLA
  mutate(SLA=ifelse(SLA>9999,NA,SLA))

Traits$Year=as.character(Traits$Year)

#Precipitation Data
Precip<-read.csv("DxG_Plant_Traits/DxG_PrecipitationData.csv") %>% 
  mutate(Time_Point=ifelse(Time_Point=="July","Monthly_Precip",ifelse(Time_Point=="June","Monthly_Precip","YeartoDate_Precip"))) %>% 
  spread(key=Time_Point,value=Rainfall..mm., fill=NA) %>% 
  rename(site=Site)
Precip$Year=as.character(Precip$Year)

#Soil Moisture Data
SM_TB<-read.csv("DxG_Plant_Traits/GMDR_TB_SoilMoisture_Controls.csv")%>% 
  rename(site=Site) %>% 
  filter(Date %in% c("4/19/19","5/2/19","5/19/19","6/3/19","6/14/19","6/24/19","5/8/20","5/19/20","6/1/20" ,"6/16/20","6/18/20","6/29/20" ,"6/30/20","7/16/20","7/30/20","4/23/21","5/6/21","5/19/21", "6/2/21","6/16/21","6/29/21","4/21/22","5/2/22","5/20/22","6/2/22","6/14/22","6/28/22")) %>% 
  group_by(site, Year) %>% 
  summarise(Soil_Moisture=mean(Soil.Moisture))
SM_TB$Year=as.character(SM_TB$Year)

####Currently no 2022 FK Soil Moisture Data ####
SM_FK<-read.csv("DxG_Plant_Traits/GMDR_FK_SoilMoisture_Controls.csv")%>% 
  rename(site=Site)%>% 
  group_by(site, Year) %>% 
  summarise(Soil_Moisture=mean(Soil_Moisture))
SM_FK$Year=as.character(SM_FK$Year)

SM_All<-SM_TB %>% 
  rbind(SM_FK)

Traits_Precip_SM<-Traits %>% 
  left_join(Precip) %>% 
  left_join(SM_All)

#### Calculate averages ####
 Traits_avg<-Traits_Precip_SM %>% 
  group_by(Year,Monthly_Precip,YeartoDate_Precip,Soil_Moisture,site,genus_species,species_code)%>% 
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




#### Traits by Year ####

#### Height Figure ####
Graph_Height_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Year,y=height_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=height_cm_Mean-height_cm_St_Error,ymax=height_cm_Mean+height_cm_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Height (cm)")+
  scale_linetype_manual(values=c(1,1,1,1,1),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  expand_limits(y=c(0,40))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Graph_Height_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Year,y=height_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=height_cm_Mean-height_cm_St_Error,ymax=height_cm_Mean+height_cm_St_Error),linewidth = 3)+
  xlab("Year")+
  ylab("Height (cm)")+
  scale_linetype_manual(values=c(0,1,0,0,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  expand_limits(y=c(0,40))+
                  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Leaf Area Figure ####
Graph_LeafArea_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Year,y=leaf_area_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_area_cm_Mean-leaf_area_cm_St_Error,ymax=leaf_area_cm_Mean+leaf_area_cm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(0,0,0,0,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Year")+
  ylab("Leaf Area (cm)")+
  expand_limits(y=c(0,10))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Graph_LeafArea_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Year,y=leaf_area_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_area_cm_Mean-leaf_area_cm_St_Error,ymax=leaf_area_cm_Mean+leaf_area_cm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,1,0,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Year")+
  ylab("Leaf Area (cm)")+
  expand_limits(y=c(0,10))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Leaf Thickness Figure ####
Graph_Thickness_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Year,y=leaf_thickness_mm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_thickness_mm_Mean-leaf_thickness_mm_St_Error,ymax=leaf_thickness_mm_Mean+leaf_thickness_mm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,1,1,1),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Year")+
  ylab("Leaf Thickness (mm)")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Graph_Thickness_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Year,y=leaf_thickness_mm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_thickness_mm_Mean-leaf_thickness_mm_St_Error,ymax=leaf_thickness_mm_Mean+leaf_thickness_mm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,1,1,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Year")+
  ylab("Leaf Thickness (mm)")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Biomass  Figure ####
Graph_Biomass_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Year,y=Plant_Biomass_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=Plant_Biomass_Mean-Plant_Biomass_St_Error,ymax=Plant_Biomass_Mean+Plant_Biomass_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,0,1,1),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Year")+
  ylab("Biomass (g)")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")


Graph_Biomass_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Year,y=Plant_Biomass_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=Plant_Biomass_Mean-Plant_Biomass_St_Error,ymax=Plant_Biomass_Mean+Plant_Biomass_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(0,1,0,1,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Year")+
  ylab("Biomass (g)")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### LDMC Figure ####
Graph_LDMC_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Year,y=LDMC_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=LDMC_Mean-LDMC_St_Error,ymax=LDMC_Mean+LDMC_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,0,0,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Year")+
  ylab("LDMC")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Graph_LDMC_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Year,y=LDMC_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=LDMC_Mean-LDMC_St_Error,ymax=LDMC_Mean+LDMC_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,1,0,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Year")+
  ylab("LDMC")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### SLA Figure ####
Graph_SLA_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Year,y=SLA_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=SLA_Mean-SLA_St_Error,ymax=SLA_Mean+SLA_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,1,1,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Year")+
  ylab("SLA")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position="none")

Graph_SLA_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Year,y=SLA_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=SLA_Mean-SLA_St_Error,ymax=SLA_Mean+SLA_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,1,0,0),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Year")+
  ylab("SLA")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position="none")

#### Create Through time Trait Figure####
Graph_Height_FK+
  Graph_Height_TB+
  Graph_LeafArea_FK+
  Graph_LeafArea_TB+
  Graph_Thickness_FK+
  Graph_Thickness_TB+
  Graph_Biomass_FK+
  Graph_Biomass_TB+
  Graph_LDMC_FK+
  Graph_LDMC_TB+
  Graph_SLA_FK+
  Graph_SLA_TB+
  plot_layout(ncol = 2,nrow = 6)
#save at 3000x5000

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



#### Traits by Monthly Precip ####
#### Height Figure ####
Monthly_Precip_Graph_Height_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Monthly_Precip,y=height_cm_Mean,color=species_code,fill=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=height_cm_Mean-height_cm_St_Error,ymax=height_cm_Mean+height_cm_St_Error),linewidth = 3)+
  xlab("Monthly_Precip")+
  ylab("Height (cm)")+
  scale_linetype_manual(values=c(0,1,0,1,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  expand_limits(y=c(0,40))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Monthly_Precip_Graph_Height_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Monthly_Precip,y=height_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=height_cm_Mean-height_cm_St_Error,ymax=height_cm_Mean+height_cm_St_Error),linewidth = 3)+
  xlab("Monthly_Precip")+
  ylab("Height (cm)")+
  #scale_alpha_manual(values=c(1,0,1,0,1))+
  scale_linetype_manual(values=c(0,0,0,1,0),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  expand_limits(y=c(0,40))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Leaf Area Figure ####
Monthly_Precip_Graph_LeafArea_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Monthly_Precip,y=leaf_area_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_area_cm_Mean-leaf_area_cm_St_Error,ymax=leaf_area_cm_Mean+leaf_area_cm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,0,1,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Monthly_Precip")+
  ylab("Leaf Area (cm)")+
  expand_limits(y=c(0,10))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Monthly_Precip_Graph_LeafArea_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Monthly_Precip,y=leaf_area_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_area_cm_Mean-leaf_area_cm_St_Error,ymax=leaf_area_cm_Mean+leaf_area_cm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,0,0,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Monthly_Precip")+
  ylab("Leaf Area (cm)")+
  expand_limits(y=c(0,10))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Leaf Thickness Figure ####
Monthly_Precip_Graph_Thickness_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Monthly_Precip,y=leaf_thickness_mm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_thickness_mm_Mean-leaf_thickness_mm_St_Error,ymax=leaf_thickness_mm_Mean+leaf_thickness_mm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(0,0,0,0,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Monthly_Precip")+
  ylab("Leaf Thickness (mm)")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Monthly_Precip_Graph_Thickness_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Monthly_Precip,y=leaf_thickness_mm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_thickness_mm_Mean-leaf_thickness_mm_St_Error,ymax=leaf_thickness_mm_Mean+leaf_thickness_mm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,1,1,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Monthly_Precip")+
  ylab("Leaf Thickness (mm)")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Biomass  Figure ####
Monthly_Precip_Graph_Biomass_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Monthly_Precip,y=Plant_Biomass_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=Plant_Biomass_Mean-Plant_Biomass_St_Error,ymax=Plant_Biomass_Mean+Plant_Biomass_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,1,1,1),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Monthly_Precip")+
  ylab("Biomass (g)")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")


Monthly_Precip_Graph_Biomass_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Monthly_Precip,y=Plant_Biomass_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=Plant_Biomass_Mean-Plant_Biomass_St_Error,ymax=Plant_Biomass_Mean+Plant_Biomass_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(0,0,0,1,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Monthly_Precip")+
  ylab("Biomass (g)")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### LDMC Figure ####
Monthly_Precip_Graph_LDMC_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Monthly_Precip,y=LDMC_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=LDMC_Mean-LDMC_St_Error,ymax=LDMC_Mean+LDMC_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(0,0,0,0,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Monthly_Precip")+
  ylab("LDMC")+
  expand_limits(y=c(0,15))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Monthly_Precip_Graph_LDMC_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Monthly_Precip,y=LDMC_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=LDMC_Mean-LDMC_St_Error,ymax=LDMC_Mean+LDMC_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,1,1,0),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Monthly_Precip")+
  ylab("LDMC")+
  expand_limits(y=c(0,15))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### SLA Figure ####
Monthly_Precip_Graph_SLA_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Monthly_Precip,y=SLA_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=SLA_Mean-SLA_St_Error,ymax=SLA_Mean+SLA_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,0,0,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Monthly_Precip")+
  ylab("SLA")+
  expand_limits(y=c(0,8000))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position="none")

Monthly_Precip_Graph_SLA_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Monthly_Precip,y=SLA_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=SLA_Mean-SLA_St_Error,ymax=SLA_Mean+SLA_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,0,0,0),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Monthly_Precip")+
  ylab("SLA")+
  expand_limits(y=c(0,8000))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position="none")

#### Create Through time Trait x Monthly Precip Figure####
Monthly_Precip_Graph_Height_FK+
  Monthly_Precip_Graph_Height_TB+
  Monthly_Precip_Graph_LeafArea_FK+
  Monthly_Precip_Graph_LeafArea_TB+
  Monthly_Precip_Graph_Thickness_FK+
  Monthly_Precip_Graph_Thickness_TB+
  Monthly_Precip_Graph_Biomass_FK+
  Monthly_Precip_Graph_Biomass_TB+
  Monthly_Precip_Graph_LDMC_FK+
  Monthly_Precip_Graph_LDMC_TB+
  Monthly_Precip_Graph_SLA_FK+
  Monthly_Precip_Graph_SLA_TB+
  plot_layout(ncol = 2,nrow = 6)
#save at 3000x5000

#### Normality: FK - height_cm: ####

#BRAR
Monthly_Precip_Monthly_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(height_cm)), log(height_cm)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Monthly_Precip_Norm_FK_BRAR) 
ols_test_normality(Monthly_Precip_Monthly_Precip_Norm_FK_BRAR) #normal

#HECO
Monthly_Precip_Monthly_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(height_cm)), (log(height_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Monthly_Precip_Norm_FK_HECO) 
ols_test_normality(Monthly_Precip_Monthly_Precip_Norm_FK_HECO) #normal

#KOMA
Monthly_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(height_cm)), (log(height_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_KOMA)
ols_test_normality(Monthly_Precip_Norm_FK_KOMA) #normalish

#SPCO
Monthly_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(height_cm)), (log(height_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_SPCO) 
ols_test_normality(Monthly_Precip_Norm_FK_SPCO) #normal

#TRDU
Monthly_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(height_cm)), ((height_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_TRDU) 
ols_test_normality(Monthly_Precip_Norm_FK_TRDU) #normal


#### Normality: TB - height_cm: ####

#BOGR
Monthly_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(height_cm)), height_cm  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_BOGR) 
ols_test_normality(Monthly_Precip_Norm_TB_BOGR) #normal

#KOMA
Monthly_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(height_cm)), (1/log(height_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_KOMA) 
ols_test_normality(Monthly_Precip_Norm_TB_KOMA) #normal

#LOAR
Monthly_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(height_cm)), (log(height_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_LOAR) 
ols_test_normality(Monthly_Precip_Norm_TB_LOAR) #normal

#PASM
Monthly_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(height_cm)), (sqrt(height_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_PASM) 
ols_test_normality(Monthly_Precip_Norm_TB_PASM) #normal

#VIAM
Monthly_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(height_cm)), (log(height_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_VIAM) 
ols_test_normality(Monthly_Precip_Norm_TB_VIAM) #normal

#### Normality: FK - leaf_area_cm: ####

#BRAR
Monthly_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_area_cm)), (1/log(leaf_area_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_BRAR) 
ols_test_normality(Monthly_Precip_Norm_FK_BRAR) #normal

#HECO
Monthly_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_HECO) 
ols_test_normality(Monthly_Precip_Norm_FK_HECO) #normal

#KOMA
Monthly_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_area_cm)), (1/sqrt(leaf_area_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_KOMA) 
ols_test_normality(Monthly_Precip_Norm_FK_KOMA) #normal

#SPCO
Monthly_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_SPCO) 
ols_test_normality(Monthly_Precip_Norm_FK_SPCO) #normal

#TRDU
Monthly_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_TRDU) 
ols_test_normality(Monthly_Precip_Norm_FK_TRDU) #normal


#### Normality: TB - leaf_area_cm: ####

#BOGR
Monthly_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_BOGR) 
ols_test_normality(Monthly_Precip_Norm_TB_BOGR) #normal

#KOMA
Monthly_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_area_cm)), (1/exp(leaf_area_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_KOMA) 
ols_test_normality(Monthly_Precip_Norm_TB_KOMA) #normal

#LOAR
Monthly_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_LOAR) 
ols_test_normality(Monthly_Precip_Norm_TB_LOAR) #normal

#PASM
Monthly_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_PASM) 
ols_test_normality(Monthly_Precip_Norm_TB_PASM) #normal

#VIAM
Monthly_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_VIAM) 
ols_test_normality(Monthly_Precip_Norm_TB_VIAM) #normal

#### Normality: FK - leaf_thickness_mm: ####

#BRAR
Monthly_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_BRAR) 
ols_test_normality(Monthly_Precip_Norm_FK_BRAR) #normal

#HECO
Monthly_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_HECO) 
ols_test_normality(Monthly_Precip_Norm_FK_HECO) #normal

#KOMA
#
Monthly_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_thickness_mm)), (1/log(leaf_thickness_mm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_KOMA) 
ols_test_normality(Monthly_Precip_Norm_FK_KOMA) #normal

#SPCO
#
Monthly_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_SPCO) 
ols_test_normality(Monthly_Precip_Norm_FK_SPCO) #normalish

#TRDU
#
Monthly_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_TRDU) 
ols_test_normality(Monthly_Precip_Norm_FK_TRDU) #normal


#### Normality: TB - leaf_thickness_mm: ####

#BOGR
#
Monthly_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_thickness_mm)), log(leaf_thickness_mm)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_BOGR) 
ols_test_normality(Monthly_Precip_Norm_TB_BOGR) #normal

#KOMA
#
Monthly_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_thickness_mm)), (log(leaf_thickness_mm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_KOMA) 
ols_test_normality(Monthly_Precip_Norm_TB_KOMA) #normal

#LOAR
#
Monthly_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_LOAR) 
ols_test_normality(Monthly_Precip_Norm_TB_LOAR) #normal

#PASM
#
Monthly_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_thickness_mm)), (sqrt(leaf_thickness_mm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_PASM) 
ols_test_normality(Monthly_Precip_Norm_TB_PASM) #normal

#VIAM
#
Monthly_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_thickness_mm)), (sqrt(leaf_thickness_mm))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_VIAM) 
ols_test_normality(Monthly_Precip_Norm_TB_VIAM) #normal


#### Normality: FK - Plant Biomass: ####

#BRAR
Monthly_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_BRAR) 
ols_test_normality(Monthly_Precip_Norm_FK_BRAR) #normal

#HECO
Monthly_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_HECO) 
ols_test_normality(Monthly_Precip_Norm_FK_HECO) #normal

#KOMA
#
Monthly_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(Plant_Biomass)), (log(Plant_Biomass))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_KOMA) 
ols_test_normality(Monthly_Precip_Norm_FK_KOMA) #normal

#SPCO
#
Monthly_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_SPCO) 
ols_test_normality(Monthly_Precip_Norm_FK_SPCO) #normal

#TRDU
#
Monthly_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(Plant_Biomass)), ((Plant_Biomass))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_TRDU) 
ols_test_normality(Monthly_Precip_Norm_FK_TRDU) #normal


#### Normality: TB - Plant_Biomass ####

#BOGR
#
Monthly_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_BOGR) 
ols_test_normality(Monthly_Precip_Norm_TB_BOGR) #normal

#KOMA
#
Monthly_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass) ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_KOMA) 
ols_test_normality(Monthly_Precip_Norm_TB_KOMA) #normalish

#LOAR
#
Monthly_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(Plant_Biomass)), (log(Plant_Biomass))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_LOAR) 
ols_test_normality(Monthly_Precip_Norm_TB_LOAR) #normal

#PASM
#
Monthly_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_PASM) 
ols_test_normality(Monthly_Precip_Norm_TB_PASM) #normal

#VIAM
#
Monthly_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(Plant_Biomass)), 1/sqrt(Plant_Biomass)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_VIAM) 
ols_test_normality(Monthly_Precip_Norm_TB_VIAM) #normalish


#### Normality: FK - LDMC: ####

#BRAR
Monthly_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(LDMC)), (log(LDMC))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_BRAR) 
ols_test_normality(Monthly_Precip_Norm_FK_BRAR) #normalish

#HECO
Monthly_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(LDMC)), (1/log1p(LDMC))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_HECO) 
ols_test_normality(Monthly_Precip_Norm_FK_HECO) #not normal but looks okay

#KOMA
#
Monthly_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(LDMC)), (1/log(LDMC))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_KOMA) 
ols_test_normality(Monthly_Precip_Norm_FK_KOMA) #not normal but looks okay

#SPCO
#
Monthly_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(LDMC)), 1/log(LDMC)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_SPCO) 
ols_test_normality(Monthly_Precip_Norm_FK_SPCO) #normal

#TRDU
#
Monthly_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(LDMC)), (log(LDMC))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_TRDU) 
ols_test_normality(Monthly_Precip_Norm_FK_TRDU) #not normal but looks okay


#### Normality: TB - LDMC ####

#BOGR
#
Monthly_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_BOGR) 
ols_test_normality(Monthly_Precip_Norm_TB_BOGR)  #not normal but looks okay

#KOMA
#
Monthly_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(LDMC)), 1/log(LDMC) ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_KOMA) 
ols_test_normality(Monthly_Precip_Norm_TB_KOMA) #normalish

#LOAR
#
Monthly_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(LDMC)), (1/exp(LDMC))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_LOAR) 
ols_test_normality(Monthly_Precip_Norm_TB_LOAR) #not normal but looks okay

#PASM
#
Monthly_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(LDMC)),(1/log(LDMC))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_PASM) 
ols_test_normality(Monthly_Precip_Norm_TB_PASM) #not normal but looks okay

#VIAM
#
Monthly_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(LDMC)), sqrt(LDMC)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_VIAM) 
ols_test_normality(Monthly_Precip_Norm_TB_VIAM) #normalish

#### Normality: FK - SLA: ####

#BRAR
Monthly_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_BRAR) 
ols_test_normality(Monthly_Precip_Norm_FK_BRAR) #normal

#HEC
Monthly_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_HECO) 
ols_test_normality(Monthly_Precip_Norm_FK_HECO) #not normal but looks okay

#KOMA
#
Monthly_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(SLA)), (sqrt(SLA))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_KOMA) 
ols_test_normality(Monthly_Precip_Norm_FK_KOMA) #normalish

#SPCO
#
Monthly_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(SLA)), (1/sqrt(SLA)) ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_SPCO) 
ols_test_normality(Monthly_Precip_Norm_FK_SPCO) #normalish

#TRDU
#
Monthly_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(SLA)), (1/log(SLA))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_FK_TRDU) 
ols_test_normality(Monthly_Precip_Norm_FK_TRDU) #normalish


#### Normality: TB - SLA ####

#BOGR
#
Monthly_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(SLA)), 1/sqrt(SLA)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_BOGR) 
ols_test_normality(Monthly_Precip_Norm_TB_BOGR) #normal

#KOMA
#
Monthly_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(SLA)), (1/sqrt(SLA)) ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_KOMA) 
ols_test_normality(Monthly_Precip_Norm_TB_KOMA) #not normal but looks okay

#LOAR
#
Monthly_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(SLA)), (1/(SLA))  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_LOAR) 
ols_test_normality(Monthly_Precip_Norm_TB_LOAR) #normalish

#PASM
#
Monthly_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(SLA)), log(SLA)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_PASM) 
ols_test_normality(Monthly_Precip_Norm_TB_PASM) #not normal but looks okay

#VIAM
#
Monthly_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(SLA)), 1/log(SLA)  ~ Monthly_Precip)
ols_plot_resid_hist(Monthly_Precip_Norm_TB_VIAM) 
ols_test_normality(Monthly_Precip_Norm_TB_VIAM) #normal



#### Stats: FK - height_cm ####

#BRAR
Monthy_Precip_FKBRAR_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(height_cm)), log(height_cm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKBRAR_H, type = 3) #NS

#HECO
Monthy_Precip_FKHECO_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(height_cm)), log(height_cm) ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKHECO_H, type = 3) #0.01863

#KOMA
Monthy_Precip_FKKOMA_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(height_cm)), log(height_cm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKKOMA_H, type = 3) #NS

#SPCO
Monthy_Precip_FKSPCO_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(height_cm)), log(height_cm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKSPCO_H, type = 3) #0.01029

#TRDU
Monthy_Precip_FKTRDU_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(height_cm)), height_cm  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKTRDU_H, type = 3) #NS

#### Stats: TB - height_cm ####

#BOGR
Monthy_Precip_TB_BOGR_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(height_cm)), height_cm  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_BOGR_H, type = 3) #NS

#KOMA
Monthy_Precip_TB_KOMA_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(height_cm)), (1/log(height_cm))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_KOMA_H, type = 3) #NS

#LOAR
Monthy_Precip_TB_LOAR_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(height_cm)),log(height_cm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_LOAR_H, type = 3) #NS

#PASM
Monthy_Precip_TB_PASM_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(height_cm)), sqrt(height_cm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_PASM_H, type = 3) #0.04595

#VIAM
Monthy_Precip_TB_VIAM_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(height_cm)),log(height_cm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_VIAM_H, type = 3) #NS


#### Stats: FK - leaf_area_cm ####

#BRAR
Monthy_Precip_FKBRAR_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_area_cm)), (1/log(leaf_area_cm))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKBRAR_LA, type = 3) #0.001057

#HECO
Monthy_Precip_FKHECO_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_area_cm)), log(leaf_area_cm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKHECO_LA, type = 3) #NS

#KOMA
Monthy_Precip_FKKOMA_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_area_cm)), (1/sqrt(leaf_area_cm))   ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKKOMA_LA, type = 3) #NS

#SPCO
Monthy_Precip_FKSPCO_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKSPCO_LA, type = 3) #0.03583

#TRDU
Monthy_Precip_FKTRDU_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKTRDU_LA, type = 3) #NS

#### Stats: TB - leaf_area_cm ####

#BOGR
Monthy_Precip_TB_BOGR_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_BOGR_LA, type = 3) #0.009031

#KOMA
Monthy_Precip_TB_KOMA_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_area_cm)), (1/exp(leaf_area_cm))   ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_KOMA_LA, type = 3) #NS

#LOAR
Monthy_Precip_TB_LOAR_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_area_cm)), log(leaf_area_cm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_LOAR_LA, type = 3) #NS

#PASM
Monthy_Precip_TB_PASM_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_area_cm)), sqrt(leaf_area_cm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_PASM_LA, type = 3) #NS

#VIAM
Monthy_Precip_TB_VIAM_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_area_cm)), sqrt(leaf_area_cm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_VIAM_LA, type = 3) #0.01053

#### Stats: FK - leaf_thickness_mm ####

#BRAR
Monthy_Precip_FKBRAR_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKBRAR_LT, type = 3) #NS

#HECO
Monthy_Precip_FKHECO_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKHECO_LT, type = 3) #NS

#KOMA
Monthy_Precip_FKKOMA_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_thickness_mm)), (1/log(leaf_thickness_mm))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKKOMA_LT, type = 3) #NS

#SPCO
Monthy_Precip_FKSPCO_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FKSPCO_LT, type = 3) #NS

#TRDU
Monthy_Precip_FK_TRDU_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_TRDU_LT, type = 3) #NS

#### Stats: TB - leaf_thickness_mm ####

#BOGR
Monthy_Precip_TB_BOGR_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_thickness_mm)), log(leaf_thickness_mm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_BOGR_LT, type = 3) #0.03699

#KOMA
Monthy_Precip_TB_KOMA_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_thickness_mm)), (log(leaf_thickness_mm))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_KOMA_LT, type = 3) #0.005529

#LOAR
Monthy_Precip_TB_LOAR_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_LOAR_LT, type = 3) #9.081e-08 

#PASM
Monthy_Precip_TB_PASM_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_thickness_mm)), sqrt(leaf_thickness_mm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_PASM_LT, type = 3) #0.0005

#VIAM
Monthy_Precip_TB_VIAM_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_thickness_mm)), sqrt(leaf_thickness_mm)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_VIAM_LT, type = 3) #3.832e-05


#### Stats: FK - Plant_Biomass ####

#BRAR
Monthy_Precip_FK_BRAR_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_BRAR_BM, type = 3) #4.861e-05

#HECO
Monthy_Precip_FK_HECO_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_HECO_BM, type = 3) #4.439e0-05

#KOMA
Monthy_Precip_FK_KOMA_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_KOMA_BM, type = 3) #0.029

#SPCO
Monthy_Precip_FK_SPCO_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_SPCO_BM, type = 3) #0.006213

#TRDU
Monthy_Precip_FK_TRDU_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(Plant_Biomass)), Plant_Biomass  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_TRDU_BM, type = 3) #0.004719

#### Stats: TB - Plant_Biomass ####

#BOGR
Monthy_Precip_TB_BOGR_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass) ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_BOGR_BM, type = 3) #NS

#KOMA
Monthy_Precip_TB_KOMA_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_KOMA_BM, type = 3) #NS

#LOAR
Monthy_Precip_TB_LOAR_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_LOAR_BM, type = 3) #NS

#PASM
Monthy_Precip_TB_PASM_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_PASM_BM, type = 3) #0.02

#VIAM
Monthy_Precip_TB_VIAM_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(Plant_Biomass)), (1/sqrt(Plant_Biomass))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_VIAM_BM, type = 3) #0.0001

#### Stats: FK - LDMC ####

#BRAR
Monthy_Precip_FK_BRAR_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_BRAR_LDMC, type = 3) #NS

#HECO
Monthy_Precip_FK_HECO_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(LDMC)), (1/log1p(LDMC))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_HECO_LDMC, type = 3) #NS

#KOMA
Monthy_Precip_FK_KOMA_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_KOMA_LDMC, type = 3) #NS

#SPCO
Monthy_Precip_FK_SPCO_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(LDMC)), (1/log(LDMC))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_SPCO_LDMC, type = 3) #NS

#TRDU
Monthy_Precip_FK_TRDU_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_TRDU_LDMC, type = 3) #NS

#### Stats: TB - LDMC ####

#BOGR
Monthy_Precip_TB_BOGR_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_BOGR_LDMC, type = 3) #0.01345

#KOMA
Monthy_Precip_TB_KOMA_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(LDMC)), 1/log(LDMC)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_KOMA_LDMC, type = 3) #NS

#LOAR
Monthy_Precip_TB_LOAR_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(LDMC)), 1/exp(LDMC)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_LOAR_LDMC, type = 3) #0.01533

#PASM
Monthy_Precip_TB_PASM_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(LDMC)), 1/log(LDMC)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_PASM_LDMC, type = 3) #0.005357

#VIAM
Monthy_Precip_TB_VIAM_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(LDMC)), sqrt(LDMC)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_VIAM_LDMC, type = 3) #NS

#### Stats: FK - SLA ####

#BRAR
Monthy_Precip_FK_BRAR_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_BRAR_SLA, type = 3) #0.003976

#HECO
Monthy_Precip_FK_HECO_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_HECO_SLA, type = 3) #NS

#KOMA
Monthy_Precip_FK_KOMA_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(SLA)), sqrt(SLA)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_KOMA_SLA, type = 3) #NS

#SPCO
Monthy_Precip_FK_SPCO_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(SLA)), (1/sqrt(SLA))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_SPCO_SLA, type = 3) #NS

#TRDU
Monthy_Precip_FK_TRDU_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(SLA)), 1/log(SLA)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_FK_TRDU_SLA, type = 3) #NS

#### Stats: TB - SLA ####

#BOGR
Monthy_Precip_TB_BOGR_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(SLA)), (1/sqrt(SLA))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_BOGR_SLA, type = 3) #0.003342

#KOMA
Monthy_Precip_TB_KOMA_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(SLA)), (1/sqrt(SLA))  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_KOMA_SLA, type = 3) #NS

#LOAR
Monthy_Precip_TB_LOAR_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(SLA)), 1/(SLA)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_LOAR_SLA, type = 3) #NS

#PASM
Monthy_Precip_TB_PASM_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(SLA)), log(SLA)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_PASM_SLA, type = 3) #NS

#VIAM
Monthy_Precip_TB_VIAM_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(SLA)), 1/log(SLA)  ~ Monthly_Precip + (1|block))
anova(Monthy_Precip_TB_VIAM_SLA, type = 3) #NS



#### Traits by Year to Date Precip ####
#### Height Figure ####
YeartoDate_Precip_Graph_Height_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=YeartoDate_Precip,y=height_cm_Mean,color=species_code,fill=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=height_cm_Mean-height_cm_St_Error,ymax=height_cm_Mean+height_cm_St_Error),linewidth = 3)+
  xlab("YeartoDate_Precip")+
  ylab("Height (cm)")+
  scale_linetype_manual(values=c(1,1,1,1,1),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  expand_limits(y=c(0,40))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

YeartoDate_Precip_Graph_Height_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=YeartoDate_Precip,y=height_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=height_cm_Mean-height_cm_St_Error,ymax=height_cm_Mean+height_cm_St_Error),linewidth = 3)+
  xlab("YeartoDate_Precip")+
  ylab("Height (cm)")+
  #scale_alpha_manual(values=c(1,0,1,0,1))+
  scale_linetype_manual(values=c(0,0,0,0,0),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  expand_limits(y=c(0,40))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Leaf Area Figure ####
YeartoDate_Precip_Graph_LeafArea_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=YeartoDate_Precip,y=leaf_area_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_area_cm_Mean-leaf_area_cm_St_Error,ymax=leaf_area_cm_Mean+leaf_area_cm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,0,0,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("YeartoDate_Precip")+
  ylab("Leaf Area (cm)")+
  expand_limits(y=c(0,10))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

YeartoDate_Precip_Graph_LeafArea_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=YeartoDate_Precip,y=leaf_area_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_area_cm_Mean-leaf_area_cm_St_Error,ymax=leaf_area_cm_Mean+leaf_area_cm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(0,0,1,0,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("YeartoDate_Precip")+
  ylab("Leaf Area (cm)")+
  expand_limits(y=c(0,10))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Leaf Thickness Figure ####
YeartoDate_Precip_Graph_Thickness_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=YeartoDate_Precip,y=leaf_thickness_mm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_thickness_mm_Mean-leaf_thickness_mm_St_Error,ymax=leaf_thickness_mm_Mean+leaf_thickness_mm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,1,1,1),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("YeartoDate_Precip")+
  ylab("Leaf Thickness (mm)")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

YeartoDate_Precip_Graph_Thickness_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=YeartoDate_Precip,y=leaf_thickness_mm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_thickness_mm_Mean-leaf_thickness_mm_St_Error,ymax=leaf_thickness_mm_Mean+leaf_thickness_mm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,1,1,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("YeartoDate_Precip")+
  ylab("Leaf Thickness (mm)")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Biomass  Figure ####
YeartoDate_Precip_Graph_Biomass_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=YeartoDate_Precip,y=Plant_Biomass_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=Plant_Biomass_Mean-Plant_Biomass_St_Error,ymax=Plant_Biomass_Mean+Plant_Biomass_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,0,1,1),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("YeartoDate_Precip")+
  ylab("Biomass (g)")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")


YeartoDate_Precip_Graph_Biomass_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=YeartoDate_Precip,y=Plant_Biomass_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=Plant_Biomass_Mean-Plant_Biomass_St_Error,ymax=Plant_Biomass_Mean+Plant_Biomass_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(0,0,0,0,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("YeartoDate_Precip")+
  ylab("Biomass (g)")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### LDMC Figure ####
YeartoDate_Precip_Graph_LDMC_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=YeartoDate_Precip,y=LDMC_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=LDMC_Mean-LDMC_St_Error,ymax=LDMC_Mean+LDMC_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,0,0,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("YeartoDate_Precip")+
  ylab("LDMC")+
  expand_limits(y=c(0,15))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

YeartoDate_Precip_Graph_LDMC_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=YeartoDate_Precip,y=LDMC_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=LDMC_Mean-LDMC_St_Error,ymax=LDMC_Mean+LDMC_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,1,0,0),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("YeartoDate_Precip")+
  ylab("LDMC")+
  expand_limits(y=c(0,15))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### SLA Figure ####
YeartoDate_Precip_Graph_SLA_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=YeartoDate_Precip,y=SLA_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=SLA_Mean-SLA_St_Error,ymax=SLA_Mean+SLA_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,1,1,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("YeartoDate_Precip")+
  ylab("SLA")+
  expand_limits(y=c(0,8000))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position="none")

YeartoDate_Precip_Graph_SLA_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=YeartoDate_Precip,y=SLA_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=SLA_Mean-SLA_St_Error,ymax=SLA_Mean+SLA_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,0,0,0),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("YeartoDate_Precip")+
  ylab("SLA")+
  expand_limits(y=c(0,8000))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position="none")

#### Create Through time Trait Figure####
YeartoDate_Precip_Graph_Height_FK+
  YeartoDate_Precip_Graph_Height_TB+
  YeartoDate_Precip_Graph_LeafArea_FK+
  YeartoDate_Precip_Graph_LeafArea_TB+
  YeartoDate_Precip_Graph_Thickness_FK+
  YeartoDate_Precip_Graph_Thickness_TB+
  YeartoDate_Precip_Graph_Biomass_FK+
  YeartoDate_Precip_Graph_Biomass_TB+
  YeartoDate_Precip_Graph_LDMC_FK+
  YeartoDate_Precip_Graph_LDMC_TB+
  YeartoDate_Precip_Graph_SLA_FK+
  YeartoDate_Precip_Graph_SLA_TB+
  plot_layout(ncol = 2,nrow = 6)
#save at 3000x5000

#### Normality: FK - height_cm: ####

#BRAR
YeartoDate_Precip_YeartoDate_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(height_cm)), log(height_cm)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_YeartoDate_Precip_Norm_FK_BRAR) 
ols_test_normality(YeartoDate_Precip_YeartoDate_Precip_Norm_FK_BRAR) #normal

#HECO
YeartoDate_Precip_YeartoDate_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(height_cm)), (log(height_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_YeartoDate_Precip_Norm_FK_HECO) 
ols_test_normality(YeartoDate_Precip_YeartoDate_Precip_Norm_FK_HECO) #normalish

#KOMA
YeartoDate_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(height_cm)), (1/log(height_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_KOMA)
ols_test_normality(YeartoDate_Precip_Norm_FK_KOMA) #normal

#SPCO
YeartoDate_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(height_cm)), (log(height_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_SPCO) 
ols_test_normality(YeartoDate_Precip_Norm_FK_SPCO) #normal

#TRDU
YeartoDate_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(height_cm)), ((height_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_TRDU) 
ols_test_normality(YeartoDate_Precip_Norm_FK_TRDU) #normal


#### Normality: TB - height_cm: ####

#BOGR
YeartoDate_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(height_cm)), height_cm  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_BOGR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_BOGR) #normal

#KOMA
YeartoDate_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(height_cm)), (1/log(height_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_KOMA) 
ols_test_normality(YeartoDate_Precip_Norm_TB_KOMA) #normal

#LOAR
YeartoDate_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(height_cm)), (log(height_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_LOAR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_LOAR) #normal

#PASM
YeartoDate_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(height_cm)), (sqrt(height_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_PASM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_PASM) #normal

#VIAM
YeartoDate_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(height_cm)), (log(height_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_VIAM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_VIAM) #normal

#### Normality: FK - leaf_area_cm: ####

#BRAR
YeartoDate_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_area_cm)), (1/log(leaf_area_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_BRAR) 
ols_test_normality(YeartoDate_Precip_Norm_FK_BRAR) #normal

#HECO
YeartoDate_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_HECO) 
ols_test_normality(YeartoDate_Precip_Norm_FK_HECO) #normal

#KOMA
YeartoDate_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_area_cm)), (1/sqrt(leaf_area_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_KOMA) 
ols_test_normality(YeartoDate_Precip_Norm_FK_KOMA) #normal

#SPCO
YeartoDate_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_SPCO) 
ols_test_normality(YeartoDate_Precip_Norm_FK_SPCO) #normal

#TRDU
YeartoDate_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_TRDU) 
ols_test_normality(YeartoDate_Precip_Norm_FK_TRDU) #normal


#### Normality: TB - leaf_area_cm: ####

#BOGR
YeartoDate_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_BOGR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_BOGR) #normal

#KOMA
YeartoDate_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_area_cm)), (1/exp(leaf_area_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_KOMA) 
ols_test_normality(YeartoDate_Precip_Norm_TB_KOMA) #normal

#LOAR
YeartoDate_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_LOAR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_LOAR) #normal

#PASM
YeartoDate_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_PASM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_PASM) #normal

#VIAM
YeartoDate_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_VIAM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_VIAM) #normal

#### Normality: FK - leaf_thickness_mm: ####

#BRAR
YeartoDate_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_BRAR) 
ols_test_normality(YeartoDate_Precip_Norm_FK_BRAR) #normal

#HECO
YeartoDate_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_HECO) 
ols_test_normality(YeartoDate_Precip_Norm_FK_HECO) #normal

#KOMA
#
YeartoDate_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_thickness_mm)), (1/log(leaf_thickness_mm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_KOMA) 
ols_test_normality(YeartoDate_Precip_Norm_FK_KOMA) #normal

#SPCO
#
YeartoDate_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/log1p(leaf_thickness_mm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_SPCO) 
ols_test_normality(YeartoDate_Precip_Norm_FK_SPCO) #normalish

#TRDU
#
YeartoDate_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_TRDU) 
ols_test_normality(YeartoDate_Precip_Norm_FK_TRDU) #normal


#### Normality: TB - leaf_thickness_mm: ####

#BOGR
#
YeartoDate_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_thickness_mm)), 1/sqrt(leaf_thickness_mm)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_BOGR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_BOGR) #normal

#KOMA
#
YeartoDate_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_thickness_mm)), (log(leaf_thickness_mm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_KOMA) 
ols_test_normality(YeartoDate_Precip_Norm_TB_KOMA) #normal

#LOAR
#
YeartoDate_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_LOAR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_LOAR) #normal

#PASM
#
YeartoDate_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_thickness_mm)), (sqrt(leaf_thickness_mm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_PASM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_PASM) #normal

#VIAM
#
YeartoDate_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_thickness_mm)), (sqrt(leaf_thickness_mm))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_VIAM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_VIAM) #normal


#### Normality: FK - Plant Biomass: ####

#BRAR
YeartoDate_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_BRAR) 
ols_test_normality(YeartoDate_Precip_Norm_FK_BRAR) #normal

#HECO
YeartoDate_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(Plant_Biomass)), (1/log1p(Plant_Biomass))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_HECO) 
ols_test_normality(YeartoDate_Precip_Norm_FK_HECO) #normal

#KOMA
#
YeartoDate_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(Plant_Biomass)), (log(Plant_Biomass))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_KOMA) 
ols_test_normality(YeartoDate_Precip_Norm_FK_KOMA) #normal

#SPCO
#
YeartoDate_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_SPCO) 
ols_test_normality(YeartoDate_Precip_Norm_FK_SPCO) #normal

#TRDU
#
YeartoDate_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(Plant_Biomass)), ((Plant_Biomass))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_TRDU) 
ols_test_normality(YeartoDate_Precip_Norm_FK_TRDU) #normal


#### Normality: TB - Plant_Biomass ####

#BOGR
#
YeartoDate_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_BOGR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_BOGR) #normal

#KOMA
#
YeartoDate_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass) ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_KOMA) 
ols_test_normality(YeartoDate_Precip_Norm_TB_KOMA) #normalish

#LOAR
#
YeartoDate_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(Plant_Biomass)), (log(Plant_Biomass))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_LOAR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_LOAR) #normal

#PASM
#
YeartoDate_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_PASM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_PASM) #normal

#VIAM
#
YeartoDate_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(Plant_Biomass)), 1/sqrt(Plant_Biomass)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_VIAM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_VIAM) #normalish


#### Normality: FK - LDMC: ####

#BRAR
YeartoDate_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(LDMC)), (log(LDMC))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_BRAR) 
ols_test_normality(YeartoDate_Precip_Norm_FK_BRAR) #normalish

#HECO
YeartoDate_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(LDMC)), (1/sqrt(LDMC))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_HECO) 
ols_test_normality(YeartoDate_Precip_Norm_FK_HECO) #not normal but looks okay

#KOMA
#
YeartoDate_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(LDMC)), (1/log(LDMC))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_KOMA) 
ols_test_normality(YeartoDate_Precip_Norm_FK_KOMA) #not normal but looks okay

#SPCO
#
YeartoDate_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(LDMC)), 1/log(LDMC)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_SPCO) 
ols_test_normality(YeartoDate_Precip_Norm_FK_SPCO) #normal

#TRDU
#
YeartoDate_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(LDMC)), (log(LDMC))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_TRDU) 
ols_test_normality(YeartoDate_Precip_Norm_FK_TRDU) #not normal but looks okay


#### Normality: TB - LDMC ####

#BOGR
#
YeartoDate_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_BOGR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_BOGR)  #not normal but looks okay

#KOMA
#
YeartoDate_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(LDMC)), 1/log(LDMC) ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_KOMA) 
ols_test_normality(YeartoDate_Precip_Norm_TB_KOMA) #normalish

#LOAR
#
YeartoDate_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(LDMC)), (log1p(LDMC))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_LOAR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_LOAR) #not normal but looks okay

#PASM
#
YeartoDate_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(LDMC)),(1/sqrt(LDMC))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_PASM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_PASM) #not normal but looks okay

#VIAM
#
YeartoDate_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_VIAM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_VIAM) #normalish

#### Normality: FK - SLA: ####

#BRAR
YeartoDate_Precip_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(SLA)), log(SLA)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_BRAR) 
ols_test_normality(YeartoDate_Precip_Norm_FK_BRAR) #normal

#HECO
YeartoDate_Precip_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_HECO) 
ols_test_normality(YeartoDate_Precip_Norm_FK_HECO) #not normal but looks okay

#KOMA
#
YeartoDate_Precip_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_KOMA) 
ols_test_normality(YeartoDate_Precip_Norm_FK_KOMA) #normalish

#SPCO
#
YeartoDate_Precip_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(SLA)), (1/sqrt(SLA)) ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_SPCO) 
ols_test_normality(YeartoDate_Precip_Norm_FK_SPCO) #normalish

#TRDU
#
YeartoDate_Precip_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_FK_TRDU) 
ols_test_normality(YeartoDate_Precip_Norm_FK_TRDU) #normalish


#### Normality: TB - SLA ####

#BOGR
#
YeartoDate_Precip_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(SLA)), 1/sqrt(SLA)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_BOGR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_BOGR) #normal

#KOMA
#
YeartoDate_Precip_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(SLA)), (1/log(SLA)) ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_KOMA) 
ols_test_normality(YeartoDate_Precip_Norm_TB_KOMA) #not normal but looks okay

#LOAR
#
YeartoDate_Precip_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(SLA)), (1/(SLA))  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_LOAR) 
ols_test_normality(YeartoDate_Precip_Norm_TB_LOAR) #normalish

#PASM
#
YeartoDate_Precip_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(SLA)), sqrt(SLA)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_PASM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_PASM) #not normal but looks okay

#VIAM
#
YeartoDate_Precip_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(SLA)), 1/log(SLA)  ~ YeartoDate_Precip)
ols_plot_resid_hist(YeartoDate_Precip_Norm_TB_VIAM) 
ols_test_normality(YeartoDate_Precip_Norm_TB_VIAM) #normal



#### Stats: FK - height_cm ####

#BRAR
YeartoDate_Precip_FKBRAR_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(height_cm)), log(height_cm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKBRAR_H, type = 3) #0.0003

#HECO
YeartoDate_Precip_FKHECO_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(height_cm)), log(height_cm) ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKHECO_H, type = 3) #1.097e-05

#KOMA
YeartoDate_Precip_FKKOMA_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(height_cm)), 1/log(height_cm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKKOMA_H, type = 3) #3.255e-05

#SPCO
YeartoDate_Precip_FKSPCO_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(height_cm)), log(height_cm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKSPCO_H, type = 3) #1.133e-06

#TRDU
YeartoDate_Precip_FKTRDU_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(height_cm)), height_cm  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKTRDU_H, type = 3) #0.0005

#### Stats: TB - height_cm ####

#BOGR
YeartoDate_Precip_TB_BOGR_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(height_cm)), height_cm  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_BOGR_H, type = 3) #NS

#KOMA
YeartoDate_Precip_TB_KOMA_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(height_cm)), (1/log(height_cm))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_KOMA_H, type = 3) #NS

#LOAR
YeartoDate_Precip_TB_LOAR_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(height_cm)),log(height_cm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_LOAR_H, type = 3) #NS

#PASM
YeartoDate_Precip_TB_PASM_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(height_cm)), sqrt(height_cm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_PASM_H, type = 3) #NS

#VIAM
YeartoDate_Precip_TB_VIAM_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(height_cm)),log(height_cm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_VIAM_H, type = 3) #NS


#### Stats: FK - leaf_area_cm ####

#BRAR
YeartoDate_Precip_FKBRAR_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_area_cm)), (1/log(leaf_area_cm))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKBRAR_LA, type = 3) #0.0019

#HECO
YeartoDate_Precip_FKHECO_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_area_cm)), log(leaf_area_cm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKHECO_LA, type = 3) #NS

#KOMA
YeartoDate_Precip_FKKOMA_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_area_cm)), (1/sqrt(leaf_area_cm))   ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKKOMA_LA, type = 3) #NS

#SPCO
YeartoDate_Precip_FKSPCO_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKSPCO_LA, type = 3) #NS

#TRDU
YeartoDate_Precip_FKTRDU_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKTRDU_LA, type = 3) #NS

#### Stats: TB - leaf_area_cm ####

#BOGR
YeartoDate_Precip_TB_BOGR_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_BOGR_LA, type = 3) #NS

#KOMA
YeartoDate_Precip_TB_KOMA_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_area_cm)), (1/exp(leaf_area_cm))   ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_KOMA_LA, type = 3) #NS

#LOAR
YeartoDate_Precip_TB_LOAR_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_area_cm)), log(leaf_area_cm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_LOAR_LA, type = 3) #NS

#PASM
YeartoDate_Precip_TB_PASM_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_area_cm)), sqrt(leaf_area_cm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_PASM_LA, type = 3) #NS

#VIAM
YeartoDate_Precip_TB_VIAM_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_area_cm)), sqrt(leaf_area_cm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_VIAM_LA, type = 3) #0.01053

#### Stats: FK - leaf_thickness_mm ####

#BRAR
YeartoDate_Precip_FKBRAR_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKBRAR_LT, type = 3) #0.02364

#HECO
YeartoDate_Precip_FKHECO_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKHECO_LT, type = 3) #0.0381

#KOMA
YeartoDate_Precip_FKKOMA_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_thickness_mm)), (1/log(leaf_thickness_mm))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKKOMA_LT, type = 3) #0.0347

#SPCO
YeartoDate_Precip_FKSPCO_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/log1p(leaf_thickness_mm))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FKSPCO_LT, type = 3) #0.001673

#TRDU
YeartoDate_Precip_FK_TRDU_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_TRDU_LT, type = 3) #0.0004422

#### Stats: TB - leaf_thickness_mm ####

#BOGR
YeartoDate_Precip_TB_BOGR_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_thickness_mm)), 1/sqrt(leaf_thickness_mm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_BOGR_LT, type = 3) #0.0001529

#KOMA
YeartoDate_Precip_TB_KOMA_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_thickness_mm)), (log(leaf_thickness_mm))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_KOMA_LT, type = 3) #NS

#LOAR
YeartoDate_Precip_TB_LOAR_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_LOAR_LT, type = 3) #9.081e-08 

#PASM
YeartoDate_Precip_TB_PASM_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_thickness_mm)), sqrt(leaf_thickness_mm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_PASM_LT, type = 3) #6.767e-06

#VIAM
YeartoDate_Precip_TB_VIAM_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_thickness_mm)), sqrt(leaf_thickness_mm)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_VIAM_LT, type = 3) #3.832e-05


#### Stats: FK - Plant_Biomass ####

#BRAR
YeartoDate_Precip_FK_BRAR_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_BRAR_BM, type = 3) #4.027e-05

#HECO
YeartoDate_Precip_FK_HECO_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(Plant_Biomass)), (1/log1p(Plant_Biomass))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_HECO_BM, type = 3) #3.009e-06

#KOMA
YeartoDate_Precip_FK_KOMA_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_KOMA_BM, type = 3) #NS

#SPCO
YeartoDate_Precip_FK_SPCO_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_SPCO_BM, type = 3) #0.0008212

#TRDU
YeartoDate_Precip_FK_TRDU_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(Plant_Biomass)), Plant_Biomass  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_TRDU_BM, type = 3) #0.01458

#### Stats: TB - Plant_Biomass ####

#BOGR
YeartoDate_Precip_TB_BOGR_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass) ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_BOGR_BM, type = 3) #NS

#KOMA
YeartoDate_Precip_TB_KOMA_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_KOMA_BM, type = 3) #NS

#LOAR
YeartoDate_Precip_TB_LOAR_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_LOAR_BM, type = 3) #NS

#PASM
YeartoDate_Precip_TB_PASM_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_PASM_BM, type = 3) #NS

#VIAM
YeartoDate_Precip_TB_VIAM_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(Plant_Biomass)), (1/sqrt(Plant_Biomass))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_VIAM_BM, type = 3) #0.0001765

#### Stats: FK - LDMC ####

#BRAR
YeartoDate_Precip_FK_BRAR_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_BRAR_LDMC, type = 3) #0.007261

#HECO
YeartoDate_Precip_FK_HECO_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(LDMC)), (1/sqrt(LDMC))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_HECO_LDMC, type = 3) #NS

#KOMA
YeartoDate_Precip_FK_KOMA_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(LDMC)), (1/log(LDMC)) ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_KOMA_LDMC, type = 3) #NS

#SPCO
YeartoDate_Precip_FK_SPCO_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(LDMC)), (1/log(LDMC))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_SPCO_LDMC, type = 3) #NS

#TRDU
YeartoDate_Precip_FK_TRDU_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_TRDU_LDMC, type = 3) #NS

#### Stats: TB - LDMC ####

#BOGR
YeartoDate_Precip_TB_BOGR_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_BOGR_LDMC, type = 3) #0.044

#KOMA
YeartoDate_Precip_TB_KOMA_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(LDMC)), 1/log(LDMC)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_KOMA_LDMC, type = 3) #0.004432

#LOAR
YeartoDate_Precip_TB_LOAR_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(LDMC)), (log1p(LDMC))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_LOAR_LDMC, type = 3) #0.03933

#PASM
YeartoDate_Precip_TB_PASM_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(LDMC)), 1/sqrt(LDMC)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_PASM_LDMC, type = 3) #NS

#VIAM
YeartoDate_Precip_TB_VIAM_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_VIAM_LDMC, type = 3) #NS

#### Stats: FK - SLA ####

#BRAR
YeartoDate_Precip_FK_BRAR_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(SLA)), log(SLA)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_BRAR_SLA, type = 3) #0.002

#HECO
YeartoDate_Precip_FK_HECO_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_HECO_SLA, type = 3) #0.00112

#KOMA
YeartoDate_Precip_FK_KOMA_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(SLA)), log(SLA)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_KOMA_SLA, type = 3) #0.0007227

#SPCO
YeartoDate_Precip_FK_SPCO_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(SLA)), (1/sqrt(SLA))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_SPCO_SLA, type = 3) #0.0002256

#TRDU
YeartoDate_Precip_FK_TRDU_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(SLA)), log(SLA)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_FK_TRDU_SLA, type = 3) #NS

#### Stats: TB - SLA ####

#BOGR
YeartoDate_Precip_TB_BOGR_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(SLA)), (1/sqrt(SLA))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_BOGR_SLA, type = 3) #0.008976

#KOMA
YeartoDate_Precip_TB_KOMA_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(SLA)), (1/log(SLA))  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_KOMA_SLA, type = 3) #NS

#LOAR
YeartoDate_Precip_TB_LOAR_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(SLA)), 1/(SLA)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_LOAR_SLA, type = 3) #NS

#PASM
YeartoDate_Precip_TB_PASM_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(SLA)), sqrt(SLA)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_PASM_SLA, type = 3) #NS

#VIAM
YeartoDate_Precip_TB_VIAM_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(SLA)), 1/log(SLA)  ~ YeartoDate_Precip + (1|block))
anova(YeartoDate_Precip_TB_VIAM_SLA, type = 3) #NS



#### Traits by Soil Moisture####
#### Height Figure ####
Soil_Moisture_Graph_Height_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Soil_Moisture,y=height_cm_Mean,color=species_code,fill=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=height_cm_Mean-height_cm_St_Error,ymax=height_cm_Mean+height_cm_St_Error),linewidth = 3)+
  xlab("Soil_Moisture")+
  ylab("Height (cm)")+
  scale_linetype_manual(values=c(1,0,0,0,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  expand_limits(y=c(0,40))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Soil_Moisture_Graph_Height_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Soil_Moisture,y=height_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=height_cm_Mean-height_cm_St_Error,ymax=height_cm_Mean+height_cm_St_Error),linewidth = 3)+
  xlab("Soil_Moisture")+
  ylab("Height (cm)")+
  #scale_alpha_manual(values=c(1,0,1,0,1))+
  scale_linetype_manual(values=c(0,0,0,1,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  expand_limits(y=c(0,40))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Leaf Area Figure ####
Soil_Moisture_Graph_LeafArea_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Soil_Moisture,y=leaf_area_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_area_cm_Mean-leaf_area_cm_St_Error,ymax=leaf_area_cm_Mean+leaf_area_cm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(0,0,0,0,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Soil_Moisture")+
  ylab("Leaf Area (cm)")+
  expand_limits(y=c(0,10))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Soil_Moisture_Graph_LeafArea_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Soil_Moisture,y=leaf_area_cm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_area_cm_Mean-leaf_area_cm_St_Error,ymax=leaf_area_cm_Mean+leaf_area_cm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(0,0,0,0,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Soil_Moisture")+
  ylab("Leaf Area (cm)")+
  expand_limits(y=c(0,10))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Leaf Thickness Figure ####
Soil_Moisture_Graph_Thickness_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Soil_Moisture,y=leaf_thickness_mm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_thickness_mm_Mean-leaf_thickness_mm_St_Error,ymax=leaf_thickness_mm_Mean+leaf_thickness_mm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,1,1,1),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Soil_Moisture")+
  ylab("Leaf Thickness (mm)")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Soil_Moisture_Graph_Thickness_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Soil_Moisture,y=leaf_thickness_mm_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=leaf_thickness_mm_Mean-leaf_thickness_mm_St_Error,ymax=leaf_thickness_mm_Mean+leaf_thickness_mm_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,1,1,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Soil_Moisture")+
  ylab("Leaf Thickness (mm)")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### Biomass  Figure ####
Soil_Moisture_Graph_Biomass_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Soil_Moisture,y=Plant_Biomass_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=Plant_Biomass_Mean-Plant_Biomass_St_Error,ymax=Plant_Biomass_Mean+Plant_Biomass_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,1,0,0,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Soil_Moisture")+
  ylab("Biomass (g)")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")


Soil_Moisture_Graph_Biomass_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Soil_Moisture,y=Plant_Biomass_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=Plant_Biomass_Mean-Plant_Biomass_St_Error,ymax=Plant_Biomass_Mean+Plant_Biomass_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(0,0,0,0,1),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Soil_Moisture")+
  ylab("Biomass (g)")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### LDMC Figure ####
Soil_Moisture_Graph_LDMC_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Soil_Moisture,y=LDMC_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=LDMC_Mean-LDMC_St_Error,ymax=LDMC_Mean+LDMC_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,0,0,0),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Soil_Moisture")+
  ylab("LDMC")+
  expand_limits(y=c(0,15))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")

Soil_Moisture_Graph_LDMC_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Soil_Moisture,y=LDMC_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=LDMC_Mean-LDMC_St_Error,ymax=LDMC_Mean+LDMC_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,1,0,0),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Soil_Moisture")+
  ylab("LDMC")+
  expand_limits(y=c(0,15))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")

#### SLA Figure ####
Soil_Moisture_Graph_SLA_FK<-ggplot(data=subset(Traits_avg,site=="FK"),aes(x=Soil_Moisture,y=SLA_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=SLA_Mean-SLA_St_Error,ymax=SLA_Mean+SLA_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(0,0,0,0,1),labels = c("BRAR","HECO","KOMA","SPCO","TRDU"), breaks = c("BRAR","HECO","KOMA","SPCO","TRDU"),name="Species")+
  xlab("Soil_Moisture")+
  ylab("SLA")+
  expand_limits(y=c(0,8000))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position="none")

Soil_Moisture_Graph_SLA_TB<-ggplot(data=subset(Traits_avg,site=="TB"),aes(x=Soil_Moisture,y=SLA_Mean,fill=species_code,color=species_code,linetype=species_code)) +  
  geom_point(size=5, stroke =2)+
  geom_smooth(method='lm', se=FALSE,size=4)+
  geom_pointrange(aes(ymin=SLA_Mean-SLA_St_Error,ymax=SLA_Mean+SLA_St_Error),linewidth = 3)+
  scale_linetype_manual(values=c(1,0,1,0,0),labels = c("BOGR","KOMA","LOAR","PASM","VIAM"), breaks = c("BOGR","KOMA","LOAR","PASM","VIAM"),name="Species")+
  xlab("Soil_Moisture")+
  ylab("SLA")+
  expand_limits(y=c(0,8000))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position="none")

#### Create Through time Trait Figure####
Soil_Moisture_Graph_Height_FK+
  Soil_Moisture_Graph_Height_TB+
  Soil_Moisture_Graph_LeafArea_FK+
  Soil_Moisture_Graph_LeafArea_TB+
  Soil_Moisture_Graph_Thickness_FK+
  Soil_Moisture_Graph_Thickness_TB+
  Soil_Moisture_Graph_Biomass_FK+
  Soil_Moisture_Graph_Biomass_TB+
  Soil_Moisture_Graph_LDMC_FK+
  Soil_Moisture_Graph_LDMC_TB+
  Soil_Moisture_Graph_SLA_FK+
  Soil_Moisture_Graph_SLA_TB+
  plot_layout(ncol = 2,nrow = 6)
#save at 3000x5000

#### Normality: FK - height_cm: ####

#BRAR
Soil_Moisture_Soil_Moisture_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(height_cm)), log(height_cm)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Soil_Moisture_Norm_FK_BRAR) 
ols_test_normality(Soil_Moisture_Soil_Moisture_Norm_FK_BRAR) #normal

#HECO
Soil_Moisture_Soil_Moisture_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(height_cm)), (log(height_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Soil_Moisture_Norm_FK_HECO) 
ols_test_normality(Soil_Moisture_Soil_Moisture_Norm_FK_HECO) #normalish

#KOMA
Soil_Moisture_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(height_cm)), (1/log(height_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_KOMA)
ols_test_normality(Soil_Moisture_Norm_FK_KOMA) #normal

#SPCO
Soil_Moisture_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(height_cm)), (log(height_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_SPCO) 
ols_test_normality(Soil_Moisture_Norm_FK_SPCO) #normal

#TRDU
Soil_Moisture_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(height_cm)), ((height_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_TRDU) 
ols_test_normality(Soil_Moisture_Norm_FK_TRDU) #normal


#### Normality: TB - height_cm: ####

#BOGR
Soil_Moisture_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(height_cm)), height_cm  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_BOGR) 
ols_test_normality(Soil_Moisture_Norm_TB_BOGR) #normal

#KOMA
Soil_Moisture_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(height_cm)), (1/log(height_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_KOMA) 
ols_test_normality(Soil_Moisture_Norm_TB_KOMA) #normal

#LOAR
Soil_Moisture_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(height_cm)), (log(height_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_LOAR) 
ols_test_normality(Soil_Moisture_Norm_TB_LOAR) #normal

#PASM
Soil_Moisture_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(height_cm)), (sqrt(height_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_PASM) 
ols_test_normality(Soil_Moisture_Norm_TB_PASM) #normal

#VIAM
Soil_Moisture_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(height_cm)), (log(height_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_VIAM) 
ols_test_normality(Soil_Moisture_Norm_TB_VIAM) #normal

#### Normality: FK - leaf_area_cm: ####

#BRAR
Soil_Moisture_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_area_cm)), (1/log(leaf_area_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_BRAR) 
ols_test_normality(Soil_Moisture_Norm_FK_BRAR) #normal

#HECO
Soil_Moisture_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_HECO) 
ols_test_normality(Soil_Moisture_Norm_FK_HECO) #normal

#KOMA
Soil_Moisture_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_area_cm)), (1/sqrt(leaf_area_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_KOMA) 
ols_test_normality(Soil_Moisture_Norm_FK_KOMA) #normal

#SPCO
Soil_Moisture_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_SPCO) 
ols_test_normality(Soil_Moisture_Norm_FK_SPCO) #normal

#TRDU
Soil_Moisture_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_TRDU) 
ols_test_normality(Soil_Moisture_Norm_FK_TRDU) #normal


#### Normality: TB - leaf_area_cm: ####

#BOGR
Soil_Moisture_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_BOGR) 
ols_test_normality(Soil_Moisture_Norm_TB_BOGR) #normal

#KOMA
Soil_Moisture_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_area_cm)), (1/exp(leaf_area_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_KOMA) 
ols_test_normality(Soil_Moisture_Norm_TB_KOMA) #normal

#LOAR
Soil_Moisture_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_LOAR) 
ols_test_normality(Soil_Moisture_Norm_TB_LOAR) #normal

#PASM
Soil_Moisture_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_PASM) 
ols_test_normality(Soil_Moisture_Norm_TB_PASM) #normal

#VIAM
Soil_Moisture_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_area_cm)), (sqrt(leaf_area_cm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_VIAM) 
ols_test_normality(Soil_Moisture_Norm_TB_VIAM) #normal

#### Normality: FK - leaf_thickness_mm: ####

#BRAR
Soil_Moisture_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_BRAR) 
ols_test_normality(Soil_Moisture_Norm_FK_BRAR) #normal

#HECO
Soil_Moisture_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_HECO) 
ols_test_normality(Soil_Moisture_Norm_FK_HECO) #normal

#KOMA
#
Soil_Moisture_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_thickness_mm)), (1/log(leaf_thickness_mm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_KOMA) 
ols_test_normality(Soil_Moisture_Norm_FK_KOMA) #normal

#SPCO
#
Soil_Moisture_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/log1p(leaf_thickness_mm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_SPCO) 
ols_test_normality(Soil_Moisture_Norm_FK_SPCO) #normalish

#TRDU
#
Soil_Moisture_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_TRDU) 
ols_test_normality(Soil_Moisture_Norm_FK_TRDU) #normal


#### Normality: TB - leaf_thickness_mm: ####

#BOGR
#
Soil_Moisture_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_thickness_mm)), 1/sqrt(leaf_thickness_mm)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_BOGR) 
ols_test_normality(Soil_Moisture_Norm_TB_BOGR) #normal

#KOMA
#
Soil_Moisture_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_thickness_mm)), (log(leaf_thickness_mm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_KOMA) 
ols_test_normality(Soil_Moisture_Norm_TB_KOMA) #normal

#LOAR
#
Soil_Moisture_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_thickness_mm)), ((leaf_thickness_mm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_LOAR) 
ols_test_normality(Soil_Moisture_Norm_TB_LOAR) #normal

#PASM
#
Soil_Moisture_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_thickness_mm)), (sqrt(leaf_thickness_mm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_PASM) 
ols_test_normality(Soil_Moisture_Norm_TB_PASM) #normal

#VIAM
#
Soil_Moisture_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_thickness_mm)), (sqrt(leaf_thickness_mm))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_VIAM) 
ols_test_normality(Soil_Moisture_Norm_TB_VIAM) #normal


#### Normality: FK - Plant Biomass: ####

#BRAR
Soil_Moisture_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(Plant_Biomass)), 1/log1p(Plant_Biomass)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_BRAR) 
ols_test_normality(Soil_Moisture_Norm_FK_BRAR) #normal

#HECO
Soil_Moisture_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(Plant_Biomass)), (1/log(Plant_Biomass))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_HECO) 
ols_test_normality(Soil_Moisture_Norm_FK_HECO) #normal

#KOMA
#
Soil_Moisture_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(Plant_Biomass)), (log(Plant_Biomass))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_KOMA) 
ols_test_normality(Soil_Moisture_Norm_FK_KOMA) #normal

#SPCO
#
Soil_Moisture_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_SPCO) 
ols_test_normality(Soil_Moisture_Norm_FK_SPCO) #normal

#TRDU
#
Soil_Moisture_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(Plant_Biomass)), (log1p(Plant_Biomass))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_TRDU) 
ols_test_normality(Soil_Moisture_Norm_FK_TRDU) #normal


#### Normality: TB - Plant_Biomass ####

#BOGR
#
Soil_Moisture_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_BOGR) 
ols_test_normality(Soil_Moisture_Norm_TB_BOGR) #normal

#KOMA
#
Soil_Moisture_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass) ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_KOMA) 
ols_test_normality(Soil_Moisture_Norm_TB_KOMA) #normalish

#LOAR
#
Soil_Moisture_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(Plant_Biomass)), (log(Plant_Biomass))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_LOAR) 
ols_test_normality(Soil_Moisture_Norm_TB_LOAR) #normal

#PASM
#
Soil_Moisture_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_PASM) 
ols_test_normality(Soil_Moisture_Norm_TB_PASM) #normal

#VIAM
#
Soil_Moisture_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(Plant_Biomass)), 1/sqrt(Plant_Biomass)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_VIAM) 
ols_test_normality(Soil_Moisture_Norm_TB_VIAM) #normalish


#### Normality: FK - LDMC: ####

#BRAR
Soil_Moisture_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(LDMC)), (log(LDMC))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_BRAR) 
ols_test_normality(Soil_Moisture_Norm_FK_BRAR) #normalish

#HECO
Soil_Moisture_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(LDMC)), (1/sqrt(LDMC))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_HECO) 
ols_test_normality(Soil_Moisture_Norm_FK_HECO) #not normal but looks okay

#KOMA
#
Soil_Moisture_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(LDMC)), (1/log(LDMC))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_KOMA) 
ols_test_normality(Soil_Moisture_Norm_FK_KOMA) #not normal but looks okay

#SPCO
#
Soil_Moisture_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(LDMC)), 1/log(LDMC)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_SPCO) 
ols_test_normality(Soil_Moisture_Norm_FK_SPCO) #normal

#TRDU
#
Soil_Moisture_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(LDMC)), (log(LDMC))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_TRDU) 
ols_test_normality(Soil_Moisture_Norm_FK_TRDU) #not normal but looks okay


#### Normality: TB - LDMC ####

#BOGR
#
Soil_Moisture_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_BOGR) 
ols_test_normality(Soil_Moisture_Norm_TB_BOGR)  #not normal but looks okay

#KOMA
#
Soil_Moisture_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(LDMC)), 1/log(LDMC) ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_KOMA) 
ols_test_normality(Soil_Moisture_Norm_TB_KOMA) #normalish

#LOAR
#
Soil_Moisture_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(LDMC)), (log1p(LDMC))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_LOAR) 
ols_test_normality(Soil_Moisture_Norm_TB_LOAR) #not normal but looks okay

#PASM
#
Soil_Moisture_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(LDMC)),(1/sqrt(LDMC))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_PASM) 
ols_test_normality(Soil_Moisture_Norm_TB_PASM) #not normal but looks okay

#VIAM
#
Soil_Moisture_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_VIAM) 
ols_test_normality(Soil_Moisture_Norm_TB_VIAM) #normalish

#### Normality: FK - SLA: ####

#BRAR
Soil_Moisture_Norm_FK_BRAR <- lm(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_BRAR) 
ols_test_normality(Soil_Moisture_Norm_FK_BRAR) #normal

#HECO
Soil_Moisture_Norm_FK_HECO <- lm(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_HECO) 
ols_test_normality(Soil_Moisture_Norm_FK_HECO) #not normal but looks okay

#KOMA
#
Soil_Moisture_Norm_FK_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_KOMA) 
ols_test_normality(Soil_Moisture_Norm_FK_KOMA) #normalish

#SPCO
#
Soil_Moisture_Norm_FK_SPCO <- lm(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(SLA)), (1/sqrt(SLA)) ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_SPCO) 
ols_test_normality(Soil_Moisture_Norm_FK_SPCO) #normalish

#TRDU
#
Soil_Moisture_Norm_FK_TRDU <- lm(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_FK_TRDU) 
ols_test_normality(Soil_Moisture_Norm_FK_TRDU) #normalish


#### Normality: TB - SLA ####

#BOGR
#
Soil_Moisture_Norm_TB_BOGR <- lm(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(SLA)), 1/sqrt(SLA)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_BOGR) 
ols_test_normality(Soil_Moisture_Norm_TB_BOGR) #normal

#KOMA
#
Soil_Moisture_Norm_TB_KOMA <- lm(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(SLA)), (1/log(SLA)) ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_KOMA) 
ols_test_normality(Soil_Moisture_Norm_TB_KOMA) #not normal but looks okay

#LOAR
#
Soil_Moisture_Norm_TB_LOAR <- lm(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(SLA)), (1/(SLA))  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_LOAR) 
ols_test_normality(Soil_Moisture_Norm_TB_LOAR) #normalish

#PASM
#
Soil_Moisture_Norm_TB_PASM <- lm(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(SLA)), sqrt(SLA)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_PASM) 
ols_test_normality(Soil_Moisture_Norm_TB_PASM) #not normal but looks okay

#VIAM
#
Soil_Moisture_Norm_TB_VIAM <- lm(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(SLA)), 1/log(SLA)  ~ Soil_Moisture)
ols_plot_resid_hist(Soil_Moisture_Norm_TB_VIAM) 
ols_test_normality(Soil_Moisture_Norm_TB_VIAM) #normal



#### Stats: FK - height_cm ####

#BRAR
Soil_Moisture_FKBRAR_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(height_cm)), log(height_cm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKBRAR_H, type = 3) #NS

#HECO
Soil_Moisture_FKHECO_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(height_cm)), log(height_cm) ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKHECO_H, type = 3) #NS

#KOMA
Soil_Moisture_FKKOMA_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(height_cm)), 1/log(height_cm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKKOMA_H, type = 3) #NS

#SPCO
Soil_Moisture_FKSPCO_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(height_cm)), log(height_cm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKSPCO_H, type = 3) #0.05

#TRDU
Soil_Moisture_FKTRDU_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(height_cm)), height_cm  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKTRDU_H, type = 3) #0.0005

#### Stats: TB - height_cm ####

#BOGR
Soil_Moisture_TB_BOGR_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(height_cm)), height_cm  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_BOGR_H, type = 3) #NS

#KOMA
Soil_Moisture_TB_KOMA_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(height_cm)), (1/log(height_cm))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_KOMA_H, type = 3) #NS

#LOAR
Soil_Moisture_TB_LOAR_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(height_cm)),log(height_cm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_LOAR_H, type = 3) #NS

#PASM
Soil_Moisture_TB_PASM_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(height_cm)), sqrt(height_cm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_PASM_H, type = 3) #0.05113

#VIAM
Soil_Moisture_TB_VIAM_H <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(height_cm)),log(height_cm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_VIAM_H, type = 3) #0.03799


#### Stats: FK - leaf_area_cm ####

#BRAR
Soil_Moisture_FKBRAR_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_area_cm)), (1/log(leaf_area_cm))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKBRAR_LA, type = 3) #NS

#HECO
Soil_Moisture_FKHECO_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_area_cm)), log(leaf_area_cm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKHECO_LA, type = 3) #NS

#KOMA
Soil_Moisture_FKKOMA_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_area_cm)), (1/sqrt(leaf_area_cm))   ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKKOMA_LA, type = 3) #NS

#SPCO
Soil_Moisture_FKSPCO_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKSPCO_LA, type = 3) #NS

#TRDU
Soil_Moisture_FKTRDU_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_area_cm)), (log(leaf_area_cm))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKTRDU_LA, type = 3) #NS

#### Stats: TB - leaf_area_cm ####

#BOGR
Soil_Moisture_TB_BOGR_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_area_cm)), leaf_area_cm  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_BOGR_LA, type = 3) #NS

#KOMA
Soil_Moisture_TB_KOMA_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_area_cm)), (1/exp(leaf_area_cm))   ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_KOMA_LA, type = 3) #NS

#LOAR
Soil_Moisture_TB_LOAR_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_area_cm)), log(leaf_area_cm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_LOAR_LA, type = 3) #NS

#PASM
Soil_Moisture_TB_PASM_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_area_cm)), sqrt(leaf_area_cm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_PASM_LA, type = 3) #NS

#VIAM
Soil_Moisture_TB_VIAM_LA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_area_cm)), sqrt(leaf_area_cm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_VIAM_LA, type = 3) #0.01417

#### Stats: FK - leaf_thickness_mm ####

#BRAR
Soil_Moisture_FKBRAR_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKBRAR_LT, type = 3) 0.0004647

#HECO
Soil_Moisture_FKHECO_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/sqrt(leaf_thickness_mm))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKHECO_LT, type = 3)#0.04283

#KOMA
Soil_Moisture_FKKOMA_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(leaf_thickness_mm)), (1/log(leaf_thickness_mm))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKKOMA_LT, type = 3)  #0.007266

#SPCO
Soil_Moisture_FKSPCO_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(leaf_thickness_mm)), (1/log1p(leaf_thickness_mm))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FKSPCO_LT, type = 3) # 8.956e-06

#TRDU
Soil_Moisture_FK_TRDU_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_TRDU_LT, type = 3) #3.432e-05

#### Stats: TB - leaf_thickness_mm ####

#BOGR
Soil_Moisture_TB_BOGR_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(leaf_thickness_mm)), 1/sqrt(leaf_thickness_mm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_BOGR_LT, type = 3) #0.0002634

#KOMA
Soil_Moisture_TB_KOMA_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(leaf_thickness_mm)), (log(leaf_thickness_mm))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_KOMA_LT, type = 3) #NS

#LOAR
Soil_Moisture_TB_LOAR_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(leaf_thickness_mm)), leaf_thickness_mm  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_LOAR_LT, type = 3) #9.081e-08 

#PASM
Soil_Moisture_TB_PASM_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(leaf_thickness_mm)), sqrt(leaf_thickness_mm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_PASM_LT, type = 3) #7.287e-06

#VIAM
Soil_Moisture_TB_VIAM_LT <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(leaf_thickness_mm)), sqrt(leaf_thickness_mm)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_VIAM_LT, type = 3) # 3.832e-05 


#### Stats: FK - Plant_Biomass ####

#BRAR
Soil_Moisture_FK_BRAR_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(Plant_Biomass)),  1/log1p(Plant_Biomass)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_BRAR_BM, type = 3) #0.006385

#HECO
Soil_Moisture_FK_HECO_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(Plant_Biomass)), (1/log(Plant_Biomass))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_HECO_BM, type = 3) #0.01137

#KOMA
Soil_Moisture_FK_KOMA_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_KOMA_BM, type = 3) #NS

#SPCO
Soil_Moisture_FK_SPCO_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(Plant_Biomass)), log(Plant_Biomass)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_SPCO_BM, type = 3) #NS

#TRDU
Soil_Moisture_FK_TRDU_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(Plant_Biomass)), log1p(Plant_Biomass)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_TRDU_BM, type = 3) #NS

#### Stats: TB - Plant_Biomass ####

#BOGR
Soil_Moisture_TB_BOGR_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass) ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_BOGR_BM, type = 3) #NS

#KOMA
Soil_Moisture_TB_KOMA_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_KOMA_BM, type = 3) #NS

#LOAR
Soil_Moisture_TB_LOAR_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_LOAR_BM, type = 3) #NS

#PASM
Soil_Moisture_TB_PASM_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(Plant_Biomass)),  log(Plant_Biomass)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_PASM_BM, type = 3) #NS

#VIAM
Soil_Moisture_TB_VIAM_BM <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(Plant_Biomass)), (1/sqrt(Plant_Biomass))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_VIAM_BM, type = 3) #1.14e-05

#### Stats: FK - LDMC ####

#BRAR
Soil_Moisture_FK_BRAR_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_BRAR_LDMC, type = 3) #0.026

#HECO
Soil_Moisture_FK_HECO_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(LDMC)), (1/sqrt(LDMC))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_HECO_LDMC, type = 3) #NS

#KOMA
Soil_Moisture_FK_KOMA_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(LDMC)), (1/log(LDMC)) ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_KOMA_LDMC, type = 3) #NS

#SPCO
Soil_Moisture_FK_SPCO_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(LDMC)), (1/log(LDMC))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_SPCO_LDMC, type = 3) #NS

#TRDU
Soil_Moisture_FK_TRDU_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(LDMC)), log(LDMC)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_TRDU_LDMC, type = 3) #NS

#### Stats: TB - LDMC ####

#BOGR
Soil_Moisture_TB_BOGR_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_BOGR_LDMC, type = 3) #0.003769

#KOMA
Soil_Moisture_TB_KOMA_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(LDMC)), 1/log(LDMC)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_KOMA_LDMC, type = 3) #0.004432

#LOAR
Soil_Moisture_TB_LOAR_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(LDMC)), (log1p(LDMC))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_LOAR_LDMC, type = 3) #0.000687 

#PASM
Soil_Moisture_TB_PASM_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(LDMC)), 1/sqrt(LDMC)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_PASM_LDMC, type = 3) #NS

#VIAM
Soil_Moisture_TB_VIAM_LDMC <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(LDMC)), log(LDMC)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_VIAM_LDMC, type = 3) #NS

#### Stats: FK - SLA ####

#BRAR
Soil_Moisture_FK_BRAR_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BRAR" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_BRAR_SLA, type = 3) #NS

#HECO
Soil_Moisture_FK_HECO_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "HECO" & site== "FK" & !is.na(SLA)), (log(SLA))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_HECO_SLA, type = 3) #NS

#KOMA
Soil_Moisture_FK_KOMA_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_KOMA_SLA, type = 3) #NS

#SPCO
Soil_Moisture_FK_SPCO_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "SPCO" & site== "FK" & !is.na(SLA)), (1/sqrt(SLA))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_SPCO_SLA, type = 3) #NS

#TRDU
Soil_Moisture_FK_TRDU_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "TRDU" & site== "FK" & !is.na(SLA)), log(SLA)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_FK_TRDU_SLA, type = 3) #0.02489

#### Stats: TB - SLA ####

#BOGR
Soil_Moisture_TB_BOGR_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "BOGR" & site== "TB" & !is.na(SLA)), (1/sqrt(SLA))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_BOGR_SLA, type = 3) #0.002162

#KOMA
Soil_Moisture_TB_KOMA_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "KOMA" & site== "TB" & !is.na(SLA)), (1/log(SLA))  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_KOMA_SLA, type = 3) #NS

#LOAR
Soil_Moisture_TB_LOAR_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "LOAR" & site== "TB" & !is.na(SLA)), 1/(SLA)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_LOAR_SLA, type = 3) #0.04949

#PASM
Soil_Moisture_TB_PASM_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "PASM" & site== "TB" & !is.na(SLA)), sqrt(SLA)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_PASM_SLA, type = 3) #NS

#VIAM
Soil_Moisture_TB_VIAM_SLA <- lmerTest::lmer(data = subset(Traits_Precip_SM, species_code == "VIAM" & site== "TB" & !is.na(SLA)), 1/log(SLA)  ~ Soil_Moisture + (1|block))
anova(Soil_Moisture_TB_VIAM_SLA, type = 3) #NS
