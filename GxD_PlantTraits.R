##########################################################################################################
#Project: Dissertation: Community Weighted Plant Traits in MGP for Drought x Grazing 

#Coder: Kathryn Bloodworth

#Created: 12/14/2021
##########################################################################################################

#### Load Libraries ####

#install.packages("lme4")
library(lme4)
#install.packages("ggfortify")
library(ggfortify)
library(ggplot2)
#install.packages("vi>>> /usr/bin/git pullsreg")
library(visreg)
library(grid)
#install.packages("lattice")
library(lattice)
#install.packages("FD")
library(FD)
#install.packages("pliman")
library(pliman)
#install.packages("multcomp")
library(multcomp)
#install.packages("factoextra")
library(factoextra)
#install.packages("emmeans")
library(emmeans)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(tidyverse) 



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
             legend.text=element_text(size=30))

#### Read in Data ####

#Trait Data
Field_Traits<-read.csv("DxG_Plant_Traits/2022_DxG_CWM_FieldTraits.csv") %>% 
  dplyr::select(Site,DxG_block, paddock, genus_species, species_code, height_cm, emerging_leaves, developed_leaves, scenesced_leaves, flower_heads, open_flowers, percent_green, lifespan, growth_form,photosynthetic_pathway, Date, Season, people, comments)

Lab_Traits<-read.csv("DxG_Plant_Traits/2022_DxG_CWM_LabTraits.csv") %>% 
  rename(Site=site) %>% 
  rename(DxG_block=block) %>% 
  rename(Season=season) %>% 
  rename(comments_lab=comments) %>% 
  rename(date_lab=date) %>% 
  rename(people_lab=personelle) %>% 
  #removing genus species and season from this dataframe to avoid spelling issues and inconsistancies with data entered
  dplyr::select(-genus_species,-Season)

Dry_Traits<-read.csv("DxG_Plant_Traits/2022_DxG_DRYLabTraits_ALL_CHECKED.csv") %>% 
  rename(DxG_block=Block) %>% 
  rename(paddock=Grazing_Paddock) %>% 
  rename(species_code=Species) %>% 
  dplyr::select(Site,DxG_block,paddock,species_code,Biomass_Type,Dry_Weight_g) %>% 
  mutate(Dry_Weight_g=ifelse(Dry_Weight_g=="<0.0001", 0.00001,ifelse(Dry_Weight_g=="<0.001",0.00001,ifelse(Dry_Weight_g=="MISSING",NA,ifelse(Dry_Weight_g=="REWEIGH",NA,ifelse(Dry_Weight_g=="Empty",NA,ifelse(Dry_Weight_g=="empty???",NA,ifelse(Dry_Weight_g=="EMPTY",NA,Dry_Weight_g))))))))

Dry_Traits_Biomass<-subset(Dry_Traits,Biomass_Type=="B") %>% 
  rename(Dry_Biomass_min_Leaf_g=Dry_Weight_g) %>% 
  dplyr::select(-Biomass_Type)

Dry_Traits_Leaf<-subset(Dry_Traits,Biomass_Type=="L") %>% 
  rename(Dry_Leaf_Weight_g=Dry_Weight_g) %>% 
  dplyr::select(-Biomass_Type)

Leaf_Area<-read.csv("DxG_Plant_Traits/2022_DxG_LeafArea.csv") %>% 
  select(Species_Code,Site,Block,Grazing_Treatment,Total.Area,NOTES) %>% 
  rename(species_code=Species_Code) %>% 
  rename(DxG_block=Block) %>% 
  rename(paddock=Grazing_Treatment) %>% 
  mutate(ID=paste(species_code,Site,DxG_block,paddock,sep = "_"))
  
#merge trait dataframes
Traits<-Field_Traits %>% 
  left_join(Lab_Traits) %>% 
  left_join(Dry_Traits_Biomass) %>% 
  left_join(Dry_Traits_Leaf) %>% 
  left_join(Leaf_Area)


#Trait Database
#Trait_Database<-read_csv("DxG_Plant_Traits/sCoRRE categorical trait data_11302021.csv")

#Species Comp Data
FK_SpComp_2018<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2018.csv")
FK_SpComp_2018$plot<-as.factor(FK_SpComp_2018$plot)
FK_SpComp_2019<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2019.csv")
FK_SpComp_2020<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2020.csv")
FK_SpComp_2021<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2021.csv")
FK_SpComp_2022<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2022.csv")
TB_SpComp_2018<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2018.csv")
TB_SpComp_2019<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2019.csv")
TB_SpComp_2020<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2020.csv")
TB_SpComp_2021<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2021.csv")
TB_SpComp_2022<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2022.csv")


#Plot Data
plot_layoutK<-read.csv("DxG_Plant_Traits/GMDR_site_plot_metadata.csv") %>% 
  dplyr::select(site,block,paddock,plot,rainfall_reduction,drought,grazing_category,grazing_treatment,livestock_util_2019,livestock_util_2020,livestock_util_2021)

plot_layoutK$plot<-as.factor(plot_layoutK$plot)

#Soil moisture data  - bring in and keep only what we need for this study and take average SM data for all months
SM_data<-read.csv("DxG_Plant_Traits/SM_FK_TB_2019-2021.csv") %>% 
  group_by(Site,Year,Block,Paddock,Plot,Drought,Grazing) %>% 
  summarise(Avg_SM=mean(Soil_Moisture,na.rm = T)) %>% 
  ungroup()

#### Determine Leaf Area - Pliman Leaf Area ####

#create a path for all analyzed images
path_outlined_leaf<-"~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/"

#### Leaf Area - FK_B1_LG ####
#create path to images of leaves - MAC
path_FK_B1_LG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/FK_B1_LG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_FK_B1_LG<-list.files(path=path_FK_B1_LG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_FK_B1_LG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_FK_B1_LG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_FK_B1_LG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_FK_B1_LG<-rbind(Leaf_Area_FK_B1_LG,a)
}

#### Leaf Area - FK_B1_MG ####
#create path to images of leaves - MAC
path_FK_B1_MG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/FK_B1_MG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_FK_B1_MG<-list.files(path=path_FK_B1_MG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_FK_B1_MG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_FK_B1_MG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_FK_B1_MG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_FK_B1_MG<-rbind(Leaf_Area_FK_B1_MG,a)
}

#### Leaf Area - FK_B1_HG ####
#create path to images of leaves - MAC
path_FK_B1_HG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/FK_B1_HG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_FK_B1_HG<-list.files(path=path_FK_B1_HG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_FK_B1_HG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_FK_B1_HG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_FK_B1_HG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_FK_B1_HG<-rbind(Leaf_Area_FK_B1_HG,a)
}

#### Leaf Area - FK_B2_LG ####
#create path to images of leaves - MAC
path_FK_B2_LG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/FK_B2_LG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_FK_B2_LG<-list.files(path=path_FK_B2_LG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_FK_B2_LG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_FK_B2_LG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_FK_B2_LG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_FK_B2_LG<-rbind(Leaf_Area_FK_B2_LG,a)
}

#### Leaf Area - FK_B2_MG ####
#create path to images of leaves - MAC
path_FK_B2_MG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/FK_B2_MG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_FK_B2_MG<-list.files(path=path_FK_B2_MG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_FK_B2_MG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_FK_B2_MG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_FK_B2_MG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_FK_B2_MG<-rbind(Leaf_Area_FK_B2_MG,a)
}

#### Leaf Area - FK_B2_HG ####
#create path to images of leaves - MAC
path_FK_B2_HG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/FK_B2_HG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_FK_B2_HG<-list.files(path=path_FK_B2_HG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_FK_B2_HG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_FK_B2_HG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_FK_B2_HG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_FK_B2_HG<-rbind(Leaf_Area_FK_B2_HG,a)
}

#### Leaf Area - FK_B3_LG ####
#create path to images of leaves - MAC
path_FK_B3_LG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/FK_B3_LG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_FK_B3_LG<-list.files(path=path_FK_B3_LG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_FK_B3_LG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_FK_B3_LG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_FK_B3_LG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_FK_B3_LG<-rbind(Leaf_Area_FK_B3_LG,a)
}

#### Leaf Area - FK_B3_MG ####
#create path to images of leaves - MAC
path_FK_B3_MG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/FK_B3_MG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_FK_B3_MG<-list.files(path=path_FK_B3_MG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_FK_B3_MG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_FK_B3_MG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_FK_B3_MG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_FK_B3_MG<-rbind(Leaf_Area_FK_B3_MG,a)
}

#### Leaf Area - FK_B3_HG ####
#create path to images of leaves - MAC
path_FK_B3_HG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/FK_B3_HG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_FK_B3_HG<-list.files(path=path_FK_B3_HG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_FK_B3_HG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_FK_B3_HG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_FK_B3_HG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_FK_B3_HG<-rbind(Leaf_Area_FK_B3_HG,a)
}

#### Leaf Area - TB_B1_LG ####
#create path to images of leaves - MAC
path_TB_B1_LG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/TB_B1_LG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_TB_B1_LG<-list.files(path=path_TB_B1_LG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_TB_B1_LG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_TB_B1_LG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_TB_B1_LG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_TB_B1_LG<-rbind(Leaf_Area_TB_B1_LG,a)
}

#### Leaf Area - TB_B1_MG ####
#create path to images of leaves - MAC
path_TB_B1_MG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/TB_B1_MG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_TB_B1_MG<-list.files(path=path_TB_B1_MG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_TB_B1_MG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_TB_B1_MG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_TB_B1_MG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_TB_B1_MG<-rbind(Leaf_Area_TB_B1_MG,a)
}

#### Leaf Area - TB_B1_HG ####
#create path to images of leaves - MAC
path_TB_B1_HG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/TB_B1_HG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_TB_B1_HG<-list.files(path=path_TB_B1_HG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_TB_B1_HG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_TB_B1_HG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_TB_B1_HG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_TB_B1_HG<-rbind(Leaf_Area_TB_B1_HG,a)
}

#### Leaf Area - TB_B2_LG ####
#create path to images of leaves - MAC
path_TB_B2_LG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/TB_B2_LG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_TB_B2_LG<-list.files(path=path_TB_B2_LG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_TB_B2_LG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_TB_B2_LG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_TB_B2_LG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_TB_B2_LG<-rbind(Leaf_Area_TB_B2_LG,a)
}

#### Leaf Area - TB_B2_MG ####
#create path to images of leaves - MAC
path_TB_B2_MG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/TB_B2_MG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_TB_B2_MG<-list.files(path=path_TB_B2_MG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_TB_B2_MG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_TB_B2_MG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_TB_B2_MG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_TB_B2_MG<-rbind(Leaf_Area_TB_B2_MG,a)
}

#### Leaf Area - TB_B2_HG ####
#create path to images of leaves - MAC
path_TB_B2_HG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/TB_B2_HG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_TB_B2_HG<-list.files(path=path_TB_B2_HG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_TB_B2_HG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_TB_B2_HG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_TB_B2_HG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_TB_B2_HG<-rbind(Leaf_Area_TB_B2_HG,a)
}

#### Leaf Area - TB_B3_LG ####
#create path to images of leaves - MAC
path_TB_B3_LG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/TB_B3_LG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_TB_B3_LG<-list.files(path=path_TB_B3_LG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_TB_B3_LG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_TB_B3_LG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_TB_B3_LG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_TB_B3_LG<-rbind(Leaf_Area_TB_B3_LG,a)
}

#### Leaf Area - TB_B3_MG ####
#create path to images of leaves - MAC
path_TB_B3_MG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/TB_B3_MG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_TB_B3_MG<-list.files(path=path_TB_B3_MG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_TB_B3_MG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_TB_B3_MG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_TB_B3_MG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_TB_B3_MG<-rbind(Leaf_Area_TB_B3_MG,a)
}

#### Leaf Area - FK_B3_HG ####
#create path to images of leaves - MAC
path_TB_B3_HG <- "~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/DxG_Plant_Traits/2022_Community_Traits_Scanned/Individual_Leaves/TB_B3_HG"

#Create a list of image file names (path is the path to folder with individual leaves and pattern is saying to select anything that ends in .jpg - * is wildcard telling it anything that ends with .jpg)
Leaf_File_Names_TB_B3_HG<-list.files(path=path_TB_B3_HG,pattern="*.jpg")

#Create an empty dataframe for the areas (this must be run everytime before the for loop)
Leaf_Area_TB_B3_HG<-data.frame()

#i is a variable that changes according to the list and then it repeats until end of list
#start a for loop where i is equal to a given Leaf file name
for (i in Leaf_File_Names_TB_B3_HG) {
  #print name of files to make sure it is grabbing all files
  print(i)
  #import image and view it
  image_import <- image_import(i,path = path_TB_B3_HG,plot = FALSE)
  #save each analyzed leaf file so I can check the outline
  png(filename=paste(path_outlined_leaf,'/Outlined_Leaf/',i,'_analyzed','.png',sep=""))
  #count number of leaves
  analyze<- analyze_objects(image_import,marker="id",watershed=FALSE,object_size = "elarge",col_background = "white")
  #close session to save photo
  dev.off()
  #get leaf area measurements 
  measures <-get_measures(analyze, dpi=72)
  #create a temporary dataframe that has a column named Leaf_ID where the names from Leaf_File_Names are placed as they're processed through the for loop and then make another column called Leaf_Area where the area from measures is placed
  a<-data.frame(Leaf_ID=i,Leaf_Area_cm=measures$area)
  #put the information from dataframe a into a permanent data frame called Leaf Area where it combines the data from every run through the for loop
  Leaf_Area_TB_B3_HG<-rbind(Leaf_Area_TB_B3_HG,a)
}

#### Combine all area data frames into one ####

Leaf_Area_All<- Leaf_Area_FK_B1_HG %>% 
  rbind(Leaf_Area_FK_B1_LG) %>% 
  rbind(Leaf_Area_FK_B1_MG) %>% 
  rbind(Leaf_Area_FK_B1_HG) %>% 
  rbind(Leaf_Area_FK_B2_LG) %>% 
  rbind(Leaf_Area_FK_B2_MG) %>% 
  rbind(Leaf_Area_FK_B2_HG) %>% 
  rbind(Leaf_Area_FK_B3_LG) %>% 
  rbind(Leaf_Area_FK_B3_MG) %>% 
  rbind(Leaf_Area_FK_B3_HG) %>% 
  rbind(Leaf_Area_TB_B1_LG) %>% 
  rbind(Leaf_Area_TB_B1_MG) %>% 
  rbind(Leaf_Area_TB_B1_HG) %>% 
  rbind(Leaf_Area_TB_B2_LG) %>% 
  rbind(Leaf_Area_TB_B2_MG) %>% 
  rbind(Leaf_Area_TB_B2_HG) %>% 
  rbind(Leaf_Area_TB_B3_LG) %>% 
  rbind(Leaf_Area_TB_B3_MG) %>% 
  rbind(Leaf_Area_TB_B3_HG) %>% 
  group_by(Leaf_ID) %>% 
  summarise(Leaf_Area_cm=sum(Leaf_Area_cm)) %>% 
  separate(Leaf_ID,c("Site","Block","Grazing_Treatment","SpCo"), sep = "_") %>% 
  separate(SpCo,c("Species_Code","jpg")) %>% 
  select(-jpg)



#### Clean Up Species Comp Data and Calculate Relative Cover ####

#get dataframe with just total cover per plot for each year
#FK - 2018
Aerial_Cover_2018_FK<-FK_SpComp_2018 %>% 
  filter(aerial_basal!="Basal")

#Create Long dataframe from wide dataframe and fix species issues
Long_Cov_2018_FK<-gather(Aerial_Cover_2018_FK,key="species","cover",18:117) %>% 
  dplyr::select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("Oenotherea.suffrutescens.1","STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii","CRCE.LELU.Penstemon.sp","Oenothera.","Unknown..7.baby.guara.","UNKN8.basal.rosette.lancroiati","Unk..3.Basal.Rosette","Unk..3.soft.point.leaf.KW.photo","Unkn..10.small.linear.leaf.KW.photo","Oneothera.n.","Rock","Moss.Lichen.Bogr.overlap")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2018_FK<-Long_Cov_2018_FK%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2018_FK$plot<-as.factor(Relative_Cover_2018_FK$plot)

#FK - 2019
#get dataframe with just total cover per plot
Total_Cover_2019_FK<-FK_SpComp_2019%>%
  #only keep added total
  filter(!is.na(genus_species)) %>% 
  filter(genus_species!="") %>% 
  group_by(block,plot) %>% 
  summarise(Total_Cover=sum(aerial_cover,na.rm = T)) %>% 
  ungroup()

#make dataframe with necessary information for relative cover calculation
Species_Cover_2019_FK<-FK_SpComp_2019 %>% 
  #take out all 'species' that are not actually plant species
  filter(!is.na(genus_species)) %>% 
  filter(genus_species!="") %>% 
  filter(!genus_species %in% c("Added_Total","Estimated_Total" ,"Rock","Litter", "Bare Ground","overlap","Overlap", "Dung","ASER_Like_Woody","Lichen","Moss", "silver_stuff_unk3", "Skinny_leaf_fuzzy_bottom","oenothera?_basal_rossette","dead_mustard_unk","oenothera?_basal_rossetta","Oenothera_waxy_leaves","Basal_rosette","Mushroom")) %>% 
  rename(Species_Cover="aerial_cover") %>% 
  dplyr::select(-observers,-date)

#Calculate Relative Cover
Relative_Cover_2019_FK<-Species_Cover_2019_FK%>%
  #Make a new column named "Treatment"
  mutate(Treatment=paste(block,plot,sep="_"))%>%
  #Add Total_Cover data into the Relative_Cover data sheet
  left_join(Total_Cover_2019_FK)%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(Species_Cover/Total_Cover)*100) %>% 
  dplyr::select(-Species_Cover,-basal_cover,-Total_Cover) %>% 
  mutate(Relative_Cover=replace_na(Relative_Cover,0)) %>% 
  mutate(year=2019)  %>% 
  rename(species="genus_species") %>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2019_FK$plot<-as.factor(Relative_Cover_2019_FK$plot)

#FK - 2020
#get dataframe with just total cover per plot
Total_Cover_2020_FK<-FK_SpComp_2020%>%
  #only keep added total
  filter(!is.na(genus_species)) %>% 
  filter(genus_species!="") %>% 
  group_by(block,plot) %>% 
  summarise(Total_Cover=sum(aerial_cover,na.rm = T)) %>% 
  ungroup()

#make dataframe with necessary information for relative cover calculation
Species_Cover_2020_FK<-FK_SpComp_2020 %>% 
  #take out all 'species' that are not actually plant species
  filter(!is.na(genus_species)) %>% 
  filter(genus_species!="") %>% 
  filter(!genus_species %in% c("Added_total","Estimated_total" ,"Rock","Litter", "Bareground","overlap","Overlap", "Dung","ASER_Like_Woody","Lichen","Moss", "silver_stuff_unk3", "Skinny_leaf_fuzzy_bottom","oenothera?_basal_rossette","dead_mustard_unk","oenothera?_basal_rossetta","Oenothera_waxy_leaves","Basal_rosette","Mushroom","basal_aster_KW_pic_unknown","lanceolate_KW_pic_unknown","Antennaria_spp_unknown","basal_rosette_2020_KW_pic_unknown","Lithospermum_KW_pic_unknown","long_pods_no_leaves_KW_pic_unknown", "Artemisia_look_alike_no_smell_KW_pic_unknown","Astragalus_KW_pic_unknown","flat_spikelet_panicle_KW_pic_unknown","Antennaria_KW_pic_unknown")) %>% 
  rename(Species_Cover="aerial_cover") %>% 
  dplyr::select(-observers,-date)

#Calculate Relative Cover
Relative_Cover_2020_FK<-Species_Cover_2020_FK%>%
  #Make a new column named "Treatment"
  mutate(Treatment=paste(block,plot,sep="_"))%>%
  #Add Total_Cover data into the Relative_Cover data sheet
  left_join(Total_Cover_2020_FK)%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(Species_Cover/Total_Cover)*100) %>% 
  dplyr::select(-Species_Cover,-basal_cover,-Total_Cover) %>% 
  mutate(Relative_Cover=replace_na(Relative_Cover,0)) %>% 
  mutate(year=2020) %>% 
  rename(species="genus_species") %>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2020_FK$plot<-as.factor(Relative_Cover_2020_FK$plot)

#FK - 2021
Aerial_Cover_2021_FK<-FK_SpComp_2021 %>% 
  filter(aerial_basal!="basal")

#Create Long dataframe from wide dataframe
Long_Cov_2021_FK<-gather(Aerial_Cover_2021_FK,key="species","cover",20:61) %>% 
  dplyr::select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("Poa_diseased_Kwpic.","Linear_leaf_hairy_red_stem_KWpic.")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2021_FK<-Long_Cov_2021_FK%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2021_FK$plot<-as.factor(Relative_Cover_2021_FK$plot)

#FK - 2022
Aerial_Cover_2022_FK<-FK_SpComp_2022 %>% 
  filter(aerial_basal!="basal")

#Create Long dataframe from wide dataframe
Long_Cov_2022_FK<-gather(Aerial_Cover_2022_FK,key="species","cover",18:68) %>% 
  dplyr::select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("basal.rosette","final_total","final_total_excel","Lygo.deomia","Lygo.deomia.1")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2022_FK<-Long_Cov_2022_FK%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2022_FK$plot<-as.factor(Relative_Cover_2022_FK$plot)

#TB- 2018
Aerial_Cover_2018_TB<-TB_SpComp_2018 %>% 
  filter(aerial_basal!="Basal")

#Create Long dataframe from wide dataframe
Long_Cov_2018_TB<-gather(Aerial_Cover_2018_TB,key="species","cover",18:113) %>% 
  dplyr::select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("Oenotherea.suffrutescens.1", "STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii",  "CRCE.LELU.Penstemon.sp","Oenothera.","Unknown..7.baby.guara.","UNKN8.basal.rosette.lancroiati","Unk..3.Basal.Rosette","Unk..3.soft.point.leaf.KW.photo","Unkn..10.small.linear.leaf.KW.photo")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2018_TB<-Long_Cov_2018_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2018_TB$plot<-as.factor(Relative_Cover_2018_TB$plot)

#TB- 2019
Aerial_Cover_2019_TB<-TB_SpComp_2019 %>% 
  filter(aerial_basal!="Basal")

#Create Long dataframe from wide dataframe
Long_Cov_2019_TB<-gather(Aerial_Cover_2019_TB,key="species","cover",18:114) %>% 
  dplyr::select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii","Penstemon.sp.","CRCE.LELU.Penstemon.sp","Oenothera.","Unknown..7.baby.guara.","UNKN8.basal.rosette.lancroiati","Unk..3.Basal.Rosette","Unk..3.soft.point.leaf.KW.photo","Unkn..10.small.linear.leaf.KW.photo")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

Long_Cov_2019_TB$cover<-as.numeric(Long_Cov_2019_TB$cover)

#Calculate Relative Cover
Relative_Cover_2019_TB<-Long_Cov_2019_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2019_TB$plot<-as.factor(Relative_Cover_2019_TB$plot)

#TB- 2020
Aerial_Cover_2020_TB<-TB_SpComp_2020 %>% 
  filter(aerial_basal!="Basal")

#Create Long dataframe from wide dataframe
Long_Cov_2020_TB<-gather(Aerial_Cover_2020_TB,key="species","cover",18:114) %>% 
  dplyr::select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii","Penstemon.sp.","CRCE.LELU.Penstemon.sp","Oenothera.","Unknown..7.baby.guara.","UNKN8.basal.rosette.lancroiati","Unk..3.Basal.Rosette","Unk..3.soft.point.leaf.KW.photo","Unkn..10.small.linear.leaf.KW.photo")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0) %>% 
  filter(cover!="<0.5")

Long_Cov_2020_TB$cover<-as.numeric(Long_Cov_2020_TB$cover)

#Calculate Relative Cover
Relative_Cover_2020_TB<-Long_Cov_2020_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2020_TB$plot<-as.factor(Relative_Cover_2020_TB$plot)

#TB- 2021
Aerial_Cover_2021_TB<-TB_SpComp_2021 %>% 
  filter(aerial_basal!="basal")

#Create Long dataframe from wide dataframe
Long_Cov_2021_TB<-gather(Aerial_Cover_2021_TB,key="species","cover",21:79) %>% 
  dplyr::select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("Unk_baby_forb_opp.")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0) %>% 
  filter(cover!="<0.5")

#Calculate Relative Cover
Relative_Cover_2021_TB<-Long_Cov_2021_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2021_TB$plot<-as.factor(Relative_Cover_2021_TB$plot)

#TB - 2022
Aerial_Cover_2022_TB<-TB_SpComp_2022 %>% 
  filter(aerial_basal!="basal")

#Create Long dataframe from wide dataframe
Long_Cov_2022_TB<-gather(Aerial_Cover_2022_TB,key="species","cover",18:85) %>% 
  dplyr::select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("basal.rosette","final_total","final_total_excel","Pointy.petals..Rhear.leaves","Flesur.linear.leaves.in.bunch..KW.pic.","Sponge.leaf..KW.pic....FRUN.PIOP.46......")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2022_TB<-Long_Cov_2022_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2022_TB$plot<-as.factor(Relative_Cover_2022_TB$plot)


####Merge all TB and FK Data Frames together
Species_Comp_RelCov_All<- 
  full_join(Relative_Cover_2018_TB,Relative_Cover_2019_TB) %>% 
  full_join(Relative_Cover_2020_TB) %>% 
  full_join(Relative_Cover_2021_TB) %>% 
  full_join(Relative_Cover_2022_TB) %>% 
  full_join(Relative_Cover_2018_FK) %>% 
  full_join(Relative_Cover_2019_FK) %>% 
  full_join(Relative_Cover_2020_FK) %>% 
  full_join(Relative_Cover_2021_FK) %>% 
  full_join(Relative_Cover_2022_FK) %>% 
  mutate(Genus_Species=ifelse(species=="Oenothera_suffruticosa","Oenothera.suffrutescens",ifelse(species=="Oenotherea.suffrutescens","Oenothera.suffrutescens",ifelse(species=="OESU","Oenothera.suffrutescens",ifelse(species=="OPPO","Opuntia.polyacantha",ifelse(species=="Opuntia_polycantha","Opuntia.polyacantha",ifelse(species=="Pascopyrum_smithii","Pascopyrum.smithii",ifelse(species=="PASM","Pascopyrum_smithii",ifelse(species=="Pediomelum_esculenta","Pediomelum.esculentum",ifelse(species=="pediomelum_esculentum","Pediomelum.esculentum",ifelse(species=="Pediomelum_esculentum","Pediomelum.esculentum",ifelse(species=="PHHO","Phlox.hoodii",ifelse(species=="Plantago_patagonica","Plantago.patagonica",ifelse(species=="PLPA","Plantago.patagonica",ifelse(species=="Poa_secunda","Poa.secunda",ifelse(species=="POSE","Poa.secunda",ifelse(species=="PSTE","Psoralidium.tenuiflorum",ifelse(species=="SPCO","Sphaeralcea.coccinea",ifelse(species=="Sphaeralcea_coccinea","Sphaeralcea.coccinea",ifelse(species=="Sporobolus_cryptandrus","Sporobolus.cryptandrus",ifelse(species=="TAOF","Taraxacum.officinale",ifelse(species=="Taraxacum_officinale","Taraxacum.officinale",ifelse(species=="Tragopogon_dubius","Tragopogon.dubius",ifelse(species=="TRDU","Tragopogon.dubius",ifelse(species=="VIAM","Vicia.americana",ifelse(species=="Vicia_americana","Vicia.americana",ifelse(species=="Vulpia_octoflora","Vulpia.octoflora",ifelse(species=="VUOC","Vulpia.octoflora",ifelse(species=="ALDE","Alyssum.desertorum",ifelse(species=="Allysum_desetorum","Alyssum.desertorum",ifelse(species=="ALTE","Allium.textile",ifelse(species=="Alyssum_desertorum","Alyssum.desertorum",ifelse(species=="Alyssum.desertorum","Alyssum.desertorum",ifelse(species=="Androsace_occidentalis","Androsace.occidentalis",ifelse(species=="ARCA","Artemisia.cana",ifelse(species=="ARDR","Artemisia.dracunculus",ifelse(species=="ARFR","Artemisia.frigida",ifelse(species=="Aristida_purpurea","Aristida.purpurea",ifelse(species=="ARPU","Aristida.purpurea",ifelse(species=="Artemisia_cana","Artemisia.cana",ifelse(species=="Artemisia_dracunculus","Artemisia.dracunculus",ifelse(species=="Artemisia_frigida","Artemisia.frigida",ifelse(species=="ARTR","Artemisia.tridentata",ifelse(species=="BODA","Bouteloua.dactyloides",ifelse(species=="BOGR" ,"Bouteloua.gracilis",ifelse(species=="Bouteloua_dactyloides","Bouteloua.dactyloides",ifelse(species=="Bouteloua_gracilis","Bouteloua.gracilis",ifelse(species=="BRAR","Bromus.arvensis",ifelse(species=="Bromus_arvensis","Bromus.arvensis",ifelse(species=="Bromus_tectorum","Bromus.tectorum",species)))))))))))))))))))))))))))))))))))))))))))))))))) %>% 
  mutate(Genus_Species_2=ifelse(Genus_Species=="BRTE","Bromus.tectorum",ifelse(Genus_Species=="CADU","Carex.duriuscula",ifelse(Genus_Species=="CAFI","Carex.filifolia",ifelse(Genus_Species=="Carex_durescula","Carex.duriuscula",ifelse(Genus_Species=="Carex_duriuscula","Carex.duriuscula",ifelse(Genus_Species=="conyza_canadensis","Conyza.canadensis",ifelse(Genus_Species=="Conyza_canadensis","Conyza.canadensis",ifelse(Genus_Species=="Coryphanthus_vivipara","Coryphantha.viviparus",ifelse(Genus_Species=="Coryphantha_viviparus","Coryphantha.viviparus",ifelse(Genus_Species=="COVI","Coryphantha.viviparus",ifelse(Genus_Species=="DEPI","Descurainia.pinnata",ifelse(Genus_Species=="ERHO","Eremogone.hookeri",ifelse(Genus_Species=="GUSA","Gutierrezia.sarothrae",ifelse(Genus_Species=="HECO","Hesperostipa.comata",ifelse(Genus_Species=="Hesperostipa_comata","Hesperostipa.comata",ifelse(Genus_Species=="Hedeoma_hispida","Hedeoma.hispida",ifelse(Genus_Species=="Koeleria_macrantha","Koeleria.macrantha",ifelse(Genus_Species=="KOMA","Koeleria.macrantha",ifelse(Genus_Species=="Lithospermum_incisum","Lithospermum.incisum",ifelse(Genus_Species=="LOAR","Logfia.arvensis",ifelse(Genus_Species=="Logfia_arvensis","Logfia.arvensis",ifelse(Genus_Species=="LYJU","Lygodesmia.juncea",ifelse(Genus_Species=="MUDI","Musineon.divaricatum",ifelse(Genus_Species=="NAVI","Nassella.viridula",ifelse(Genus_Species=="Oenothera_suffrutescens","Oenothera.suffrutescens",ifelse(Genus_Species=="oenothera_suffruticosa","Oenothera.suffrutescens",ifelse(Genus_Species=="Carex_filifolia","Carex.filifolia", ifelse(Genus_Species=="Liatrus_punctata","Liatris.punctata",ifelse(Genus_Species== "LOFO","Lomatium.foeniculaceum",ifelse(Genus_Species=="Pascopyrum_smithii","Pascopyrum.smithii",ifelse(Genus_Species=="Lygodesmia_juncea","Lygodesmia.juncea",ifelse(Genus_Species=="Linum_rigidum","Linum.rigidum",ifelse(Genus_Species=="Asclepias_stenophylla","Asclepias.stenophylla",ifelse(Genus_Species=="Lepidium_densiflorum","Lepidium.densiflorum",ifelse(Genus_Species=="Astragalus_gracilis","Astragalus.gracilis",ifelse(Genus_Species== "Euphorbia_nutans","Euphorbia.nutans",ifelse(Genus_Species=="Liatris_punctata","Liatris.punctata",ifelse(Genus_Species=="Astragalus_purshii","Astragalus.purshii",ifelse(Genus_Species=="Lactuca_serriola","Lactuca.serriola",ifelse(Genus_Species=="COLI","Collomia.linearis",Genus_Species))))))))))))))))))))))))))))))))))))))))) %>%
  mutate(Genus_Species_3=ifelse(Genus_Species_2=="DRRE","Draba.reptans",ifelse(Genus_Species_2=="ANPA","Antennaria.parvifolia",ifelse(Genus_Species_2=="CAMI","Camelina.microcarpa",ifelse(Genus_Species_2=="ERCA.","Erigeron.canus",ifelse(Genus_Species_2=="ERPU","Erigeron.pumilus",ifelse(Genus_Species_2=="ERPU.","Erigeron.pumilus",ifelse(Genus_Species_2=="HEHI","Hedeoma.hispida",ifelse(Genus_Species_2=="LEDE","Lepidium.densiflorum",ifelse(Genus_Species_2=="LIIN","Lithospermum.incisum",ifelse(Genus_Species_2=="LIPU","Liatris.punctata",ifelse(Genus_Species_2=="MODI","Musineon.divaricatum",ifelse(Genus_Species_2=="MODI","Musineon.divaricatum",ifelse(Genus_Species_2=="MODI","Musineon.divaricatum",ifelse(Genus_Species_2=="NOCU","Nothocalais.cuspidata",ifelse(Genus_Species_2=="PEES","Pediomelum.esculentum",ifelse(Genus_Species_2=="PIOP","Picradeniopsis.oppositifolia",ifelse(Genus_Species_2=="POAV","Polygonum.aviculare",ifelse(Genus_Species_2=="VEPE","Veronica.peregrina", ifelse(Genus_Species_2=="ZIVE","Zigadenus.venenosus", ifelse(Genus_Species_2=="ANOC","Androsace.occidentalis", ifelse(Genus_Species_2=="ASGR","Astragalus.gracilis",ifelse(Genus_Species_2=="ASPU","Astragalus.purshii",ifelse(Genus_Species_2=="COCA","Conyza.canadensis",ifelse(Genus_Species_2=="LIRI","Linum.rigidum",ifelse(Genus_Species_2=="MAGR","Machaeranthera.grindelioides",ifelse(Genus_Species_2=="PEAL","Pediomelum.esculentum",ifelse(Genus_Species_2=="SPCR","Sporobolus.cryptandrus",ifelse(Genus_Species_2=="ASBI","Astragalus.bisulcatus",ifelse(Genus_Species_2=="CIUN","Cirsium.undulatum",ifelse(Genus_Species_2=="Coryphantha.viviparus","Coryphantha.vivipara",ifelse(Genus_Species_2=="ERDI","Erigeron.divergens.ochroleucus",ifelse(Genus_Species_2=="GUSA.1","Gutierrezia.sarothrae",Genus_Species_2))))))))))))))))))))))))))))))))) %>% 
  mutate(Genus_Species_Correct=ifelse(Genus_Species_3=="LEMO","Leucocrinum.montanum",ifelse(Genus_Species_3=="LOFO.1","Lomatium.foeniculaceum",ifelse(Genus_Species_3=="Oenotherea.suffrutescens.1","Oenothera.suffrutescens",ifelse(Genus_Species_3=="SCLA","Scorzonera.laciniata",ifelse(Genus_Species_3=="SCPA","Schedonnardus.paniculatus",ifelse(Genus_Species_3=="TROC","Tradescantia.occidentalis",ifelse(Genus_Species_3=="VINU","Viola.nuttallii",ifelse(Genus_Species_3=="PEAN","Penstamom.angus",ifelse(Genus_Species_3=="PHLO","Phlox.longifoli",ifelse(Genus_Species_3=="CHPR","Chenopudium.pratericola",ifelse(Genus_Species_3=="CRMI","Cryptans.minima",ifelse(Genus_Species_3=="DRNE","Draba.nemorosa",ifelse(Genus_Species_3=="MILI","Mirabilis.linearis",ifelse(Genus_Species_3=="OEAL","Oenothera.albicaulis",ifelse(Genus_Species_3=="OOMU","Oonopsis.multicaulis",Genus_Species_3)))))))))))))))) %>% 
  dplyr::select(-species,-Genus_Species,-Genus_Species_2,-Genus_Species_3)


#### Calculate top 90 % of each plot for each year ####

#sort dataframe by year, site, plot, and relative cover (from highest to lowest)
Species_Cover_90_all<-Species_Comp_RelCov_All[order(Species_Comp_RelCov_All$year, Species_Comp_RelCov_All$site,Species_Comp_RelCov_All$plot,-Species_Comp_RelCov_All$Relative_Cover),]

#create a dataframe that groups by year, site, and plot and then calculates the cummulative sum of relative cover within each yearXsiteXplot
Species_Cover_90_all<-Species_Cover_90_all %>% 
group_by(year,site,plot) %>%
mutate(Total_Percent = cumsum(Relative_Cover)) %>% 
ungroup() %>% 
#remove any species after the 90% threshold is met
filter(Total_Percent<=93)

#### Clean up trait data ####


##make dataframes match up

#remove all NAs from height column to remove any plants not collected/measured but to avoid removing plants where percent green was not collected  by accident
Traits_Clean <- Traits [complete.cases(Traits[ , 6]),] %>% 
  filter(comments_lab!="not BRTE - did not measure, remove from data") %>% 
  filter(comments_lab!="maybe KOMA?") %>% 
  filter(comments_lab!="add 0.0012 to total biomass (wet)") %>% 
  mutate(wet_leaf_weight_g=as.numeric(ifelse(wet_leaf_weight_g=="<0.0001","0.00001",ifelse(wet_leaf_weight_g=="0..0233",0.0233, wet_leaf_weight_g))))

#Changing ARTR to ARFR based on comments on lab traits
Traits_Clean[58, "genus_species"] <- "Artemisia_frigida"
Traits_Clean[58, "species_code"] <- "ARFR"

#changing LIPU to LIIN based on comments on lab traits
Traits_Clean[394, "genus_species"] <- "Lithospermum_incisum"
Traits_Clean[394, "species_code"] <- "LIIN"

#changing MUDI to PIOP based on comments on lab traits
Traits_Clean[445, "genus_species"] <- "Picradeniopsis_oppositifolia"
Traits_Clean[445, "species_code"] <- "PIOP"

#changing LIIN to LIPU based on comments on lab traits
Traits_Clean[388, "genus_species"] <- "Liatris_punctata"
Traits_Clean[388, "species_code"] <- "LIPU"

#changing KOMA to PASM based on comments on lab traits
Traits_Clean[356, "genus_species"] <- "Pascopyrum_smithii"
Traits_Clean[356, "species_code"] <- "PASM"

Traits_Clean$Dry_Leaf_Weight_g<-as.numeric(Traits_Clean$Dry_Leaf_Weight_g)
Traits_Clean$Dry_Biomass_min_Leaf_g<-as.numeric(Traits_Clean$Dry_Biomass_min_Leaf_g)

#remove SLA that is unusually big -- PLPA_TB_3_LG - make an NA
Traits_Clean[524, "Total.Area"] <- NA


#Clean up leaf traits and calculate SLA and average traits by site
Traits_Clean_2<-Traits_Clean %>% 
  mutate(total_flower_num=flower_heads+open_flowers) %>% 
  mutate(total_leaf_num=emerging_leaves+developed_leaves+scenesced_leaves) %>% 
  mutate(SLA=Total.Area/Dry_Leaf_Weight_g) %>% 
  mutate(Dry_Leaf_Weight_g_update=ifelse(Dry_Leaf_Weight_g=="REWEIGH",NA,ifelse(Dry_Leaf_Weight_g=="<0.001",0.00005,ifelse(Dry_Leaf_Weight_g=="<0.0001",0.00005,ifelse(Dry_Leaf_Weight_g=="Empty",NA,ifelse(Dry_Leaf_Weight_g=="EMPTY",NA,ifelse(Dry_Leaf_Weight_g=="MISSING",NA,Dry_Leaf_Weight_g))))))) %>% 
  mutate(Dry_Biomass_min_Leaf_g_update=ifelse(Dry_Biomass_min_Leaf_g=="REWEIGH",NA,ifelse(Dry_Biomass_min_Leaf_g=="<0.001",0.00005,ifelse(Dry_Biomass_min_Leaf_g=="<0.0001",0.00005,ifelse(Dry_Biomass_min_Leaf_g=="Empty",NA,ifelse(Dry_Biomass_min_Leaf_g=="EMPTY",NA,ifelse(Dry_Biomass_min_Leaf_g=="MISSING",NA,Dry_Biomass_min_Leaf_g))))))) %>% 
  mutate(LDMC=as.numeric(Dry_Leaf_Weight_g_update)/wet_leaf_weight_g) %>% 
  mutate(Plant_Biomass=as.numeric(Dry_Leaf_Weight_g_update)+as.numeric(Dry_Biomass_min_Leaf_g_update)) %>% 
  #making lifespan binary (annual, biennial = 0, perennial =1)
  mutate(lifespan_binary=ifelse(lifespan=="annual",0,ifelse(lifespan=="perennial",1,ifelse(lifespan=="annual_biennial",0,ifelse(lifespan=="biennial",0,NA))))) %>% 
  #making growth form binary (forb, woody = 0, graminoid = 1)
  mutate(growth_form_binary=ifelse(growth_form=="forb",0,ifelse(growth_form=="woody",0,ifelse(growth_form=="graminoid",1,NA)))) %>% 
  #making photosynthetic pathway binary (C3=0 , C4=1)
  mutate(photosynthetic_pathway_binary=ifelse(photosynthetic_pathway=="C3",0,ifelse(photosynthetic_pathway=="C4",1,NA))) %>% 
  #edit genus species to match species comp data
  mutate(Genus_Species_2=ifelse(genus_species=="Allium_textile","Allium.textile",ifelse(genus_species=="Alyssum_desetorum","Alyssum.desertorum",ifelse(genus_species=="Antennaria_parvifolia","Antennaria.parvifolia",ifelse(genus_species=="Astragalus_bisulcatus","Astragalus.bisulcatus",ifelse(genus_species=="Bromus_arvensis","Bromus.arvensis",ifelse(genus_species=="Bromus_tectorum","Bromus.tectorum",ifelse(genus_species=="Carex_duriuscula","Carex.duriuscula",ifelse(genus_species=="Carex_filifolia","Carex.filifolia",ifelse(genus_species=="Cirsium_undulatum","Cirsium.undulatum",ifelse(genus_species=="Collomia_linearis","Collomia.linearis",ifelse(genus_species=="Descurainia_pinnata","Descurainia.pinnata",ifelse(genus_species=="Draba_reptans","Draba.reptans",ifelse(genus_species=="Eremogone_hookeri","Eremogone.hookeri",ifelse(genus_species=="Erigeron_canus","Erigeron.canus",ifelse(genus_species=="Erigeron_pumilus","Erigeron.pumilus",ifelse(genus_species=="Hedeoma_hispida","Hedeoma.hispida",ifelse(genus_species=="Hesperostipa_comata","Hesperostipa.comata",ifelse(genus_species=="Koeleria_macrantha","Koeleria.macrantha",ifelse(genus_species=="Lepidium_densiflorum","Lepidium.densiflorum",ifelse(genus_species=="Lithospermum_incisum","Lithospermum.incisum",ifelse(genus_species=="Logfia_arvensis","Logfia.arvensis",ifelse(genus_species=="Lomatium_foeniculaceum","Lomatium.foeniculaceum",ifelse(genus_species=="Musineon_divaricatum","Musineon.divaricatum",ifelse(genus_species=="Nassella_viridula","Nassella.viridula",ifelse(genus_species=="Nothocalais_cuspidate","Nothocalais.cuspidata",ifelse(genus_species=="Oenothera_suffrtescuns","Oenothera.suffrtescuns",ifelse(genus_species=="Pascopyrum_smithii","Pascopyrum.smithii",ifelse(genus_species=="Phlox_hoodia","Phlox.hoodii",ifelse(genus_species=="Picradeniopsis_oppositifolia","Picradeniopsis.oppositifolia",ifelse(genus_species=="Plantago_patagonica","Plantago.patagonica",ifelse(genus_species=="Poa_secunda","Poa.secunda",ifelse(genus_species=="Psoralidium_tenuiflorum","Psoralidium.tenuiflorum",genus_species))))))))))))))))))))))))))))))))) %>%
  mutate(Genus_Species_Correct=ifelse(Genus_Species_2=="Sphaeralcea_coccinea","Sphaeralcea.coccinea",ifelse(Genus_Species_2=="Taraxacum_officinale","Taraxacum.officinale",ifelse(Genus_Species_2=="Tetraneuris_acaulis","Tetraneuris.acaulis",ifelse(Genus_Species_2=="Tragopogon_dubius","Tragopogon.dubius",ifelse(Genus_Species_2=="Vulpia_octoflora","Vulpia.octoflora",ifelse(Genus_Species_2=="Vicia_americana","Vicia.americana",ifelse(Genus_Species_2=="Elymus_elymoides","Elymus.elymoides",ifelse(Genus_Species_2=="Androsace_occidentalis","Androsace.occidentalis",ifelse(Genus_Species_2=="Astragalus_purshii","Astragalus.purshii",ifelse(Genus_Species_2=="Astragalus_gracilis","Astragalus.gracilis",ifelse(Genus_Species_2=="Conyza_canadensis","Conyza.canadensis",ifelse(Genus_Species_2=="Liatris_punctata","Liatris.punctata",ifelse(Genus_Species_2=="Lydogesmia_juncea","Lygodesmia.juncea",ifelse(Genus_Species_2=="Pediomelum_esculentum","Pediomelum.esculentum",ifelse(Genus_Species_2=="Linum_rigidum","Linum.rigidum",ifelse(Genus_Species_2=="Aristida_purpurea","Aristida.purpurea",ifelse(Genus_Species_2=="Artemisia_frigida","Artemisia.frigida",ifelse(Genus_Species_2=="Artemisia_tridentata","Artemisia.tridentata",ifelse(Genus_Species_2=="Bouteloua_gracilis","Bouteloua.gracilis",ifelse(Genus_Species_2=="Gutierrezia_sarothrae","Gutierrezia.sarothrae",ifelse(Genus_Species_2=="Artemisia_cana","Artemisia.cana",ifelse(Genus_Species_2=="Artemisia_dracunculus","Artemisia.dracunculus",ifelse(Genus_Species_2=="Bouteloua_dactyloides","Bouteloua.dactyloides",ifelse(Genus_Species_2=="Sporobolus_cryptandrus","Sporobolus.cryptandrus",Genus_Species_2))))))))))))))))))))))))) %>% 
  dplyr::select(-genus_species,-Genus_Species_2)

AverageTraits<-Traits_Clean_2%>% 
  group_by(Site,Genus_Species_Correct,species_code,Season,DxG_block) %>% 
  summarise(
    Avg_height_cm=mean(height_cm,na.rm=T),
    Avg_biomass_g=mean(Plant_Biomass,na.rm=T),
    Avg_percent_green=mean(percent_green,na.rm=T),
    Avg_emerging_leaves=mean(emerging_leaves,na.rm=T),
    Avg_developed_leaves=mean(developed_leaves,na.rm=T),
    Avg_scenesced_leaves=mean(scenesced_leaves,na.rm=T),
    Avg_flower_heads=mean(flower_heads,na.rm=T),
    Avg_open_flowers=mean(open_flowers,na.rm=T),
    Avg_leaf_thickness=mean(leaf_thickness_.mm.,na.rm=T),
    Avg_flower_num=mean(total_flower_num,na.rm=T), 
    Avg_LDMC=mean(LDMC,na.rm=T),
    Avg_total_leaf=mean(total_leaf_num,na.rm=T),
    Avg_SLA=mean(SLA,na.rm=T),
    Avg_lifespan=mean(lifespan_binary,na.rm=T),
    Avg_growth_form=mean(growth_form_binary,na.rm=T),
    Avg_photosynthetic_pathway=mean(photosynthetic_pathway_binary,na.rm=T),
    Avg_Area=mean(Total.Area,na.rm=T)
  ) %>% 
  ungroup() 

CWM_Collected_Data<- Species_Comp_RelCov_All %>% 
  left_join(plot_layoutK) %>% 
  rename(Site=site) %>%
  filter(!is.na(Relative_Cover)) %>% 
  filter(Relative_Cover!=0) %>% 
  left_join(AverageTraits) %>%
  group_by(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment) %>% 
  #calculate CWM using tidyr function, removing NAs for now until more data are collected
  summarise(
    Height_CWM=weighted.mean(Avg_height_cm,Relative_Cover,na.rm = T),
    #Biomass_CWM=weighted.mean(Avg_biomass_mg,Relative_Cover,na.rm=T),
    PercentGreen_CWM=weighted.mean(Avg_percent_green,Relative_Cover,na.rm=T),
    EmergingLeaves_CWM=weighted.mean(Avg_emerging_leaves,Relative_Cover,na.rm=T),
    DevelopedLeaves_CWM=weighted.mean(Avg_developed_leaves,Relative_Cover,na.rm=T),
    ScenescedLeaves_CWM=weighted.mean(Avg_scenesced_leaves,Relative_Cover,na.rm=T),
    FlowerHeads_CWM=weighted.mean(Avg_flower_heads,Relative_Cover,na.rm=T),
    OpenFlowers_CWM=weighted.mean(Avg_open_flowers,Relative_Cover,na.rm=T),
    LeafThickness_CWM=weighted.mean(Avg_leaf_thickness,Relative_Cover,na.rm=T),
    FlowerNum_CWM=weighted.mean(Avg_flower_num,Relative_Cover,na.rm=T),
    LDMC_CWM=weighted.mean(Avg_LDMC,Relative_Cover,na.rm=T),
    Biomass_CWM=weighted.mean(Avg_biomass_g,Relative_Cover,na.rm=T),
    TotalLeaf_CWM=weighted.mean(Avg_total_leaf,Relative_Cover,na.rm=T),
    Avg_SLA_CWM=weighted.mean(Avg_SLA,Relative_Cover,na.rm=T),
    Lifespan_CWM=weighted.mean(Avg_lifespan,Relative_Cover,na.rm=T),
    GrowthForm_CWM=weighted.mean(Avg_growth_form,Relative_Cover,na.rm=T),
    PhotosyntheticPathway_CWM=weighted.mean(Avg_photosynthetic_pathway,Relative_Cover,na.rm=T),
    Area_CWM=weighted.mean(Avg_Area,na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Trtm=paste(Rainfall_reduction_cat,grazing_treatment,sep = "_")) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))

#### Trait Correlation Testing CWM ####

#Transform Data - FK
CWM_Collected_Data_FK<-CWM_Collected_Data %>%
  filter(Site=="FK") %>% 
  mutate(Height_CWM_TF=sqrt(Height_CWM)) %>% 
  mutate(PercentGreen_CWM_TF=log10(PercentGreen_CWM)) %>% 
  mutate(EmergingLeaves_CWM_TF=sqrt(EmergingLeaves_CWM)) %>% 
  mutate(DevelopedLeaves_CWM_TF=sqrt(DevelopedLeaves_CWM)) %>% 
  mutate(ScenescedLeaves_CWM_TF=sqrt(ScenescedLeaves_CWM)) %>% 
  mutate(FlowerHeads_CWM_TF=sqrt(FlowerHeads_CWM)) %>% 
  mutate(OpenFlowers_CWM_TF=sqrt(OpenFlowers_CWM)) %>% 
  mutate(LeafThickness_CWM_TF=log10(LeafThickness_CWM)) %>% 
  mutate(FlowerNum_CWM_TF=sqrt(FlowerNum_CWM)) %>% 
  mutate(LDMC_CWM_TF=sqrt(LDMC_CWM)) %>% 
  mutate(Biomass_CWM_TF=Biomass_CWM) %>% 
  mutate(TotalLeaf_CWM_TF=sqrt(TotalLeaf_CWM)) %>% 
  mutate(Avg_SLA_CWM_TF=log10(Avg_SLA_CWM)) %>%
  mutate(Lifespan_CWM_TF=sqrt(Lifespan_CWM)) %>% 
  mutate(GrowthForm_CWM_TF=exp(GrowthForm_CWM)) %>% 
  mutate(PhotosyntheticPathway_CWM_TF=log(PhotosyntheticPathway_CWM)) %>% 
  mutate(Area_CWM_TF=sqrt(Area_CWM))

#all correlations
chart.Correlation(CWM_Collected_Data_FK[30:46],pch="41", cex = 4, method="spearman", histogram = TRUE)

#make chart correlation with just traits we discussed
#transformed
chart.Correlation(CWM_Collected_Data_FK[c(30,31,37,39,42,43,44,46)],pch="41", cex = 4, method="spearman", histogram = TRUE)
#not transformed
chart.Correlation(CWM_Collected_Data_FK[c(10,11,17,19,22,23,24,26)],pch="41", cex = 4, method="spearman", histogram = TRUE)

#TB
CWM_Collected_Data_TB<-CWM_Collected_Data %>%
  filter(Site=="TB") %>% 
  mutate(Height_CWM_TF=sqrt(Height_CWM)) %>% 
  mutate(PercentGreen_CWM_TF=log10(PercentGreen_CWM)) %>% 
  mutate(EmergingLeaves_CWM_TF=sqrt(EmergingLeaves_CWM)) %>% 
  mutate(DevelopedLeaves_CWM_TF=sqrt(DevelopedLeaves_CWM)) %>% 
  mutate(ScenescedLeaves_CWM_TF=sqrt(ScenescedLeaves_CWM)) %>% 
  mutate(FlowerHeads_CWM_TF=sqrt(FlowerHeads_CWM)) %>% 
  mutate(OpenFlowers_CWM_TF=sqrt(OpenFlowers_CWM)) %>% 
  mutate(LeafThickness_CWM_TF=log(LeafThickness_CWM)) %>% 
  mutate(FlowerNum_CWM_TF=sqrt(FlowerNum_CWM)) %>% 
  mutate(LDMC_CWM_TF=1/(LDMC_CWM))%>% 
  mutate(Biomass_CWM_TF=Biomass_CWM) %>% 
  mutate(TotalLeaf_CWM_TF=sqrt(TotalLeaf_CWM)) %>% 
  mutate(Avg_SLA_CWM_TF=log10(Avg_SLA_CWM)) %>%
  mutate(Lifespan_CWM_TF=exp(Lifespan_CWM)) %>% 
  mutate(GrowthForm_CWM_TF=exp(GrowthForm_CWM)) %>% 
  mutate(PhotosyntheticPathway_CWM_TF=log(PhotosyntheticPathway_CWM))%>% 
  mutate(Area_CWM_TF=sqrt(Area_CWM))

#all correlations
chart.Correlation(CWM_Collected_Data_TB[30:46],pch="41", cex = 4, method="spearman", histogram = TRUE)

#make chart correlation with just traits we discussed - TB
chart.Correlation(CWM_Collected_Data_TB[c(30,31,37,39,42,43,44,46)],pch="41", cex = 4, method="spearman", histogram = TRUE)
#not transformed
chart.Correlation(CWM_Collected_Data_TB[c(10,11,17,19,22,23,24,26)],pch="41", cex = 4, method="spearman", histogram = TRUE)

CWM_Collected_Data<-CWM_Collected_Data_FK %>% 
  rbind(CWM_Collected_Data_TB)

#### Plot the data ####

####CWM - Height Plots and Stats #### 
#2022 still needs to be added in

#CWM of height - 2019 and FK
Height_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=30, label = "FK 2019", size=20)

#CWM of height - 2020 and FK
Height_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=30, label = "FK 2020", size=20)


#CWM of height - 2021 and FK
Height_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=30, label = "FK 2021", size=20)

#CWM of height - 2022 and FK
Height_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=Height_CWM)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=30, label = "FK 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Height_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Height_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Height_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Height_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of height for Fort Keogh 2018 - LMER
FK_Height_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), Height_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Height_2018_LMER, type = 3)
#grazing (p=0.2133), drought (p=0.7010), grazing*drought(p=0.1052)

#CWM of height for Fort Keogh 2019 - LMER
FK_Height_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Height_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Height_2019_LMER, type = 3)
#drought (p=0.1765)

#CWM of height for Fort Keogh 2020 - LMER
FK_Height_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), Height_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Height_2020_LMER, type = 3)
#grazing (p=0.5786), drought (p=0.6126), grazing*drought(p=0.4688)

#CWM of height for Fort Keogh 2021 - LMER
FK_Height_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), Height_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
summary(FK_Height_2021_LMER)
anova(FK_Height_2021_LMER, type = 3)
#grazing (p=0.6997953), drought (p=0.0009396), grazing*drought(p=0.0819964)
#post hoc test for lmer test on rainfall reduction
summary(glht(FK_Height_2021_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#75-0 (p=0.005979), 50-25 (0.36559), 75-25 (0.000825)
#post hoc comparing slopes of lines


#CWM of height for Fort Keogh 2022 - LMER
FK_Height_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), Height_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Height_2022_LMER, type = 3)
#grazing (p=0.86285), drought (p=0.02237), grazing*drought(p=0.48520)

##Thunder Basin

#CWM of height - 2019 and TB
Height_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=30, label = "TB 2019", size=20)


#CWM of height - 2020 and TB
Height_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=30, label = "TB 2020", size=20)


#CWM of height - 2021 and TB
Height_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=30, label = "TB 2021", size=20)

#CWM of height - 2022 and TB
Height_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=30, label = "TB 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Height_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Height_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Height_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Height_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of height for Thunder Basin 2018 - LMER
TB_Height_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), Height_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Height_2018_LMER, type = 3)
#grazing (p=0.009498), drought (p=0.521174), grazing*drought(p=0.314285)
#post hoc test for lmer test
summary(glht(TB_Height_2018_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))

#CWM of height for Thunder Basin 2019 - LMER
TB_Height_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Height_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Height_2019_LMER, type = 3)
#drought (p=0.4983)

#CWM of height for Thunder Basin 2020 - LMER
TB_Height_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), Height_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Height_2020_LMER, type = 3)
#grazing (p=0.2947), drought (p=0.3690), grazing*drought(p=0.4527)

#CWM of height for Thunder Basin 2021 - LMER
TB_Height_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), Height_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Height_2021_LMER, type = 3)
#grazing (p=0.1568), drought (p=0.3947), grazing*drought(p=0.7429)

#CWM of height for Thunder Basin 2022 - LMER
TB_Height_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), Height_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Height_2022_LMER, type = 3)
#grazing (p=0.04306), drought (p=0.24875), grazing*drought(p=0.79121)
#post hoc test for lmer test on rainfall reduction
summary(glht(TB_Height_2022_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 

####CWM - Percent Green Plots and Stats #### 
#2022 still needs to be added in 

#CWM of % Green - 2019 and FK
Green_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=100, label = "FK 2019", size=20)


#CWM of % Green - 2020 and FK
Green_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=PercentGreen_CWM)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=100, label = "FK 2020", size=20)


#CWM of % Green - 2021 and FK
Green_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=PercentGreen_CWM)) +
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","solid","solid"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=100, label = "FK 2021", size=20)

#CWM of % Green - 2022 and FK
Green_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","solid","solid"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=100, label = "FK 2022", size=20)

#Create graph of all years for % Green data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Green_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Green_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Green_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Green_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of PercentGreen for Fort Keogh 2018 - LMER
FK_PercentGreen_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), PercentGreen_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2018_LMER, type = 3)
#grazing (p=0.15757), drought (p=0.01724), grazing*drought(p=0.14112)
#post hoc test for lmer test
summary(glht(FK_PercentGreen_2018_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance

#CWM of PercentGreen for Fort Keogh 2019 - LMER
FK_PercentGreen_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), PercentGreen_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2019_LMER, type = 3)
#drought (p=0.8495)

#CWM of PercentGreen for Fort Keogh 2020 - LMER
FK_PercentGreen_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), PercentGreen_CWM ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2020_LMER, type = 3)
#grazing (p=0.98790), drought (p=0.02319), grazing*drought(p=0.60878)
#post hoc test for lmer test
summary(glht(FK_PercentGreen_2020_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance

#CWM of PercentGreen for Fort Keogh 2021 - LMER
FK_PercentGreen_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), PercentGreen_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2021_LMER, type = 3)
#grazing (p=0.855253), drought (p=0.004181), grazing*drought(p=0.935394)
#post hoc test for lmer test
summary(glht(FK_PercentGreen_2021_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance

FK_PercentGreen_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), PercentGreen_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2022_LMER, type = 3)
#grazing (p=0.3831), drought (p=0.6320), grazing*drought(p=0.7856)

##Thunder Basin

#CWM of % Green - 2019 and TB
Green_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=100, label = "TB 2019", size=20)


#CWM of % Green - 2020 and TB
Green_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=100, label = "TB 2020", size=20)


#CWM of % Green - 2021 and TB
Green_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=100, label = "TB 2021", size=20)

#CWM of % Green - 2022 and TB
Green_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=100, label = "TB 2022", size=20)

#Create graph of all years for % Green data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Green_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Green_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Green_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Green_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of PercentGreen for Thunder Basin 2018 - LMER
TB_PercentGreen_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), PercentGreen_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2018_LMER, type = 3)
#grazing (p=0.1805), drought (p=0.1084), grazing*drought(p=0.2070)

#CWM of PercentGreen for Thunder Basin 2019 - LMER
TB_PercentGreen_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), PercentGreen_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2019_LMER, type = 3)
#drought (p=0.3487)

#CWM of PercentGreen for Thunder Basin 2020 - LMER
TB_PercentGreen_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), PercentGreen_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2020_LMER, type = 3)
#grazing (p=0.49677), drought (p=0.19312), grazing*drought(p=0.06024)


#CWM of PercentGreen for Thunder Basin 2021 - LMER
TB_PercentGreen_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), PercentGreen_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2021_LMER, type = 3)
#grazing (p=0.2083), drought (p=0.4037), grazing*drought(p=0.8290)

#CWM of PercentGreen for Thunder Basin 2022 - LMER
TB_PercentGreen_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), PercentGreen_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2022_LMER, type = 3)
#grazing (p=0.09664), drought (p=0.04725), grazing*drought(p=0.66708)
#post hoc test for lmer test drought
summary(glht(TB_PercentGreen_2022_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance 
#post hoc test for lmer test grazing
summary(glht(TB_PercentGreen_2022_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))
#heavy-destock (0.0955)

####CWM - Emerging Leaves Plots and Stats #### 
#2022 still needs to be added in 

#CWM of Emerging Leaves - 2019 and FK
EmergingLeaves_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Emerging Leaves (cm)")+
  expand_limits(y=15)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=15, label = "FK 2019", size=20)


#CWM of Emerging Leaves - 2020 and FK
EmergingLeaves_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=EmergingLeaves_CWM)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Emerging Leaves (cm)")+
  expand_limits(y=15)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=15, label = "FK 2020", size=20)


#CWM of Emerging Leaves - 2021 and FK
EmergingLeaves_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Emerging Leaves (cm)")+
  expand_limits(y=10)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=10, label = "FK 2021", size=20)

#CWM of Emerging Leaves - 2022 and FK
EmergingLeaves_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Emerging Leaves (cm)")+
  expand_limits(y=10)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=10, label = "FK 2022", size=20)

#Create graph of all years for Emerging Leaves data
pushViewport(viewport(layout=grid.layout(2,2)))
print(EmergingLeaves_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(EmergingLeaves_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(EmergingLeaves_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(EmergingLeaves_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of EmergingLeaves for Fort Keogh 2018 - LMER
FK_EmergingLeaves_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), EmergingLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_EmergingLeaves_2018_LMER, type = 3)
#grazing (p=0.62651), drought (p=0.01113), grazing*drought(p=0.62248)
#post hoc test for lmer test
summary(glht(FK_EmergingLeaves_2018_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significane

#CWM of EmergingLeaves for Fort Keogh 2019 - LMER
FK_EmergingLeaves_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), EmergingLeaves_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_EmergingLeaves_2019_LMER, type = 3)
#drought (p=0.2397)

#CWM of EmergingLeaves for Fort Keogh 2020 - LMER
FK_EmergingLeaves_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), EmergingLeaves_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_EmergingLeaves_2020_LMER, type = 3)
#grazing (p=0.31190), drought (p=0.02356), grazing*drought(p=0.98542)
#post hoc test for lmer test
summary(glht(FK_EmergingLeaves_2020_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance

#CWM of Emerging Leaves for Fort Keogh 2021 - LMER
FK_EmergingLeaves_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), EmergingLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_EmergingLeaves_2021_LMER, type = 3)
#grazing (p=0.3024), drought (p=0.4010), grazing*drought(p=0.5852)

##CWM of EmergingLeaves for Fort Keogh 2022 - LMER
FK_EmergingLeaves_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), EmergingLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_EmergingLeaves_2022_LMER, type = 3)
#grazing (p=0.3438), drought (p=0.3546), grazing*drought(p=0.4631)


##Thunder Basin

#CWM of Emerging Leaves - 2019 and TB
EmergingLeaves_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Emerging Leaves (cm)")+
  expand_limits(y=6)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=6, label = "TB 2019", size=20)


#CWM of Emerging Leaves - 2020 and TB
EmergingLeaves_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Emerging Leaves (cm)")+
  expand_limits(y=6)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=6, label = "TB 2020", size=20)


#CWM of Emerging Leaves - 2021 and TB
EmergingLeaves_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Emerging Leaves (cm)")+
  expand_limits(y=6)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=6, label = "TB 2021", size=20)

#CWM of Emerging Leaves - 2022 and TB
EmergingLeaves_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Emerging Leaves (cm)")+
  expand_limits(y=6)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=6, label = "TB 2022", size=20)

#Create graph of all years for Emerging Leaves data
pushViewport(viewport(layout=grid.layout(2,2)))
print(EmergingLeaves_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(EmergingLeaves_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(EmergingLeaves_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(EmergingLeaves_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of EmergingLeaves for Thunder Basin 2018 - LMER
TB_EmergingLeaves_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), EmergingLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_EmergingLeaves_2018_LMER, type = 3)
#grazing (p=0.02996), drought (p=0.83287), grazing*drought(p=0.30538)

#CWM of EmergingLeaves for Thunder Basin 2019 - LMER
TB_EmergingLeaves_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), EmergingLeaves_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_EmergingLeaves_2019_LMER, type = 3)
#drought (p=0.4193)

#CWM of EmergingLeaves for Thunder Basin 2020 - LMER
TB_EmergingLeaves_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), EmergingLeaves_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_EmergingLeaves_2020_LMER, type = 3)
#grazing (p=0.1858), drought (p=0.6625), grazing*drought(p=0.3265)

#CWM of EmergingLeaves for Thunder Basin 2021 - LMER
TB_EmergingLeaves_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), EmergingLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_EmergingLeaves_2021_LMER, type = 3)
#grazing (p=0.3690), drought (p=0.9297), grazing*drought(p=0.4890)

#CWM of EmergingLeaves for Thunder Basin 2022 - LMER
TB_EmergingLeaves_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), EmergingLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_EmergingLeaves_2022_LMER, type = 3)
#grazing (p=0.1576), drought (p=0.6863), grazing*drought(p=0.5750)

####CWM - Developed Leaves Plots and Stats #### 
#2022 still needs to be added in 

#CWM of Developed Leaves - 2019 and FK
DevelopedLeaves_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=15)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=15, label = "FK 2019", size=20)


#CWM of Developed Leaves - 2020 and FK
DevelopedLeaves_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=15)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=15, label = "FK 2020", size=20)


#CWM of Developed Leaves - 2021 and FK
DevelopedLeaves_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=15)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=15, label = "FK 2021", size=20)

#CWM of Developed Leaves - 2022 and FK
DevelopedLeaves_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=15)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=15, label = "FK 2022", size=20)

#Create graph of all years for Developed Leaves data
pushViewport(viewport(layout=grid.layout(2,2)))
print(DevelopedLeaves_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(DevelopedLeaves_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(DevelopedLeaves_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(DevelopedLeaves_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of DevelopedLeaves for Fort Keogh 2018 - LMER
FK_DevelopedLeaves_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), DevelopedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_DevelopedLeaves_2018_LMER, type = 3)
#grazing (p=0.1006), drought (p=0.1373), grazing*drought(p=0.2652)

#CWM of DevelopedLeaves for Fort Keogh 2019 - LMER
FK_DevelopedLeaves_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), DevelopedLeaves_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_DevelopedLeaves_2019_LMER, type = 3)
#drought (p=0.5037)

#CWM of DevelopedLeaves for Fort Keogh 2020 - LMER
FK_DevelopedLeaves_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), DevelopedLeaves_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_DevelopedLeaves_2020_LMER, type = 3)
#grazing (p=0.8189), drought (p=0.6054), grazing*drought(p=0.9377)

#CWM of DevelopedLeaves for Fort Keogh 2021 - LMER
FK_DevelopedLeaves_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), DevelopedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_DevelopedLeaves_2021_LMER, type = 3)
#grazing (p=0.4296), drought (p=0.3093), grazing*drought(p=0.6436)

#CWM of DevelopedLeaves for Fort Keogh 2022 - LMER
FK_DevelopedLeaves_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), DevelopedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_DevelopedLeaves_2022_LMER, type = 3)
#grazing (p=0.4296), drought (p=0.3093), grazing*drought(p=0.6436)

##Thunder Basin

#CWM of Developed Leaves - 2019 and TB #line graphs
DevelopedLeaves_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=20)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=20, label = "TB 2019", size=20)


#CWM of Developed Leaves - 2020 and TB #line graphs
DevelopedLeaves_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=20)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=20, label = "TB 2020", size=20)


#CWM of Developed Leaves - 2021 and TB #line graphs
DevelopedLeaves_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=20)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=20, label = "TB 2021", size=20)

#CWM of Developed Leaves - 2022 and TB #line graphs
DevelopedLeaves_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=20)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=20, label = "TB 2022", size=20)

#Create graph of all years for Developed Leaves data
pushViewport(viewport(layout=grid.layout(2,2)))
print(DevelopedLeaves_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(DevelopedLeaves_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(DevelopedLeaves_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(DevelopedLeaves_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#### Box plot for Developed Leaves ####
DevelopedLeaves_TB_19_box<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=grazing_treatment,y=DevelopedLeaves_CWM)) +  
  geom_boxplot()+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Grazing Treatment")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=20)+
  theme(axis.text.y=element_text(size=55),axis.title.y=element_text(size=55),axis.text.x=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=20, label = "TB 2019", size=20)

DevelopedLeaves_TB_20_box<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=grazing_treatment,y=DevelopedLeaves_CWM)) +  
  geom_boxplot()+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Grazing Treatment")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=20)+
  theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=20, label = "TB 2020", size=20)

DevelopedLeaves_TB_21_box<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=grazing_treatment,y=DevelopedLeaves_CWM)) +  
  geom_boxplot()+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Grazing Treatment")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=20)+
  theme(axis.text.y=element_text(size=55),axis.title.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=1, y=20, label = "TB 2021", size=20)

DevelopedLeaves_TB_22_box<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=grazing_treatment,y=DevelopedLeaves_CWM)) +  
  geom_boxplot()+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Grazing Treatment")+
  ylab("CWM Developed Leaves (cm)")+
  expand_limits(y=20)+
  theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.text.x=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=1, y=20, label = "TB 2022", size=20)

#Create graph of all years for Developed Leaves data
pushViewport(viewport(layout=grid.layout(2,2)))
print(DevelopedLeaves_TB_19_box,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(DevelopedLeaves_TB_20_box,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(DevelopedLeaves_TB_21_box,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(DevelopedLeaves_TB_22_box,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of DevelopedLeaves for Thunder Basin 2018 - LMER
TB_DevelopedLeaves_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), DevelopedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_DevelopedLeaves_2018_LMER, type = 3)
#grazing (p=0.003759), drought (p=0.419828), grazing*drought(p=0.488266)

#CWM of DevelopedLeaves for Thunder Basin 2019 - LMER
TB_DevelopedLeaves_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), DevelopedLeaves_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_DevelopedLeaves_2019_LMER, type = 3)
#drought (p=0.3751)

#CWM of DevelopedLeaves for Thunder Basin 2020 - LMER
TB_DevelopedLeaves_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), DevelopedLeaves_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_DevelopedLeaves_2020_LMER, type = 3)
#grazing (p=0.08299), drought (p=0.81360), grazing*drought(p=0.43868)
#post hoc test for lmer test
summary(glht(TB_DevelopedLeaves_2020_LMER, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH"))
#no significance


#CWM of DevelopedLeaves for Thunder Basin 2021 - LMER
TB_DevelopedLeaves_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), DevelopedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_DevelopedLeaves_2021_LMER, type = 3)
#grazing (p=0.4952), drought (p=0.9982), grazing*drought(p=0.5508)

#CWM of DevelopedLeaves for Thunder Basin 2022 - LMER
TB_DevelopedLeaves_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), DevelopedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_DevelopedLeaves_2022_LMER, type = 3)
#grazing (p=0.3378), drought (p=0.8275), grazing*drought(p=0.7658)

####CWM - Scenesced Leaves Plots and Stats #### 
#2022 still needs to be added in 

#CWM of Scenesced Leaves - 2019 and FK
ScenescedLeaves_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=ScenescedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Scenesced Leaves (cm)")+
  expand_limits(y=5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=5, label = "FK 2019", size=20)


#CWM of Scenesced Leaves - 2020 and FK
ScenescedLeaves_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=ScenescedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Scenesced Leaves (cm)")+
  expand_limits(y=5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=5, label = "FK 2020", size=20)


#CWM of Scenesced Leaves - 2021 and FK
ScenescedLeaves_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=ScenescedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Scenesced Leaves (cm)")+
  expand_limits(y=5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "FK 2021", size=20)

#CWM of Scenesced Leaves - 2022 and FK
ScenescedLeaves_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=ScenescedLeaves_CWM)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Scenesced Leaves (cm)")+
  expand_limits(y=5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "FK 2022", size=20)

#Create graph of all years for Scenesced Leaves data
pushViewport(viewport(layout=grid.layout(2,2)))
print(ScenescedLeaves_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(ScenescedLeaves_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(ScenescedLeaves_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(ScenescedLeaves_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of ScenescedLeaves for Fort Keogh 2018 - LMER
FK_ScenescedLeaves_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), ScenescedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_ScenescedLeaves_2018_LMER, type = 3)
#grazing (p=0.9375), drought (p=0.5642), grazing*drought(p=0.3112)

#CWM of ScenescedLeaves for Fort Keogh 2019 - LMER
FK_ScenescedLeaves_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), ScenescedLeaves_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_ScenescedLeaves_2019_LMER, type = 3)
#drought (p=0.6101)

#CWM of ScenescedLeaves for Fort Keogh 2020 - LMER
FK_ScenescedLeaves_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), ScenescedLeaves_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_ScenescedLeaves_2020_LMER, type = 3)
#grazing (p=0.6085), drought (p=0.2820), grazing*drought(p=0.7992)

#CWM of ScenescedLeaves for Fort Keogh 2021 - LMER
FK_ScenescedLeaves_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), ScenescedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_ScenescedLeaves_2021_LMER, type = 3)
#grazing (p=0.7516), drought (p=0.2891), grazing*drought(p=0.2450)

#CWM of ScenescedLeaves for Fort Keogh 2022 - LMER
FK_ScenescedLeaves_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), ScenescedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_ScenescedLeaves_2022_LMER, type = 3)
#grazing (p=0.37198), drought (p=0.01175), grazing*drought(p=0.28801)
#post hoc test for lmer test
summary(glht(FK_ScenescedLeaves_2022_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))

##Thunder Basin

#CWM of Scenesced Leaves - 2019 and TB
ScenescedLeaves_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=ScenescedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Scenesced Leaves (cm)")+
  expand_limits(y=5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=5, label = "TB 2019", size=20)


#CWM of Scenesced Leaves - 2020 and TB
ScenescedLeaves_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=ScenescedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","solid","solid"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Scenesced Leaves (cm)")+
  expand_limits(y=5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=5, label = "TB 2020", size=20)


#CWM of Scenesced Leaves - 2021 and TB
ScenescedLeaves_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=ScenescedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Scenesced Leaves (cm)")+
  expand_limits(y=5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "TB 2021", size=20)

#CWM of Scenesced Leaves - 2022 and TB
ScenescedLeaves_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=ScenescedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Scenesced Leaves (cm)")+
  expand_limits(y=5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "TB 2022", size=20)

#Create graph of all years for Scenesced Leaves data
pushViewport(viewport(layout=grid.layout(2,2)))
print(ScenescedLeaves_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(ScenescedLeaves_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(ScenescedLeaves_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(ScenescedLeaves_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of ScenescedLeaves for Thunder Basin 2018 - LMER
TB_ScenescedLeaves_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), ScenescedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_ScenescedLeaves_2018_LMER, type = 3)
#grazing (p=0.003424), drought (p=0.231840), grazing*drought(p=0.333466)
#post hoc test for lmer test
summary(glht(TB_ScenescedLeaves_2018_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))
#no significance

#CWM of ScenescedLeaves for Thunder Basin 2019 - LMER
TB_ScenescedLeaves_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), ScenescedLeaves_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_ScenescedLeaves_2019_LMER, type = 3)
#drought (p=0.3325)

#CWM of ScenescedLeaves for Thunder Basin 2020 - LMER
TB_ScenescedLeaves_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), ScenescedLeaves_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_ScenescedLeaves_2020_LMER, type = 3)
#grazing (p=0.9383), drought (p=0.2637), grazing*drought(p=0.4256)


#CWM of ScenescedLeaves for Thunder Basin 2021 - LMER
TB_ScenescedLeaves_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), ScenescedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_ScenescedLeaves_2021_LMER, type = 3)
#grazing (p=0.3414), drought (p=0.4713), grazing*drought(p=0.7283)

#CWM of ScenescedLeaves for Thunder Basin 2022 - LMER
TB_ScenescedLeaves_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), ScenescedLeaves_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_ScenescedLeaves_2022_LMER, type = 3)
#grazing (p=0.0412), drought (p=0.4911), grazing*drought(p=0.7575)
#post hoc test for lmer test
summary(glht(TB_ScenescedLeaves_2022_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))

####CWM - Flower Heads Plots and Stats #### 
#2022 still needs to be added in 

#CWM of Flower Heads - 2019 and FK
FlowerHeads_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FlowerHeads_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Flower Heads")+
  expand_limits(y=5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=5, label = "FK 2019", size=20)


#CWM of Flower Heads - 2020 and FK
FlowerHeads_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FlowerHeads_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Flower Heads")+
  expand_limits(y=5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=5, label = "FK 2020", size=20)


#CWM of Flower Heads - 2021 and FK
FlowerHeads_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FlowerHeads_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Flower Heads")+
  expand_limits(y=5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "FK 2021", size=20)

#CWM of Flower Heads - 2022 and FK
FlowerHeads_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FlowerHeads_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Flower Heads")+
  expand_limits(y=5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "FK 2022", size=20)

#Create graph of all years for Flower Heads data
pushViewport(viewport(layout=grid.layout(2,2)))
print(FlowerHeads_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FlowerHeads_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FlowerHeads_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FlowerHeads_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of FlowerHeads for Fort Keogh 2018 - LMER
FK_FlowerHeads_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), FlowerHeads_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_FlowerHeads_2018_LMER, type = 3)
#grazing (p=0.9341), drought (p=0.9073), grazing*drought(p=0.4458)

#CWM of FlowerHeads for Fort Keogh 2019 - LMER
FK_FlowerHeads_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), FlowerHeads_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_FlowerHeads_2019_LMER, type = 3)
#drought (p=0.4926)

#CWM of FlowerHeads for Fort Keogh 2020 - LMER
FK_FlowerHeads_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), FlowerHeads_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_FlowerHeads_2020_LMER, type = 3)
#grazing (p=0.6706), drought (p=0.8065), grazing*drought(p=0.6929)

#CWM of FlowerHeads for Fort Keogh 2021 - LMER
FK_FlowerHeads_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), FlowerHeads_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_FlowerHeads_2021_LMER, type = 3)
#grazing (p=0.5648), drought (p=0.5355), grazing*drought(p=0.8493)

#CWM of FlowerHeads for Fort Keogh 2022 - LMER
FK_FlowerHeads_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), FlowerHeads_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_FlowerHeads_2022_LMER, type = 3)
#grazing (p=0.7063), drought (p=0.2274), grazing*drought(p=0.1471)

##Thunder Basin

#CWM of Flower Heads - 2019 and TB
FlowerHeads_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FlowerHeads_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Flower Heads (cm)")+
  expand_limits(y=10)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=10, label = "TB 2019", size=20)


#CWM of Flower Heads - 2020 and TB
FlowerHeads_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FlowerHeads_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Flower Heads (cm)")+
  expand_limits(y=10)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=10, label = "TB 2020", size=20)


#CWM of Flower Heads - 2021 and TB
FlowerHeads_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FlowerHeads_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Flower Heads (cm)")+
  expand_limits(y=10)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=10, label = "TB 2021", size=20)

#CWM of Flower Heads - 2022 and TB
FlowerHeads_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FlowerHeads_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Flower Heads (cm)")+
  expand_limits(y=10)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=10, label = "TB 2022", size=20)

#Create graph of all years for Flower Heads data
pushViewport(viewport(layout=grid.layout(2,2)))
print(FlowerHeads_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FlowerHeads_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FlowerHeads_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FlowerHeads_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of FlowerHeads for Thunder Basin 2018 - LMER
TB_FlowerHeads_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), FlowerHeads_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_FlowerHeads_2018_LMER, type = 3)
#grazing (p=0.8140, drought (p=0.3877), grazing*drought(p=0.3052)

#CWM of FlowerHeads for Thunder Basin 2019 - LMER
TB_FlowerHeads_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), FlowerHeads_CWM ~Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_FlowerHeads_2019_LMER, type = 3)
#drought (p=0.511)

#CWM of FlowerHeads for Thunder Basin 2020 - LMER
TB_FlowerHeads_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), FlowerHeads_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_FlowerHeads_2020_LMER, type = 3)
#grazing (p=0.3180), drought (p=0.3374), grazing*drought(p=0.2850)

#CWM of FlowerHeads for Thunder Basin 2021 - LMER
TB_FlowerHeads_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), FlowerHeads_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_FlowerHeads_2021_LMER, type = 3)
#grazing (p=0.9082), drought (p=0.1777), grazing*drought(p=0.5872)

#CWM of FlowerHeads for Thunder Basin 2022 - LMER
TB_FlowerHeads_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), FlowerHeads_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_FlowerHeads_2022_LMER, type = 3)
#grazing (p=0.7883), drought (p=0.2169), grazing*drought(p=0.1963)

####CWM - Open Flowers Plots and Stats #### 
#2022 still needs to be added in 

#CWM of Open Flowers - 2019 and FK
OpenFlowers_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=OpenFlowers_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Open Flowers")+
  expand_limits(y=2)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=2, label = "FK 2019", size=20)


#CWM of Open Flowers - 2020 and FK
OpenFlowers_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=OpenFlowers_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Open Flowers")+
  expand_limits(y=2)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=2, label = "FK 2020", size=20)


#CWM of Open Flowers - 2021 and FK
OpenFlowers_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=OpenFlowers_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Open Flowers")+
  expand_limits(y=2)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=2, label = "FK 2021", size=20)

#CWM of Open Flowers - 2022 and FK
OpenFlowers_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=OpenFlowers_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Open Flowers")+
  expand_limits(y=2)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=2, label = "FK 2022", size=20)

#Create graph of all years for Open Flowers data
pushViewport(viewport(layout=grid.layout(2,2)))
print(OpenFlowers_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(OpenFlowers_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(OpenFlowers_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(OpenFlowers_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of OpenFlowers for Fort Keogh 2018 - LMER
FK_OpenFlowers_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), OpenFlowers_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_OpenFlowers_2018_LMER, type = 3)
#grazing (p=0.3524), drought (p=0.3894), grazing*drought(p=0.2157)

#CWM of OpenFlowers for Fort Keogh 2019 - LMER
FK_OpenFlowers_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), OpenFlowers_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_OpenFlowers_2019_LMER, type = 3)
#drought (p=0.6919)

#CWM of OpenFlowers for Fort Keogh 2020 - LMER
FK_OpenFlowers_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), OpenFlowers_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_OpenFlowers_2020_LMER, type = 3)
#grazing (p=0.7696), drought (p=0.6802), grazing*drought(p=0.8328)

#CWM of OpenFlowers for Fort Keogh 2021 - LMER
FK_OpenFlowers_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), OpenFlowers_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_OpenFlowers_2021_LMER, type = 3)
#grazing (p=0.5933), drought (p=0.2636), grazing*drought(p=0.7423)

#CWM of OpenFlowers for Fort Keogh 2022 - LMER
FK_OpenFlowers_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), OpenFlowers_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_OpenFlowers_2022_LMER, type = 3)
#grazing (p=0.2108), drought (p=0.2610), grazing*drought(p=0.9359)

##Thunder Basin

#CWM of Open Flowers - 2019 and TB
OpenFlowers_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=OpenFlowers_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Open Flowers (cm)")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=1, label = "TB 2019", size=20)


#CWM of Open Flowers - 2020 and TB
OpenFlowers_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=OpenFlowers_CWM)) +  
  geom_point(aes(shape=grazing_treatment,color=grazing_treatment),size=6, stroke =2)+
  geom_smooth(method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Open Flowers (cm)")+
  expand_limits(y=1)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=1, label = "TB 2020", size=20)


#CWM of Open Flowers - 2021 and TB
OpenFlowers_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=OpenFlowers_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("dashed","twodash","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Open Flowers (cm)")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "TB 2021", size=20)

#CWM of Open Flowers - 2022 and TB
OpenFlowers_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=OpenFlowers_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Open Flowers (cm)")+
  expand_limits(y=1)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "TB 2022", size=20)

#Create graph of all years for Open Flowers data
pushViewport(viewport(layout=grid.layout(2,2)))
print(OpenFlowers_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(OpenFlowers_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(OpenFlowers_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(OpenFlowers_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of OpenFlowers for Thunder Basin 2018 - LMER
TB_OpenFlowers_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), OpenFlowers_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_OpenFlowers_2018_LMER, type = 3)
#grazing (p=0.001421, drought (p=0.062471), grazing*drought(p=0.542256)
#post hoc test for lmer test
summary(glht(TB_OpenFlowers_2018_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))
#no significance
#post hoc test for lmer test
summary(glht(TB_OpenFlowers_2018_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance 

#CWM of OpenFlowers for Thunder Basin 2019 - LMER
TB_OpenFlowers_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), OpenFlowers_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_OpenFlowers_2019_LMER, type = 3)
# drought (p=0.3958)

#CWM of OpenFlowers for Thunder Basin 2020 - LMER
TB_OpenFlowers_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), OpenFlowers_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_OpenFlowers_2020_LMER, type = 3)
#grazing (p=0.91760), drought (p=0.02809), grazing*drought(p=0.16068)
#post hoc test for lmer test
summary(glht(TB_OpenFlowers_2020_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#99-0 (0.0467), 99-25 (0.0467), 99-75 (0.0511)

#CWM of OpenFlowers for Thunder Basin 2021 - LMER
TB_OpenFlowers_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), OpenFlowers_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_OpenFlowers_2021_LMER, type = 3)
#grazing (p=0.84051), drought (p=0.27396), grazing*drought(p=0.06227)
#for interaction
TB_OpenFlowers_2021_LMER_C <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), OpenFlowers_CWM ~ Trtm + (1|block) + (1|block:paddock))
anova(TB_OpenFlowers_2021_LMER_C)
#now anova isnt sign though?
#post hoc test for lmer test
summary(glht(TB_OpenFlowers_2021_LMER_C, linfct = mcp(Trtm = "Tukey")), test = adjusted(type = "BH"))
#no significance 

#CWM of OpenFlowers for Thunder Basin 2022 - LMER
TB_OpenFlowers_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), OpenFlowers_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_OpenFlowers_2022_LMER, type = 3)
#grazing (p=0.1738), drought (p=0.7592), grazing*drought(p=0.4359)

####CWM - Total Flowers Plots and Stats #### 
#2022 still needs to be added in

#CWM of total flowers - 2019 and FK
Flowers_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FlowerNum_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Total Flower Number")+
  expand_limits(y=8)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=8, label = "FK 2019", size=20)


#CWM of total flowers - 2020 and FK
Flowers_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FlowerNum_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Total Flower Number")+
  expand_limits(y=8)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=8, label = "FK 2020", size=20)


#CWM of total flowers - 2021 and FK
Flowers_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FlowerNum_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Total Flower Number")+
  expand_limits(y=8)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=8, label = "FK 2021", size=20)

#CWM of total flowers - 2022 and FK
Flowers_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FlowerNum_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Total Flower Number")+
  expand_limits(y=8)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=8, label = "FK 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Flowers_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Flowers_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Flowers_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Flowers_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of FlowerNum for Fort Keogh 2018 - LMER
FK_FlowerNum_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), FlowerNum_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_FlowerNum_2018_LMER, type = 3)
#grazing (p=0.9003), drought (p=0.9447), grazing*drought(p=0.5371)

#CWM of FlowerNum for Fort Keogh 2019 - LMER
FK_FlowerNum_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), FlowerNum_CWM ~Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_FlowerNum_2019_LMER, type = 3)
#drought (p=0.6135)

#CWM of FlowerNum for Fort Keogh 2020 - LMER
FK_FlowerNum_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), FlowerNum_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_FlowerNum_2020_LMER, type = 3)
#grazing (p=0.7125), drought (p=0.8593), grazing*drought(p=0.7219)

#CWM of FlowerNum for Fort Keogh 2021 - LMER
FK_FlowerNum_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), FlowerNum_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_FlowerNum_2021_LMER, type = 3)
#grazing (p=0.5836), drought (p=0.5081), grazing*drought(p=0.8208)

#CWM of FlowerNum for Fort Keogh 2022 - LMER
FK_FlowerNum_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), FlowerNum_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_FlowerNum_2022_LMER, type = 3)
#grazing (p=0.6362), drought (p=0.1777), grazing*drought(p=0.1384)


##Thunder Basin

##CWM of total flowers - 2019 and TB
Flowers_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FlowerNum_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Total Flower Number")+
  expand_limits(y=8)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=8, label = "TB 2019", size=20)


#CWM of total flowers - 2020 and TB
Flowers_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FlowerNum_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Total Flower Number")+
  expand_limits(y=8)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=8, label = "TB 2020", size=20)


#CWM of total flowers - 2021 and TB
Flowers_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FlowerNum_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Total Flower Number")+
  expand_limits(y=8)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=8, label = "TB 2021", size=20)

#CWM of total flowers - 2022 and TB
Flowers_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FlowerNum_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Total Flower Number")+
  expand_limits(y=8)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=8, label = "TB 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Flowers_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Flowers_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Flowers_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Flowers_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of FlowerNum for Thunder Basin 2018 - LMER
TB_FlowerNum_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), FlowerNum_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_FlowerNum_2018_LMER, type = 3)
#grazing (p=0.5832), drought (p=0.5281), grazing*drought(p=0.3523)

#CWM of FlowerNum for TB 2019 - LMER
TB_FlowerNum_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), FlowerNum_CWM ~Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_FlowerNum_2019_LMER, type = 3)
#drought (p=0.5776)

#CWM of FlowerNum for TB 2020 - LMER
TB_FlowerNum_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), FlowerNum_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_FlowerNum_2020_LMER, type = 3)
#grazing (p=0.3411), drought (p=0.3867), grazing*drought(p=0.2589)

#CWM of FlowerNum for TB 2021 - LMER
TB_FlowerNum_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), FlowerNum_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_FlowerNum_2021_LMER, type = 3)
#grazing (p=0.9296), drought (p=0.2541), grazing*drought(p=0.6119)

#CWM of FlowerNum for TB 2022 - LMER
TB_FlowerNum_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), FlowerNum_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_FlowerNum_2022_LMER, type = 3)
#grazing (p=0.7685), drought (p=0.2689), grazing*drought(p=0.2045)

####CWM - Leaf Thickness Plots and Stats #### 
#2022 still needs to be added in

#CWM of Leaf Thickness - 2019 and FK
LeafThickness_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.5, label = "FK 2019", size=20)


#CWM of Leaf Thickness - 2020 and FK
LeafThickness_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "FK 2020", size=20)


#CWM of Leaf Thickness - 2021 and FK
LeafThickness_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "FK 2021", size=20)

#CWM of Leaf Thickness - 2022 and FK
LeafThickness_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "FK 2022", size=20)

#Create graph of all years for Leaf Thickness data
pushViewport(viewport(layout=grid.layout(2,2)))
print(LeafThickness_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(LeafThickness_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LeafThickness_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LeafThickness_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of LeafThickness for Fort Keogh 2018 - LMER
FK_LeafThickness_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), LeafThickness_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2018_LMER, type = 3)
#grazing (p=0.4198), drought (p=0.9533), grazing*drought(p=0.1858)

#CWM of LeafThickness for Fort Keogh 2019 - LMER
FK_LeafThickness_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), LeafThickness_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2019_LMER, type = 3)
#drought (p=0.389)

#CWM of LeafThickness for Fort Keogh 2020 - LMER
FK_LeafThickness_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), LeafThickness_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2020_LMER, type = 3)
#grazing (p=0.8634), drought (p=0.8172), grazing*drought(p=0.8932)

#CWM of LeafThickness for Fort Keogh 2021 - LMER
FK_LeafThickness_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), LeafThickness_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2021_LMER, type = 3)
#grazing (p=0.5948), drought (p=0.8867), grazing*drought(p=0.8539)

#CWM of LeafThickness for Fort Keogh 2022 - LMER
FK_LeafThickness_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), LeafThickness_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2022_LMER, type = 3)
#grazing (p=0.9697), drought (p=0.2508), grazing*drought(p=0.9704)

##Thunder Basin

#CWM of Leaf Thickness - 2019 and TB
LeafThickness_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.5, label = "TB 2019", size=20)


#CWM of Leaf Thickness - 2020 and TB
LeafThickness_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "TB 2020", size=20)


#CWM of Leaf Thickness - 2021 and TB
LeafThickness_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "TB 2021", size=20)

#CWM of Leaf Thickness - 2022 and TB
LeafThickness_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "TB 2022", size=20)

#Create graph of all years for Open Flowers data
pushViewport(viewport(layout=grid.layout(2,2)))
print(LeafThickness_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(LeafThickness_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LeafThickness_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LeafThickness_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  


#### Box plot for Leaf Thickness ####
Thickness_TB_19_box<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=factor(grazing_treatment,level=c("destock","stable","heavy")),y=LeafThickness_CWM)) +  
  geom_boxplot()+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Grazing Treatment")+
  ylab("CWM Leaf Thickness (cm)")+
  expand_limits(y=0.4)+
  theme(axis.text.y=element_text(size=55),axis.title.y=element_text(size=55),axis.text.x=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=0.4, label = "TB 2019", size=20)

Thickness_TB_20_box<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=factor(grazing_treatment,level=c("destock","stable","heavy")),y=LeafThickness_CWM)) +  
  geom_boxplot()+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Grazing Treatment")+
  ylab("CWM Leaf Thickness (cm)")+
  expand_limits(y=0.4)+
  theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=0.4, label = "TB 2020", size=20)

Thickness_TB_21_box<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=factor(grazing_treatment,level=c("destock","stable","heavy")),y=LeafThickness_CWM))+
  geom_boxplot()+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Grazing Treatment")+
  ylab("CWM Leaf Thickness (cm)")+
  expand_limits(y=0.4)+
  theme(axis.text.y=element_text(size=55),axis.title.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=1, y=0.4, label = "TB 2021", size=20)

Thickness_TB_22_box<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=factor(grazing_treatment,level=c("destock","stable","heavy")),y=LeafThickness_CWM)) +  
  geom_boxplot()+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Grazing Treatment")+
  ylab("CWM Leaf Thickness (cm)")+
  expand_limits(y=0.4)+
  theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.text.x=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=1, y=0.4, label = "TB 2022", size=20)

#Create graph of all years for Developed Leaves data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Thickness_TB_19_box,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Thickness_TB_20_box,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Thickness_TB_21_box,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Thickness_TB_22_box,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of LeafThickness for Thunder Basin 2018 - LMER
TB_LeafThickness_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), LeafThickness_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2018_LMER, type = 3)
#grazing (p=0.009469, drought (p=0.251629), grazing*drought(p=0.331860)
#post hoc test for lmer test
summary(glht(TB_LeafThickness_2018_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))
#stable-destock(p=0.00618)


#CWM of LeafThickness for Thunder Basin 2019 - LMER
TB_LeafThickness_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), LeafThickness_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2019_LMER, type = 3)
#drought (p=0.5277)

#CWM of LeafThickness for Thunder Basin 2020 - LMER
TB_LeafThickness_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), LeafThickness_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2020_LMER, type = 3)
#grazing (p=0.5722), drought (p=0.3767), grazing*drought(p=0.9549)

#CWM of LeafThickness for Thunder Basin 2021 - LMER
TB_LeafThickness_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), LeafThickness_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2021_LMER, type = 3)
#grazing (p=0.005631), drought (p=0.608871), grazing*drought(p=0.965710)
#post hoc test for lmer test
summary(glht(TB_LeafThickness_2021_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))
#stable-destock(p=0.0859)

#CWM of LeafThickness for Thunder Basin 2022 - LMER
TB_LeafThickness_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), LeafThickness_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2022_LMER, type = 3)
#grazing (p=3.144e-05), drought (p=0.4181), grazing*drought(p=0.9265)

####CWM - LDMC Plots and Stats #### 
#2022 still needs to be added in
#update data once it's all collected

#CWM of LDMC - 2019 and FK
LDMC_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.5, label = "FK 2019", size=20)


#CWM of LDMC - 2020 and FK
LDMC_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2020", size=20)


#CWM of LDMC - 2021 and FK
LDMC_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2021", size=20)

#CWM of LDMC - 2022 and FK
LDMC_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(LDMC_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(LDMC_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LDMC_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of LDMC for Fort Keogh 2018 - LMER
FK_LDMC_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), LDMC_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LDMC_2018_LMER, type = 3)
#grazing (p=0.2665), drought (p=0.2227), grazing*drought(p=0.3045)

#CWM of LDMC for Fort Keogh 2019 - LMER
FK_LDMC_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), LDMC_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LDMC_2019_LMER, type = 3)
#drought (p=0.9641)

#CWM of LDMC for Fort Keogh 2020 - LMER
FK_LDMC_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), LDMC_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LDMC_2020_LMER, type = 3)
#grazing (p=0.3492), drought (p=0.5718), grazing*drought(p=0.6738)

#CWM of LDMC for Fort Keogh 2021 - LMER
FK_LDMC_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), LDMC_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LDMC_2021_LMER, type = 3)
#grazing (p=0.8974), drought (p=0.6057), grazing*drought(p=0.9743)

#CWM of LDMC for Fort Keogh 2022 - LMER
FK_LDMC_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), LDMC_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LDMC_2022_LMER, type = 3)
#grazing (p=0.9281), drought (p=0.9429), grazing*drought(p=0.5343)

##Thunder Basin 
#CWM of LDMC - 2019 and TB
LDMC_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=c(0,0.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.5, label = "TB 2019", size=20)


#CWM of LDMC - 2020 and TB
LDMC_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=c(0,0.5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "TB 2020", size=20)


#CWM of LDMC - 2021 and TB
LDMC_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=c(0,0.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "TB 2021", size=20)

#CWM of LDMC - 2022 and TB
LDMC_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=c(0,0.5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "TB 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(LDMC_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(LDMC_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LDMC_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of LDMC for TB 2018 - LMER
TB_LDMC_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), LDMC_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LDMC_2018_LMER, type = 3)
#grazing (p=0.1568), drought (p=0.1427), grazing*drought(p=0.3960)

#CWM of LDMC for TB 2019 - LMER
TB_LDMC_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), LDMC_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LDMC_2019_LMER, type = 3)
#drought (p=0.5297)

#CWM of LDMC for TB 2020 - LMER
TB_LDMC_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), LDMC_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LDMC_2020_LMER, type = 3)
#grazing (p=0.5243), drought (p=0.1575), grazing*drought(p=0.3674)

#CWM of LDMC for TB 2021 - LMER
TB_LDMC_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), LDMC_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LDMC_2021_LMER, type = 3)
#grazing (p=0.07248), drought (p=0.24112), grazing*drought(p=0.78809)

#CWM of LDMC for TB 2022 - LMER
TB_LDMC_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), LDMC_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LDMC_2022_LMER, type = 3)
#grazing (p=0.2010), drought (p=0.2214), grazing*drought(p=0.8344)

####CWM - Biomass Plots and Stats #### 
#2022 still needs to be added in
#update data once it's all collected

#CWM of Biomass - 2019 and FK
Biomass_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.5, label = "FK 2019", size=20)


#CWM of Biomass - 2020 and FK
Biomass_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2020", size=20)


#CWM of Biomass - 2021 and FK
Biomass_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=OpenFlowers_CWM)) +  
  geom_point(aes(shape=grazing_treatment,color=grazing_treatment),size=6, stroke =2)+
  geom_smooth(method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2021", size=20)

#CWM of Biomass - 2022 and FK
Biomass_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Biomass_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Biomass_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Biomass_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Biomass_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of Biomass for Fort Keogh 2018 - LMER
FK_Biomass_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), Biomass_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Biomass_2018_LMER, type = 3)
#grazing (p=0.7076), drought (p=0.3050), grazing*drought(p=0.3160)

#CWM of Biomass for Fort Keogh 2019 - LMER
FK_Biomass_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Biomass_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Biomass_2019_LMER, type = 3)
#drought (p=0.6426)

#CWM of Biomass for Fort Keogh 2020 - LMER
FK_Biomass_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), Biomass_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Biomass_2020_LMER, type = 3)
#grazing (p=0.8889), drought (p=0.7126), grazing*drought(p=0.9006)

#CWM of Biomass for Fort Keogh 2021 - LMER
FK_Biomass_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), Biomass_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Biomass_2021_LMER, type = 3)
#grazing (p=0.54448), drought (p=0.02502), grazing*drought(p=0.39830)
#post hoc test for lmer test
summary(glht(FK_Biomass_2021_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))

#CWM of Biomass for Fort Keogh 2022 - LMER
FK_Biomass_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), Biomass_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Biomass_2022_LMER, type = 3)
#grazing (p=0.5073), drought (p=0.5087), grazing*drought(p=0.8485)


## Thunder Basin

#CWM of Biomass - 2019 and TB
Biomass_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.5, label = "TB 2019", size=20)


#CWM of Biomass - 2020 and TB
Biomass_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "TB 2020", size=20)


#CWM of Biomass - 2021 and TB
Biomass_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=OpenFlowers_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "TB 2021", size=20)

#CWM of Biomass - 2022 and TB
Biomass_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "TB 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Biomass_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Biomass_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Biomass_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Biomass_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of Biomass for TB 2018 - LMER
TB_Biomass_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), Biomass_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Biomass_2018_LMER, type = 3)
#grazing (p=0.7076), drought (p=0.3050), grazing*drought(p=0.3160)

#CWM of Biomass for TB 2019 - LMER
TB_Biomass_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Biomass_CWM ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Biomass_2019_LMER, type = 3)
#drought (p=0.1469)

#CWM of Biomass for TB 2020 - LMER
TB_Biomass_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), Biomass_CWM ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Biomass_2020_LMER, type = 3)
#grazing (p=0.8264), drought (p=0.1169), grazing*drought(p=0.9245)

#CWM of Biomass for TB 2021 - LMER
TB_Biomass_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), Biomass_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Biomass_2021_LMER, type = 3)
#grazing (p=0.06516), drought (p=0.02987), grazing*drought(p=0.54141)
#post hoc test for lmer test
summary(glht(TB_Biomass_2021_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#post hoc test for lmer test
summary(glht(TB_Biomass_2021_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))

#CWM of Biomass for TB 2022 - LMER
TB_Biomass_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), Biomass_CWM ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Biomass_2022_LMER, type = 3)
#grazing (p=0.1076), drought (p=0.7132), grazing*drought(p=0.9793)

#### Running % Green with Soil moisture data ####
#instead of categorical drought treatments to see if there is a difference

####CWM - Percent Green Plots and Stats #### 

#CWM of % Green - 2019 and FK
Green_FK_19_SM<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=Avg_SM,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Soil Moisture (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(85,95))+
  expand_limits(x=c(0,25))+
  annotate("text", x=8, y=95, label = "FK 2019", size=20)


#CWM of % Green - 2020 and FK
Green_FK_20_SM<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=Avg_SM,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Soil Moisture (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(85,95))+
  expand_limits(x=c(0,25))+
  annotate("text", x=8, y=95, label = "FK 2020", size=20)


#Create graph of all years for % Green data
pushViewport(viewport(layout=grid.layout(2,1)))
print(Green_FK_19_SM,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Green_FK_20_SM,vp=viewport(layout.pos.row=2, layout.pos.col =1))
#Save at 3000 x 2000  

#CWM of PercentGreen for Fort Keogh 2019 - LMER
FK_PercentGreen_2019_LMER_SM <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), PercentGreen_CWM ~ grazing_treatment*Avg_SM + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2019_LMER_SM, type = 3)
#grazing (p=0.3419), drought (p=0.2850), grazing*drought(p=0.3954)

#CWM of PercentGreen for Fort Keogh 2020 - LMER
FK_PercentGreen_2020_LMER_SM <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), PercentGreen_CWM ~ grazing_treatment*Avg_SM + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2020_LMER_SM, type = 3)
#grazing (p=0.53324), drought (p=0.00586), grazing*drought(p=0.84682)


####CWM - Percent Green Plots and Stats #### 

#CWM of % Green - 2019 and TB
Green_TB_19_SM<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=Avg_SM,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Soil Moisture (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(85,95))+
  expand_limits(x=c(0,25))+
  annotate("text", x=8, y=95, label = "TB 2019", size=20)


#CWM of % Green - 2020 and TB
Green_TB_20_SM<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=Avg_SM,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Soil Moisture (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(85,95))+
  expand_limits(x=c(0,25))+
  annotate("text", x=8, y=95, label = "TB 2020", size=20)

#Create graph of all years for % Green data
pushViewport(viewport(layout=grid.layout(2,1)))
print(Green_TB_19_SM,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Green_TB_20_SM,vp=viewport(layout.pos.row=2, layout.pos.col =1))
#Save at 3000 x 2000  

#CWM of PercentGreen for TB 2019 - LMER
TB_PercentGreen_2019_LMER_SM <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), PercentGreen_CWM ~ grazing_treatment*Avg_SM + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2019_LMER_SM, type = 3)
#grazing (p=0.4994), drought (p=0.2530), grazing*drought(p=0.3929)

#CWM of PercentGreen for TB 2020 - LMER
TB_PercentGreen_2020_LMER_SM <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), PercentGreen_CWM ~ grazing_treatment*Avg_SM + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2020_LMER_SM, type = 3)
#grazing (p=0.5720), drought (p=0.4665), grazing*drought(p=0.9153)

#### Running Emerging Leaves with all years in 1 figure ####

####CWM - Percent Green Plots and Stats #### 
#2022 still needs to be added in 

#CWM of Emerging - Fort Keogh all years
Emerging_FK_ALL<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Emerging Leaves")+
  expand_limits(y=c(0,10))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = c(0.75,0.80))+
  annotate("text", x=20, y=10, label = "Fort Keogh", size=20)

#CWM of Emerging - Thunder Basin all years
Emerging_TB_ALL<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Emerging Leaves")+
  expand_limits(y=c(0,10))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=20, y=10, label = "Thunder Basin", size=20)

#Create graph of all years for Emerging Data
pushViewport(viewport(layout=grid.layout(1,2)))
print(Emerging_FK_ALL,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Emerging_TB_ALL,vp=viewport(layout.pos.row=1, layout.pos.col =2))
#Save at 2500 x 1500  


#### PCAs ####

#Create seperate dataframes for each site and year for PCAs
CWM_Collected_Data_FK_19<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2019)

CWM_Collected_Data_FK_20<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2020)

CWM_Collected_Data_FK_21<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2021)

CWM_Collected_Data_FK_22<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2022)

CWM_Collected_Data_TB_19<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2019) %>% 
  na.omit(Biomass_CWM)

CWM_Collected_Data_TB_20<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2020) %>% 
  na.omit(Biomass_CWM) %>% 
  na.omit(LDMC_CWM)

CWM_Collected_Data_TB_21<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2021) %>% 
  filter(!is.na(Biomass_CWM))

CWM_Collected_Data_TB_22<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2022)

#### PCA for FK 2019 ####
PCA_FK_19<-prcomp(CWM_Collected_Data_FK_19[,11:21],scale=TRUE)
PCA_FK_19
summary(PCA_FK_19)

axes_FK_19 <- predict(PCA_FK_19, newdata = CWM_Collected_Data_FK_19)
head(axes_FK_19, 4)

#put PCA axes with site and plot #   
PCA_FK_19_meta<-cbind(CWM_Collected_Data_FK_19,axes_FK_19)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_19 <- get_pca_var(PCA_FK_19)
var_FK_19
head(var_FK_19$contrib, 13)

#### PCA for FK 2020 ####
PCA_FK_20<-prcomp(CWM_Collected_Data_FK_20[,11:21],scale=TRUE)
PCA_FK_20
summary(PCA_FK_20)

axes_FK_20 <- predict(PCA_FK_20, newdata = CWM_Collected_Data_FK_20)
head(axes_FK_20, 4)

#put PCA axes with site and plot #   
PCA_FK_20_meta<-cbind(CWM_Collected_Data_FK_20,axes_FK_20)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_20 <- get_pca_var(PCA_FK_20)
var_FK_20
head(var_FK_20$contrib, 12)

#### PCA for FK 2021 ####
PCA_FK_21<-prcomp(CWM_Collected_Data_FK_21[,11:21],scale=TRUE)
PCA_FK_21
summary(PCA_FK_21)

axes_FK_21 <- predict(PCA_FK_21, newdata = CWM_Collected_Data_FK_21)
head(axes_FK_21, 4)

#put PCA axes with site and plot #   
PCA_FK_21_meta<-cbind(CWM_Collected_Data_FK_21,axes_FK_21)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_21 <- get_pca_var(PCA_FK_21)
var_FK_21
head(var_FK_21$contrib, 12)

#### PCA for FK 2022 ####
PCA_FK_22<-prcomp(CWM_Collected_Data_FK_22[,11:21],scale=TRUE)
PCA_FK_22
summary(PCA_FK_22)

axes_FK_22 <- predict(PCA_FK_22, newdata = CWM_Collected_Data_FK_22)
head(axes_FK_22, 4)

#put PCA axes with site and plot #   
PCA_FK_22_meta<-cbind(CWM_Collected_Data_FK_22,axes_FK_22)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_22 <- get_pca_var(PCA_FK_22)
var_FK_22
head(var_FK_22$contrib, 12)

#### PCA for TB 2019 ####
PCA_TB_19<-prcomp(CWM_Collected_Data_TB_19[,11:21],scale=TRUE)
PCA_TB_19
summary(PCA_TB_19)

axes_TB_19 <- predict(PCA_TB_19, newdata = CWM_Collected_Data_TB_19)
head(axes_TB_19, 4)

#put PCA axes with site and plot #   
PCA_TB_19_meta<-cbind(CWM_Collected_Data_TB_19,axes_TB_19)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_19 <- get_pca_var(PCA_TB_19)
var_TB_19
head(var_TB_19$contrib, 12)

#### PCA for TB 2020 ####
PCA_TB_20<-prcomp(CWM_Collected_Data_TB_20[,11:21],scale=TRUE)
PCA_TB_20
summary(PCA_TB_20)

axes_TB_20 <- predict(PCA_TB_20, newdata = CWM_Collected_Data_TB_20)
head(axes_TB_20, 4)

#put PCA axes with site and plot #   
PCA_TB_20_meta<-cbind(CWM_Collected_Data_TB_20,axes_TB_20)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_20 <- get_pca_var(PCA_TB_20)
var_TB_20
head(var_TB_20$contrib, 12)

#### PCA for TB 2021 ####
PCA_TB_21<-prcomp(CWM_Collected_Data_TB_21[,11:21],scale=TRUE)
PCA_TB_21
summary(PCA_TB_21)

axes_TB_21 <- predict(PCA_TB_21, newdata = CWM_Collected_Data_TB_21)
head(axes_TB_21, 4)

#put PCA axes with site and plot #   
PCA_TB_21_meta<-cbind(CWM_Collected_Data_TB_21,axes_TB_21)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_21 <- get_pca_var(PCA_TB_21)
var_TB_21
head(var_TB_21$contrib, 11)

#### PCA for TB 2022 ####
PCA_TB_22<-prcomp(CWM_Collected_Data_TB_22[,11:21],scale=TRUE)
PCA_TB_22
summary(PCA_TB_22)

axes_TB_22 <- predict(PCA_TB_22, newdata = CWM_Collected_Data_TB_22)
head(axes_TB_22, 4)

#put PCA axes with site and plot #   
PCA_TB_22_meta<-cbind(CWM_Collected_Data_TB_22,axes_TB_22)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_22 <- get_pca_var(PCA_TB_22)
var_TB_22
head(var_TB_22$contrib, 12)


#### PCA Graphs #### 

#FK
PCA_FK_19_G<-autoplot(PCA_FK_19, data=CWM_Collected_Data_FK_19, scale=0, colour="Trtm", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="Trtm")
#save as 1500x1000

PCA_FK_20<-autoplot(PCA_FK_20, data=CWM_Collected_Data_FK_20, scale=0, colour="Trtm", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="Trtm")

PCA_FK_21<-autoplot(PCA_FK_21, data=CWM_Collected_Data_FK_21, scale=0, colour="Trtm", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="Trtm")

PCA_FK_22<-autoplot(PCA_FK_22, data=CWM_Collected_Data_FK_22, scale=0, colour="Trtm", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="Trtm")

#TB
PCA_TB_19<-autoplot(PCA_TB_19, data=CWM_Collected_Data_TB_19, scale=0, colour="Trtm", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="Trtm")

PCA_TB_20<-autoplot(PCA_TB_20, data=CWM_Collected_Data_TB_20, scale=0, colour="Trtm", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="Trtm")

PCA_TB_21<-autoplot(PCA_TB_21, data=CWM_Collected_Data_TB_21, scale=0, colour="Trtm", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="Trtm")

PCA_TB_22<-autoplot(PCA_TB_22, data=CWM_Collected_Data_TB_22, scale=0, colour="Trtm", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="Trtm")

#Create graph of all years for PCAs
pushViewport(viewport(layout=grid.layout(4,4)))
print(PCA_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(PCA_FK_20,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(PCA_FK_21,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(PCA_FK_22,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(PCA_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(PCA_TB_20,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(PCA_TB_21,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(PCA_TB_22,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#Save at 2500 x 1500  

#### PCA Stats ####

#make two seperate dataframes for each year and site to have treatment data and trait data seperate

## FK ##
#2019
CWM_FK_19_Trait<-CWM_Collected_Data_FK_19 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Avg_SM,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)

CWM_FK_19_Treatment<-CWM_Collected_Data_FK_19 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Avg_SM,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2020
CWM_FK_20_Trait<-CWM_Collected_Data_FK_20 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Avg_SM,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)

CWM_FK_20_Treatment<-CWM_Collected_Data_FK_20 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Avg_SM,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2021
CWM_FK_21_Trait<-CWM_Collected_Data_FK_21 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Avg_SM,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)

CWM_FK_21_Treatment<-CWM_Collected_Data_FK_21 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Avg_SM,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2022
CWM_FK_22_Trait<-CWM_Collected_Data_FK_22 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Avg_SM,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)

CWM_FK_22_Treatment<-CWM_Collected_Data_FK_22 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Avg_SM,Rainfall_reduction_cat,Trtm,Grazing_2020)

## TB ##
#2019
CWM_TB_19_Trait<-CWM_Collected_Data_TB_19 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Avg_SM,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)

CWM_TB_19_Treatment<-CWM_Collected_Data_TB_19 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Avg_SM,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2020
CWM_TB_20_Trait<-CWM_Collected_Data_TB_20 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Avg_SM,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)

CWM_TB_20_Treatment<-CWM_Collected_Data_TB_20 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Avg_SM,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2021
CWM_TB_21_Trait<-CWM_Collected_Data_TB_21 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Avg_SM,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)

CWM_TB_21_Treatment<-CWM_Collected_Data_TB_21 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Avg_SM,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2022
CWM_TB_22_Trait<-CWM_Collected_Data_TB_22 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Avg_SM,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)

CWM_TB_22_Treatment<-CWM_Collected_Data_TB_22 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Avg_SM,Rainfall_reduction_cat,Trtm,Grazing_2020)


# run PERMANOVA using adonis using trait dataframe as data to run adonis on and treatment dataframe as variables

## FK ##
#FK 2019
PERMANOVA_FK_19 <-adonis2(CWM_FK_19_Trait~Rainfall_reduction_cat + (1|block/paddock), data = CWM_FK_19_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_19) 
# drought (p=0.953)

#FK 2020
PERMANOVA_FK_20 <-adonis2(CWM_FK_20_Trait~Rainfall_reduction_cat*Grazing_2020 + (1|block/paddock), data = CWM_FK_20_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_20)
#drought (p=0.3876), grazing (p=0.7842), DxG (0.9930)

#FK 2021
PERMANOVA_FK_21 <-adonis2(CWM_FK_21_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/paddock), data = CWM_FK_21_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_21)
#drought (p=0.01199), grazing (p=0.79520), DxG (0.74625)

#FK 2022
PERMANOVA_FK_22 <-adonis2(CWM_FK_22_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/paddock), data = CWM_FK_22_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_22)
#drought (p=0.3477), grazing (p=0.4396), DxG (0.7223)

##TB##

#TB 2019
PERMANOVA_TB_19 <-adonis2(CWM_TB_19_Trait~Rainfall_reduction_cat + (1|block/paddock), data = CWM_TB_19_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_19) 
# drought (p=0.8521)

#TB 2020
PERMANOVA_TB_20 <-adonis2(CWM_TB_20_Trait~Rainfall_reduction_cat*Grazing_2020 + (1|block/paddock), data = CWM_TB_20_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_20)
#drought (p=0.8062), grazing (p=0.1449), DxG (0.6913)

#TB 2021
PERMANOVA_TB_21 <-adonis2(CWM_TB_21_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/paddock), data = CWM_TB_21_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_21)
#drought (p=0.78222), grazing (p=0.09191), DxG (0.98901)

#TB 2022
PERMANOVA_TB_22 <-adonis2(CWM_TB_22_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/paddock), data = CWM_TB_22_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_22)
#drought (p=0.23673), grazing (p=0.008991), DxG (0.847153)

## PERMDISP ##

# FK 2019
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_19 <- vegdist(CWM_FK_19_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_19_drought <- betadisper(BC_Distance_Matrix_FK_19,CWM_FK_19_Treatment$Rainfall_reduction_cat)
anova(Dispersion_FK_19_drought) #p=0.6567


# FK 2020
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_20 <- vegdist(CWM_FK_20_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_20_drought <- betadisper(BC_Distance_Matrix_FK_20,CWM_FK_20_Treatment$Rainfall_reduction_cat)
anova(Dispersion_FK_20_drought) #p=0.8268

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_FK_20_graze <- betadisper(BC_Distance_Matrix_FK_20,CWM_FK_20_Treatment$Grazing_2020)
anova(Dispersion_FK_20_graze) #p=0.842

#combine 2020 grazing+drought
CWM_FK_20_Treatment<-CWM_FK_20_Treatment %>% 
  mutate(Trtm_20=paste(Rainfall_reduction_cat,Grazing_2020,sep="."))

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_FK_20_DxG <- betadisper(BC_Distance_Matrix_FK_20,CWM_FK_20_Treatment$Trtm_20)
anova(Dispersion_FK_20_DxG) #p=0.984


# FK 2021
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_21 <- vegdist(CWM_FK_21_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_21_drought <- betadisper(BC_Distance_Matrix_FK_21,CWM_FK_21_Treatment$Rainfall_reduction_cat)
anova(Dispersion_FK_21_drought) #p=0.2659

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_FK_21_graze <- betadisper(BC_Distance_Matrix_FK_21,CWM_FK_21_Treatment$grazing_treatment)
anova(Dispersion_FK_21_graze) #p=0.6441

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_FK_21_DxG <- betadisper(BC_Distance_Matrix_FK_21,CWM_FK_21_Treatment$Trtm)
anova(Dispersion_FK_21_DxG) #p=0.9959


# FK 2022
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_22 <- vegdist(CWM_FK_22_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_22_drought <- betadisper(BC_Distance_Matrix_FK_22,CWM_FK_22_Treatment$Rainfall_reduction_cat)
anova(Dispersion_FK_22_drought) #p=0.1634

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_FK_22_graze <- betadisper(BC_Distance_Matrix_FK_22,CWM_FK_22_Treatment$grazing_treatment)
anova(Dispersion_FK_22_graze) #p=0.4731

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_FK_22_DxG <- betadisper(BC_Distance_Matrix_FK_22,CWM_FK_22_Treatment$Trtm)
anova(Dispersion_FK_22_DxG) #p=0.9886

# TB 2019
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_19 <- vegdist(CWM_TB_19_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_19_drought <- betadisper(BC_Distance_Matrix_TB_19,CWM_TB_19_Treatment$Rainfall_reduction_cat)
anova(Dispersion_TB_19_drought) #p=0.3649

# TB 2020
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_20 <- vegdist(CWM_TB_20_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_20_drought <- betadisper(BC_Distance_Matrix_TB_20,CWM_TB_20_Treatment$Rainfall_reduction_cat)
anova(Dispersion_TB_20_drought) #p=0.5175

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_TB_20_graze <- betadisper(BC_Distance_Matrix_TB_20,CWM_TB_20_Treatment$Grazing_2020)
anova(Dispersion_TB_20_graze) #p=0.3177

#combine 2020 grazing+drought
CWM_TB_20_Treatment<-CWM_TB_20_Treatment %>% 
  mutate(Trtm_20=paste(Rainfall_reduction_cat,Grazing_2020,sep="."))

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_TB_20_DxG <- betadisper(BC_Distance_Matrix_TB_20,CWM_TB_20_Treatment$Trtm_20)
anova(Dispersion_TB_20_DxG) #p=0.1283

# TB 2021
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_21 <- vegdist(CWM_TB_21_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_21_drought <- betadisper(BC_Distance_Matrix_TB_21,CWM_TB_21_Treatment$Rainfall_reduction_cat)
anova(Dispersion_TB_21_drought) #p=0.8262

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_TB_21_graze <- betadisper(BC_Distance_Matrix_TB_21,CWM_TB_21_Treatment$grazing_treatment)
anova(Dispersion_TB_21_graze) #p=0.3113

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_TB_21_DxG <- betadisper(BC_Distance_Matrix_TB_21,CWM_TB_21_Treatment$Trtm)
anova(Dispersion_TB_21_DxG) #p=0.5866


# TB 2022
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_22 <- vegdist(CWM_TB_22_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_22_drought <- betadisper(BC_Distance_Matrix_TB_22,CWM_TB_22_Treatment$Rainfall_reduction_cat)
anova(Dispersion_TB_22_drought) #p=0.6337

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_TB_22_graze <- betadisper(BC_Distance_Matrix_TB_22,CWM_TB_22_Treatment$grazing_treatment)
anova(Dispersion_TB_22_graze) #p=0.3852

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_TB_22_DxG <- betadisper(BC_Distance_Matrix_TB_22,CWM_TB_22_Treatment$Trtm)
anova(Dispersion_TB_22_DxG) #p=0.646


#### Functional Diversity Calculations ####

###FK
  
##create dataframe from the raw trait data where wesubset FK data and then average across blocks, paddocks. then add species numbers 1-33 to assign to each species for future identification and analysis 
Avg_Traits_FK<-Traits_Clean_2 %>%
  filter(Site=="FK") %>% 
  group_by(Site,Genus_Species_Correct) %>% 
  summarise(
    Avg_height_cm=mean(height_cm,na.rm=T),
    Avg_biomass_g=mean(Plant_Biomass,na.rm=T),
    Avg_percent_green=mean(percent_green,na.rm=T),
    Avg_emerging_leaves=mean(emerging_leaves,na.rm=T),
    Avg_developed_leaves=mean(developed_leaves,na.rm=T),
    Avg_scenesced_leaves=mean(scenesced_leaves,na.rm=T),
    Avg_flower_heads=mean(flower_heads,na.rm=T),
    Avg_open_flowers=mean(open_flowers,na.rm=T),
    Avg_leaf_thickness=mean(leaf_thickness_.mm.,na.rm=T),
    Avg_flower_num=mean(total_flower_num,na.rm=T), 
    Avg_LDMC=mean(LDMC,na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(Sp_Num=c(1:33))

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data<-Avg_Traits_FK %>% 
  select(-Genus_Species_Correct,-Sp_Num,-Site) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data) <- c(1:33)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames<-Avg_Traits_FK %>% 
  select(Genus_Species_Correct,Sp_Num)


#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK19_Wide<-Species_Comp_FK %>% 
  select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK19_Wide_Data<-Species_Comp_FK19_Wide %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK19_Wide_PlotData<-Species_Comp_FK19_Wide %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 


#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity <- dbFD(Avg_Traits_FK_Data, Species_Comp_FK19_Wide_Data,corr = "none")

#create dataframe from the raw trait data where wesubset FK data and then average across blocks, paddocks. then add species numbers 1-33 to assign to each species for future identification and analysis 
Avg_Traits_FK<-Traits_Clean_2 %>%
  filter(Site=="FK") %>% 
  group_by(Site,Genus_Species_Correct) %>% 
  summarise(
    Avg_height_cm=mean(height_cm,na.rm=T),
    Avg_biomass_g=mean(Plant_Biomass,na.rm=T),
    Avg_percent_green=mean(percent_green,na.rm=T),
    Avg_emerging_leaves=mean(emerging_leaves,na.rm=T),
    Avg_developed_leaves=mean(developed_leaves,na.rm=T),
    Avg_scenesced_leaves=mean(scenesced_leaves,na.rm=T),
    Avg_flower_heads=mean(flower_heads,na.rm=T),
    Avg_open_flowers=mean(open_flowers,na.rm=T),
    Avg_leaf_thickness=mean(leaf_thickness_.mm.,na.rm=T),
    Avg_flower_num=mean(total_flower_num,na.rm=T), 
    Avg_LDMC=mean(LDMC,na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(Sp_Num=c(1:33))

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data<-Avg_Traits_FK %>% 
  select(-Genus_Species_Correct,-Sp_Num,-Site) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data) <- c(1:33)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames<-Avg_Traits_FK %>% 
  select(Genus_Species_Correct,Sp_Num)


#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide<-Species_Comp_FK %>% 
  select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data<-Species_Comp_FK_Wide %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData<-Species_Comp_FK_Wide %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 


#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity <- dbFD(Avg_Traits_FK_Data, Species_Comp_FK_Wide_Data,corr = "none")
FK_FunctionalDiversity

### TB
##create dataframe from the raw trait data where wesubset TB data and then average across blocks, paddocks. then add species numbers 1-33 to assign to each species for future identification and analysis 
Avg_Traits_TB<-Traits_Clean_2 %>%
  filter(Site=="TB") %>% 
  group_by(Site,Genus_Species_Correct) %>% 
  summarise(
    Avg_height_cm=mean(height_cm,na.rm=T),
    Avg_biomass_g=mean(Plant_Biomass,na.rm=T),
    Avg_percent_green=mean(percent_green,na.rm=T),
    Avg_emerging_leaves=mean(emerging_leaves,na.rm=T),
    Avg_developed_leaves=mean(developed_leaves,na.rm=T),
    Avg_scenesced_leaves=mean(scenesced_leaves,na.rm=T),
    Avg_flower_heads=mean(flower_heads,na.rm=T),
    Avg_open_flowers=mean(open_flowers,na.rm=T),
    Avg_leaf_thickness=mean(leaf_thickness_.mm.,na.rm=T),
    Avg_flower_num=mean(total_flower_num,na.rm=T), 
    Avg_LDMC=mean(LDMC,na.rm=T)
  ) %>% 
  ungroup() %>% 
  filter(!Genus_Species_Correct %in% c("Oenothera.suffrtescuns")) %>% 
  mutate(Sp_Num=c(1:43))

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data<-Avg_Traits_TB %>% 
  select(-Genus_Species_Correct,-Sp_Num,-Site) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data) <- c(1:43)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames<-Avg_Traits_TB %>% 
  select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide<-Species_Comp_TB %>% 
  select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)
  
#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data<-Species_Comp_TB_Wide %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData<-Species_Comp_TB_Wide %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity <- dbFD(Avg_Traits_TB_Data, Species_Comp_TB_Wide_Data,corr = "none")
TB_FunctionalDiversity

### Functional Diversity Stats #### 

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK<-as.data.frame(FK_FunctionalDiversity) %>% 
  cbind(Species_Comp_FK_Wide_PlotData)

Functional_Diversity_TB<-as.data.frame(TB_FunctionalDiversity) %>% 
  cbind(Species_Comp_TB_Wide_PlotData)

Functional_Diversity<-Functional_Diversity_FK %>% 
  rbind(Functional_Diversity_TB) %>% 
  separate(ID,c("year","Site","plot"), sep = "_") %>% 
  select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))
  
###Functional Richness

#Functional Richness (FRic) FK 18
FK_18_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FRiC_LMER, type = 3)
#grazing (p=0.9756), drought (p=0.9787), grazing*drought(p=0.9789)

#Functional Richness (FRic) FK 19
FK_19_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="FK"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FRiC_LMER, type = 3)
#drought (p=0.9843)

#Functional Richness (FRic) FK 20
FK_20_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="FK"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FRiC_LMER, type = 3)
#grazing (p=0.9708), drought (p=0.9759), grazing*drought(p=0.9819)

#Functional Richness (FRic) FK 21
FK_21_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FRiC_LMER, type = 3)
#grazing (p=0.9957), drought (p=0.9950), grazing*drought(p=0.9957)

#Functional Richness (FRic) FK 22
FK_22_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FRiC_LMER, type = 3)
#grazing (p=0.9839), drought (p=0.9855), grazing*drought(p=0.9851)

#Functional Richness (FRic) TB 18
TB_18_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FRiC_LMER, type = 3)
#grazing (p=0.10127), drought (p=0.05941), grazing*drought(p=0.60270)
#post hoc test for lmer test
summary(glht(TB_18_FRiC_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#not significant

#Functional Richness (FRic) TB 19
TB_19_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="TB"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FRiC_LMER, type = 3)
#drought (p=0.8989)

#Functional Richness (FRic) TB 20
TB_20_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="TB"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FRiC_LMER, type = 3)
#grazing (p=0.5238), drought (p=0.9734), grazing*drought(p=0.5234)

#Functional Richness (FRic) TB 21
TB_21_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FRiC_LMER, type = 3)
#grazing (p=0.1131), drought (p=0.9064), grazing*drought(p=0.6431)

#Functional Richness (FRic) TB 22
TB_22_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FRiC_LMER, type = 3)
#grazing (p=0.3020), drought (p=0.4240), grazing*drought(p=0.9272)

###Functional Evenness

#Functional Evenness (FEve) FK 18
FK_18_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FEve_LMER, type = 3)
#grazing (p=0.8462), drought (p=0.2256), grazing*drought(p=0.2256)

#Functional Evenness (FEve) FK 19
FK_19_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="FK"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FEve_LMER, type = 3)
#drought (p=0.03506)
#post hoc test for lmer test
summary(glht(FK_19_FEve_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#75-20 (0.0202),75-50 (0.0749)


#Functional Evenness (FEve) FK 20
FK_20_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="FK"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FEve_LMER, type = 3)
#grazing (p=0.7899), drought (p=0.2131), grazing*drought(p=0.9864)

#Functional Evenness (FEve) FK 21
FK_21_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FEve_LMER, type = 3)
#grazing (p=0.8930849), drought (p=0.0001245), grazing*drought(p=0.3828739)
#post hoc test for lmer test
summary(glht(FK_21_FEve_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance

#Functional Evenness (FEve) FK 22
FK_22_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FEve_LMER, type = 3)
#grazing (p=0.8779), drought (p=0.1469), grazing*drought(p=0.4673)

#Functional Evenness (FEve) TB 18
TB_18_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FEve_LMER, type = 3)
#grazing (p=0.7977), drought (p=0.8548), grazing*drought(p=0.6541)

#Functional Evenness (FEve) TB 19
TB_19_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="TB"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FEve_LMER, type = 3)
#drought (p=0.0622)
#post hoc test for lmer test
summary(glht(TB_19_FEve_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#50-0 (0.0604), 99-50 (0.0529)

#Functional Evenness (FEve) TB 20
TB_20_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="TB"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FEve_LMER, type = 3)
#grazing (p=0.88482), drought (p=0.03235), grazing*drought(p=0.87809)
#post hoc test for lmer test
summary(glht(TB_20_FEve_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance

#Functional Evenness (FEve) TB 21
TB_21_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FEve_LMER, type = 3)
#grazing (p=0.83595), drought (p=0.04388), grazing*drought(p=0.71326)
#post hoc test for lmer test
summary(glht(TB_21_FEve_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance

#Functional Evenness (FEve) TB 22
TB_22_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FEve_LMER, type = 3)
#grazing (p=0.97868), drought (p=0.05684), grazing*drought(p=0.34553)
#post hoc test for lmer test
summary(glht(TB_22_FEve_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance

###Functional Diversity

#Functional Diversity (FDiv) FK 18
FK_18_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="FK"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDiv_LMER, type = 3)
#grazing (p=0.86847), drought (p=0.01476), grazing*drought(p=0.84613)
#post hoc test for lmer test
summary(glht(FK_18_FDiv_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance

#Functional Diversity (FDiv) FK 19
FK_19_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="FK"), FDiv ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDiv_LMER, type = 3)
#drought (p=0.007593)
#post hoc test for lmer test
summary(glht(FK_19_FDiv_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#75-0 (0.01784), 75-50(0.00308)

#Functional Diversity (FDiv) FK 20
FK_20_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="FK"), FDiv ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDiv_LMER, type = 3)
#grazing (p=0.9108), drought (p=0.1948), grazing*drought(p=0.6843)

#Functional Diversity (FDiv) FK 21
FK_21_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="FK"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDiv_LMER, type = 3)
#grazing (p=0.623397), drought (p=0.000835), grazing*drought(p=0.783319)
#post hoc test for lmer test
summary(glht(FK_21_FDiv_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance

#Functional Diversity (FDiv) FK 22
FK_22_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="FK"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDiv_LMER, type = 3)
#grazing (p=0.930356), drought (p=0.001071), grazing*drought(p=0.963570)
#post hoc test for lmer test
summary(glht(FK_22_FDiv_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#99-0 (0.0987), 99-25(0.0987),99-75(0.0987)

#Functional Diversity (FDiv) TB 18
TB_18_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="TB"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDiv_LMER, type = 3)
#grazing (p=0.1901), drought (p=0.2330), grazing*drought(p=0.6831)

#Functional Diversity (FDiv) TB 19
TB_19_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="TB"), FDiv ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDiv_LMER, type = 3)
#drought (p=0.02707)
#post hoc test for lmer test
summary(glht(TB_19_FDiv_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#50-0 (0.0110), 50-25 (0.0464), 75-50 (0.0782), 99-50 (0.0474)

#Functional Diversity (FDiv) TB 20
TB_20_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="TB"), FDiv ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDiv_LMER, type = 3)
#grazing (p=0.9142), drought (p=0.5069), grazing*drought(p=0.5791)

#Functional Diversity (FDiv) TB 21
TB_21_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="TB"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDiv_LMER, type = 3)
#grazing (p=0.6438), drought (p=0.2292), grazing*drought(p=0.1494)

#Functional Diversity (FDiv) TB 22
TB_22_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="TB"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDiv_LMER, type = 3)
#grazing (p=0.5061), drought (p=0.7065), grazing*drought(p=0.8979)

###Functional Dispersion

#Functional Dispersion (FDis) FK 18
FK_18_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDis_LMER, type = 3)
#grazing (p=0.12777), drought (p=0.01055), grazing*drought(p=0.23658)
#post hoc test for lmer test
summary(glht(FK_18_FDis_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER, type = 3)
#drought (p=0.0112)
#post hoc test for lmer test
summary(glht(FK_19_FDis_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#75-0 (0.0164), 99-0(0.0814), 75-50 (0.0301)


#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER, type = 3)
#grazing (p=0.1685), drought (p=0.3464), grazing*drought(p=0.9718)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER, type = 3)
#grazing (p=0.06973), drought (p=0.01641), grazing*drought(p=0.49868)
#post hoc test for lmer test
summary(glht(FK_21_FDis_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#75-0 (0.223)
#post hoc test for lmer test
summary(glht(FK_21_FDis_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))
#not significant

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER, type = 3)
#grazing (p=0.07433), drought (p=0.02983), grazing*drought(p=0.43246)
#post hoc test for lmer test
summary(glht(FK_22_FDis_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))
#no significance
#post hoc test for lmer test
summary(glht(FK_22_FDis_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))
#no significance

#Functional Dispersion (FDis) TB 18
TB_18_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDis_LMER, type = 3)
#grazing (p=0.01015), drought (p=0.64001), grazing*drought(p=0.29925)
#post hoc test for lmer test
summary(glht(TB_18_FDis_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))
#no significance

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER, type = 3)
#drought (p=0.4418)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER, type = 3)
#grazing (p=0.9417), drought (p=0.4565), grazing*drought(p=0.3277)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER, type = 3)
#grazing (p=0.1721), drought (p=0.7788), grazing*drought(p=0.2305)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER, type = 3)
#grazing (p=0.004292), drought (p=0.481674), grazing*drought(p=0.354431)
#post hoc test for lmer test
summary(glht(TB_22_FDis_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))
#no significance


#### Updated Figures ####

####CWM - Percent Green Plots and Stats #### 

#CWM of % Green - 2019 and FK
Green_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=100, label = "FK 2019", size=20)


#CWM of % Green - 2020 and FK
Green_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=PercentGreen_CWM)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=100, label = "FK 2020", size=20)


#CWM of % Green - 2021 and FK
Green_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=PercentGreen_CWM)) +
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","solid","solid"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=100, label = "FK 2021", size=20)

#CWM of % Green - 2022 and FK
Green_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","solid","solid"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=100, label = "FK 2022", size=20)

#Create graph of all years for % Green data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Green_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Green_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Green_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Green_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of PercentGreen for Fort Keogh 2018 - LMER
FK_PercentGreen_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2018_LMER, type = 3)
#post hoc test for lmer test
summary(glht(FK_PercentGreen_2018_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) #not significant

#CWM of PercentGreen for Fort Keogh 2019 - LMER
FK_PercentGreen_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), PercentGreen_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2019_LMER, type = 3)

#CWM of PercentGreen for Fort Keogh 2020 - LMER
FK_PercentGreen_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), PercentGreen_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2020_LMER, type = 3)
#### post hoc test needed ####

#CWM of PercentGreen for Fort Keogh 2021 - LMER
FK_PercentGreen_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2021_LMER, type = 3)
#### post hoc test needed ####

#CWM of PercentGreen for Fort Keogh 2022 - LMER
FK_PercentGreen_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2022_LMER, type = 3)

##Thunder Basin

#CWM of % Green - 2019 and TB
Green_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=100, label = "TB 2019", size=20)


#CWM of % Green - 2020 and TB
Green_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=100, label = "TB 2020", size=20)


#CWM of % Green - 2021 and TB
Green_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=100, label = "TB 2021", size=20)

#CWM of % Green - 2022 and TB
Green_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM % Green (cm)")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=100, label = "TB 2022", size=20)

#Create graph of all years for % Green data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Green_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Green_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Green_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Green_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of PercentGreen for Thunder Basin 2018 - LMER
TB_PercentGreen_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2018_LMER, type = 3)

#CWM of PercentGreen for Thunder Basin 2019 - LMER
TB_PercentGreen_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), PercentGreen_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2019_LMER, type = 3)

#CWM of PercentGreen for Thunder Basin 2020 - LMER
TB_PercentGreen_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), PercentGreen_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2020_LMER, type = 3)
#### post hoc test needed ####

#CWM of PercentGreen for Thunder Basin 2021 - LMER
TB_PercentGreen_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2021_LMER, type = 3)

#CWM of PercentGreen for Thunder Basin 2022 - LMER
TB_PercentGreen_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2022_LMER, type = 3) 
#### post hoc test needed ####

####CWM - Biomass Plots and Stats #### 

#CWM of Biomass - 2019 and FK
Biomass_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.5, label = "FK 2019", size=20)


#CWM of Biomass - 2020 and FK
Biomass_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2020", size=20)


#CWM of Biomass - 2021 and FK
Biomass_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=OpenFlowers_CWM)) +  
  geom_point(aes(shape=grazing_treatment,color=grazing_treatment),size=6, stroke =2)+
  geom_smooth(method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2021", size=20)

#CWM of Biomass - 2022 and FK
Biomass_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Biomass_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Biomass_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Biomass_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Biomass_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of Biomass for Fort Keogh 2018 - LMER
FK_Biomass_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), Biomass_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Biomass_2018_LMER, type = 3)

#CWM of Biomass for Fort Keogh 2019 - LMER
FK_Biomass_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Biomass_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Biomass_2019_LMER, type = 3)

#CWM of Biomass for Fort Keogh 2020 - LMER
FK_Biomass_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), Biomass_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Biomass_2020_LMER, type = 3)

#CWM of Biomass for Fort Keogh 2021 - LMER
FK_Biomass_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), Biomass_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Biomass_2021_LMER, type = 3)
#### post hoc test needed ####

#CWM of Biomass for Fort Keogh 2022 - LMER
FK_Biomass_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), Biomass_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Biomass_2022_LMER, type = 3)

## Thunder Basin

#CWM of Biomass - 2019 and TB
Biomass_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.5, label = "TB 2019", size=20)


#CWM of Biomass - 2020 and TB
Biomass_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "TB 2020", size=20)


#CWM of Biomass - 2021 and TB
Biomass_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=OpenFlowers_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "TB 2021", size=20)

#CWM of Biomass - 2022 and TB
Biomass_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Biomass (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "TB 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Biomass_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Biomass_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Biomass_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Biomass_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of Biomass for TB 2018 - LMER
TB_Biomass_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), Biomass_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Biomass_2018_LMER, type = 3)

#CWM of Biomass for TB 2019 - LMER
TB_Biomass_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Biomass_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Biomass_2019_LMER, type = 3)

#CWM of Biomass for TB 2020 - LMER
TB_Biomass_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), Biomass_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Biomass_2020_LMER, type = 3)

#CWM of Biomass for TB 2021 - LMER
TB_Biomass_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), Biomass_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Biomass_2021_LMER, type = 3)

#CWM of Biomass for TB 2022 - LMER
TB_Biomass_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), Biomass_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Biomass_2022_LMER, type = 3)

####CWM - Height Plots and Stats #### 

#CWM of height - 2019 and FK
Height_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=30, label = "FK 2019", size=20)

#CWM of height - 2020 and FK
Height_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=30, label = "FK 2020", size=20)


#CWM of height - 2021 and FK
Height_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=30, label = "FK 2021", size=20)

#CWM of height - 2022 and FK
Height_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=Height_CWM)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=30, label = "FK 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Height_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Height_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Height_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Height_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of height for Fort Keogh 2018 - LMER
FK_Height_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Height_2018_LMER, type = 3)

#CWM of height for Fort Keogh 2019 - LMER
FK_Height_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Height_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Height_2019_LMER, type = 3)

#CWM of height for Fort Keogh 2020 - LMER
FK_Height_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), Height_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Height_2020_LMER, type = 3)

#CWM of height for Fort Keogh 2021 - LMER
FK_Height_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Height_2021_LMER, type = 3)
#### post hoc test needed ####

#CWM of height for Fort Keogh 2022 - LMER
FK_Height_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Height_2022_LMER, type = 3)
#### post hoc test needed ####

##Thunder Basin

#CWM of height - 2019 and TB
Height_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=30, label = "TB 2019", size=20)


#CWM of height - 2020 and TB
Height_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=30, label = "TB 2020", size=20)


#CWM of height - 2021 and TB
Height_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=30, label = "TB 2021", size=20)

#CWM of height - 2022 and TB
Height_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Height (cm)")+
  expand_limits(y=25)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=30, label = "TB 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Height_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Height_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Height_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Height_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of height for Thunder Basin 2018 - LMER
TB_Height_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Height_2018_LMER, type = 3)

#CWM of height for Thunder Basin 2019 - LMER
TB_Height_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Height_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Height_2019_LMER, type = 3)

#CWM of height for Thunder Basin 2020 - LMER
TB_Height_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), Height_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Height_2020_LMER, type = 3)

#CWM of height for Thunder Basin 2021 - LMER
TB_Height_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Height_2021_LMER, type = 3)

#CWM of height for Thunder Basin 2022 - LMER
TB_Height_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Height_2022_LMER, type = 3)
#post hoc test for lmer test on rainfall reduction
summary(glht(TB_Height_2022_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) # no significance

####CWM - LDMC Plots and Stats #### 

#CWM of LDMC - 2019 and FK
LDMC_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.5, label = "FK 2019", size=20)


#CWM of LDMC - 2020 and FK
LDMC_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2020", size=20)


#CWM of LDMC - 2021 and FK
LDMC_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2021", size=20)

#CWM of LDMC - 2022 and FK
LDMC_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=0.5, label = "FK 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(LDMC_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(LDMC_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LDMC_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of LDMC for Fort Keogh 2018 - LMER
FK_LDMC_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LDMC_2018_LMER, type = 3)

#CWM of LDMC for Fort Keogh 2019 - LMER
FK_LDMC_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), LDMC_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LDMC_2019_LMER, type = 3)

#CWM of LDMC for Fort Keogh 2020 - LMER
FK_LDMC_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), LDMC_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LDMC_2020_LMER, type = 3)

#CWM of LDMC for Fort Keogh 2021 - LMER
FK_LDMC_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LDMC_2021_LMER, type = 3)

#CWM of LDMC for Fort Keogh 2022 - LMER
FK_LDMC_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LDMC_2022_LMER, type = 3)

##Thunder Basin 
#CWM of LDMC - 2019 and TB
LDMC_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=1.5, label = "TB 2019", size=20)


#CWM of LDMC - 2020 and TB
LDMC_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "NONE")+
  annotate("text", x=8, y=1.5, label = "TB 2020", size=20)


#CWM of LDMC - 2021 and TB
LDMC_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=1.5, label = "TB 2021", size=20)

#CWM of LDMC - 2022 and TB
LDMC_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=LDMC_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM LDMC (g)")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=1.5, label = "TB 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(LDMC_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(LDMC_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LDMC_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of LDMC for TB 2018 - LMER
TB_LDMC_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LDMC_2018_LMER, type = 3)

#CWM of LDMC for TB 2019 - LMER
TB_LDMC_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), LDMC_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LDMC_2019_LMER, type = 3)

#CWM of LDMC for TB 2020 - LMER
TB_LDMC_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), LDMC_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LDMC_2020_LMER, type = 3)
#### post hoc test needed ####

#CWM of LDMC for TB 2021 - LMER
TB_LDMC_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LDMC_2021_LMER, type = 3)

#CWM of LDMC for TB 2022 - LMER
TB_LDMC_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LDMC_2022_LMER, type = 3)

####CWM - SLA Plots and Stats #### 

#CWM of SLA - 2019 and FK
SLA_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=Avg_SLA_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM SLA")+
  expand_limits(y=c(0,1500))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=1500, label = "FK 2019", size=20)


#CWM of SLA - 2020 and FK
SLA_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=Avg_SLA_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM SLA")+
  expand_limits(y=c(0,1500))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "NONE")+
  annotate("text", x=8, y=1500, label = "FK 2020", size=20)


#CWM of SLA - 2021 and FK
SLA_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=Avg_SLA_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM SLA")+
  expand_limits(y=c(0,1500))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=1500, label = "FK 2021", size=20)

#CWM of SLA - 2022 and FK
SLA_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=Avg_SLA_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM SLA")+
  expand_limits(y=c(0,1500))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=1500, label = "FK 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(SLA_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(SLA_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(SLA_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(SLA_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of SLA for Fort Keogh 2018 - LMER
FK_SLA_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_SLA_2018_LMER, type = 3)

#CWM of SLA for Fort Keogh 2019 - LMER
FK_SLA_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Avg_SLA_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_SLA_2019_LMER, type = 3)

#CWM of SLA for Fort Keogh 2020 - LMER
FK_SLA_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), Avg_SLA_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_SLA_2020_LMER, type = 3)
#### post hoc test needed ####

#CWM of SLA for Fort Keogh 2021 - LMER
FK_SLA_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_SLA_2021_LMER, type = 3)

#CWM of SLA for Fort Keogh 2022 - LMER
FK_SLA_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_SLA_2022_LMER, type = 3)

##Thunder Basin 
#CWM of SLA - 2019 and TB
SLA_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=Avg_SLA_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM SLA")+
  expand_limits(y=c(0,2500))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=2500, label = "TB 2019", size=20)


#CWM of SLA - 2020 and TB
SLA_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=Avg_SLA_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM SLA")+
  expand_limits(y=c(0,2500))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "NONE")+
  annotate("text", x=8, y=2500, label = "TB 2020", size=20)


#CWM of SLA - 2021 and TB
SLA_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=Avg_SLA_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM SLA")+
  expand_limits(y=c(0,2500))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=2500, label = "TB 2021", size=20)

#CWM of SLA - 2022 and TB
SLA_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=Avg_SLA_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM SLA")+
  expand_limits(y=c(0,2500))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=8, y=2500, label = "TB 2022", size=20)

#Create graph of all years for height data
pushViewport(viewport(layout=grid.layout(2,2)))
print(SLA_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(SLA_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(SLA_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(SLA_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of SLA for TB 2018 - LMER
TB_SLA_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_SLA_2018_LMER, type = 3)

#CWM of SLA for TB 2019 - LMER
TB_SLA_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Avg_SLA_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_SLA_2019_LMER, type = 3)

#CWM of SLA for TB 2020 - LMER
TB_SLA_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), Avg_SLA_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_SLA_2020_LMER, type = 3)

#CWM of SLA for TB 2021 - LMER
TB_SLA_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_SLA_2021_LMER, type = 3)

#CWM of SLA for TB 2022 - LMER
TB_SLA_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_SLA_2022_LMER, type = 3)

####CWM - Leaf Thickness Plots and Stats #### 


#CWM of Leaf Thickness - 2019 and FK
LeafThickness_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.5, label = "FK 2019", size=20)

#CWM of Leaf Thickness - 2020 and FK
LeafThickness_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "FK 2020", size=20)


#CWM of Leaf Thickness - 2021 and FK
LeafThickness_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "FK 2021", size=20)

#CWM of Leaf Thickness - 2022 and FK
LeafThickness_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "FK 2022", size=20)

#Create graph of all years for Leaf Thickness data
pushViewport(viewport(layout=grid.layout(2,2)))
print(LeafThickness_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(LeafThickness_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LeafThickness_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LeafThickness_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of LeafThickness for Fort Keogh 2018 - LMER
FK_LeafThickness_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2018_LMER, type = 3)

#CWM of LeafThickness for Fort Keogh 2019 - LMER
FK_LeafThickness_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), LeafThickness_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2019_LMER, type = 3)

#CWM of LeafThickness for Fort Keogh 2020 - LMER
FK_LeafThickness_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), LeafThickness_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2020_LMER, type = 3)

#CWM of LeafThickness for Fort Keogh 2021 - LMER
FK_LeafThickness_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2021_LMER, type = 3)

#CWM of LeafThickness for Fort Keogh 2022 - LMER
FK_LeafThickness_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2022_LMER, type = 3)

##Thunder Basin

#CWM of Leaf Thickness - 2019 and TB
LeafThickness_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.5, label = "TB 2019", size=20)


#CWM of Leaf Thickness - 2020 and TB
LeafThickness_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "TB 2020", size=20)


#CWM of Leaf Thickness - 2021 and TB
LeafThickness_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "TB 2021", size=20)

#CWM of Leaf Thickness - 2022 and TB
LeafThickness_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Leaf Thickness (mm)")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.5, label = "TB 2022", size=20)

#Create graph of all years for Open Flowers data
pushViewport(viewport(layout=grid.layout(2,2)))
print(LeafThickness_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(LeafThickness_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LeafThickness_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LeafThickness_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of LeafThickness for Thunder Basin 2018 - LMER
TB_LeafThickness_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2018_LMER, type = 3)
#post hoc test for lmer test
summary(glht(TB_LeafThickness_2018_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))

#CWM of LeafThickness for Thunder Basin 2019 - LMER
TB_LeafThickness_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), LeafThickness_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2019_LMER, type = 3)

#CWM of LeafThickness for Thunder Basin 2020 - LMER
TB_LeafThickness_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), LeafThickness_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2020_LMER, type = 3)

#CWM of LeafThickness for Thunder Basin 2021 - LMER
TB_LeafThickness_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2021_LMER, type = 3)
#post hoc test for lmer test
summary(glht(TB_LeafThickness_2021_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) #no significance

#CWM of LeafThickness for Thunder Basin 2022 - LMER
TB_LeafThickness_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2022_LMER, type = 3)
#post hoc test for lmer test
summary(glht(TB_LeafThickness_2022_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) #stable-destock (p=0.00939)

####CWM - Leaf Area Plots and Stats #### 

##CWM of Area for Fort Keogh 2018 - LMER
FK_Area_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Area_2018_LMER, type = 3)

#CWM of Area for Fort Keogh 2019 - LMER
FK_Area_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Area_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Area_2019_LMER, type = 3)

#CWM of Area for Fort Keogh 2020 - LMER
FK_Area_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), Area_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Area_2020_LMER, type = 3)

#CWM of Area for Fort Keogh 2021 - LMER
FK_Area_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Area_2021_LMER, type = 3)

#CWM of Area for Fort Keogh 2022 - LMER
FK_Area_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Area_2022_LMER, type = 3)

##CWM of Area for Thunder Basin 2018 - LMER
TB_Area_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Area_2018_LMER, type = 3)

#CWM of Area for Thunder Basin 2019 - LMER
TB_Area_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Area_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Area_2019_LMER, type = 3)

#CWM of Area for Thunder Basin 2020 - LMER
TB_Area_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), Area_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Area_2020_LMER, type = 3)

#CWM of Area for Thunder Basin 2021 - LMER
TB_Area_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Area_2021_LMER, type = 3)

#CWM of Area for Thunder Basin 2022 - LMER
TB_Area_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Area_2022_LMER, type = 3)

####CWM - Lifespan Plots and Stats #### 

#CWM of LifeSpan - 2019 and FK
Lifespan_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=Lifespan_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Lifespan")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=1.5, label = "FK 2019", size=20)

#CWM of Lifespan - 2020 and FK
Lifespan_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=Lifespan_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Lifespan")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "FK 2020", size=20)


#CWM of LifeSpan - 2021 and FK
Lifespan_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=Lifespan_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Lifespan")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "FK 2021", size=20)

#CWM of Lifespan - 2022 and FK
Lifespan_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=Lifespan_CWM)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Lifespan")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "FK 2022", size=20)

#Create graph of all years for Leaf Thickness data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Lifespan_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Lifespan_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Lifespan_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Lifespan_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of Lifespan for Fort Keogh 2018 - LMER
FK_Lifespan_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Lifespan_2018_LMER, type = 3)

#CWM of Lifespan for Fort Keogh 2019 - LMER
FK_Lifespan_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Lifespan_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Lifespan_2019_LMER, type = 3)

#CWM of Lifespan for Fort Keogh 2020 - LMER
FK_Lifespan_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), Lifespan_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Lifespan_2020_LMER, type = 3)

#CWM of Lifespan for Fort Keogh 2021 - LMER
FK_Lifespan_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Lifespan_2021_LMER, type = 3)

#CWM of Lifespan for Fort Keogh 2022 - LMER
FK_Lifespan_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Lifespan_2022_LMER, type = 3)

##Thunder Basin

#CWM of Lifespan - 2019 and TB
Lifespan_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=Lifespan_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Lifespan")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=1.5, label = "TB 2019", size=20)


#CWM of Lifespan - 2020 and TB
Lifespan_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=Lifespan_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Lifespan")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "TB 2020", size=20)


#CWM of Lifespan - 2021 and TB
Lifespan_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=Lifespan_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Lifespan")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "TB 2021", size=20)

#CWM of Lifespan - 2022 and TB
Lifespan_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=Lifespan_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Lifespan")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "TB 2022", size=20)

#Create graph of all years for Lifespan data
pushViewport(viewport(layout=grid.layout(2,2)))
print(Lifespan_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Lifespan_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Lifespan_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Lifespan_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of Lifespan for Thunder Basin 2018 - LMER
TB_Lifespan_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Lifespan_2018_LMER, type = 3)

#CWM of Lifespan for Thunder Basin 2019 - LMER
TB_Lifespan_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Lifespan_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Lifespan_2019_LMER, type = 3)

#CWM of Lifespan for Thunder Basin 2020 - LMER
TB_Lifespan_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), Lifespan_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Lifespan_2020_LMER, type = 3)

#CWM of Lifespan for Thunder Basin 2021 - LMER
TB_Lifespan_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Lifespan_2021_LMER, type = 3)

#CWM of Lifespan for Thunder Basin 2022 - LMER
TB_Lifespan_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Lifespan_2022_LMER, type = 3)


####CWM - GrowthForm Plots and Stats #### 


#CWM of GrowthForm - 2019 and FK
GrowthForm_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=GrowthForm_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Growth Form")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=1.5, label = "FK 2019", size=20)

#CWM of GrowthForm - 2020 and FK
GrowthForm_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=GrowthForm_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Growth Form")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "FK 2020", size=20)


#CWM of GrowthForm - 2021 and FK
GrowthForm_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=GrowthForm_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Growth Form")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "FK 2021", size=20)

#CWM of GrowthForm - 2022 and FK
GrowthForm_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=GrowthForm_CWM)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Growth Form")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "FK 2022", size=20)

#Create graph of all years for Leaf Thickness data
pushViewport(viewport(layout=grid.layout(2,2)))
print(GrowthForm_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(GrowthForm_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(GrowthForm_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(GrowthForm_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of GrowthForm for Fort Keogh 2018 - LMER
FK_GrowthForm_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_GrowthForm_2018_LMER, type = 3)

#CWM of GrowthForm for Fort Keogh 2019 - LMER
FK_GrowthForm_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), GrowthForm_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_GrowthForm_2019_LMER, type = 3)

#CWM of GrowthForm for Fort Keogh 2020 - LMER
FK_GrowthForm_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), GrowthForm_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_GrowthForm_2020_LMER, type = 3)

#CWM of GrowthForm for Fort Keogh 2021 - LMER
FK_GrowthForm_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_GrowthForm_2021_LMER, type = 3)

#CWM of GrowthForm for Fort Keogh 2022 - LMER
FK_GrowthForm_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_GrowthForm_2022_LMER, type = 3)

##Thunder Basin

#CWM of GrowthForm - 2019 and TB
GrowthForm_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=GrowthForm_CWM)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Growth Form")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=1.5, label = "TB 2019", size=20)


#CWM of GrowthForm - 2020 and TB
GrowthForm_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=GrowthForm_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Growth Form")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "TB 2020", size=20)


#CWM of GrowthForm - 2021 and TB
GrowthForm_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=GrowthForm_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Growth Form")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "TB 2021", size=20)

#CWM of GrowthForm - 2022 and TB
GrowthForm_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=GrowthForm_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Growth Form")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "TB 2022", size=20)

#Create graph of all years for GrowthForm data
pushViewport(viewport(layout=grid.layout(2,2)))
print(GrowthForm_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(GrowthForm_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(GrowthForm_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(GrowthForm_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of GrowthForm for Thunder Basin 2018 - LMER
TB_GrowthForm_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_GrowthForm_2018_LMER, type = 3)

#CWM of GrowthForm for Thunder Basin 2019 - LMER
TB_GrowthForm_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), GrowthForm_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_GrowthForm_2019_LMER, type = 3)

#CWM of GrowthForm for Thunder Basin 2020 - LMER
TB_GrowthForm_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), GrowthForm_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_GrowthForm_2020_LMER, type = 3)

#CWM of GrowthForm for Thunder Basin 2021 - LMER
TB_GrowthForm_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_GrowthForm_2021_LMER, type = 3)

#CWM of GrowthForm for Thunder Basin 2022 - LMER
TB_GrowthForm_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_GrowthForm_2022_LMER, type = 3)

####CWM - PhotosyntheticPathway Plots and Stats #### 


#CWM of PhotosyntheticPathway - 2019 and FK
PhotosyntheticPathway_FK_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=PhotosyntheticPathway_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Photosynthetic Pathway")+
  expand_limits(y=0.75)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=0.75, label = "FK 2019", size=20)

#CWM of PhotosyntheticPathway - 2020 and FK
PhotosyntheticPathway_FK_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=PhotosyntheticPathway_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Photosynthetic Pathway")+
  expand_limits(y=0.75)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=0.75, label = "FK 2020", size=20)


#CWM of PhotosyntheticPathway - 2021 and FK
PhotosyntheticPathway_FK_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=PhotosyntheticPathway_CWM)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Photosynthetic Pathway")+
  expand_limits(y=0.75)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.75, label = "FK 2021", size=20)

#CWM of PhotosyntheticPathway - 2022 and FK
PhotosyntheticPathway_FK_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=PhotosyntheticPathway_CWM)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Photosynthetic Pathway")+
  expand_limits(y=0.75)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.75, label = "FK 2022", size=20)

#Create graph of all years for Leaf Thickness data
pushViewport(viewport(layout=grid.layout(2,2)))
print(PhotosyntheticPathway_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(PhotosyntheticPathway_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(PhotosyntheticPathway_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(PhotosyntheticPathway_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

##CWM of PhotosyntheticPathway for Fort Keogh 2018 - LMER
FK_PhotosyntheticPathway_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), PhotosyntheticPathway_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PhotosyntheticPathway_2018_LMER, type = 3)

#CWM of PhotosyntheticPathway for Fort Keogh 2019 - LMER
FK_PhotosyntheticPathway_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), PhotosyntheticPathway_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PhotosyntheticPathway_2019_LMER, type = 3)

#CWM of PhotosyntheticPathway for Fort Keogh 2020 - LMER
FK_PhotosyntheticPathway_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), PhotosyntheticPathway_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PhotosyntheticPathway_2020_LMER, type = 3)

#CWM of PhotosyntheticPathway for Fort Keogh 2021 - LMER
FK_PhotosyntheticPathway_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), PhotosyntheticPathway_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PhotosyntheticPathway_2021_LMER, type = 3)

#CWM of PhotosyntheticPathway for Fort Keogh 2022 - LMER
FK_PhotosyntheticPathway_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), PhotosyntheticPathway_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PhotosyntheticPathway_2022_LMER, type = 3)

##Thunder Basin

#CWM of PhotosyntheticPathway - 2019 and TB
PhotosyntheticPathway_TB_19<-ggplot(subset(CWM_Collected_Data,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=PhotosyntheticPathway_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Photosynthetic Pathway")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=8, y=1, label = "TB 2019", size=20)


#CWM of PhotosyntheticPathway - 2020 and TB
PhotosyntheticPathway_TB_20<-ggplot(subset(CWM_Collected_Data,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=PhotosyntheticPathway_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Photosynthetic Pathway")+
  expand_limits(y=1)+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=8, y=1, label = "TB 2020", size=20)


#CWM of PhotosyntheticPathway - 2021 and TB
PhotosyntheticPathway_TB_21<-ggplot(subset(CWM_Collected_Data,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=PhotosyntheticPathway_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Photosynthetic Pathway")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "TB 2021", size=20)

#CWM of PhotosyntheticPathway - 2022 and TB
PhotosyntheticPathway_TB_22<-ggplot(subset(CWM_Collected_Data,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=PhotosyntheticPathway_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM Photosynthetic Pathway")+
  expand_limits(y=1)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "TB 2022", size=20)

#Create graph of all years for PhotosyntheticPathway data
pushViewport(viewport(layout=grid.layout(2,2)))
print(PhotosyntheticPathway_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(PhotosyntheticPathway_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(PhotosyntheticPathway_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(PhotosyntheticPathway_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3000 x 2000  

#CWM of PhotosyntheticPathway for Thunder Basin 2018 - LMER
TB_PhotosyntheticPathway_2018_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), PhotosyntheticPathway_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PhotosyntheticPathway_2018_LMER, type = 3)

#CWM of PhotosyntheticPathway for Thunder Basin 2019 - LMER
TB_PhotosyntheticPathway_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), PhotosyntheticPathway_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PhotosyntheticPathway_2019_LMER, type = 3)

#CWM of PhotosyntheticPathway for Thunder Basin 2020 - LMER
TB_PhotosyntheticPathway_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), PhotosyntheticPathway_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PhotosyntheticPathway_2020_LMER, type = 3)

#CWM of PhotosyntheticPathway for Thunder Basin 2021 - LMER
TB_PhotosyntheticPathway_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), PhotosyntheticPathway_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PhotosyntheticPathway_2021_LMER, type = 3)

#CWM of PhotosyntheticPathway for Thunder Basin 2022 - LMER
TB_PhotosyntheticPathway_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), PhotosyntheticPathway_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PhotosyntheticPathway_2022_LMER, type = 3)

#### PCAs ####

#Create seperate dataframes for each site and year for PCAs
#FK

CWM_Collected_Data_FK_18<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2018) %>% 
  select(-EmergingLeaves_CWM,-DevelopedLeaves_CWM,-ScenescedLeaves_CWM,-FlowerHeads_CWM,-OpenFlowers_CWM,-FlowerNum_CWM,-TotalLeaf_CWM,-Avg_SM,-EmergingLeaves_CWM_TF,-DevelopedLeaves_CWM_TF,-ScenescedLeaves_CWM_TF,-FlowerHeads_CWM_TF,-OpenFlowers_CWM_TF,-FlowerNum_CWM_TF,-TotalLeaf_CWM_TF)

CWM_Collected_Data_FK_19<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2019)%>% 
  select(-EmergingLeaves_CWM,-DevelopedLeaves_CWM,-ScenescedLeaves_CWM,-FlowerHeads_CWM,-OpenFlowers_CWM,-FlowerNum_CWM,-TotalLeaf_CWM,-Avg_SM,-EmergingLeaves_CWM_TF,-DevelopedLeaves_CWM_TF,-ScenescedLeaves_CWM_TF,-FlowerHeads_CWM_TF,-OpenFlowers_CWM_TF,-FlowerNum_CWM_TF,-TotalLeaf_CWM_TF)

CWM_Collected_Data_FK_20<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2020)%>% 
  select(-EmergingLeaves_CWM,-DevelopedLeaves_CWM,-ScenescedLeaves_CWM,-FlowerHeads_CWM,-OpenFlowers_CWM,-FlowerNum_CWM,-TotalLeaf_CWM,-Avg_SM,-EmergingLeaves_CWM_TF,-DevelopedLeaves_CWM_TF,-ScenescedLeaves_CWM_TF,-FlowerHeads_CWM_TF,-OpenFlowers_CWM_TF,-FlowerNum_CWM_TF,-TotalLeaf_CWM_TF)

CWM_Collected_Data_FK_21<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2021)%>% 
  select(-EmergingLeaves_CWM,-DevelopedLeaves_CWM,-ScenescedLeaves_CWM,-FlowerHeads_CWM,-OpenFlowers_CWM,-FlowerNum_CWM,-TotalLeaf_CWM,-Avg_SM,-EmergingLeaves_CWM_TF,-DevelopedLeaves_CWM_TF,-ScenescedLeaves_CWM_TF,-FlowerHeads_CWM_TF,-OpenFlowers_CWM_TF,-FlowerNum_CWM_TF,-TotalLeaf_CWM_TF)

CWM_Collected_Data_FK_22<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2022)%>% 
  select(-EmergingLeaves_CWM,-DevelopedLeaves_CWM,-ScenescedLeaves_CWM,-FlowerHeads_CWM,-OpenFlowers_CWM,-FlowerNum_CWM,-TotalLeaf_CWM,-Avg_SM,-EmergingLeaves_CWM_TF,-DevelopedLeaves_CWM_TF,-ScenescedLeaves_CWM_TF,-FlowerHeads_CWM_TF,-OpenFlowers_CWM_TF,-FlowerNum_CWM_TF,-TotalLeaf_CWM_TF)

#TB

CWM_Collected_Data_TB_18<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2018) %>% 
  select(-EmergingLeaves_CWM,-DevelopedLeaves_CWM,-ScenescedLeaves_CWM,-FlowerHeads_CWM,-OpenFlowers_CWM,-FlowerNum_CWM,-TotalLeaf_CWM,-Avg_SM,-EmergingLeaves_CWM_TF,-DevelopedLeaves_CWM_TF,-ScenescedLeaves_CWM_TF,-FlowerHeads_CWM_TF,-OpenFlowers_CWM_TF,-FlowerNum_CWM_TF,-TotalLeaf_CWM_TF)

CWM_Collected_Data_TB_19<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2019) %>% 
  na.omit(Biomass_CWM)%>% 
  select(-EmergingLeaves_CWM,-DevelopedLeaves_CWM,-ScenescedLeaves_CWM,-FlowerHeads_CWM,-OpenFlowers_CWM,-FlowerNum_CWM,-TotalLeaf_CWM,-Avg_SM,-EmergingLeaves_CWM_TF,-DevelopedLeaves_CWM_TF,-ScenescedLeaves_CWM_TF,-FlowerHeads_CWM_TF,-OpenFlowers_CWM_TF,-FlowerNum_CWM_TF,-TotalLeaf_CWM_TF)

CWM_Collected_Data_TB_20<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2020) %>% 
  na.omit(Biomass_CWM) %>% 
  na.omit(LDMC_CWM)%>% 
  select(-EmergingLeaves_CWM,-DevelopedLeaves_CWM,-ScenescedLeaves_CWM,-FlowerHeads_CWM,-OpenFlowers_CWM,-FlowerNum_CWM,-TotalLeaf_CWM,-Avg_SM,-EmergingLeaves_CWM_TF,-DevelopedLeaves_CWM_TF,-ScenescedLeaves_CWM_TF,-FlowerHeads_CWM_TF,-OpenFlowers_CWM_TF,-FlowerNum_CWM_TF,-TotalLeaf_CWM_TF)

CWM_Collected_Data_TB_21<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2021) %>% 
  filter(!is.na(Biomass_CWM))%>% 
  select(-EmergingLeaves_CWM,-DevelopedLeaves_CWM,-ScenescedLeaves_CWM,-FlowerHeads_CWM,-OpenFlowers_CWM,-FlowerNum_CWM,-TotalLeaf_CWM,-Avg_SM,-EmergingLeaves_CWM_TF,-DevelopedLeaves_CWM_TF,-ScenescedLeaves_CWM_TF,-FlowerHeads_CWM_TF,-OpenFlowers_CWM_TF,-FlowerNum_CWM_TF,-TotalLeaf_CWM_TF)

CWM_Collected_Data_TB_22<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2022)%>% 
  select(-EmergingLeaves_CWM,-DevelopedLeaves_CWM,-ScenescedLeaves_CWM,-FlowerHeads_CWM,-OpenFlowers_CWM,-FlowerNum_CWM,-TotalLeaf_CWM,-Avg_SM,-EmergingLeaves_CWM_TF,-DevelopedLeaves_CWM_TF,-ScenescedLeaves_CWM_TF,-FlowerHeads_CWM_TF,-OpenFlowers_CWM_TF,-FlowerNum_CWM_TF,-TotalLeaf_CWM_TF)

#### PCA for FK 2018 ####
PCA_FK_18<-prcomp(CWM_Collected_Data_FK_18[,19:24],scale=TRUE)
PCA_FK_18
summary(PCA_FK_18)

axes_FK_18 <- predict(PCA_FK_18, newdata = CWM_Collected_Data_FK_18)
head(axes_FK_18, 4)

#put PCA axes with site and plot #   
PCA_FK_18_meta<-cbind(CWM_Collected_Data_FK_18,axes_FK_18)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_18 <- get_pca_var(PCA_FK_18)
var_FK_18
head(var_FK_18$contrib, 13)

#### PCA for FK 2019 ####
PCA_FK_19<-prcomp(CWM_Collected_Data_FK_19[,19:24],scale=TRUE)
PCA_FK_19
summary(PCA_FK_19)

axes_FK_19 <- predict(PCA_FK_19, newdata = CWM_Collected_Data_FK_19)
head(axes_FK_19, 4)

#put PCA axes with site and plot #   
PCA_FK_19_meta<-cbind(CWM_Collected_Data_FK_19,axes_FK_19)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_19 <- get_pca_var(PCA_FK_19)
var_FK_19
head(var_FK_19$contrib, 13)

#### PCA for FK 2020 ####
PCA_FK_20<-prcomp(CWM_Collected_Data_FK_20[,19:24],scale=TRUE)
PCA_FK_20
summary(PCA_FK_20)

axes_FK_20 <- predict(PCA_FK_20, newdata = CWM_Collected_Data_FK_20)
head(axes_FK_20, 4)

#put PCA axes with site and plot #   
PCA_FK_20_meta<-cbind(CWM_Collected_Data_FK_20,axes_FK_20)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_20 <- get_pca_var(PCA_FK_20)
var_FK_20
head(var_FK_20$contrib, 12)

#### PCA for FK 2021 ####
PCA_FK_21<-prcomp(CWM_Collected_Data_FK_21[,19:24],scale=TRUE)
PCA_FK_21
summary(PCA_FK_21)

axes_FK_21 <- predict(PCA_FK_21, newdata = CWM_Collected_Data_FK_21)
head(axes_FK_21, 4)

#put PCA axes with site and plot #   
PCA_FK_21_meta<-cbind(CWM_Collected_Data_FK_21,axes_FK_21)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_21 <- get_pca_var(PCA_FK_21)
var_FK_21
head(var_FK_21$contrib, 12)

#### PCA for FK 2022 ####
PCA_FK_22<-prcomp(CWM_Collected_Data_FK_22[,19:24],scale=TRUE)
PCA_FK_22
summary(PCA_FK_22)

axes_FK_22 <- predict(PCA_FK_22, newdata = CWM_Collected_Data_FK_22)
head(axes_FK_22, 4)

#put PCA axes with site and plot #   
PCA_FK_22_meta<-cbind(CWM_Collected_Data_FK_22,axes_FK_22)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_22 <- get_pca_var(PCA_FK_22)
var_FK_22
head(var_FK_22$contrib, 12)

#### PCA for TB 2018 ####
PCA_TB_18<-prcomp(CWM_Collected_Data_TB_18[,19:24],scale=TRUE)
PCA_TB_18
summary(PCA_TB_18)

axes_TB_18 <- predict(PCA_TB_18, newdata = CWM_Collected_Data_TB_18)
head(axes_TB_18, 4)

#put PCA axes with site and plot #   
PCA_TB_18_meta<-cbind(CWM_Collected_Data_TB_18,axes_TB_18)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_18 <- get_pca_var(PCA_TB_18)
var_TB_18
head(var_TB_18$contrib, 12)


#### PCA for TB 2019 ####
PCA_TB_19<-prcomp(CWM_Collected_Data_TB_19[,19:24],scale=TRUE)
PCA_TB_19
summary(PCA_TB_19)

axes_TB_19 <- predict(PCA_TB_19, newdata = CWM_Collected_Data_TB_19)
head(axes_TB_19, 4)

#put PCA axes with site and plot #   
PCA_TB_19_meta<-cbind(CWM_Collected_Data_TB_19,axes_TB_19)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_19 <- get_pca_var(PCA_TB_19)
var_TB_19
head(var_TB_19$contrib, 12)

#### PCA for TB 2020 ####
PCA_TB_20<-prcomp(CWM_Collected_Data_TB_20[,19:24],scale=TRUE)
PCA_TB_20
summary(PCA_TB_20)

axes_TB_20 <- predict(PCA_TB_20, newdata = CWM_Collected_Data_TB_20)
head(axes_TB_20, 4)

#put PCA axes with site and plot #   
PCA_TB_20_meta<-cbind(CWM_Collected_Data_TB_20,axes_TB_20)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_20 <- get_pca_var(PCA_TB_20)
var_TB_20
head(var_TB_20$contrib, 12)

#### PCA for TB 2021 ####
PCA_TB_21<-prcomp(CWM_Collected_Data_TB_21[,19:24],scale=TRUE)
PCA_TB_21
summary(PCA_TB_21)

axes_TB_21 <- predict(PCA_TB_21, newdata = CWM_Collected_Data_TB_21)
head(axes_TB_21, 4)

#put PCA axes with site and plot #   
PCA_TB_21_meta<-cbind(CWM_Collected_Data_TB_21,axes_TB_21)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_21 <- get_pca_var(PCA_TB_21)
var_TB_21
head(var_TB_21$contrib, 11)

#### PCA for TB 2022 ####
PCA_TB_22<-prcomp(CWM_Collected_Data_TB_22[,19:24],scale=TRUE)
PCA_TB_22
summary(PCA_TB_22)

axes_TB_22 <- predict(PCA_TB_22, newdata = CWM_Collected_Data_TB_22)
head(axes_TB_22, 4)

#put PCA axes with site and plot #   
PCA_TB_22_meta<-cbind(CWM_Collected_Data_TB_22,axes_TB_22)%>%
  select(plot,block,paddock,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_22 <- get_pca_var(PCA_TB_22)
var_TB_22
head(var_TB_22$contrib, 12)


#### PCA Graphs #### 

#FK
PCA_FK_18_G<-autoplot(PCA_FK_18, data=CWM_Collected_Data_FK_18, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="grazing_treatment")+
  theme(legend.position = c(0.1,0.9))
#save as 1300x1000

PCA_FK_19_G<-autoplot(PCA_FK_19, data=CWM_Collected_Data_FK_19, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="grazing_treatment")+
  theme(legend.position = c(0.1,0.9))

PCA_FK_20<-autoplot(PCA_FK_20, data=CWM_Collected_Data_FK_20, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="grazing_treatment")+
  theme(legend.position = c(0.1,0.9))

PCA_FK_21<-autoplot(PCA_FK_21, data=CWM_Collected_Data_FK_21, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="grazing_treatment")+
  theme(legend.position = c(0.1,0.9))

PCA_FK_22<-autoplot(PCA_FK_22, data=CWM_Collected_Data_FK_22, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="grazing_treatment")+
  theme(legend.position = c(0.1,0.9))

#TB
PCA_TB_18<-autoplot(PCA_TB_18, data=CWM_Collected_Data_TB_18, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="grazing_treatment")+
  theme(legend.position = c(0.1,0.9))

PCA_TB_19<-autoplot(PCA_TB_19, data=CWM_Collected_Data_TB_19, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="grazing_treatment")+
  theme(legend.position = c(0.1,0.9))

PCA_TB_20<-autoplot(PCA_TB_20, data=CWM_Collected_Data_TB_20, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="grazing_treatment")+
  theme(legend.position = c(0.1,0.9))

PCA_TB_21<-autoplot(PCA_TB_21, data=CWM_Collected_Data_TB_21, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="grazing_treatment")+
  theme(legend.position = c(0.1,0.9))

PCA_TB_22<-autoplot(PCA_TB_22, data=CWM_Collected_Data_TB_22, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=T, frame.colour="grazing_treatment")+
  theme(legend.position = c(0.1,0.9))

#Create graph of all years for PCAs
pushViewport(viewport(layout=grid.layout(5,2)))
print(PCA_FK_18,vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(PCA_FK_19,vp=viewport(layout.pos.row=2, layout.pos.col=1))
print(PCA_FK_20,vp=viewport(layout.pos.row=3, layout.pos.col=1))
print(PCA_FK_21,vp=viewport(layout.pos.row=4, layout.pos.col=1))
print(PCA_FK_22,vp=viewport(layout.pos.row=5, layout.pos.col=1))
print(PCA_TB_18,vp=viewport(layout.pos.row=1, layout.pos.col=2))
print(PCA_TB_19,vp=viewport(layout.pos.row=2, layout.pos.col=2))
print(PCA_TB_20,vp=viewport(layout.pos.row=3, layout.pos.col=2))
print(PCA_TB_21,vp=viewport(layout.pos.row=4, layout.pos.col=2))
print(PCA_TB_22,vp=viewport(layout.pos.row=5, layout.pos.col=2))
#Save at 2500 x 1500  

#### PCA Stats ####

#make two seperate dataframes for each year and site to have treatment data and trait data seperate

## FK ##
#2018
CWM_FK_18_Trait<-CWM_Collected_Data_FK_18 %>% 
  select(Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_FK_18_Treatment<-CWM_Collected_Data_FK_18 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2019
CWM_FK_19_Trait<-CWM_Collected_Data_FK_19 %>% 
  select(Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_FK_19_Treatment<-CWM_Collected_Data_FK_19 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2020
CWM_FK_20_Trait<-CWM_Collected_Data_FK_20 %>% 
  select(Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_FK_20_Treatment<-CWM_Collected_Data_FK_20 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2021
CWM_FK_21_Trait<-CWM_Collected_Data_FK_21 %>% 
  select(Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_FK_21_Treatment<-CWM_Collected_Data_FK_21 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2022
CWM_FK_22_Trait<-CWM_Collected_Data_FK_22 %>% 
  select(Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_FK_22_Treatment<-CWM_Collected_Data_FK_22 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

## TB ##
#2018
CWM_TB_18_Trait<-CWM_Collected_Data_TB_18 %>% 
  select(Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_TB_18_Treatment<-CWM_Collected_Data_TB_18 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2019
CWM_TB_19_Trait<-CWM_Collected_Data_TB_19 %>% 
  select(Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_TB_19_Treatment<-CWM_Collected_Data_TB_19 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2020
CWM_TB_20_Trait<-CWM_Collected_Data_TB_20 %>% 
  select(Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_TB_20_Treatment<-CWM_Collected_Data_TB_20 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2021
CWM_TB_21_Trait<-CWM_Collected_Data_TB_21 %>% 
  select(Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_TB_21_Treatment<-CWM_Collected_Data_TB_21 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2022
CWM_TB_22_Trait<-CWM_Collected_Data_TB_22 %>% 
  select(Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_TB_22_Treatment<-CWM_Collected_Data_TB_22 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)


# run PERMANOVA using adonis using trait dataframe as data to run adonis on and treatment dataframe as variables

## FK ##
#FK 2018
PERMANOVA_FK_18 <-adonis2(CWM_FK_18_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/paddock), data = CWM_FK_18_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_18) 

#FK 2019
PERMANOVA_FK_19 <-adonis2(CWM_FK_19_Trait~Rainfall_reduction_cat + (1|block/paddock), data = CWM_FK_19_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_19) 

#FK 2020
PERMANOVA_FK_20 <-adonis2(CWM_FK_20_Trait~Rainfall_reduction_cat*Grazing_2020 + (1|block/paddock), data = CWM_FK_20_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_20)

#FK 2021
PERMANOVA_FK_21 <-adonis2(CWM_FK_21_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/paddock), data = CWM_FK_21_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_21)

#FK 2022
PERMANOVA_FK_22 <-adonis2(CWM_FK_22_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/paddock), data = CWM_FK_22_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_22)

##TB##

#TB 2018
PERMANOVA_TB_18 <-adonis2(CWM_TB_18_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/paddock), data = CWM_TB_18_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_18)

#TB 2019
PERMANOVA_TB_19 <-adonis2(CWM_TB_19_Trait~Rainfall_reduction_cat + (1|block/paddock), data = CWM_TB_19_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_19) 

#TB 2020
PERMANOVA_TB_20 <-adonis2(CWM_TB_20_Trait~Rainfall_reduction_cat*Grazing_2020 + (1|block/paddock), data = CWM_TB_20_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_20)

#TB 2021
PERMANOVA_TB_21 <-adonis2(CWM_TB_21_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/paddock), data = CWM_TB_21_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_21)

#TB 2022
PERMANOVA_TB_22 <-adonis2(CWM_TB_22_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/paddock), data = CWM_TB_22_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_22)

## PERMDISP ##

# FK 2018
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_18 <- vegdist(CWM_FK_18_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_18_drought <- betadisper(BC_Distance_Matrix_FK_18,CWM_FK_18_Treatment$Rainfall_reduction_cat)
anova(Dispersion_FK_18_drought) 

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_FK_18_graze <- betadisper(BC_Distance_Matrix_FK_18,CWM_FK_18_Treatment$grazing_treatment)
anova(Dispersion_FK_18_graze)

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_FK_18_DxG <- betadisper(BC_Distance_Matrix_FK_18,CWM_FK_18_Treatment$Trtm)
anova(Dispersion_FK_18_DxG) 

# FK 2019
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_19 <- vegdist(CWM_FK_19_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_19_drought <- betadisper(BC_Distance_Matrix_FK_19,CWM_FK_19_Treatment$Rainfall_reduction_cat)
anova(Dispersion_FK_19_drought) 


# FK 2020
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_20 <- vegdist(CWM_FK_20_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_20_drought <- betadisper(BC_Distance_Matrix_FK_20,CWM_FK_20_Treatment$Rainfall_reduction_cat)
anova(Dispersion_FK_20_drought)

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_FK_20_graze <- betadisper(BC_Distance_Matrix_FK_20,CWM_FK_20_Treatment$Grazing_2020)
anova(Dispersion_FK_20_graze) 

#combine 2020 grazing+drought
CWM_FK_20_Treatment<-CWM_FK_20_Treatment %>% 
  mutate(Trtm_20=paste(Rainfall_reduction_cat,Grazing_2020,sep="."))

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_FK_20_DxG <- betadisper(BC_Distance_Matrix_FK_20,CWM_FK_20_Treatment$Trtm_20)
anova(Dispersion_FK_20_DxG) 


# FK 2021
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_21 <- vegdist(CWM_FK_21_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_21_drought <- betadisper(BC_Distance_Matrix_FK_21,CWM_FK_21_Treatment$Rainfall_reduction_cat)
anova(Dispersion_FK_21_drought) 

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_FK_21_graze <- betadisper(BC_Distance_Matrix_FK_21,CWM_FK_21_Treatment$grazing_treatment)
anova(Dispersion_FK_21_graze)

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_FK_21_DxG <- betadisper(BC_Distance_Matrix_FK_21,CWM_FK_21_Treatment$Trtm)
anova(Dispersion_FK_21_DxG)


# FK 2022
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_22 <- vegdist(CWM_FK_22_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_22_drought <- betadisper(BC_Distance_Matrix_FK_22,CWM_FK_22_Treatment$Rainfall_reduction_cat)
anova(Dispersion_FK_22_drought) 

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_FK_22_graze <- betadisper(BC_Distance_Matrix_FK_22,CWM_FK_22_Treatment$grazing_treatment)
anova(Dispersion_FK_22_graze) 

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_FK_22_DxG <- betadisper(BC_Distance_Matrix_FK_22,CWM_FK_22_Treatment$Trtm)
anova(Dispersion_FK_22_DxG)

# TB 2018
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_18 <- vegdist(CWM_TB_18_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_18_drought <- betadisper(BC_Distance_Matrix_TB_18,CWM_TB_18_Treatment$Rainfall_reduction_cat)
anova(Dispersion_TB_18_drought)

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_TB_18_graze <- betadisper(BC_Distance_Matrix_TB_18,CWM_TB_18_Treatment$grazing_treatment)
anova(Dispersion_TB_18_graze) #p=0.1685

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_TB_18_DxG <- betadisper(BC_Distance_Matrix_TB_18,CWM_TB_18_Treatment$Trtm)
anova(Dispersion_TB_18_DxG) 

# TB 2019
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_19 <- vegdist(CWM_TB_19_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_19_drought <- betadisper(BC_Distance_Matrix_TB_19,CWM_TB_19_Treatment$Rainfall_reduction_cat)
anova(Dispersion_TB_19_drought)

# TB 2020
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_20 <- vegdist(CWM_TB_20_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_20_drought <- betadisper(BC_Distance_Matrix_TB_20,CWM_TB_20_Treatment$Rainfall_reduction_cat)
anova(Dispersion_TB_20_drought) 

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_TB_20_graze <- betadisper(BC_Distance_Matrix_TB_20,CWM_TB_20_Treatment$Grazing_2020)
anova(Dispersion_TB_20_graze)

#combine 2020 grazing+drought
CWM_TB_20_Treatment<-CWM_TB_20_Treatment %>% 
  mutate(Trtm_20=paste(Rainfall_reduction_cat,Grazing_2020,sep="."))

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_TB_20_DxG <- betadisper(BC_Distance_Matrix_TB_20,CWM_TB_20_Treatment$Trtm_20)
anova(Dispersion_TB_20_DxG) 

# TB 2021
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_21 <- vegdist(CWM_TB_21_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_21_drought <- betadisper(BC_Distance_Matrix_TB_21,CWM_TB_21_Treatment$Rainfall_reduction_cat)
anova(Dispersion_TB_21_drought)

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_TB_21_graze <- betadisper(BC_Distance_Matrix_TB_21,CWM_TB_21_Treatment$grazing_treatment)
anova(Dispersion_TB_21_graze)

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_TB_21_DxG <- betadisper(BC_Distance_Matrix_TB_21,CWM_TB_21_Treatment$Trtm)
anova(Dispersion_TB_21_DxG) 

# TB 2022
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_22 <- vegdist(CWM_TB_22_Trait)

#drought
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_22_drought <- betadisper(BC_Distance_Matrix_TB_22,CWM_TB_22_Treatment$Rainfall_reduction_cat)
anova(Dispersion_TB_22_drought) 

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_TB_22_graze <- betadisper(BC_Distance_Matrix_TB_22,CWM_TB_22_Treatment$grazing_treatment)
anova(Dispersion_TB_22_graze)

#Run a dissimilarity matrix (PermDisp) comparing grazing treatment*drought
Dispersion_TB_22_DxG <- betadisper(BC_Distance_Matrix_TB_22,CWM_TB_22_Treatment$Trtm)
anova(Dispersion_TB_22_DxG) 

#### SIMPER ####

#Run a SIMPER test comparing data
SIMPER_FK_18 <- with(CWM_FK_18_Treatment,simper(CWM_FK_18_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
summary(SIMPER_FK_18)

#Run a SIMPER test comparing data 
SIMPER_FK_19 <- with(CWM_FK_19_Treatment,simper(CWM_FK_19_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
summary(SIMPER_FK_19)

#Run a SIMPER test comparing data 
SIMPER_FK_20 <- with(CWM_FK_20_Treatment,simper(CWM_FK_20_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
summary(SIMPER_FK_20)

#Run a SIMPER test comparing data 
SIMPER_FK_21 <- with(CWM_FK_21_Treatment,simper(CWM_FK_21_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
summary(SIMPER_FK_21)

#Run a SIMPER test comparing data 
SIMPER_FK_22 <- with(CWM_FK_22_Treatment,simper(CWM_FK_22_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
summary(SIMPER_FK_22)

#Run a SIMPER test comparing data
SIMPER_TB_18 <- with(CWM_TB_18_Treatment,simper(CWM_TB_18_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
summary(SIMPER_TB_18)

#Run a SIMPER test comparing data 
SIMPER_TB_19 <- with(CWM_TB_19_Treatment,simper(CWM_TB_19_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
summary(SIMPER_TB_19)

#Run a SIMPER test comparing data 
SIMPER_TB_20 <- with(CWM_TB_20_Treatment,simper(CWM_TB_20_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
summary(SIMPER_TB_20)

#Run a SIMPER test comparing data 
SIMPER_TB_21 <- with(CWM_TB_21_Treatment,simper(CWM_TB_21_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
summary(SIMPER_TB_21)

#Run a SIMPER test comparing data 
SIMPER_TB_22 <- with(CWM_TB_22_Treatment,simper(CWM_TB_22_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
summary(SIMPER_TB_22)

#### Multivariate Functional Diversity Calculations ####

###FK
#create dataframe from the raw trait data where we subset FK data and then average across blocks, paddocks. then add species numbers 1-33 to assign to each species for future identification and analysis 
Avg_Traits_FK<-Traits_Clean_2 %>%
  filter(Site=="FK") %>% 
  group_by(Site,Genus_Species_Correct) %>% 
  summarise(
    Avg_height_cm=mean(height_cm,na.rm=T),
    Avg_percent_green=mean(percent_green,na.rm=T),
    Avg_leaf_thickness=mean(leaf_thickness_.mm.,na.rm=T),
    Avg_LDMC=mean(LDMC,na.rm=T),
    Avg_SLA=mean(SLA,na.rm=T),
    Avg_Lifespan=mean(lifespan_binary, na.rm=T),
    Avg_GrowthForm=mean(growth_form_binary, na.rm=T),
    Avg_Area=mean(Total.Area, na.rm=T)
  ) %>% 
  mutate(Sp_Num=c(1:33)) %>% 
  ungroup()

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data<-Avg_Traits_FK %>% 
  select(-Genus_Species_Correct,-Sp_Num,-Site) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data) <- c(1:33)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames<-Avg_Traits_FK %>% 
  select(Genus_Species_Correct,Sp_Num)


#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide<-Species_Comp_FK %>% 
  select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data<-Species_Comp_FK_Wide %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData<-Species_Comp_FK_Wide %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 


#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity <- dbFD(Avg_Traits_FK_Data, Species_Comp_FK_Wide_Data,corr = "none")
summary(FK_FunctionalDiversity)



### TB
##create dataframe from the raw trait data where wesubset TB data and then average across blocks, paddocks. then add species numbers 1-33 to assign to each species for future identification and analysis 
Avg_Traits_TB<-Traits_Clean_2 %>%
  filter(Site=="TB") %>% 
  group_by(Site,Genus_Species_Correct) %>% 
  summarise(
    Avg_height_cm=mean(height_cm,na.rm=T),
    Avg_percent_green=mean(percent_green,na.rm=T),
    Avg_leaf_thickness=mean(leaf_thickness_.mm.,na.rm=T),
    Avg_LDMC=mean(LDMC,na.rm=T),
    Avg_SLA=mean(SLA,na.rm=T),
    Avg_Lifespan=mean(lifespan_binary, na.rm=T),
    Avg_GrowthForm=mean(growth_form_binary, na.rm=T),
    Avg_Area=mean(Total.Area, na.rm=T)
  ) %>% 
  ungroup() %>% 
  filter(!Genus_Species_Correct %in% c("Oenothera.suffrtescuns")) %>% 
  mutate(Sp_Num=c(1:43))

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data<-Avg_Traits_TB %>% 
  select(-Genus_Species_Correct,-Sp_Num,-Site) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data) <- c(1:43)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames<-Avg_Traits_TB %>% 
  select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide<-Species_Comp_TB %>% 
  select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data<-Species_Comp_TB_Wide %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData<-Species_Comp_TB_Wide %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity <- dbFD(Avg_Traits_TB_Data, Species_Comp_TB_Wide_Data,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK<-as.data.frame(FK_FunctionalDiversity) %>% 
  cbind(Species_Comp_FK_Wide_PlotData)

Functional_Diversity_TB<-as.data.frame(TB_FunctionalDiversity) %>% 
  cbind(Species_Comp_TB_Wide_PlotData)

Functional_Diversity<-Functional_Diversity_FK %>% 
  rbind(Functional_Diversity_TB) %>% 
  separate(ID,c("year","Site","plot"), sep = "_") %>% 
  select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))

#### Multivariate Functional Metric Graphs ####

#### Mutlivariate Functional Richness Graphs ####
#Functional Richness - FK 2019
FRic_FK_19<-ggplot(subset(Functional_Diversity,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=6e-06)+
  annotate("text", x=8.5, y=6e-06, label = "FK 2019", size=20)

#Functional Richness - FK 2020
FRic_FK_20<-ggplot(subset(Functional_Diversity,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=6e-06)+
  annotate("text", x=8, y=6e-06, label = "FK 2020", size=20)


#Functional Richness - FK 2021
FRic_FK_21<-ggplot(subset(Functional_Diversity,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=6e-06)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=6e-06, label = "FK 2021", size=20)

#Functional Richness - FK 2022
FRic_FK_22<-ggplot(subset(Functional_Diversity,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=6e-06)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=6e-06, label = "FK 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Richness - TB 2019
FRic_TB_19<-ggplot(subset(Functional_Diversity,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=0.03)+
  annotate("text", x=8.5, y=0.03, label = "TB 2019", size=20)

#Functional Richness - TB 2020
FRic_TB_20<-ggplot(subset(Functional_Diversity,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=0.03)+
  annotate("text", x=8, y=0.03, label = "TB 2020", size=20)

#Functional Richness - TB 2021
FRic_TB_21<-ggplot(subset(Functional_Diversity,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=0.03)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=0.03, label = "TB 2021", size=20)

#Functional Richness - TB 2022
FRic_TB_22<-ggplot(subset(Functional_Diversity,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=0.03)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.03, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  


#### Multivariate Functional Evenness Graphs ####

#Functional Evenness - FK 2019
FEve_FK_19<-ggplot(subset(Functional_Diversity,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Evenness - FK 2020
FEve_FK_20<-ggplot(subset(Functional_Diversity,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Evenness - FK 2021
FEve_FK_21<-ggplot(subset(Functional_Diversity,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Evenness - FK 2022
FEve_FK_22<-ggplot(subset(Functional_Diversity,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FEve
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Evenness - TB 2019
FEve_TB_19<-ggplot(subset(Functional_Diversity,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Evenness - TB 2020
FEve_TB_20<-ggplot(subset(Functional_Diversity,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Evenness - TB 2021
FEve_TB_21<-ggplot(subset(Functional_Diversity,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Evenness - TB 2022
FEve_TB_22<-ggplot(subset(Functional_Diversity,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  


#### Multivariate Functional Diversity Graphs ####

#Functional Diversity - FK 2019
FDiv_FK_19<-ggplot(subset(Functional_Diversity,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FDiv,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Diversity")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Diversity - FK 2020
FDiv_FK_20<-ggplot(subset(Functional_Diversity,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FDiv,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Diversity")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Diversity - FK 2021
FDiv_FK_21<-ggplot(subset(Functional_Diversity,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FDiv)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("FunctionalDiversity")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Diversity - FK 2022
FDiv_FK_22<-ggplot(subset(Functional_Diversity,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FDiv)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Diversity")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FDiv
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDiv_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDiv_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDiv_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDiv_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Diversity - TB 2019
FDiv_TB_19<-ggplot(subset(Functional_Diversity,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FDiv,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Diversity")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Diversity - TB 2020
FDiv_TB_20<-ggplot(subset(Functional_Diversity,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FDiv)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Diversity")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Diversity - TB 2021
FDiv_TB_21<-ggplot(subset(Functional_Diversity,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FDiv)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Diversity")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Diversity - TB 2022
FDiv_TB_22<-ggplot(subset(Functional_Diversity,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FDiv)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Diversity")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FDiv
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDiv_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDiv_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDiv_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDiv_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  


#### Multivariate Functional Dispersion Graphs ####

#Functional Dispersion - FK 2019
FDis_FK_19<-ggplot(subset(Functional_Diversity,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,0.3))+
  annotate("text", x=8.5, y=0.3, label = "FK 2019", size=20)

#Functional Dispersion - FK 2020
FDis_FK_20<-ggplot(subset(Functional_Diversity,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,0.3))+
  annotate("text", x=8, y=0.3, label = "FK 2020", size=20)


#Functional Dispersion - FK 2021
FDis_FK_21<-ggplot(subset(Functional_Diversity,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,0.3))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=0.3, label = "FK 2021", size=20)

#Functional Dispersion - FK 2022
FDis_FK_22<-ggplot(subset(Functional_Diversity,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,0.3))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=0.3, label = "FK 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_FK_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_FK_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_FK_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Dispersion - TB 2019
FDis_TB_19<-ggplot(subset(Functional_Diversity,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,0.3))+
  annotate("text", x=8.5, y=0.3, label = "TB 2019", size=20)

#Functional Dispersion - TB 2020
FDis_TB_20<-ggplot(subset(Functional_Diversity,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,0.3))+
  annotate("text", x=8.5, y=0.3, label = "TB 2020", size=20)

#Functional Dispersion - TB 2021
FDis_TB_21<-ggplot(subset(Functional_Diversity,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,0.3))+
  annotate("text", x=8.5, y=0.3, label = "TB 2021", size=20)

#Functional Dispersion - TB 2022
FDis_TB_22<-ggplot(subset(Functional_Diversity,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,0.3))+
  annotate("text", x=8.5, y=0.3, label = "TB 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_TB_20,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_TB_21,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_TB_22,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

### Multivariate Functional Diversity Stats #### 


###Functional Richness

#Functional Richness (FRic) FK 18
FK_18_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FRiC_LMER, type = 3)

#Functional Richness (FRic) FK 19
FK_19_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="FK"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FRiC_LMER, type = 3)

#Functional Richness (FRic) FK 20
FK_20_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="FK"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FRiC_LMER, type = 3)

#Functional Richness (FRic) FK 21
FK_21_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FRiC_LMER, type = 3)

#Functional Richness (FRic) FK 22
FK_22_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FRiC_LMER, type = 3)

#Functional Richness (FRic) TB 18
TB_18_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FRiC_LMER, type = 3)

#Functional Richness (FRic) TB 19
TB_19_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="TB"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FRiC_LMER, type = 3)

#Functional Richness (FRic) TB 20
TB_20_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="TB"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FRiC_LMER, type = 3)

#Functional Richness (FRic) TB 21
TB_21_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FRiC_LMER, type = 3)

#Functional Richness (FRic) TB 22
TB_22_FRiC_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FRiC_LMER, type = 3)

###Functional Evenness

#Functional Evenness (FEve) FK 18
FK_18_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FEve_LMER, type = 3)

#Functional Evenness (FEve) FK 19
FK_19_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="FK"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FEve_LMER, type = 3)

#Functional Evenness (FEve) FK 20
FK_20_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="FK"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FEve_LMER, type = 3)

#Functional Evenness (FEve) FK 21
FK_21_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FEve_LMER, type = 3)
#post hoc test for lmer test
summary(glht(FK_21_FEve_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))

#Functional Evenness (FEve) FK 22
FK_22_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FEve_LMER, type = 3)

#Functional Evenness (FEve) TB 18
TB_18_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FEve_LMER, type = 3)

#Functional Evenness (FEve) TB 19
TB_19_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="TB"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FEve_LMER, type = 3)

#Functional Evenness (FEve) TB 20
TB_20_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="TB"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FEve_LMER, type = 3)
#post hoc test for lmer test
summary(glht(TB_20_FEve_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))

#Functional Evenness (FEve) TB 21
TB_21_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FEve_LMER, type = 3)

#Functional Evenness (FEve) TB 22
TB_22_FEve_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FEve_LMER, type = 3)
#post hoc test for lmer test
summary(glht(TB_22_FEve_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))

###Functional Diversity

#Functional Diversity (FDiv) FK 18
FK_18_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="FK"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDiv_LMER, type = 3)
#post hoc test for lmer test
summary(glht(FK_18_FDiv_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))

#Functional Diversity (FDiv) FK 19
FK_19_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="FK"), FDiv ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDiv_LMER, type = 3)

FK_20_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="FK"), FDiv ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDiv_LMER, type = 3)

#Functional Diversity (FDiv) FK 21
FK_21_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="FK"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDiv_LMER, type = 3)

#Functional Diversity (FDiv) FK 22
FK_22_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="FK"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDiv_LMER, type = 3)
#post hoc test for lmer test
summary(glht(FK_22_FDiv_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))

#Functional Diversity (FDiv) TB 18
TB_18_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="TB"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDiv_LMER, type = 3)

#Functional Diversity (FDiv) TB 19
TB_19_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="TB"), FDiv ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDiv_LMER, type = 3)

#Functional Diversity (FDiv) TB 20
TB_20_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="TB"), FDiv ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDiv_LMER, type = 3)

#Functional Diversity (FDiv) TB 21
TB_21_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="TB"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDiv_LMER, type = 3)

#Functional Diversity (FDiv) TB 22
TB_22_FDiv_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="TB"), FDiv ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDiv_LMER, type = 3)

###Functional Dispersion

#Functional Dispersion (FDis) FK 18
FK_18_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDis_LMER, type = 3)
#post hoc test for lmer test
summary(glht(FK_18_FDis_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER, type = 3)
#post hoc test for lmer test
summary(glht(FK_19_FDis_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))


#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER, type = 3)
#post hoc test for lmer test
summary(glht(FK_22_FDis_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH"))

#Functional Dispersion (FDis) TB 18
TB_18_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDis_LMER, type = 3)
#post hoc test for lmer test
summary(glht(TB_18_FDis_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))
#no significance

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER, type = 3)
#post hoc test for lmer test
summary(glht(TB_22_FDis_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH"))
#no significance

#### Single Trait Functional Metrics ####

#### Height Diversity Metrics ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data_Height<-Avg_Traits_FK %>% 
  select(Avg_height_cm) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_Height) <- c(1:33)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_Height<-Avg_Traits_FK %>% 
  select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_Height<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames_Height) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_Height<-Species_Comp_FK_Height %>% 
  select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_Height<-Species_Comp_FK_Wide_Height %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_Height<-Species_Comp_FK_Wide_Height %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_Height <- dbFD(Avg_Traits_FK_Data_Height, Species_Comp_FK_Wide_Data_Height,corr = "none")
summary(FK_FunctionalDiversity_Height)


#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_Height<-Avg_Traits_TB%>% 
  select(Avg_height_cm) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_Height) <- c(1:43)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_Height<-Avg_Traits_TB %>% 
  select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_Height<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames_Height) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_Height<-Species_Comp_TB_Height %>% 
  select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_Height<-Species_Comp_TB_Wide_Height %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_Height<-Species_Comp_TB_Wide_Height %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_Height <- dbFD(Avg_Traits_TB_Data_Height, Species_Comp_TB_Wide_Data_Height,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_Height<-as.data.frame(FK_FunctionalDiversity_Height) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_Height)

Functional_Diversity_TB_Height<-as.data.frame(TB_FunctionalDiversity_Height) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_Height)

Functional_Diversity_Height<-Functional_Diversity_FK_Height %>% 
  rbind(Functional_Diversity_TB_Height) %>% 
  separate(ID,c("year","Site","plot"), sep = "_") %>% 
  select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))

#### Height Functional Metric Graphs ####

#### Height Functional Richness Graphs ####
#Functional Richness - FK 2019
FRic_FK_19_Height<-ggplot(subset(Functional_Diversity_Height,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,5))+
  annotate("text", x=8.5, y=5, label = "FK 2019", size=20)

#Functional Richness - FK 2020
FRic_FK_20_Height<-ggplot(subset(Functional_Diversity_Height,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,5))+
  annotate("text", x=8, y=5, label = "FK 2020", size=20)


#Functional Richness - FK 2021
FRic_FK_21_Height<-ggplot(subset(Functional_Diversity_Height,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=5, label = "FK 2021", size=20)

#Functional Richness - FK 2022
FRic_FK_22_Height<-ggplot(subset(Functional_Diversity_Height,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "FK 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_FK_19_Height,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_FK_20_Height,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_FK_21_Height,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_FK_22_Height,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Richness - TB 2019
FRic_TB_19_Height<-ggplot(subset(Functional_Diversity_Height,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,5))+
  annotate("text", x=8.5, y=5, label = "TB 2019", size=20)

#Functional Richness - TB 2020
FRic_TB_20_Height<-ggplot(subset(Functional_Diversity_Height,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,5))+
  annotate("text", x=8, y=5, label = "TB 2020", size=20)

#Functional Richness - TB 2021
FRic_TB_21_Height<-ggplot(subset(Functional_Diversity_Height,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=5, label = "TB 2021", size=20)

#Functional Richness - TB 2022
FRic_TB_22_Height<-ggplot(subset(Functional_Diversity_Height,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_TB_19_Height,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_TB_20_Height,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_TB_21_Height,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_TB_22_Height,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  


#### Height Functional Evenness Graphs ####

#Functional Evenness - FK 2019
FEve_FK_19_Height<-ggplot(subset(Functional_Diversity_Height,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Evenness - FK 2020
FEve_FK_20_Height<-ggplot(subset(Functional_Diversity_Height,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Evenness - FK 2021
FEve_FK_21_Height<-ggplot(subset(Functional_Diversity_Height,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Evenness - FK 2022
FEve_FK_22_Height<-ggplot(subset(Functional_Diversity_Height,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FEve
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_FK_19_Height,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_FK_20_Height,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_FK_21_Height,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_FK_22_Height,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Evenness - TB 2019
FEve_TB_19_Height<-ggplot(subset(Functional_Diversity_Height,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Evenness - TB 2020
FEve_TB_20_Height<-ggplot(subset(Functional_Diversity_Height,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Evenness - TB 2021
FEve_TB_21_Height<-ggplot(subset(Functional_Diversity_Height,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Evenness - TB 2022
FEve_TB_22_Height<-ggplot(subset(Functional_Diversity_Height,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_TB_19_Height,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_TB_20_Height,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_TB_21_Height,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_TB_22_Height,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#### Height Functional Dispersion Graphs ####

#Functional Dispersion - FK 2019
FDis_FK_19_Height<-ggplot(subset(Functional_Diversity_Height,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Dispersion - FK 2020
FDis_FK_20_Height<-ggplot(subset(Functional_Diversity_Height,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Dispersion - FK 2021
FDis_FK_21_Height<-ggplot(subset(Functional_Diversity_Height,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Dispersion - FK 2022
FDis_FK_22_Height<-ggplot(subset(Functional_Diversity_Height,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_FK_19_Height,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_FK_20_Height,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_FK_21_Height,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_FK_22_Height,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Dispersion - TB 2019
FDis_TB_19_Height<-ggplot(subset(Functional_Diversity_Height,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Dispersion - TB 2020
FDis_TB_20_Height<-ggplot(subset(Functional_Diversity_Height,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Dispersion - TB 2021
FDis_TB_21_Height<-ggplot(subset(Functional_Diversity_Height,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Dispersion - TB 2022
FDis_TB_22_Height<-ggplot(subset(Functional_Diversity_Height,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_TB_19_Height,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_TB_20_Height,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_TB_21_Height,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_TB_22_Height,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

### Height Functional Diversity Stats #### 

###Functional Richness

#Functional Richness (FRic) FK 18
FK_18_FRiC_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2018&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FRiC_LMER_Height, type = 3)

#Functional Richness (FRic) FK 19
FK_19_FRiC_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2019&Site=="FK"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FRiC_LMER_Height, type = 3)

#Functional Richness (FRic) FK 20
FK_20_FRiC_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2020&Site=="FK"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FRiC_LMER_Height, type = 3)

#Functional Richness (FRic) FK 21
FK_21_FRiC_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2021&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FRiC_LMER_Height, type = 3)

#Functional Richness (FRic) FK 22
FK_22_FRiC_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2022&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FRiC_LMER_Height, type = 3)

#Functional Richness (FRic) TB 18
TB_18_FRiC_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2018&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FRiC_LMER_Height, type = 3)

#Functional Richness (FRic) TB 19
TB_19_FRiC_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2019&Site=="TB"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FRiC_LMER_Height, type = 3)

#Functional Richness (FRic) TB 20
TB_20_FRiC_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2020&Site=="TB"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FRiC_LMER_Height, type = 3)

#Functional Richness (FRic) TB 21
TB_21_FRiC_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2021&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FRiC_LMER_Height, type = 3)

#Functional Richness (FRic) TB 22
TB_22_FRiC_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2022&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FRiC_LMER_Height, type = 3)

###Functional Evenness

#Functional Evenness (FEve) FK 18
FK_18_FEve_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2018&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FEve_LMER_Height, type = 3)

#Functional Evenness (FEve) FK 19
FK_19_FEve_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2019&Site=="FK"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FEve_LMER_Height, type = 3)

#Functional Evenness (FEve) FK 20
FK_20_FEve_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2020&Site=="FK"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FEve_LMER_Height, type = 3)

#Functional Evenness (FEve) FK 21
FK_21_FEve_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2021&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FEve_LMER_Height, type = 3)

#Functional Evenness (FEve) FK 22
FK_22_FEve_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2022&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FEve_LMER_Height, type = 3)

#Functional Evenness (FEve) TB 18
TB_18_FEve_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2018&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FEve_LMER_Height, type = 3)

#Functional Evenness (FEve) TB 19
TB_19_FEve_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2019&Site=="TB"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FEve_LMER_Height, type = 3)

#Functional Evenness (FEve) TB 20
TB_20_FEve_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2020&Site=="TB"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FEve_LMER_Height, type = 3)

#Functional Evenness (FEve) TB 21
TB_21_FEve_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2021&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FEve_LMER_Height, type = 3)

#Functional Evenness (FEve) TB 22
TB_22_FEve_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2022&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FEve_LMER_Height, type = 3)

###Functional Dispersion

#Functional Dispersion (FDis) FK 18
FK_18_FDis_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2018&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDis_LMER_Height, type = 3)

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_Height, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_Height, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_Height, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_Height, type = 3)

#Functional Dispersion (FDis) TB 18
TB_18_FDis_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2018&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDis_LMER_Height, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_Height, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_Height, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_Height, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_Height <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_Height, type = 3)

#### SLA Diversity Metrics ####

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_FK_Data_SLA_1<-Avg_Traits_FK %>% 
  select(Avg_SLA,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:31))

Avg_Traits_FK_Data_SLA<-Avg_Traits_FK_Data_SLA_1 %>% 
  select(Avg_SLA) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_SLA) <- c(1:31)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_SLA<-Avg_Traits_FK_Data_SLA_1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_SLA<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames_SLA) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>% 
  filter(Genus_Species_Correct!="	
Hedeoma.hispida" & Genus_Species_Correct!="Linum.rigidum")

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_SLA<-Species_Comp_FK_SLA %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_SLA<-Species_Comp_FK_Wide_SLA %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_SLA<-Species_Comp_FK_Wide_SLA %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_SLA <- dbFD(Avg_Traits_FK_Data_SLA, Species_Comp_FK_Wide_Data_SLA,corr = "none")

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_SLA_1<-Avg_Traits_TB %>% 
  select(Avg_SLA,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:36))

Avg_Traits_TB_Data_SLA<-Avg_Traits_TB_Data_SLA_1 %>% 
  select(Avg_SLA) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_SLA) <- c(1:36)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_SLA<-Avg_Traits_TB_Data_SLA_1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_SLA<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames_SLA) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>%
  filter(Genus_Species_Correct!="	
Aristida.purpurea" & Genus_Species_Correct!="Artemisia.frigida"& Genus_Species_Correct!="Artemisia.tridentata"& Genus_Species_Correct!="Bouteloua.gracilis"& Genus_Species_Correct!="Elymus.elymoides"& Genus_Species_Correct!="Gutierrezia.sarothrae"& Genus_Species_Correct!="Hedeoma.hispida")

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_SLA<-Species_Comp_TB_SLA %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_SLA<-Species_Comp_TB_Wide_SLA %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_SLA<-Species_Comp_TB_Wide_SLA %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_SLA <- dbFD(Avg_Traits_TB_Data_SLA, Species_Comp_TB_Wide_Data_SLA,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_SLA<-as.data.frame(FK_FunctionalDiversity_SLA) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_SLA)

Functional_Diversity_TB_SLA<-as.data.frame(TB_FunctionalDiversity_SLA) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_SLA)

Functional_Diversity_SLA<-Functional_Diversity_FK_SLA %>% 
  rbind(Functional_Diversity_TB_SLA) %>% 
  separate(ID,c("year","Site","plot"), sep = "_") %>% 
  select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))

#### SLA Functional Metric Graphs ####

#### SLA Functional Richness Graphs ####
#Functional Richness - FK 2019
FRic_FK_19_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,5))+
  annotate("text", x=8.5, y=5, label = "FK 2019", size=20)

#Functional Richness - FK 2020
FRic_FK_20_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,5))+
  annotate("text", x=8, y=5, label = "FK 2020", size=20)


#Functional Richness - FK 2021
FRic_FK_21_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=5, label = "FK 2021", size=20)

#Functional Richness - FK 2022
FRic_FK_22_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "FK 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_FK_19_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_FK_20_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_FK_21_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_FK_22_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Richness - TB 2019
FRic_TB_19_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,5))+
  annotate("text", x=8.5, y=5, label = "TB 2019", size=20)

#Functional Richness - TB 2020
FRic_TB_20_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,5))+
  annotate("text", x=8, y=5, label = "TB 2020", size=20)

#Functional Richness - TB 2021
FRic_TB_21_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=5, label = "TB 2021", size=20)

#Functional Richness - TB 2022
FRic_TB_22_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_TB_19_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_TB_20_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_TB_21_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_TB_22_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  


#### SLA Functional Evenness Graphs ####

#Functional Evenness - FK 2019
FEve_FK_19_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Evenness - FK 2020
FEve_FK_20_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Evenness - FK 2021
FEve_FK_21_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Evenness - FK 2022
FEve_FK_22_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FEve
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_FK_19_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_FK_20_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_FK_21_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_FK_22_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Evenness - TB 2019
FEve_TB_19_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Evenness - TB 2020
FEve_TB_20_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Evenness - TB 2021
FEve_TB_21_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Evenness - TB 2022
FEve_TB_22_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_TB_19_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_TB_20_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_TB_21_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_TB_22_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#### SLA Functional Dispersion Graphs ####

#Functional Dispersion - FK 2019
FDis_FK_19_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,2))+
  annotate("text", x=8.5, y=2, label = "FK 2019", size=20)

#Functional Dispersion - FK 2020
FDis_FK_20_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,2))+
  annotate("text", x=8, y=2, label = "FK 2020", size=20)


#Functional Dispersion - FK 2021
FDis_FK_21_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=2, label = "FK 2021", size=20)

#Functional Dispersion - FK 2022
FDis_FK_22_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,2))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=2, label = "FK 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_FK_19_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_FK_20_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_FK_21_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_FK_22_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Dispersion - TB 2019
FDis_TB_19_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,2))+
  annotate("text", x=8.5, y=2, label = "TB 2019", size=20)

#Functional Dispersion - TB 2020
FDis_TB_20_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,2))+
  annotate("text", x=8.5, y=2, label = "TB 2020", size=20)

#Functional Dispersion - TB 2021
FDis_TB_21_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,2))+
  annotate("text", x=8.5, y=2, label = "TB 2021", size=20)

#Functional Dispersion - TB 2022
FDis_TB_22_SLA<-ggplot(subset(Functional_Diversity_SLA,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,2))+
  annotate("text", x=8.5, y=2, label = "TB 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_TB_19_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_TB_20_SLA,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_TB_21_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_TB_22_SLA,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

### SLA Functional Diversity Stats #### 

###Functional Richness

#Functional Richness (FRic) FK 18
FK_18_FRiC_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2018&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FRiC_LMER_SLA, type = 3)

#Functional Richness (FRic) FK 19
FK_19_FRiC_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2019&Site=="FK"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FRiC_LMER_SLA, type = 3)

#Functional Richness (FRic) FK 20
FK_20_FRiC_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2020&Site=="FK"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FRiC_LMER_SLA, type = 3)

#Functional Richness (FRic) FK 21
FK_21_FRiC_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2021&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FRiC_LMER_SLA, type = 3)

#Functional Richness (FRic) FK 22
FK_22_FRiC_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2022&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FRiC_LMER_SLA, type = 3)

#Functional Richness (FRic) TB 18
TB_18_FRiC_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2018&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FRiC_LMER_SLA, type = 3)

#Functional Richness (FRic) TB 19
TB_19_FRiC_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2019&Site=="TB"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FRiC_LMER_SLA, type = 3)

#Functional Richness (FRic) TB 20
TB_20_FRiC_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2020&Site=="TB"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FRiC_LMER_SLA, type = 3)

#Functional Richness (FRic) TB 21
TB_21_FRiC_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2021&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FRiC_LMER_SLA, type = 3)

#Functional Richness (FRic) TB 22
TB_22_FRiC_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2022&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FRiC_LMER_SLA, type = 3)

###Functional Evenness

#Functional Evenness (FEve) FK 18
FK_18_FEve_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2018&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FEve_LMER_SLA, type = 3)

#Functional Evenness (FEve) FK 19
FK_19_FEve_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2019&Site=="FK"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FEve_LMER_SLA, type = 3)

#Functional Evenness (FEve) FK 20
FK_20_FEve_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2020&Site=="FK"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FEve_LMER_SLA, type = 3)

#Functional Evenness (FEve) FK 21
FK_21_FEve_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2021&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FEve_LMER_SLA, type = 3)

#Functional Evenness (FEve) FK 22
FK_22_FEve_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2022&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FEve_LMER_SLA, type = 3)

#Functional Evenness (FEve) TB 18
TB_18_FEve_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2018&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FEve_LMER_SLA, type = 3)

#Functional Evenness (FEve) TB 19
TB_19_FEve_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2019&Site=="TB"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FEve_LMER_SLA, type = 3)

#Functional Evenness (FEve) TB 20
TB_20_FEve_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2020&Site=="TB"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FEve_LMER_SLA, type = 3)

#Functional Evenness (FEve) TB 21
TB_21_FEve_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2021&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FEve_LMER_SLA, type = 3)

#Functional Evenness (FEve) TB 22
TB_22_FEve_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2022&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FEve_LMER_SLA, type = 3)

###Functional Dispersion

#Functional Dispersion (FDis) FK 18
FK_18_FDis_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2018&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDis_LMER_SLA, type = 3)

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_SLA, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_SLA, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_SLA, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_SLA, type = 3)

#Functional Dispersion (FDis) TB 18
TB_18_FDis_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2018&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDis_LMER_SLA, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_SLA, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_SLA, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_SLA, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_SLA <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_SLA, type = 3)

#### percent_green Diversity Metrics ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data_percent_green<-Avg_Traits_FK %>% 
  select(Avg_percent_green) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_percent_green) <- c(1:33)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_percent_green<-Avg_Traits_FK %>% 
  select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_percent_green<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames_percent_green) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_percent_green<-Species_Comp_FK_percent_green %>% 
  select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_percent_green<-Species_Comp_FK_Wide_percent_green %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_percent_green<-Species_Comp_FK_Wide_percent_green %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_percent_green <- dbFD(Avg_Traits_FK_Data_percent_green, Species_Comp_FK_Wide_Data_percent_green,corr = "none")

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_percent_green<-Avg_Traits_TB%>% 
  select(Avg_percent_green) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_percent_green) <- c(1:43)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_percent_green<-Avg_Traits_TB %>% 
  select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_percent_green<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames_percent_green) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_percent_green<-Species_Comp_TB_percent_green %>% 
  select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_percent_green<-Species_Comp_TB_Wide_percent_green %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_percent_green<-Species_Comp_TB_Wide_percent_green %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_percent_green <- dbFD(Avg_Traits_TB_Data_percent_green, Species_Comp_TB_Wide_Data_percent_green,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_percent_green<-as.data.frame(FK_FunctionalDiversity_percent_green) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_percent_green)

Functional_Diversity_TB_percent_green<-as.data.frame(TB_FunctionalDiversity_percent_green) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_percent_green)

Functional_Diversity_percent_green<-Functional_Diversity_FK_percent_green %>% 
  rbind(Functional_Diversity_TB_percent_green) %>% 
  separate(ID,c("year","Site","plot"), sep = "_") %>% 
  select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))

#### percent_green Functional Metric Graphs ####

#### percent_green Functional Richness Graphs ####
#Functional Richness - FK 2019
FRic_FK_19_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,5))+
  annotate("text", x=8.5, y=5, label = "FK 2019", size=20)

#Functional Richness - FK 2020
FRic_FK_20_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,5))+
  annotate("text", x=8, y=5, label = "FK 2020", size=20)


#Functional Richness - FK 2021
FRic_FK_21_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=5, label = "FK 2021", size=20)

#Functional Richness - FK 2022
FRic_FK_22_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "FK 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_FK_19_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_FK_20_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_FK_21_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_FK_22_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Richness - TB 2019
FRic_TB_19_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,5))+
  annotate("text", x=8.5, y=5, label = "TB 2019", size=20)

#Functional Richness - TB 2020
FRic_TB_20_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,5))+
  annotate("text", x=8, y=5, label = "TB 2020", size=20)

#Functional Richness - TB 2021
FRic_TB_21_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=5, label = "TB 2021", size=20)

#Functional Richness - TB 2022
FRic_TB_22_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_TB_19_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_TB_20_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_TB_21_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_TB_22_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  


#### percent_green Functional Evenness Graphs ####

#Functional Evenness - FK 2019
FEve_FK_19_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Evenness - FK 2020
FEve_FK_20_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Evenness - FK 2021
FEve_FK_21_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Evenness - FK 2022
FEve_FK_22_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FEve
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_FK_19_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_FK_20_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_FK_21_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_FK_22_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Evenness - TB 2019
FEve_TB_19_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Evenness - TB 2020
FEve_TB_20_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Evenness - TB 2021
FEve_TB_21_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Evenness - TB 2022
FEve_TB_22_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_TB_19_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_TB_20_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_TB_21_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_TB_22_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#### percent_green Functional Dispersion Graphs ####

#Functional Dispersion - FK 2019
FDis_FK_19_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Dispersion - FK 2020
FDis_FK_20_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Dispersion - FK 2021
FDis_FK_21_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Dispersion - FK 2022
FDis_FK_22_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_FK_19_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_FK_20_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_FK_21_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_FK_22_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Dispersion - TB 2019
FDis_TB_19_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Dispersion - TB 2020
FDis_TB_20_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Dispersion - TB 2021
FDis_TB_21_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Dispersion - TB 2022
FDis_TB_22_percent_green<-ggplot(subset(Functional_Diversity_percent_green,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_TB_19_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_TB_20_percent_green,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_TB_21_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_TB_22_percent_green,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

### percent_green Functional Diversity Stats #### 

###Functional Richness

#Functional Richness (FRic) FK 18
FK_18_FRiC_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2018&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FRiC_LMER_percent_green, type = 3)

#Functional Richness (FRic) FK 19
FK_19_FRiC_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2019&Site=="FK"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FRiC_LMER_percent_green, type = 3)

#Functional Richness (FRic) FK 20
FK_20_FRiC_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2020&Site=="FK"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FRiC_LMER_percent_green, type = 3)

#Functional Richness (FRic) FK 21
FK_21_FRiC_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2021&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FRiC_LMER_percent_green, type = 3)

#Functional Richness (FRic) FK 22
FK_22_FRiC_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2022&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FRiC_LMER_percent_green, type = 3)

#Functional Richness (FRic) TB 18
TB_18_FRiC_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2018&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FRiC_LMER_percent_green, type = 3)

#Functional Richness (FRic) TB 19
TB_19_FRiC_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2019&Site=="TB"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FRiC_LMER_percent_green, type = 3)

#Functional Richness (FRic) TB 20
TB_20_FRiC_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2020&Site=="TB"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FRiC_LMER_percent_green, type = 3)

#Functional Richness (FRic) TB 21
TB_21_FRiC_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2021&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FRiC_LMER_percent_green, type = 3)

#Functional Richness (FRic) TB 22
TB_22_FRiC_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2022&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FRiC_LMER_percent_green, type = 3)

###Functional Evenness

#Functional Evenness (FEve) FK 18
FK_18_FEve_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2018&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FEve_LMER_percent_green, type = 3)

#Functional Evenness (FEve) FK 19
FK_19_FEve_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2019&Site=="FK"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FEve_LMER_percent_green, type = 3)

#Functional Evenness (FEve) FK 20
FK_20_FEve_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2020&Site=="FK"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FEve_LMER_percent_green, type = 3)

#Functional Evenness (FEve) FK 21
FK_21_FEve_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2021&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FEve_LMER_percent_green, type = 3)

#Functional Evenness (FEve) FK 22
FK_22_FEve_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2022&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FEve_LMER_percent_green, type = 3)

#Functional Evenness (FEve) TB 18
TB_18_FEve_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2018&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FEve_LMER_percent_green, type = 3)

#Functional Evenness (FEve) TB 19
TB_19_FEve_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2019&Site=="TB"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FEve_LMER_percent_green, type = 3)

#Functional Evenness (FEve) TB 20
TB_20_FEve_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2020&Site=="TB"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FEve_LMER_percent_green, type = 3)

#Functional Evenness (FEve) TB 21
TB_21_FEve_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2021&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FEve_LMER_percent_green, type = 3)

#Functional Evenness (FEve) TB 22
TB_22_FEve_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2022&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FEve_LMER_percent_green, type = 3)

###Functional Dispersion

#Functional Dispersion (FDis) FK 18
FK_18_FDis_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2018&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDis_LMER_percent_green, type = 3)

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_percent_green, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_percent_green, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_percent_green, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_percent_green, type = 3)

#Functional Dispersion (FDis) TB 18
TB_18_FDis_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2018&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDis_LMER_percent_green, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_percent_green, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_percent_green, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_percent_green, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_percent_green <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_percent_green, type = 3)

#### Plant_Biomass Diversity Metrics ####

Avg_Traits_FK_Data_Plant_Biomass_1<-Avg_Traits_FK %>% 
  select(Sqrt_biomass,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:31))

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data_Plant_Biomass<-Avg_Traits_FK_Data_Plant_Biomass_1 %>%
  select(Sqrt_biomass) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_Plant_Biomass) <- c(1:31)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_Plant_Biomass<-Avg_Traits_FK_Data_Plant_Biomass_1%>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_Plant_Biomass<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames_Plant_Biomass) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))%>% 
  filter(Genus_Species_Correct!="	
Hedeoma.hispida" & Genus_Species_Correct!="Linum.rigidum")

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_Plant_Biomass<-Species_Comp_FK_Plant_Biomass %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_Plant_Biomass<-Species_Comp_FK_Wide_Plant_Biomass %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_Plant_Biomass<-Species_Comp_FK_Wide_Plant_Biomass %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_Plant_Biomass <- dbFD(Avg_Traits_FK_Data_Plant_Biomass, Species_Comp_FK_Wide_Data_Plant_Biomass,corr = "none")

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_Plant_Biomass_1<-Avg_Traits_TB %>% 
  select(Sqrt_biomass,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:41))

Avg_Traits_TB_Data_Plant_Biomass<-Avg_Traits_TB_Data_Plant_Biomass_1%>% 
  select(Sqrt_biomass) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_Plant_Biomass) <- c(1:41)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_Plant_Biomass<-Avg_Traits_TB_Data_Plant_Biomass_1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_Plant_Biomass<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames_Plant_Biomass) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>% 
  filter(Genus_Species_Correct!="	
Hedeoma.hispida" & Genus_Species_Correct!="Elymus.elymoides")

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_Plant_Biomass<-Species_Comp_TB_Plant_Biomass %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_Plant_Biomass<-Species_Comp_TB_Wide_Plant_Biomass %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_Plant_Biomass<-Species_Comp_TB_Wide_Plant_Biomass %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_Plant_Biomass <- dbFD(Avg_Traits_TB_Data_Plant_Biomass, Species_Comp_TB_Wide_Data_Plant_Biomass,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_Plant_Biomass<-as.data.frame(FK_FunctionalDiversity_Plant_Biomass) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_Plant_Biomass)

Functional_Diversity_TB_Plant_Biomass<-as.data.frame(TB_FunctionalDiversity_Plant_Biomass) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_Plant_Biomass)

Functional_Diversity_Plant_Biomass<-Functional_Diversity_FK_Plant_Biomass %>% 
  rbind(Functional_Diversity_TB_Plant_Biomass) %>% 
  separate(ID,c("year","Site","plot"), sep = "_") %>% 
  select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))

#### Plant_Biomass Functional Metric Graphs ####

#### Plant_Biomass Functional Richness Graphs ####
#Functional Richness - FK 2019
FRic_FK_19_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,5))+
  annotate("text", x=8.5, y=5, label = "FK 2019", size=20)

#Functional Richness - FK 2020
FRic_FK_20_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,5))+
  annotate("text", x=8, y=5, label = "FK 2020", size=20)


#Functional Richness - FK 2021
FRic_FK_21_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=5, label = "FK 2021", size=20)

#Functional Richness - FK 2022
FRic_FK_22_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "FK 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_FK_19_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_FK_20_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_FK_21_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_FK_22_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Richness - TB 2019
FRic_TB_19_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,5))+
  annotate("text", x=8.5, y=5, label = "TB 2019", size=20)

#Functional Richness - TB 2020
FRic_TB_20_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,5))+
  annotate("text", x=8, y=5, label = "TB 2020", size=20)

#Functional Richness - TB 2021
FRic_TB_21_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=5, label = "TB 2021", size=20)

#Functional Richness - TB 2022
FRic_TB_22_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_TB_19_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_TB_20_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_TB_21_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_TB_22_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  


#### Plant_Biomass Functional Evenness Graphs ####

#Functional Evenness - FK 2019
FEve_FK_19_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Evenness - FK 2020
FEve_FK_20_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Evenness - FK 2021
FEve_FK_21_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Evenness - FK 2022
FEve_FK_22_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FEve
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_FK_19_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_FK_20_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_FK_21_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_FK_22_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Evenness - TB 2019
FEve_TB_19_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Evenness - TB 2020
FEve_TB_20_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Evenness - TB 2021
FEve_TB_21_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Evenness - TB 2022
FEve_TB_22_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_TB_19_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_TB_20_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_TB_21_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_TB_22_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#### Plant_Biomass Functional Dispersion Graphs ####

#Functional Dispersion - FK 2019
FDis_FK_19_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1.5))+
  annotate("text", x=8.5, y=1.5, label = "FK 2019", size=20)

#Functional Dispersion - FK 2020
FDis_FK_20_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1.5))+
  annotate("text", x=8, y=1.5, label = "FK 2020", size=20)


#Functional Dispersion - FK 2021
FDis_FK_21_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1.5, label = "FK 2021", size=20)

#Functional Dispersion - FK 2022
FDis_FK_22_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1.5, label = "FK 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_FK_19_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_FK_20_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_FK_21_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_FK_22_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Dispersion - TB 2019
FDis_TB_19_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Dispersion - TB 2020
FDis_TB_20_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Dispersion - TB 2021
FDis_TB_21_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Dispersion - TB 2022
FDis_TB_22_Plant_Biomass<-ggplot(subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_TB_19_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_TB_20_Plant_Biomass,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_TB_21_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_TB_22_Plant_Biomass,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

### Plant_Biomass Functional Diversity Stats #### 

###Functional Richness

#Functional Richness (FRic) FK 18
FK_18_FRiC_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2018&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FRiC_LMER_Plant_Biomass, type = 3)

#Functional Richness (FRic) FK 19
FK_19_FRiC_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="FK"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FRiC_LMER_Plant_Biomass, type = 3)

#Functional Richness (FRic) FK 20
FK_20_FRiC_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="FK"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FRiC_LMER_Plant_Biomass, type = 3)

#Functional Richness (FRic) FK 21
FK_21_FRiC_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FRiC_LMER_Plant_Biomass, type = 3)

#Functional Richness (FRic) FK 22
FK_22_FRiC_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FRiC_LMER_Plant_Biomass, type = 3)

#Functional Richness (FRic) TB 18
TB_18_FRiC_LMER_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2018&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FRiC_LMER_Biomass, type = 3)

#Functional Richness (FRic) TB 19
TB_19_FRiC_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="TB"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FRiC_LMER_Plant_Biomass, type = 3)

#Functional Richness (FRic) TB 20
TB_20_FRiC_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="TB"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FRiC_LMER_Plant_Biomass, type = 3)

#Functional Richness (FRic) TB 21
TB_21_FRiC_LMER_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FRiC_LMER_Biomass, type = 3)

#Functional Richness (FRic) TB 22
TB_22_FRiC_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FRiC_LMER_Plant_Biomass, type = 3)

###Functional Evenness

#Functional Evenness (FEve) FK 18
FK_18_FEve_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2018&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FEve_LMER_Plant_Biomass, type = 3)

#Functional Evenness (FEve) FK 19
FK_19_FEve_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="FK"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FEve_LMER_Plant_Biomass, type = 3)

#Functional Evenness (FEve) FK 20
FK_20_FEve_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="FK"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FEve_LMER_Plant_Biomass, type = 3)

#Functional Evenness (FEve) FK 21
FK_21_FEve_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FEve_LMER_Plant_Biomass, type = 3)

#Functional Evenness (FEve) FK 22
FK_22_FEve_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FEve_LMER_Plant_Biomass, type = 3)

#Functional Evenness (FEve) TB 18
TB_18_FEve_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2018&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FEve_LMER_Plant_Biomass, type = 3)

#Functional Evenness (FEve) TB 19
TB_19_FEve_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="TB"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FEve_LMER_Plant_Biomass, type = 3)

#Functional Evenness (FEve) TB 20
TB_20_FEve_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="TB"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FEve_LMER_Plant_Biomass, type = 3)

#Functional Evenness (FEve) TB 21
TB_21_FEve_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FEve_LMER_Plant_Biomass, type = 3)

#Functional Evenness (FEve) TB 22
TB_22_FEve_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FEve_LMER_Plant_Biomass, type = 3)

###Functional Dispersion

#Functional Dispersion (FDis) FK 18
FK_18_FDis_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2018&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDis_LMER_Plant_Biomass, type = 3)

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_Plant_Biomass, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_Plant_Biomass, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_Plant_Biomass, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_Plant_Biomass, type = 3)

#Functional Dispersion (FDis) TB 18
TB_18_FDis_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2018&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDis_LMER_Plant_Biomass, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_Plant_Biomass, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_Plant_Biomass, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_Plant_Biomass, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_Plant_Biomass <- lmerTest::lmer(data = subset(Functional_Diversity_Plant_Biomass,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_Plant_Biomass, type = 3)

#### leaf_thickness_.mm. Diversity Metrics ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data_leaf_thickness_.mm._1<-Avg_Traits_FK %>% 
  select(Avg_leaf_thickness,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:32))

Avg_Traits_FK_Data_leaf_thickness_.mm.<-Avg_Traits_FK_Data_leaf_thickness_.mm._1 %>% 
  select(Avg_leaf_thickness) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_leaf_thickness_.mm.) <- c(1:32)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_leaf_thickness_.mm.<-Avg_Traits_FK_Data_leaf_thickness_.mm._1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_leaf_thickness_.mm.<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames_leaf_thickness_.mm.) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>% 
  filter(Genus_Species_Correct!="Linum.rigidum")

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_leaf_thickness_.mm.<-Species_Comp_FK_leaf_thickness_.mm. %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_leaf_thickness_.mm.<-Species_Comp_FK_Wide_leaf_thickness_.mm. %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_leaf_thickness_.mm.<-Species_Comp_FK_Wide_leaf_thickness_.mm. %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_leaf_thickness_.mm. <- dbFD(Avg_Traits_FK_Data_leaf_thickness_.mm., Species_Comp_FK_Wide_Data_leaf_thickness_.mm.,corr = "none")

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_leaf_thickness_.mm._1<-Avg_Traits_TB %>% 
  select(Avg_leaf_thickness,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:42))

Avg_Traits_TB_Data_leaf_thickness_.mm.<-Avg_Traits_TB_Data_leaf_thickness_.mm._1%>% 
  select(Avg_leaf_thickness) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_leaf_thickness_.mm.) <- c(1:42)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_leaf_thickness_.mm.<-Avg_Traits_TB_Data_leaf_thickness_.mm._1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_leaf_thickness_.mm.<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames_leaf_thickness_.mm.) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>% 
  filter(Genus_Species_Correct!="Elymus.elymoides")

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_leaf_thickness_.mm.<-Species_Comp_TB_leaf_thickness_.mm. %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_leaf_thickness_.mm.<-Species_Comp_TB_Wide_leaf_thickness_.mm. %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_leaf_thickness_.mm.<-Species_Comp_TB_Wide_leaf_thickness_.mm. %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_leaf_thickness_.mm. <- dbFD(Avg_Traits_TB_Data_leaf_thickness_.mm., Species_Comp_TB_Wide_Data_leaf_thickness_.mm.,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_leaf_thickness_.mm.<-as.data.frame(FK_FunctionalDiversity_leaf_thickness_.mm.) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_leaf_thickness_.mm.)

Functional_Diversity_TB_leaf_thickness_.mm.<-as.data.frame(TB_FunctionalDiversity_leaf_thickness_.mm.) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_leaf_thickness_.mm.)

Functional_Diversity_leaf_thickness_.mm.<-Functional_Diversity_FK_leaf_thickness_.mm. %>% 
  rbind(Functional_Diversity_TB_leaf_thickness_.mm.) %>% 
  separate(ID,c("year","Site","plot"), sep = "_") %>% 
  select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))

#### leaf_thickness_.mm. Functional Metric Graphs ####

#### leaf_thickness_.mm. Functional Richness Graphs ####
#Functional Richness - FK 2019
FRic_FK_19_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,5))+
  annotate("text", x=8.5, y=5, label = "FK 2019", size=20)

#Functional Richness - FK 2020
FRic_FK_20_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,5))+
  annotate("text", x=8, y=5, label = "FK 2020", size=20)


#Functional Richness - FK 2021
FRic_FK_21_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=5, label = "FK 2021", size=20)

#Functional Richness - FK 2022
FRic_FK_22_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "FK 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_FK_19_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_FK_20_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_FK_21_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_FK_22_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Richness - TB 2019
FRic_TB_19_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,5))+
  annotate("text", x=8.5, y=5, label = "TB 2019", size=20)

#Functional Richness - TB 2020
FRic_TB_20_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,5))+
  annotate("text", x=8, y=5, label = "TB 2020", size=20)

#Functional Richness - TB 2021
FRic_TB_21_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=5, label = "TB 2021", size=20)

#Functional Richness - TB 2022
FRic_TB_22_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=5, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_TB_19_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_TB_20_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_TB_21_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_TB_22_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  


#### leaf_thickness_.mm. Functional Evenness Graphs ####

#Functional Evenness - FK 2019
FEve_FK_19_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Evenness - FK 2020
FEve_FK_20_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Evenness - FK 2021
FEve_FK_21_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Evenness - FK 2022
FEve_FK_22_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FEve
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_FK_19_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_FK_20_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_FK_21_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_FK_22_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Evenness - TB 2019
FEve_TB_19_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Evenness - TB 2020
FEve_TB_20_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Evenness - TB 2021
FEve_TB_21_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Evenness - TB 2022
FEve_TB_22_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_TB_19_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_TB_20_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_TB_21_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_TB_22_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#### leaf_thickness_.mm. Functional Dispersion Graphs ####

#Functional Dispersion - FK 2019
FDis_FK_19_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Dispersion - FK 2020
FDis_FK_20_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Dispersion - FK 2021
FDis_FK_21_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Dispersion - FK 2022
FDis_FK_22_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_FK_19_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_FK_20_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_FK_21_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_FK_22_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Dispersion - TB 2019
FDis_TB_19_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Dispersion - TB 2020
FDis_TB_20_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Dispersion - TB 2021
FDis_TB_21_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Dispersion - TB 2022
FDis_TB_22_leaf_thickness_.mm.<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FDis,color=grazing_treatment)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_TB_19_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_TB_20_leaf_thickness_.mm.,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_TB_21_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_TB_22_leaf_thickness_.mm.,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

### leaf_thickness_.mm. Functional Diversity Stats #### 

###Functional Richness

#Functional Richness (FRic) FK 18
FK_18_FRiC_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2018&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FRiC_LMER_leaf_thickness_.mm., type = 3)

#Functional Richness (FRic) FK 19
FK_19_FRiC_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="FK"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FRiC_LMER_leaf_thickness_.mm., type = 3)

#Functional Richness (FRic) FK 20
FK_20_FRiC_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="FK"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FRiC_LMER_leaf_thickness_.mm., type = 3)

#Functional Richness (FRic) FK 21
FK_21_FRiC_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FRiC_LMER_leaf_thickness_.mm., type = 3)

#Functional Richness (FRic) FK 22
FK_22_FRiC_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FRiC_LMER_leaf_thickness_.mm., type = 3)

#Functional Richness (FRic) TB 18
TB_18_FRiC_LMER_leaf_thickness_.mm <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2018&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FRiC_LMER_leaf_thickness_.mm, type = 3)

#Functional Richness (FRic) TB 19
TB_19_FRiC_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="TB"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FRiC_LMER_leaf_thickness_.mm., type = 3)

#Functional Richness (FRic) TB 20
TB_20_FRiC_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="TB"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FRiC_LMER_leaf_thickness_.mm., type = 3)

#Functional Richness (FRic) TB 21
TB_21_FRiC_LMER_leaf_thickness_.mm <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FRiC_LMER_leaf_thickness_.mm, type = 3)

#Functional Richness (FRic) TB 22
TB_22_FRiC_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FRiC_LMER_leaf_thickness_.mm., type = 3)

###Functional Evenness

#Functional Evenness (FEve) FK 18
FK_18_FEve_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2018&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FEve_LMER_leaf_thickness_.mm., type = 3)

#Functional Evenness (FEve) FK 19
FK_19_FEve_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="FK"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FEve_LMER_leaf_thickness_.mm., type = 3)

#Functional Evenness (FEve) FK 20
FK_20_FEve_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="FK"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FEve_LMER_leaf_thickness_.mm., type = 3)

#Functional Evenness (FEve) FK 21
FK_21_FEve_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FEve_LMER_leaf_thickness_.mm., type = 3)

#Functional Evenness (FEve) FK 22
FK_22_FEve_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FEve_LMER_leaf_thickness_.mm., type = 3)

#Functional Evenness (FEve) TB 18
TB_18_FEve_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2018&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FEve_LMER_leaf_thickness_.mm., type = 3)

#Functional Evenness (FEve) TB 19
TB_19_FEve_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="TB"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FEve_LMER_leaf_thickness_.mm., type = 3)

#Functional Evenness (FEve) TB 20
TB_20_FEve_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="TB"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FEve_LMER_leaf_thickness_.mm., type = 3)

#Functional Evenness (FEve) TB 21
TB_21_FEve_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FEve_LMER_leaf_thickness_.mm., type = 3)

#Functional Evenness (FEve) TB 22
TB_22_FEve_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FEve_LMER_leaf_thickness_.mm., type = 3)

###Functional Dispersion

#Functional Dispersion (FDis) FK 18
FK_18_FDis_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2018&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDis_LMER_leaf_thickness_.mm., type = 3)

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_leaf_thickness_.mm., type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_leaf_thickness_.mm., type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_leaf_thickness_.mm., type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_leaf_thickness_.mm., type = 3)

#Functional Dispersion (FDis) TB 18
TB_18_FDis_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2018&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDis_LMER_leaf_thickness_.mm., type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_leaf_thickness_.mm., type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_leaf_thickness_.mm., type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_leaf_thickness_.mm., type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_leaf_thickness_.mm. <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm.,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_leaf_thickness_.mm., type = 3)

#### LDMC Diversity Metrics ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data_LDMC_1<-Avg_Traits_FK %>% 
  select(Avg_LDMC,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:31))

Avg_Traits_FK_Data_LDMC<-Avg_Traits_FK_Data_LDMC_1 %>% 
  select(Avg_LDMC) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_LDMC) <- c(1:31)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_LDMC<-Avg_Traits_FK_Data_LDMC_1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_LDMC<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames_LDMC) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>% 
  filter(Genus_Species_Correct!="	
Hedeoma.hispida" & Genus_Species_Correct!="Linum.rigidum")

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_LDMC<-Species_Comp_FK_LDMC %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_LDMC<-Species_Comp_FK_Wide_LDMC %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_LDMC<-Species_Comp_FK_Wide_LDMC %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_LDMC <- dbFD(Avg_Traits_FK_Data_LDMC, Species_Comp_FK_Wide_Data_LDMC,corr = "none")

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_LDMC_1<-Avg_Traits_TB %>% 
  select(Avg_LDMC,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:41))

Avg_Traits_TB_Data_LDMC<-Avg_Traits_TB_Data_LDMC_1%>% 
  select(Avg_LDMC) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_LDMC) <- c(1:41)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_LDMC<-Avg_Traits_TB_Data_LDMC_1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_LDMC<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames_LDMC) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>% 
  filter(Genus_Species_Correct!="Hedeoma.hispida" & Genus_Species_Correct!="Elymus.elymoides")

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_LDMC<-Species_Comp_TB_LDMC %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_LDMC<-Species_Comp_TB_Wide_LDMC %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_LDMC<-Species_Comp_TB_Wide_LDMC %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_LDMC <- dbFD(Avg_Traits_TB_Data_LDMC, Species_Comp_TB_Wide_Data_LDMC,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_LDMC<-as.data.frame(FK_FunctionalDiversity_LDMC) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_LDMC)

Functional_Diversity_TB_LDMC<-as.data.frame(TB_FunctionalDiversity_LDMC) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_LDMC)

Functional_Diversity_LDMC<-Functional_Diversity_FK_LDMC %>% 
  rbind(Functional_Diversity_TB_LDMC) %>% 
  separate(ID,c("year","Site","plot"), sep = "_") %>% 
  select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))

#### LDMC Functional Metric Graphs ####

#### LDMC Functional Richness Graphs ####
#Functional Richness - FK 2019
FRic_FK_19_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,6))+
  annotate("text", x=8.5, y=6, label = "FK 2019", size=20)

#Functional Richness - FK 2020
FRic_FK_20_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,6))+
  annotate("text", x=8, y=6, label = "FK 2020", size=20)


#Functional Richness - FK 2021
FRic_FK_21_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FRic,color=grazing_treatment,shape=grazing_treatment))  +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,6))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=6, label = "FK 2021", size=20)

#Functional Richness - FK 2022
FRic_FK_22_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,6))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=6, label = "FK 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_FK_19_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_FK_20_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_FK_21_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_FK_22_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Richness - TB 2019
FRic_TB_19_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,8))+
  annotate("text", x=8.5, y=8, label = "TB 2019", size=20)

#Functional Richness - TB 2020
FRic_TB_20_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,8))+
  annotate("text", x=8, y=8, label = "TB 2020", size=20)

#Functional Richness - TB 2021
FRic_TB_21_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,8))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=8, label = "TB 2021", size=20)

#Functional Richness - TB 2022
FRic_TB_22_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FRic)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,8))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=8, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FRic_TB_19_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FRic_TB_20_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_TB_21_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_TB_22_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  


#### LDMC Functional Evenness Graphs ####

#Functional Evenness - FK 2019
FEve_FK_19_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Evenness - FK 2020
FEve_FK_20_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Evenness - FK 2021
FEve_FK_21_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Evenness - FK 2022
FEve_FK_22_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='glm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FEve
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_FK_19_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_FK_20_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_FK_21_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_FK_22_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Evenness - TB 2019
FEve_TB_19_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2019", size=20)

#Functional Evenness - TB 2020
FEve_TB_20_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2020", size=20)

#Functional Evenness - TB 2021
FEve_TB_21_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FEve)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2021", size=20)

#Functional Evenness - TB 2022
FEve_TB_22_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FEve,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "TB 2022", size=20)

#Create graph of all years for FRic
pushViewport(viewport(layout=grid.layout(2,2)))
print(FEve_TB_19_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FEve_TB_20_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FEve_TB_21_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FEve_TB_22_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#### LDMC Functional Dispersion Graphs ####

#Functional Dispersion - FK 2019
FDis_FK_19_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2019&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,1))+
  annotate("text", x=8.5, y=1, label = "FK 2019", size=20)

#Functional Dispersion - FK 2020
FDis_FK_20_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2020&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,1))+
  annotate("text", x=8, y=1, label = "FK 2020", size=20)


#Functional Dispersion - FK 2021
FDis_FK_21_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2021&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8.5, y=1, label = "FK 2021", size=20)

#Functional Dispersion - FK 2022
FDis_FK_22_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2022&Site=="FK"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=8, y=1, label = "FK 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_FK_19_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_FK_20_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_FK_21_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_FK_22_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

#Functional Dispersion - TB 2019
FDis_TB_19_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2019&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  #scale_linetype_manual(values=c("dashed","dashed","dashed"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.90))+
  expand_limits(y=c(0,2))+
  annotate("text", x=8.5, y=2, label = "TB 2019", size=20)

#Functional Dispersion - TB 2020
FDis_TB_20_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2020&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=c(0,2))+
  annotate("text", x=8.5, y=2, label = "TB 2020", size=20)

#Functional Dispersion - TB 2021
FDis_TB_21_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2021&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,2))+
  annotate("text", x=8.5, y=2, label = "TB 2021", size=20)

#Functional Dispersion - TB 2022
FDis_TB_22_LDMC<-ggplot(subset(Functional_Diversity_LDMC,year==2022&Site=="TB"),aes(x=rainfall_reduction,y=FDis)) +  
  geom_point(aes(color=grazing_treatment,shape=grazing_treatment),size=6, stroke =2)+
  #geom_smooth(color = "black", method='lm', se = FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","blue4","maroon4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  expand_limits(y=c(0,2))+
  annotate("text", x=8.5, y=2, label = "TB 2022", size=20)

#Create graph of all years for FDis
pushViewport(viewport(layout=grid.layout(2,2)))
print(FDis_TB_19_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_TB_20_LDMC,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_TB_21_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_TB_22_LDMC,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 3500 x 2000  

### LDMC Functional Diversity Stats #### 

###Functional Richness

#Functional Richness (FRic) FK 18
FK_18_FRiC_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2018&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FRiC_LMER_LDMC, type = 3)

#Functional Richness (FRic) FK 19
FK_19_FRiC_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2019&Site=="FK"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FRiC_LMER_LDMC, type = 3)

#Functional Richness (FRic) FK 20
FK_20_FRiC_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2020&Site=="FK"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FRiC_LMER_LDMC, type = 3)

#Functional Richness (FRic) FK 21
FK_21_FRiC_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2021&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FRiC_LMER_LDMC, type = 3)

#Functional Richness (FRic) FK 22
FK_22_FRiC_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2022&Site=="FK"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FRiC_LMER_LDMC, type = 3)

#Functional Richness (FRic) TB 18
TB_18_FRiC_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC, year==2018&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FRiC_LMER_LDMC, type = 3)

#Functional Richness (FRic) TB 19
TB_19_FRiC_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2019&Site=="TB"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FRiC_LMER_LDMC, type = 3)

#Functional Richness (FRic) TB 20
TB_20_FRiC_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2020&Site=="TB"), FRic ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FRiC_LMER_LDMC, type = 3)

#Functional Richness (FRic) TB 21
TB_21_FRiC_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2021&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FRiC_LMER_LDMC, type = 3)

#Functional Richness (FRic) TB 22
TB_22_FRiC_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2022&Site=="TB"), FRic ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FRiC_LMER_LDMC, type = 3)

###Functional Evenness

#Functional Evenness (FEve) FK 18
FK_18_FEve_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2018&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FEve_LMER_LDMC, type = 3)

#Functional Evenness (FEve) FK 19
FK_19_FEve_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2019&Site=="FK"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FEve_LMER_LDMC, type = 3)

#Functional Evenness (FEve) FK 20
FK_20_FEve_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2020&Site=="FK"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FEve_LMER_LDMC, type = 3)

#Functional Evenness (FEve) FK 21
FK_21_FEve_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2021&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FEve_LMER_LDMC, type = 3)

#Functional Evenness (FEve) FK 22
FK_22_FEve_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2022&Site=="FK"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FEve_LMER_LDMC, type = 3)

#Functional Evenness (FEve) TB 18
TB_18_FEve_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2018&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FEve_LMER_LDMC, type = 3)

#Functional Evenness (FEve) TB 19
TB_19_FEve_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2019&Site=="TB"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FEve_LMER_LDMC, type = 3)

#Functional Evenness (FEve) TB 20
TB_20_FEve_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2020&Site=="TB"), FEve ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FEve_LMER_LDMC, type = 3)

#Functional Evenness (FEve) TB 21
TB_21_FEve_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2021&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FEve_LMER_LDMC, type = 3)

#Functional Evenness (FEve) TB 22
TB_22_FEve_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2022&Site=="TB"), FEve ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FEve_LMER_LDMC, type = 3)

###Functional Dispersion

#Functional Dispersion (FDis) FK 18
FK_18_FDis_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2018&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDis_LMER_LDMC, type = 3)

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_LDMC, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_LDMC, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_LDMC, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_LDMC, type = 3)

#Functional Dispersion (FDis) TB 18
TB_18_FDis_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2018&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDis_LMER_LDMC, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_LDMC, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_LDMC, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_LDMC, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_LDMC <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_LDMC, type = 3)


#### Paper Figure 1 and 2 (CWM by drought) ####

####CWM of Height - Fort Keogh all years####
Height_FK_ALL<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=Height_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=20)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=20, y=20, label = "Height", size=20)

#### CWM of Height - Thunder Basin all years####
Height_TB_ALL<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=Height_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=20)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=20, y=20, label = "Height", size=20)


#### CWM of PercentGreen - Fort Keogh all years####
PercentGreen_FK_ALL<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=100)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=100, label = "Percent Green", size=20)

####CWM of PercentGreen - Thunder Basin all years####
PercentGreen_TB_ALL<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=100)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=100, label = "Percent Green", size=20)


####CWM of LeafThickness - Fort Keogh all years####
LeafThickness_FK_ALL<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=0.5, label = "Leaf Thickness", size=20)

####CWM of LeafThickness - Thunder Basin all years####
LeafThickness_TB_ALL<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=LeafThickness_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=0.5, label = "Leaf Thickness", size=20)


####CWM of LDMC - Fort Keogh all years####
LDMC_FK_ALL<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=LDMC_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=0.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=0.5, label = "LDMC", size=20)

####CWM of LDMC - Thunder Basin all years####
LDMC_TB_ALL<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=LDMC_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=2)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=2, label = "LDMC", size=20)

####CWM of Avg_SLA - Fort Keogh all years####
Avg_SLA_FK_ALL<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=Avg_SLA_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=2500)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=2500, label = "SLA", size=20)

####CWM of Avg_SLA - Thunder Basin all years####
Avg_SLA_TB_ALL<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=Avg_SLA_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=2500)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=2500, label = "SLA", size=20)


####CWM of LeafArea - Fort Keogh all years####
LeafArea_FK_ALL<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=Area_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=3)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=3, label = "Leaf Area", size=20)

####CWM of LeafArea - Thunder Basin all years####
LeafArea_TB_ALL<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=Area_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=4)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=4, label = "Leaf Area", size=20)

####CWM of Lifespan - Fort Keogh all years####
Lifespan_FK_ALL<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=Lifespan_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=20, y=1.5, label = "Lifespan", size=20)

####CWM of Lifespan - Thunder Basin all years####
Lifespan_TB_ALL<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=Lifespan_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=20, y=1.5, label = "Lifespan", size=20)


####CWM of GrowthForm - Fort Keogh all years####
GrowthForm_FK_ALL<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=GrowthForm_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=20, y=1.5, label = "Growth Form", size=20)

####CWM of GrowthForm - Thunder Basin all years####
GrowthForm_TB_ALL<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=GrowthForm_CWM,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weight Mean")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=20, y=1.5, label = "Growth Form", size=20)

#### Create graph of all years for CWM FK ####
pushViewport(viewport(layout=grid.layout(4,2)))
print(Height_FK_ALL,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(PercentGreen_FK_ALL,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LeafThickness_FK_ALL,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_FK_ALL,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(Avg_SLA_FK_ALL,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(LeafArea_FK_ALL,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(Lifespan_FK_ALL,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(GrowthForm_FK_ALL,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#Save at 3000 x 4000  

#### Create graph of all years for CWM TB ####
pushViewport(viewport(layout=grid.layout(4,2)))
print(Height_TB_ALL,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(PercentGreen_TB_ALL,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LeafThickness_TB_ALL,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_TB_ALL,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(Avg_SLA_TB_ALL,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(LeafArea_TB_ALL,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(Lifespan_TB_ALL,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(GrowthForm_TB_ALL,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#Save at 3000 x 4000  

#### Stats with 2018 as covariate ####

CWM_Collected_Data_2018<-CWM_Collected_Data %>% 
  filter(year==2018) %>% 
  rename(Height_CWM_TF_18=Height_CWM_TF) %>% 
  rename(PercentGreen_CWM_TF_18=PercentGreen_CWM_TF) %>% 
  rename(LeafThickness_CWM_TF_18=LeafThickness_CWM_TF) %>% 
  rename(LDMC_CWM_TF_18=LDMC_CWM_TF) %>% 
  rename(Biomass_CWM_TF_18=Biomass_CWM_TF) %>% 
  rename(Avg_SLA_CWM_TF_18=Avg_SLA_CWM_TF) %>% 
  rename(Lifespan_CWM_TF_18=Lifespan_CWM_TF) %>% 
  rename(GrowthForm_CWM_TF_18=GrowthForm_CWM_TF) %>% 
  rename(PhotosyntheticPathway_CWM_TF_18=PhotosyntheticPathway_CWM_TF) %>% 
  rename(Area_CWM_TF_18=Area_CWM_TF) %>% 
  select(Site, plot,block,paddock,Height_CWM_TF_18,PercentGreen_CWM_TF_18,LeafThickness_CWM_TF_18,LDMC_CWM_TF_18,Biomass_CWM_TF_18,Avg_SLA_CWM_TF_18,Lifespan_CWM_TF_18,GrowthForm_CWM_TF_18,PhotosyntheticPathway_CWM_TF_18,Area_CWM_TF_18)

CWM_Collected_Data_18<-CWM_Collected_Data %>% 
  filter(year!=2018) %>% 
  left_join(CWM_Collected_Data_2018)
  
#### Percent Green FK ####

#CWM of PercentGreen for Fort Keogh 2019 - LMER
FK_PercentGreen_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="FK"), PercentGreen_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2019_LMER_18, type = 3)

#CWM of PercentGreen for Fort Keogh 2020 - LMER
FK_PercentGreen_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="FK"), PercentGreen_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2020_LMER_18, type = 3)

#CWM of PercentGreen for Fort Keogh 2021 - LMER
FK_PercentGreen_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="FK"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2021_LMER_18, type = 3)

#CWM of PercentGreen for Fort Keogh 2022 - LMER
FK_PercentGreen_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="FK"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2022_LMER_18, type = 3)

#### Percent Green TB ####

#CWM of PercentGreen for Thunder Basin 2019 - LMER
TB_PercentGreen_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="TB"), PercentGreen_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2019_LMER_18, type = 3)

#CWM of PercentGreen for Thunder Basin 2020 - LMER
TB_PercentGreen_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="TB"), PercentGreen_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2020_LMER_18, type = 3)

#CWM of PercentGreen for Thunder Basin 2021 - LMER
TB_PercentGreen_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="TB"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2021_LMER_18, type = 3)

#CWM of PercentGreen for Thunder Basin 2022 - LMER
TB_PercentGreen_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="TB"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2022_LMER_18, type = 3) 

#### Biomass FK ####

#CWM of Biomass for Fort Keogh 2019 - LMER
FK_Biomass_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="FK"), Biomass_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Biomass_2019_LMER_18, type = 3)

#CWM of Biomass for Fort Keogh 2020 - LMER
FK_Biomass_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="FK"), Biomass_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Biomass_2020_LMER_18, type = 3)

#CWM of Biomass for Fort Keogh 2021 - LMER
FK_Biomass_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="FK"), Biomass_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Biomass_2021_LMER_18, type = 3)

#CWM of Biomass for Fort Keogh 2022 - LMER
FK_Biomass_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="FK"), Biomass_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Biomass_2022_LMER_18, type = 3)

#### Biomass TB ####

#CWM of Biomass for TB 2019 - LMER
TB_Biomass_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="TB"), Biomass_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Biomass_2019_LMER_18, type = 3)

#CWM of Biomass for TB 2020 - LMER
TB_Biomass_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="TB"), Biomass_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Biomass_2020_LMER_18, type = 3)

#CWM of Biomass for TB 2021 - LMER
TB_Biomass_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="TB"), Biomass_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Biomass_2021_LMER_18, type = 3)

#CWM of Biomass for TB 2022 - LMER
TB_Biomass_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="TB"), Biomass_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Biomass_2022_LMER_18, type = 3)

#### Height FK ####

#CWM of height for Fort Keogh 2019 - LMER
FK_Height_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="FK"), Height_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Height_2019_LMER_18, type = 3)

#CWM of height for Fort Keogh 2020 - LMER
FK_Height_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="FK"), Height_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Height_2020_LMER_18, type = 3)

#CWM of height for Fort Keogh 2021 - LMER
FK_Height_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="FK"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Height_2021_LMER_18, type = 3)

#CWM of height for Fort Keogh 2022 - LMER
FK_Height_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="FK"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Height_2022_LMER_18, type = 3)

#### Height TB ####

#CWM of height for Thunder Basin 2019 - LMER
TB_Height_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="TB"), Height_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Height_2019_LMER_18, type = 3)

#CWM of height for Thunder Basin 2020 - LMER
TB_Height_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="TB"), Height_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Height_2020_LMER_18, type = 3)

#CWM of height for Thunder Basin 2021 - LMER
TB_Height_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="TB"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Height_2021_LMER_18, type = 3)

#CWM of height for Thunder Basin 2022 - LMER
TB_Height_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="TB"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Height_2022_LMER_18, type = 3)

#### LDMC FK ####

#CWM of LDMC for Fort Keogh 2019 - LMER
FK_LDMC_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="FK"), LDMC_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_LDMC_2019_LMER_18, type = 3)

#CWM of LDMC for Fort Keogh 2020 - LMER
FK_LDMC_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="FK"), LDMC_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_LDMC_2020_LMER_18, type = 3)

#CWM of LDMC for Fort Keogh 2021 - LMER
FK_LDMC_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="FK"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_LDMC_2021_LMER_18, type = 3)

#CWM of LDMC for Fort Keogh 2022 - LMER
FK_LDMC_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="FK"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_LDMC_2022_LMER_18, type = 3)

#### LDMC TB ####

#CWM of LDMC for TB 2019 - LMER
TB_LDMC_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="TB"), LDMC_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_LDMC_2019_LMER_18, type = 3)

#CWM of LDMC for TB 2020 - LMER
TB_LDMC_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="TB"), LDMC_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_LDMC_2020_LMER_18, type = 3)

#CWM of LDMC for TB 2021 - LMER
TB_LDMC_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="TB"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_LDMC_2021_LMER_18, type = 3)

#CWM of LDMC for TB 2022 - LMER
TB_LDMC_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="TB"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_LDMC_2022_LMER_18, type = 3)

#### SLA FK ####

#CWM of SLA for Fort Keogh 2019 - LMER
FK_SLA_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="FK"), Avg_SLA_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_SLA_2019_LMER_18, type = 3)

#CWM of SLA for Fort Keogh 2020 - LMER
FK_SLA_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="FK"), Avg_SLA_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_SLA_2020_LMER_18, type = 3)

#CWM of SLA for Fort Keogh 2021 - LMER
FK_SLA_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="FK"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_SLA_2021_LMER_18, type = 3)

#CWM of SLA for Fort Keogh 2022 - LMER
FK_SLA_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="FK"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_SLA_2022_LMER_18, type = 3)

#### SLA TB ####

#CWM of SLA for TB 2019 - LMER
TB_SLA_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="TB"), Avg_SLA_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_SLA_2019_LMER_18, type = 3)

#CWM of SLA for TB 2020 - LMER
TB_SLA_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="TB"), Avg_SLA_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_SLA_2020_LMER_18, type = 3)

#CWM of SLA for TB 2021 - LMER
TB_SLA_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="TB"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_SLA_2021_LMER_18, type = 3)

#CWM of SLA for TB 2022 - LMER
TB_SLA_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="TB"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_SLA_2022_LMER_18, type = 3)

#### Thickness FK ####

#CWM of LeafThickness for Fort Keogh 2019 - LMER
FK_LeafThickness_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="FK"), LeafThickness_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2019_LMER_18, type = 3)

#CWM of LeafThickness for Fort Keogh 2020 - LMER
FK_LeafThickness_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="FK"), LeafThickness_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2020_LMER_18, type = 3)

#CWM of LeafThickness for Fort Keogh 2021 - LMER
FK_LeafThickness_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="FK"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2021_LMER_18, type = 3)

#CWM of LeafThickness for Fort Keogh 2022 - LMER
FK_LeafThickness_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="FK"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2022_LMER_18, type = 3)

#### Thickness TB ####

#CWM of LeafThickness for Thunder Basin 2019 - LMER
TB_LeafThickness_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="TB"), LeafThickness_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2019_LMER_18, type = 3)

#CWM of LeafThickness for Thunder Basin 2020 - LMER
TB_LeafThickness_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="TB"), LeafThickness_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2020_LMER_18, type = 3)

#CWM of LeafThickness for Thunder Basin 2021 - LMER
TB_LeafThickness_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="TB"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2021_LMER_18, type = 3)

#CWM of LeafThickness for Thunder Basin 2022 - LMER
TB_LeafThickness_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="TB"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2022_LMER_18, type = 3)

#### Area FK ####

#CWM of Area for Fort Keogh 2019 - LMER
FK_Area_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="FK"), Area_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Area_2019_LMER_18, type = 3)

#CWM of Area for Fort Keogh 2020 - LMER
FK_Area_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="FK"), Area_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Area_2020_LMER_18, type = 3)

#CWM of Area for Fort Keogh 2021 - LMER
FK_Area_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="FK"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Area_2021_LMER_18, type = 3)

#CWM of Area for Fort Keogh 2022 - LMER
FK_Area_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="FK"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Area_2022_LMER_18, type = 3)

#### Area TB ####

#CWM of Area for Thunder Basin 2019 - LMER
TB_Area_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="TB"), Area_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Area_2019_LMER_18, type = 3)

#CWM of Area for Thunder Basin 2020 - LMER
TB_Area_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18, year==2020&Site=="TB"), Area_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Area_2020_LMER_18, type = 3)

#CWM of Area for Thunder Basin 2021 - LMER
TB_Area_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="TB"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Area_2021_LMER_18, type = 3)

#CWM of Area for Thunder Basin 2022 - LMER
TB_Area_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="TB"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Area_2022_LMER_18, type = 3)

#### Lifespan FK ####

#CWM of Lifespan for Fort Keogh 2019 - LMER
FK_Lifespan_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="FK"), Lifespan_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Lifespan_2019_LMER_18, type = 3)

#CWM of Lifespan for Fort Keogh 2020 - LMER
FK_Lifespan_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="FK"), Lifespan_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Lifespan_2020_LMER_18, type = 3)

#CWM of Lifespan for Fort Keogh 2021 - LMER
FK_Lifespan_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="FK"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Lifespan_2021_LMER_18, type = 3)

#CWM of Lifespan for Fort Keogh 2022 - LMER
FK_Lifespan_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="FK"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_Lifespan_2022_LMER_18, type = 3)

#### Lifespan TB ####

#CWM of Lifespan for Thunder Basin 2019 - LMER
TB_Lifespan_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="TB"), Lifespan_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Lifespan_2019_LMER_18, type = 3)

#CWM of Lifespan for Thunder Basin 2020 - LMER
TB_Lifespan_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="TB"), Lifespan_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Lifespan_2020_LMER_18, type = 3)

#CWM of Lifespan for Thunder Basin 2021 - LMER
TB_Lifespan_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="TB"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Lifespan_2021_LMER_18, type = 3)

#CWM of Lifespan for Thunder Basin 2022 - LMER
TB_Lifespan_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="TB"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_Lifespan_2022_LMER_18, type = 3)

#### Growth Form FK ####

#CWM of GrowthForm for Fort Keogh 2019 - LMER
FK_GrowthForm_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="FK"), GrowthForm_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_GrowthForm_2019_LMER_18, type = 3)

#CWM of GrowthForm for Fort Keogh 2020 - LMER
FK_GrowthForm_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="FK"), GrowthForm_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_GrowthForm_2020_LMER_18, type = 3)

#CWM of GrowthForm for Fort Keogh 2021 - LMER
FK_GrowthForm_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="FK"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_GrowthForm_2021_LMER_18, type = 3)

#CWM of GrowthForm for Fort Keogh 2022 - LMER
FK_GrowthForm_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="FK"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_GrowthForm_2022_LMER_18, type = 3)

####Growth Form TB ####

#CWM of GrowthForm for Thunder Basin 2019 - LMER
TB_GrowthForm_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="TB"), GrowthForm_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_GrowthForm_2019_LMER_18, type = 3)

#CWM of GrowthForm for Thunder Basin 2020 - LMER
TB_GrowthForm_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="TB"), GrowthForm_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_GrowthForm_2020_LMER_18, type = 3)

#CWM of GrowthForm for Thunder Basin 2021 - LMER
TB_GrowthForm_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18, year==2021&Site=="TB"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_GrowthForm_2021_LMER_18, type = 3)

#CWM of GrowthForm for Thunder Basin 2022 - LMER
TB_GrowthForm_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="TB"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_GrowthForm_2022_LMER_18, type = 3)

#### Photosynthetic Pathway FK ####

#CWM of PhotosyntheticPathway for Fort Keogh 2019 - LMER
FK_PhotosyntheticPathway_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="FK"), PhotosyntheticPathway_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_PhotosyntheticPathway_2019_LMER_18, type = 3)

#CWM of PhotosyntheticPathway for Fort Keogh 2020 - LMER
FK_PhotosyntheticPathway_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="FK"), PhotosyntheticPathway_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_PhotosyntheticPathway_2020_LMER_18, type = 3)

#CWM of PhotosyntheticPathway for Fort Keogh 2021 - LMER
FK_PhotosyntheticPathway_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="FK"), PhotosyntheticPathway_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_PhotosyntheticPathway_2021_LMER_18, type = 3)

#CWM of PhotosyntheticPathway for Fort Keogh 2022 - LMER
FK_PhotosyntheticPathway_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="FK"), PhotosyntheticPathway_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(FK_PhotosyntheticPathway_2022_LMER_18, type = 3)

#### Photosynthetic pathway TB ####

#CWM of PhotosyntheticPathway for Thunder Basin 2019 - LMER
TB_PhotosyntheticPathway_2019_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2019&Site=="TB"), PhotosyntheticPathway_CWM_TF ~ Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_PhotosyntheticPathway_2019_LMER_18, type = 3)

#CWM of PhotosyntheticPathway for Thunder Basin 2020 - LMER
TB_PhotosyntheticPathway_2020_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2020&Site=="TB"), PhotosyntheticPathway_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_PhotosyntheticPathway_2020_LMER_18, type = 3)

#CWM of PhotosyntheticPathway for Thunder Basin 2021 - LMER
TB_PhotosyntheticPathway_2021_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2021&Site=="TB"), PhotosyntheticPathway_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_PhotosyntheticPathway_2021_LMER_18, type = 3)

#CWM of PhotosyntheticPathway for Thunder Basin 2022 - LMER
TB_PhotosyntheticPathway_2022_LMER_18 <- lmerTest::lmer(data = subset(CWM_Collected_Data_18,year==2022&Site=="TB"), PhotosyntheticPathway_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + PercentGreen_CWM_TF_18 + (1|block) + (1|block:paddock))
anova(TB_PhotosyntheticPathway_2022_LMER_18, type = 3)

#### Rerunning 2020 without destock ####

##New table without destock data for running 2020 stats
CWM_Collected_Data_nodestock<-CWM_Collected_Data %>% 
  filter(grazing_treatment!="destock")

#CWM of PercentGreen for Fort Keogh 2020 - LMER (no destock)
FK_PercentGreen_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="FK"), PercentGreen_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PercentGreen_2020_LMER_nodestock, type = 3)

#CWM of PercentGreen for TB 2020 - LMER (no destock)
TB_PercentGreen_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="TB"), PercentGreen_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PercentGreen_2020_LMER_nodestock, type = 3)

#CWM of Biomass for Fort Keogh 2020 - LMER (no destock)
FK_Biomass_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="FK"), Biomass_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Biomass_2020_LMER_nodestock, type = 3)

#CWM of Biomass for TB 2020 - LMER (no destock)
TB_Biomass_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="TB"), Biomass_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Biomass_2020_LMER_nodestock, type = 3)

#CWM of Height for Fort Keogh 2020 - LMER (no destock)
FK_Height_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="FK"), Height_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Height_2020_LMER_nodestock, type = 3)

#CWM of Height for TB 2020 - LMER (no destock)
TB_Height_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="TB"), Height_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Height_2020_LMER_nodestock, type = 3)

#CWM of LDMC for Fort Keogh 2020 - LMER (no destock)
FK_LDMC_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="FK"), LDMC_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LDMC_2020_LMER_nodestock, type = 3)

#CWM of LDMC for TB 2020 - LMER (no destock)
TB_LDMC_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="TB"), LDMC_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LDMC_2020_LMER_nodestock, type = 3)

#CWM of Avg_SLA for Fort Keogh 2020 - LMER (no destock)
FK_Avg_SLA_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="FK"), Avg_SLA_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Avg_SLA_2020_LMER_nodestock, type = 3)

#CWM of Avg_SLA for TB 2020 - LMER (no destock)
TB_Avg_SLA_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="TB"), Avg_SLA_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Avg_SLA_2020_LMER_nodestock, type = 3)

#CWM of LeafThickness for Fort Keogh 2020 - LMER (no destock)
FK_LeafThickness_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="FK"), LeafThickness_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_LeafThickness_2020_LMER_nodestock, type = 3)

#CWM of LeafThickness for TB 2020 - LMER (no destock)
TB_LeafThickness_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="TB"), LeafThickness_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_LeafThickness_2020_LMER_nodestock, type = 3)

#CWM of Area for Fort Keogh 2020 - LMER (no destock)
FK_Area_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="FK"), Area_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Area_2020_LMER_nodestock, type = 3)

#CWM of Area for TB 2020 - LMER (no destock)
TB_Area_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="TB"), Area_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Area_2020_LMER_nodestock, type = 3)

#CWM of Lifespan for Fort Keogh 2020 - LMER (no destock)
FK_Lifespan_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="FK"), Lifespan_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_Lifespan_2020_LMER_nodestock, type = 3)

#CWM of Lifespan for TB 2020 - LMER (no destock)
TB_Lifespan_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="TB"), Lifespan_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_Lifespan_2020_LMER_nodestock, type = 3)

#CWM of GrowthForm for Fort Keogh 2020 - LMER (no destock)
FK_GrowthForm_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="FK"), GrowthForm_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_GrowthForm_2020_LMER_nodestock, type = 3)

#CWM of GrowthForm for TB 2020 - LMER (no destock)
TB_GrowthForm_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="TB"), GrowthForm_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_GrowthForm_2020_LMER_nodestock, type = 3)

#CWM of PhotosyntheticPathway for Fort Keogh 2020 - LMER (no destock)
FK_PhotosyntheticPathway_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="FK"), PhotosyntheticPathway_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_PhotosyntheticPathway_2020_LMER_nodestock, type = 3)

#CWM of PhotosyntheticPathway for TB 2020 - LMER (no destock)
TB_PhotosyntheticPathway_2020_LMER_nodestock <- lmerTest::lmer(data = subset(CWM_Collected_Data_nodestock,year==2020&Site=="TB"), PhotosyntheticPathway_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_PhotosyntheticPathway_2020_LMER_nodestock, type = 3)

#### Paper Figure 3 and 4 (Functional Dispersion by drought) ####

####Multivariate Functional Dispersion - Fort Keogh all years####
Multivariate_FDis_FK<-ggplot(subset(Functional_Diversity,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=0.3)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=20, y=0.3, label = "Multivariate", size=20)

#### Multivariate Functional Dispersion - Thunder Basin all years####
Multivariate_FDis_TB<-ggplot(subset(Functional_Diversity,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=0.3)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.75,0.80))+
  annotate("text", x=20, y=0.3, label = "Multivariate", size=20)

####Height Functional Dispersion - Fort Keogh all years####
Height_FDis_FK<-ggplot(subset(Functional_Diversity_Height,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=1, label = "Height", size=20)

####Height Functional Dispersion - TB all years####
Height_FDis_TB<-ggplot(subset(Functional_Diversity_Height,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=1, label = "Height", size=20)

####percent_green Functional Dispersion - Fort Keogh all years####
percent_green_FDis_FK<-ggplot(subset(Functional_Diversity_percent_green,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=1, label = "Percent Green", size=20)

####percent_green Functional Dispersion - TB all years####
percent_green_FDis_TB<-ggplot(subset(Functional_Diversity_percent_green,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=1, label = "Percent Green", size=20)

#### Leaf Thickness Functional Dispersion - Fort Keogh all years####
LeafThickness_FDis_FK<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=1, label = "Leaf Thickness", size=20)

####Leaf Thickness Functional Dispersion - TB all years####
LeafThickness_FDis_TB<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm.,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=1, label = "Leaf Thickness", size=20)

#### LDMC Functional Dispersion - Fort Keogh all years####
LDMC_FDis_FK<-ggplot(subset(Functional_Diversity_LDMC,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=1, label = "LDMC", size=20)

####LDMC Functional Dispersion - TB all years####
LDMC_FDis_TB<-ggplot(subset(Functional_Diversity_LDMC,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=2.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=2.5, label = "LDMC", size=20)

#### SLA Functional Dispersion - Fort Keogh all years####
SLA_FDis_FK<-ggplot(subset(Functional_Diversity_SLA,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=2.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position ="none")+
  annotate("text", x=20, y=2.5, label = "SLA", size=20)

####SLA Functional Dispersion - TB all years####
SLA_FDis_TB<-ggplot(subset(Functional_Diversity_SLA,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=2.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=2.5, label = "SLA", size=20)

#### Area Diversity Metrics ####

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_FK_Data_Area_1<-Avg_Traits_FK %>% 
  select(Avg_Area,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:32))

Avg_Traits_FK_Data_Area<-Avg_Traits_FK_Data_Area_1 %>% 
  select(Avg_Area) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_Area) <- c(1:32)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_Area<-Avg_Traits_FK_Data_Area_1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_Area <- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames_Area) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>% 
  filter(Genus_Species_Correct!="Linum.rigidum")

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_Area<-Species_Comp_FK_Area %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_Area<-Species_Comp_FK_Wide_Area %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_Area<-Species_Comp_FK_Wide_Area %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_Area <- dbFD(Avg_Traits_FK_Data_Area, Species_Comp_FK_Wide_Data_Area,corr = "none")


#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_Area_1<-Avg_Traits_TB %>% 
  select(Avg_Area,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:37))

Avg_Traits_TB_Data_Area<-Avg_Traits_TB_Data_Area_1 %>% 
  select(Avg_Area) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_Area) <- c(1:37)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_Area<-Avg_Traits_TB_Data_Area_1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_Area <- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames_Area) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>%
  filter(Genus_Species_Correct!="	
Aristida.purpurea" & Genus_Species_Correct!="Artemisia.frigida"& Genus_Species_Correct!="Artemisia.tridentata"& Genus_Species_Correct!="Bouteloua.gracilis"& Genus_Species_Correct!="Elymus.elymoides"& Genus_Species_Correct!="Gutierrezia.sarothrae")


#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_Area<-Species_Comp_TB_Area %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_Area<-Species_Comp_TB_Wide_Area %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_Area<-Species_Comp_TB_Wide_Area %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_Area <- dbFD(Avg_Traits_TB_Data_Area, Species_Comp_TB_Wide_Data_Area,corr = "none")


#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_Area<-as.data.frame(FK_FunctionalDiversity_Area) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_Area)

Functional_Diversity_TB_Area<-as.data.frame(TB_FunctionalDiversity_Area) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_Area)

Functional_Diversity_Area<-Functional_Diversity_FK_Area %>% 
  rbind(Functional_Diversity_TB_Area) %>% 
  separate(ID,c("year","Site","plot"), sep = "_") %>% 
  select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))

#### Area Functional Dispersion - Fort Keogh all years####
Area_FDis_FK<-ggplot(subset(Functional_Diversity_Area,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=20, y=1.5, label = "Leaf Area", size=20)

#### Area Functional Dispersion - TB all years####
Area_FDis_TB<-ggplot(subset(Functional_Diversity_Area,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=20, y=1.5, label = "Leaf Area", size=20)

###Functional Dispersion - Area Stats ####

#Functional Dispersion (FDis) FK 18
FK_18_FDis_LMER_Area <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2018&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDis_LMER_Area, type = 3)

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_Area <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_Area, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_Area <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_Area, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_Area <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_Area, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_Area <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_Area, type = 3)

#Functional Dispersion (FDis) TB 18
TB_18_FDis_LMER_Area <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2018&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDis_LMER_Area, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_Area <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_Area, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_Area <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_Area, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_Area <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_Area, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_Area <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_Area, type = 3)

#### Lifespan Diversity Metrics ####

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_FK_Data_Lifespan_1<-Avg_Traits_FK %>% 
  select(Avg_Lifespan,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:32))

Avg_Traits_FK_Data_Lifespan<-Avg_Traits_FK_Data_Lifespan_1 %>% 
  select(Avg_Lifespan) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_Lifespan) <- c(1:32)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_Lifespan<-Avg_Traits_FK_Data_Lifespan_1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_Lifespan <- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames_Lifespan) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>% 
  filter(Genus_Species_Correct!="Linum.rigidum")

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_Lifespan<-Species_Comp_FK_Lifespan %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_Lifespan<-Species_Comp_FK_Wide_Lifespan %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_Lifespan<-Species_Comp_FK_Wide_Lifespan %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_Lifespan <- dbFD(Avg_Traits_FK_Data_Lifespan, Species_Comp_FK_Wide_Data_Lifespan,corr = "none")


#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_Lifespan_1<-Avg_Traits_TB %>% 
  select(Avg_Lifespan,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:42))

Avg_Traits_TB_Data_Lifespan<-Avg_Traits_TB_Data_Lifespan_1 %>% 
  select(Avg_Lifespan) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_Lifespan) <- c(1:42)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_Lifespan<-Avg_Traits_TB_Data_Lifespan_1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_Lifespan <- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames_Lifespan) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>%
  filter(Genus_Species_Correct!="Elymus.elymoides")


#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_Lifespan<-Species_Comp_TB_Lifespan %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_Lifespan<-Species_Comp_TB_Wide_Lifespan %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_Lifespan<-Species_Comp_TB_Wide_Lifespan %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_Lifespan <- dbFD(Avg_Traits_TB_Data_Lifespan, Species_Comp_TB_Wide_Data_Lifespan,corr = "none")


#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_Lifespan<-as.data.frame(FK_FunctionalDiversity_Lifespan) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_Lifespan)

Functional_Diversity_TB_Lifespan<-as.data.frame(TB_FunctionalDiversity_Lifespan) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_Lifespan)

Functional_Diversity_Lifespan<-Functional_Diversity_FK_Lifespan %>% 
  rbind(Functional_Diversity_TB_Lifespan) %>% 
  separate(ID,c("year","Site","plot"), sep = "_") %>% 
  select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))

#### Lifespan Functional Dispersion - Fort Keogh all years####
Lifespan_FDis_FK<-ggplot(subset(Functional_Diversity_Lifespan,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=20, y=1.5, label = "Lifespan", size=20)

#### Lifespan Functional Dispersion - TB all years####
Lifespan_FDis_TB<-ggplot(subset(Functional_Diversity_Lifespan,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=20, y=1.5, label = "Lifespan", size=20)

###Functional Dispersion - Lifespan Stats ####

#Functional Dispersion (FDis) FK 18
FK_18_FDis_LMER_Lifespan <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan,year==2018&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDis_LMER_Lifespan, type = 3)

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_Lifespan <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_Lifespan, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_Lifespan <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_Lifespan, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_Lifespan <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_Lifespan, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_Lifespan <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_Lifespan, type = 3)

#Functional Dispersion (FDis) TB 18
TB_18_FDis_LMER_Lifespan <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan,year==2018&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDis_LMER_Lifespan, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_Lifespan <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_Lifespan, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_Lifespan <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_Lifespan, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_Lifespan <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_Lifespan, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_Lifespan <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_Lifespan, type = 3)

#### GrowthForm Diversity Metrics ####

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_FK_Data_GrowthForm_1<-Avg_Traits_FK %>% 
  select(Avg_GrowthForm,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:32))

Avg_Traits_FK_Data_GrowthForm<-Avg_Traits_FK_Data_GrowthForm_1 %>% 
  select(Avg_GrowthForm) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_GrowthForm) <- c(1:32)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_GrowthForm<-Avg_Traits_FK_Data_GrowthForm_1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_GrowthForm <- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames_GrowthForm) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>% 
  filter(Genus_Species_Correct!="Linum.rigidum")

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_GrowthForm<-Species_Comp_FK_GrowthForm %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_GrowthForm<-Species_Comp_FK_Wide_GrowthForm %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_GrowthForm<-Species_Comp_FK_Wide_GrowthForm %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_GrowthForm <- dbFD(Avg_Traits_FK_Data_GrowthForm, Species_Comp_FK_Wide_Data_GrowthForm,corr = "none")


#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_GrowthForm_1<-Avg_Traits_TB %>% 
  select(Avg_GrowthForm,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:42))

Avg_Traits_TB_Data_GrowthForm<-Avg_Traits_TB_Data_GrowthForm_1 %>% 
  select(Avg_GrowthForm) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_GrowthForm) <- c(1:42)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_GrowthForm<-Avg_Traits_TB_Data_GrowthForm_1 %>% 
  select(Genus_Species_Correct,Sp_Num_2)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_GrowthForm <- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames_GrowthForm) %>% 
  na.omit(Sp_Num_2) %>% 
  mutate(ID=paste(year,site,plot,sep="_")) %>%
  filter(Genus_Species_Correct!="Elymus.elymoides")


#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_GrowthForm<-Species_Comp_TB_GrowthForm %>% 
  select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_GrowthForm<-Species_Comp_TB_Wide_GrowthForm %>% 
  select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_GrowthForm<-Species_Comp_TB_Wide_GrowthForm %>% 
  mutate(ID_Num=c(1:270)) %>% 
  select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_GrowthForm <- dbFD(Avg_Traits_TB_Data_GrowthForm, Species_Comp_TB_Wide_Data_GrowthForm,corr = "none")


#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_GrowthForm<-as.data.frame(FK_FunctionalDiversity_GrowthForm) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_GrowthForm)

Functional_Diversity_TB_GrowthForm<-as.data.frame(TB_FunctionalDiversity_GrowthForm) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_GrowthForm)

Functional_Diversity_GrowthForm<-Functional_Diversity_FK_GrowthForm %>% 
  rbind(Functional_Diversity_TB_GrowthForm) %>% 
  separate(ID,c("year","Site","plot"), sep = "_") %>% 
  select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category))))

#### GrowthForm Functional Dispersion - Fort Keogh all years####
GrowthForm_FDis_FK<-ggplot(subset(Functional_Diversity_GrowthForm,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=20, y=1.5, label = "Growth Form", size=20)

#### GrowthForm Functional Dispersion - TB all years####
GrowthForm_FDis_TB<-ggplot(subset(Functional_Diversity_GrowthForm,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis,color=as.factor(year),linetype=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=6, stroke =2)+
  #geom_smooth(aes(linetype=as.factor(year)),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=1.5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=20, y=1.5, label = "Growth Form", size=20)

###Functional Dispersion - GrowthForm Stats ####

#Functional Dispersion (FDis) FK 18
FK_18_FDis_LMER_GrowthForm <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm,year==2018&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_18_FDis_LMER_GrowthForm, type = 3)

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_GrowthForm <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_GrowthForm, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_GrowthForm <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_GrowthForm, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_GrowthForm <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_GrowthForm, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_GrowthForm <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_GrowthForm, type = 3)

#Functional Dispersion (FDis) TB 18
TB_18_FDis_LMER_GrowthForm <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm,year==2018&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_18_FDis_LMER_GrowthForm, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_GrowthForm <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_GrowthForm, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_GrowthForm <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_GrowthForm, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_GrowthForm <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_GrowthForm, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_GrowthForm <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_GrowthForm, type = 3)

#### Create graph of all years for FDis FK ####
pushViewport(viewport(layout=grid.layout(3,3)))
print(Multivariate_FDis_FK,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Height_FDis_FK,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(percent_green_FDis_FK,vp=viewport(layout.pos.row=1, layout.pos.col =3))
print(LeafThickness_FDis_FK,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_FDis_FK,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(SLA_FDis_FK,vp=viewport(layout.pos.row=2, layout.pos.col =3))
print(Area_FDis_FK,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(Lifespan_FDis_FK,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(GrowthForm_FDis_FK,vp=viewport(layout.pos.row=3, layout.pos.col =3))
#Save at 4000 x 3000  

#### Create graph of all years for FDis TB ####
pushViewport(viewport(layout=grid.layout(3,3)))
print(Multivariate_FDis_TB,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Height_FDis_TB,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(percent_green_FDis_TB,vp=viewport(layout.pos.row=1, layout.pos.col =3))
print(LeafThickness_FDis_TB,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_FDis_TB,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(SLA_FDis_TB,vp=viewport(layout.pos.row=2, layout.pos.col =3))
print(Area_FDis_TB,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(Lifespan_FDis_TB,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(GrowthForm_FDis_TB,vp=viewport(layout.pos.row=3, layout.pos.col =3))
#Save at 4000 x 3000  

#### Rerunning 2020 without destock - FDis ####

##New table without destock data for running 2020 stats
Functional_Diversity_nodestock<-Functional_Diversity %>% 
  filter(grazing_treatment!="destock")

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_nodestock,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_nodestock, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_nodestock,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_nodestock, type = 3)

Functional_Diversity_height_nodestock<-Functional_Diversity_Height %>% 
  filter(grazing_treatment!="destock")

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_height_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_height_nodestock,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_height_nodestock, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_height_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_height_nodestock,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_height_nodestock, type = 3)

Functional_Diversity_percent_green_nodestock<-Functional_Diversity_percent_green %>% 
  filter(grazing_treatment!="destock")

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_percent_green_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green_nodestock,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_percent_green_nodestock, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_percent_green_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green_nodestock,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_percent_green_nodestock, type = 3)

Functional_Diversity_leaf_thickness_.mm._nodestock<-Functional_Diversity_leaf_thickness_.mm. %>% 
  filter(grazing_treatment!="destock")

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_leaf_thickness_.mm._nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm._nodestock,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_leaf_thickness_.mm._nodestock, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_leaf_thickness_.mm._nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm._nodestock,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_leaf_thickness_.mm._nodestock, type = 3)

Functional_Diversity_LDMC_nodestock<-Functional_Diversity_LDMC %>% 
  filter(grazing_treatment!="destock")

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_LDMC_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC_nodestock,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_LDMC_nodestock, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_LDMC_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC_nodestock,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_LDMC_nodestock, type = 3)

Functional_Diversity_SLA_nodestock<-Functional_Diversity_SLA %>% 
  filter(grazing_treatment!="destock")

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_SLA_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_SLA_nodestock,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_SLA_nodestock, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_SLA_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_SLA_nodestock,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_SLA_nodestock, type = 3)

Functional_Diversity_Area_nodestock<-Functional_Diversity_Area %>% 
  filter(grazing_treatment!="destock")

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_Area_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_Area_nodestock,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_Area_nodestock, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_Area_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_Area_nodestock,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_Area_nodestock, type = 3)

Functional_Diversity_Lifespan_nodestock<-Functional_Diversity_Lifespan %>% 
  filter(grazing_treatment!="destock")

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_Lifespan_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan_nodestock,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_Lifespan_nodestock, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_Lifespan_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan_nodestock,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_Lifespan_nodestock, type = 3)

Functional_Diversity_GrowthForm_nodestock<-Functional_Diversity_GrowthForm %>% 
  filter(grazing_treatment!="destock")

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_GrowthForm_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm_nodestock,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_GrowthForm_nodestock, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_GrowthForm_nodestock <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm_nodestock,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_GrowthForm_nodestock, type = 3)


#### Stats with 2018 as covariate - FDis ####

#### Multivariate with 18 as covariate ####
Functional_Diversity_2018<-Functional_Diversity %>% 
  filter(year==2018) %>% 
  rename(FDis_18=FDis) %>% 
  select(Site, plot,block,paddock,FDis_18)

Functional_Diversity_18<-Functional_Diversity %>% 
  filter(year!=2018) %>% 
  left_join(Functional_Diversity_2018)

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_18 <- lmerTest::lmer(data = subset(Functional_Diversity_18,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_18, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_18 <- lmerTest::lmer(data = subset(Functional_Diversity_18,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_18, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_18 <- lmerTest::lmer(data = subset(Functional_Diversity_18,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_18, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_18 <- lmerTest::lmer(data = subset(Functional_Diversity_18,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_18, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_18 <- lmerTest::lmer(data = subset(Functional_Diversity_18,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_18, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_18 <- lmerTest::lmer(data = subset(Functional_Diversity_18,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_18, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_18 <- lmerTest::lmer(data = subset(Functional_Diversity_18,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_18, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_18 <- lmerTest::lmer(data = subset(Functional_Diversity_18,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_18, type = 3)

#### percent_green with 18 as covariate ####
Functional_Diversity_percent_green_2018<-Functional_Diversity_percent_green %>% 
  filter(year==2018) %>% 
  rename(FDis_18=FDis) %>% 
  select(Site, plot,block,paddock,FDis_18)

Functional_Diversity_percent_green_18<-Functional_Diversity_percent_green %>% 
  filter(year!=2018) %>% 
  left_join(Functional_Diversity_percent_green_2018)

###Functional Dispersion

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_percent_green_18 <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green_18,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_percent_green_18, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_percent_green_18 <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green_18,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_percent_green_18, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_percent_green_18 <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green_18,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_percent_green_18, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_percent_green_18 <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green_18,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_percent_green_18, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_percent_green_18 <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green_18,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_percent_green_18, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_percent_green_18 <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green_18,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_percent_green_18, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_percent_green_18 <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green_18,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_percent_green_18, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_percent_green_18 <- lmerTest::lmer(data = subset(Functional_Diversity_percent_green_18,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_percent_green_18, type = 3)

#### Height with 18 as covariate ####
Functional_Diversity_Height_2018<-Functional_Diversity_Height %>% 
  filter(year==2018) %>% 
  rename(FDis_18=FDis) %>% 
  select(Site, plot,block,paddock,FDis_18)

Functional_Diversity_Height_18<-Functional_Diversity_Height %>% 
  filter(year!=2018) %>% 
  left_join(Functional_Diversity_Height_2018)

###Functional Dispersion

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_Height_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Height_18,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_Height_18, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_Height_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Height_18,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_Height_18, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_Height_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Height_18,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_Height_18, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_Height_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Height_18,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_Height_18, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_Height_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Height_18,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_Height_18, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_Height_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Height_18,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_Height_18, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_Height_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Height_18,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_Height_18, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_Height_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Height_18,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_Height_18, type = 3)

#### LDMC with 18 as covariate ####
Functional_Diversity_LDMC_2018<-Functional_Diversity_LDMC %>% 
  filter(year==2018) %>% 
  rename(FDis_18=FDis) %>% 
  select(Site, plot,block,paddock,FDis_18)

Functional_Diversity_LDMC_18<-Functional_Diversity_LDMC %>% 
  filter(year!=2018) %>% 
  left_join(Functional_Diversity_LDMC_2018)

###Functional Dispersion

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_LDMC_18 <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC_18,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_LDMC_18, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_LDMC_18 <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC_18,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_LDMC_18, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_LDMC_18 <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC_18,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_LDMC_18, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_LDMC_18 <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC_18,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_LDMC_18, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_LDMC_18 <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC_18,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_LDMC_18, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_LDMC_18 <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC_18,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_LDMC_18, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_LDMC_18 <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC_18,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_LDMC_18, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_LDMC_18 <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC_18,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_LDMC_18, type = 3)

#### SLA with 18 as covariate ####
Functional_Diversity_SLA_2018<-Functional_Diversity_SLA %>% 
  filter(year==2018) %>% 
  rename(FDis_18=FDis) %>% 
  select(Site, plot,block,paddock,FDis_18)

Functional_Diversity_SLA_18<-Functional_Diversity_SLA %>% 
  filter(year!=2018) %>% 
  left_join(Functional_Diversity_SLA_2018)

###Functional Dispersion

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_SLA_18 <- lmerTest::lmer(data = subset(Functional_Diversity_SLA_18,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_SLA_18, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_SLA_18 <- lmerTest::lmer(data = subset(Functional_Diversity_SLA_18,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_SLA_18, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_SLA_18 <- lmerTest::lmer(data = subset(Functional_Diversity_SLA_18,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_SLA_18, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_SLA_18 <- lmerTest::lmer(data = subset(Functional_Diversity_SLA_18,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_SLA_18, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_SLA_18 <- lmerTest::lmer(data = subset(Functional_Diversity_SLA_18,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_SLA_18, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_SLA_18 <- lmerTest::lmer(data = subset(Functional_Diversity_SLA_18,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_SLA_18, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_SLA_18 <- lmerTest::lmer(data = subset(Functional_Diversity_SLA_18,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_SLA_18, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_SLA_18 <- lmerTest::lmer(data = subset(Functional_Diversity_SLA_18,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_SLA_18, type = 3)

#### leaf_thickness_.mm. with 18 as covariate ####
Functional_Diversity_leaf_thickness_.mm._2018<-Functional_Diversity_leaf_thickness_.mm. %>% 
  filter(year==2018) %>% 
  rename(FDis_18=FDis) %>% 
  select(Site, plot,block,paddock,FDis_18)

Functional_Diversity_leaf_thickness_.mm._18<-Functional_Diversity_leaf_thickness_.mm. %>% 
  filter(year!=2018) %>% 
  left_join(Functional_Diversity_leaf_thickness_.mm._2018)

###Functional Dispersion

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_leaf_thickness_.mm._18 <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm._18,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_leaf_thickness_.mm._18, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_leaf_thickness_.mm._18 <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm._18,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_leaf_thickness_.mm._18, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_leaf_thickness_.mm._18 <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm._18,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_leaf_thickness_.mm._18, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_leaf_thickness_.mm._18 <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm._18,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_leaf_thickness_.mm._18, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_leaf_thickness_.mm._18 <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm._18,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_leaf_thickness_.mm._18, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_leaf_thickness_.mm._18 <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm._18,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_leaf_thickness_.mm._18, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_leaf_thickness_.mm._18 <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm._18,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_leaf_thickness_.mm._18, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_leaf_thickness_.mm._18 <- lmerTest::lmer(data = subset(Functional_Diversity_leaf_thickness_.mm._18,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_leaf_thickness_.mm._18, type = 3)

#### Area with 18 as covariate ####
Functional_Diversity_Area_2018<-Functional_Diversity_Area %>% 
  filter(year==2018) %>% 
  rename(FDis_18=FDis) %>% 
  select(Site, plot,block,paddock,FDis_18)

Functional_Diversity_Area_18<-Functional_Diversity_Area %>% 
  filter(year!=2018) %>% 
  left_join(Functional_Diversity_Area_2018)

###Functional Dispersion

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_Area_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Area_18,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_Area_18, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_Area_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Area_18,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_Area_18, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_Area_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Area_18,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_Area_18, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_Area_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Area_18,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_Area_18, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_Area_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Area_18,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_Area_18, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_Area_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Area_18,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_Area_18, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_Area_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Area_18,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_Area_18, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_Area_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Area_18,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_Area_18, type = 3)

#### Lifespan with 18 as covariate ####
Functional_Diversity_Lifespan_2018<-Functional_Diversity_Lifespan %>% 
  filter(year==2018) %>% 
  rename(FDis_18=FDis) %>% 
  select(Site, plot,block,paddock,FDis_18)

Functional_Diversity_Lifespan_18<-Functional_Diversity_Lifespan %>% 
  filter(year!=2018) %>% 
  left_join(Functional_Diversity_Lifespan_2018)

###Functional Dispersion

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_Lifespan_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan_18,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_Lifespan_18, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_Lifespan_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan_18,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_Lifespan_18, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_Lifespan_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan_18,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_Lifespan_18, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_Lifespan_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan_18,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_Lifespan_18, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_Lifespan_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan_18,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_Lifespan_18, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_Lifespan_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan_18,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_Lifespan_18, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_Lifespan_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan_18,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_Lifespan_18, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_Lifespan_18 <- lmerTest::lmer(data = subset(Functional_Diversity_Lifespan_18,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_Lifespan_18, type = 3)

#### GrowthForm with 18 as covariate ####
Functional_Diversity_GrowthForm_2018<-Functional_Diversity_GrowthForm %>% 
  filter(year==2018) %>% 
  rename(FDis_18=FDis) %>% 
  select(Site, plot,block,paddock,FDis_18)

Functional_Diversity_GrowthForm_18<-Functional_Diversity_GrowthForm %>% 
  filter(year!=2018) %>% 
  left_join(Functional_Diversity_GrowthForm_2018)

###Functional Dispersion

#Functional Dispersion (FDis) FK 19
FK_19_FDis_LMER_GrowthForm_18 <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm_18,year==2019&Site=="FK"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_19_FDis_LMER_GrowthForm_18, type = 3)

#Functional Dispersion (FDis) FK 20
FK_20_FDis_LMER_GrowthForm_18 <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm_18,year==2020&Site=="FK"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_20_FDis_LMER_GrowthForm_18, type = 3)

#Functional Dispersion (FDis) FK 21
FK_21_FDis_LMER_GrowthForm_18 <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm_18,year==2021&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_21_FDis_LMER_GrowthForm_18, type = 3)

#Functional Dispersion (FDis) FK 22
FK_22_FDis_LMER_GrowthForm_18 <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm_18,year==2022&Site=="FK"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(FK_22_FDis_LMER_GrowthForm_18, type = 3)

#Functional Dispersion (FDis) TB 19
TB_19_FDis_LMER_GrowthForm_18 <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm_18,year==2019&Site=="TB"), FDis ~ Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_19_FDis_LMER_GrowthForm_18, type = 3)

#Functional Dispersion (FDis) TB 20
TB_20_FDis_LMER_GrowthForm_18 <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm_18,year==2020&Site=="TB"), FDis ~ Grazing_2020*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_20_FDis_LMER_GrowthForm_18, type = 3)

#Functional Dispersion (FDis) TB 21
TB_21_FDis_LMER_GrowthForm_18 <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm_18,year==2021&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_21_FDis_LMER_GrowthForm_18, type = 3)

#Functional Dispersion (FDis) TB 22
TB_22_FDis_LMER_GrowthForm_18 <- lmerTest::lmer(data = subset(Functional_Diversity_GrowthForm_18,year==2022&Site=="TB"), FDis ~ grazing_treatment*Rainfall_reduction_cat + FDis_18 + (1|block) + (1|block:paddock))
anova(TB_22_FDis_LMER_GrowthForm_18, type = 3)


#### compare 2018 to 2019 data ####

#filtering out so that we only have 2018 data from TB that lines up with future plots
Sp_18<-Species_Comp_RelCov_All %>% 
  filter(year==2018 & site=="TB") %>% 
  filter(plot!=1 & plot!=2 & plot!=3 & plot!=4& plot!=5 & plot!=6 & plot!=19 & plot!=20 & plot!=21 & plot!=22 & plot!=23 & plot!=24 & plot!=25 & plot!=26) %>%
  rename(Relative_Cover_18=Relative_Cover) %>% 
  select(-year)

#filtering out so that we only have 2019 data from TB that lines up with 2018 plots
Sp_19<-Species_Comp_RelCov_All %>% 
  filter(year==2019 & site=="TB") %>% 
  filter(plot!=1 & plot!=2 & plot!=3 & plot!=4& plot!=5 & plot!=6 & plot!=19 & plot!=20 & plot!=21 & plot!=22 & plot!=23 & plot!=24 & plot!=25 & plot!=26)%>%
  rename(Relative_Cover_19=Relative_Cover) %>% 
  select(-year)

#merge dataframes together
Sp_18_19_Dif_1<-Sp_19 %>% 
  left_join(Sp_18) %>% 
  replace(is.na(.), 0)

Sp_18_19_Dif_2<-Sp_18 %>% 
  left_join(Sp_19) %>% 
  replace(is.na(.), 0)

Sp_18_19_Dif_Species<Sp_18_19_Dif_1 %>% 
  rbind(Sp_18_19_Dif_2) %>% 
  select(-site,plot,)

Sp_18_19_Dif<-Sp_18_19_Dif_1 %>% 
  rbind(Sp_18_19_Dif_2) %>% 
  unique() %>% 
  mutate(Difference=Relative_Cover_19-Relative_Cover_18) %>% 
  left_join(plot_layoutK) %>% 
  select(site,plot,block,Genus_Species_Correct,Difference,grazing_treatment) %>% 
  group_by(Genus_Species_Correct,grazing_treatment,block) %>% 
  summarize(Difference_Std=sd(Difference),Difference_Mean=mean(Difference),Difference_n=length(Difference)) %>% 
  mutate(Difference_St_Error=Difference_Std/sqrt(Difference_n)) %>% 
  mutate(Lower=Difference_Mean-Difference_St_Error)%>% 
  mutate(Upper=Difference_Mean+Difference_St_Error) %>% 
  select(block,Genus_Species_Correct,grazing_treatment,Lower,Difference_Mean,Upper) %>% 
  ungroup() 

ggplot(subset(Sp_18_19_Dif,grazing_treatment=="stable"),aes(x=Difference_Mean, y=Genus_Species_Correct)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "solid") +
  geom_errorbarh(aes(xmin=Lower,xmax=Upper), linewidth = .8, height = .2, color = "gray50")+
  geom_point(size=4)+
  facet_wrap(~block)

ggplot(subset(Sp_18_19_Dif,grazing_treatment=="destock"),aes(x=Difference_Mean, y=Genus_Species_Correct)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "solid") +
  geom_errorbarh(aes(xmin=Lower,xmax=Upper), linewidth = .8, height = .2, color = "gray50")+
  geom_point(size=4)+
  facet_wrap(~block)

ggplot(subset(Sp_18_19_Dif,grazing_treatment=="heavy"),aes(x=Difference_Mean, y=Genus_Species_Correct)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "solid") +
  geom_errorbarh(aes(xmin=Lower,xmax=Upper), linewidth = .8, height = .2, color = "gray50")+
  geom_point(size=4)+
  facet_wrap(~block)

#### magnitude of significance from 2018 compared to 2022 #### 

#2018 significance
#height: grazing (2018: 2022)
#leaf thickness: grazing (2018: 2021 , 2018:2022)

CWM_Collected_Data_TB_18_nt<-CWM_Collected_Data %>% 
  filter(Site=="TB",year==2018) %>% 
  rename(Height_CWM_18=Height_CWM) %>% 
  rename(LeafThickness_CWM_18=LeafThickness_CWM) %>% 
  select(Site,plot,block,paddock,grazing_treatment,Rainfall_reduction_cat,Height_CWM_18,LeafThickness_CWM_18)

CWM_Collected_Data_TB_21_nt<-CWM_Collected_Data %>% 
  filter(Site=="TB",year==2021) %>% 
  rename(LeafThickness_CWM_21=LeafThickness_CWM) %>% 
  select(Site,plot,block,paddock,grazing_treatment,Rainfall_reduction_cat,LeafThickness_CWM_21)

CWM_Collected_Data_TB_22_nt<-CWM_Collected_Data %>% 
  filter(Site=="TB",year==2022) %>% 
  rename(Height_CWM_22=Height_CWM) %>% 
  rename(LeafThickness_CWM_22=LeafThickness_CWM) %>% 
  select(Site,plot,block,paddock,grazing_treatment,Rainfall_reduction_cat,Height_CWM_22,LeafThickness_CWM_22)

CWM_Collected_Data_TB_Sig<-CWM_Collected_Data_TB_18_nt %>% 
  left_join(CWM_Collected_Data_TB_21_nt) %>% 
  left_join(CWM_Collected_Data_TB_22_nt) 

CWM_Collected_Data_TB_dif <-CWM_Collected_Data_TB_Sig %>% 
  group_by(grazing_treatment) %>% 
  summarise(Height_18=mean(Height_CWM_18),Height_22=mean(Height_CWM_22),Thickness_18=mean(LeafThickness_CWM_18),Thickness_21=mean(LeafThickness_CWM_21),Thickness_22=mean(LeafThickness_CWM_22)) %>% 
  ungroup()
  
CWM_Differences_TB<-t(CWM_Collected_Data_TB_dif)
  
CWM_Differences_TB_df<-as.data.frame(CWM_Differences_TB) 

colnames(CWM_Differences_TB_df) = c("destock", "heavy", "stable")

CWM_Differences_TB_df<-rownames_to_column(CWM_Differences_TB_df, var = "rowname")

CWM_Differences_TB_df= CWM_Differences_TB_df[-1,]

CWM_Differences_TB_difference<-CWM_Differences_TB_df %>% 
  group_by(rowname) %>% 
  summarise(Stable_Heavy=as.numeric(stable)-as.numeric(heavy),Stable_Destock=as.numeric(stable)-as.numeric(destock),Heavy_Destock=as.numeric(heavy)-as.numeric(destock)) %>% 
  ungroup()

CWM_Differences_TB_difference_t<-t(CWM_Differences_TB_difference)

colnames(CWM_Differences_TB_difference_t) = c("Height_18", "Height_22", "Thickness_18","Thickness_21","Thickness_22")

CWM_Differences_TB_difference_t= CWM_Differences_TB_difference_t[-1,]

CWM_Differences_TB_difference_t<-as.data.frame(CWM_Differences_TB_difference_t)

CWM_Differences_TB_difference_t$Height_18<-as.numeric(CWM_Differences_TB_difference_t$Height_18)
CWM_Differences_TB_difference_t$Height_22<-as.numeric(CWM_Differences_TB_difference_t$Height_22)
CWM_Differences_TB_difference_t$Thickness_18<-as.numeric(CWM_Differences_TB_difference_t$Thickness_18)
CWM_Differences_TB_difference_t$Thickness_21<-as.numeric(CWM_Differences_TB_difference_t$Thickness_21)
CWM_Differences_TB_difference_t$Thickness_22<-as.numeric(CWM_Differences_TB_difference_t$Thickness_22)

CWM_Differences_TB_difference_percent<-CWM_Differences_TB_difference_t %>% 
  mutate(Mean_Height=(Height_18+Height_22)/2, Mean_Thickness_21=(Thickness_18+Thickness_21)/2, Mean_Thickness_22=(Thickness_18+Thickness_22)/2,Dif_Height=abs(Height_18-Height_22),Dif_Thickness_21=abs(Thickness_18-Thickness_21),Dif_Thickness_22=abs(Thickness_18-Thickness_22)) %>% 
  mutate(Perc_Difference_Height=(Dif_Height/Mean_Height)*100,Perc_Difference_Thickness_21=(Dif_Thickness_21/Mean_Thickness_21)*100,Perc_Difference_Thickness_22=(Dif_Thickness_22/Mean_Thickness_22)*100) %>% 
  select(Perc_Difference_Height,Perc_Difference_Thickness_21,Perc_Difference_Thickness_22)







