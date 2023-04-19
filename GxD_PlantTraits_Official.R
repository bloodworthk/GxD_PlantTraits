##########################################################################################################
#Project: Dissertation: Community Weighted Plant Traits in MGP for Drought x Grazing 

##########################################################################################################

#### Load Libraries ####

#install.packages("lme4")
library(lme4)
#install.packages("ggfortify")
library(ggfortify)
library(ggplot2)
library(grid)
#install.packages("lattice")
library(lattice)
#install.packages("FD")
library(FD)
#install.packages("emmeans")
library(emmeans)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages("factoextra")
library(factoextra)
#install.packages("multcomp")
library(multcomp)
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
Field_Traits<-read.csv("DxG_Plant_Traits/2022_DxG_CWM_FieldTraits.csv") %>% 
  dplyr::select(Site,DxG_block, paddock, genus_species, species_code, height_cm, emerging_leaves, developed_leaves, scenesced_leaves, flower_heads, open_flowers, percent_green, lifespan, growth_form,photosynthetic_pathway, Date, Season, people, comments)

#Read in lab data and then rename columns for future merging steps
Lab_Traits<-read.csv("DxG_Plant_Traits/2022_DxG_CWM_LabTraits.csv") %>% 
  rename(Site=site) %>% 
  rename(DxG_block=block) %>% 
  rename(Season=season) %>% 
  rename(comments_lab=comments) %>% 
  rename(date_lab=date) %>% 
  rename(people_lab=personelle) %>% 
  #removing genus species and season from this dataframe to avoid spelling issues and inconsistancies with data entered
  dplyr::select(-genus_species,-Season)

#Read in dry lab data and then rename columns for future merging steps
Dry_Traits<-read.csv("DxG_Plant_Traits/2022_DxG_DRYLabTraits_ALL_CHECKED.csv") %>% 
  rename(DxG_block=Block) %>% 
  rename(paddock=Grazing_Paddock) %>% 
  rename(species_code=Species) %>% 
  dplyr::select(Site,DxG_block,paddock,species_code,Biomass_Type,Dry_Weight_g) %>% 
  #change incorrect, inconsistent, or inproperly written data
  mutate(Dry_Weight_g=ifelse(Dry_Weight_g=="<0.0001", 0.00001,ifelse(Dry_Weight_g=="<0.001",0.00001,ifelse(Dry_Weight_g=="MISSING",NA,ifelse(Dry_Weight_g=="REWEIGH",NA,ifelse(Dry_Weight_g=="Empty",NA,ifelse(Dry_Weight_g=="empty???",NA,ifelse(Dry_Weight_g=="EMPTY",NA,Dry_Weight_g))))))))

#Make dataframe with just dry biomass
Dry_Traits_Biomass<-subset(Dry_Traits,Biomass_Type=="B") %>% 
  rename(Dry_Biomass_min_Leaf_g=Dry_Weight_g) %>% 
  dplyr::select(-Biomass_Type)

#make dataframe with just dry leaf weight
Dry_Traits_Leaf<-subset(Dry_Traits,Biomass_Type=="L") %>% 
  rename(Dry_Leaf_Weight_g=Dry_Weight_g) %>% 
  dplyr::select(-Biomass_Type)

#read in leaf area data and rename columns for future merging steps
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


#Read in Species Comp Data
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


#Read in Plot Data
plot_layoutK<-read.csv("DxG_Plant_Traits/GMDR_site_plot_metadata.csv") %>% 
  dplyr::select(site,block,paddock,plot,slope,rainfall_reduction,drought,grazing_category,grazing_treatment,livestock_util_2019,livestock_util_2020,livestock_util_2021)
plot_layoutK$plot<-as.factor(plot_layoutK$plot)

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
  group_by(year,Site,plot,block,paddock,slope,rainfall_reduction,drought,grazing_category,grazing_treatment) %>% 
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

#combine FK and TB data together
CWM_Collected_Data<-CWM_Collected_Data_FK %>% 
  rbind(CWM_Collected_Data_TB)


#### Plot CWM data ####

CWM_Collected_Data_avg<-CWM_Collected_Data %>% 
  group_by(Site, year, rainfall_reduction)%>%
  summarize(Height_CWM_Std=sd(Height_CWM),Height_CWM_Mean=mean(Height_CWM),Height_CWM_n=length(Height_CWM),
            PercentGreen_CWM_Std=sd(PercentGreen_CWM),PercentGreen_CWM_Mean=mean(PercentGreen_CWM),PercentGreen_CWM_n=length(PercentGreen_CWM),
            LeafThickness_CWM_Std=sd(LeafThickness_CWM),LeafThickness_CWM_Mean=mean(LeafThickness_CWM),LeafThickness_CWM_n=length(LeafThickness_CWM),
            LDMC_CWM_Std=sd(LDMC_CWM),LDMC_CWM_Mean=mean(LDMC_CWM),LDMC_CWM_n=length(LDMC_CWM),
            Avg_SLA_CWM_Std=sd(Avg_SLA_CWM),Avg_SLA_CWM_Mean=mean(Avg_SLA_CWM),Avg_SLA_CWM_n=length(Avg_SLA_CWM),
            Area_CWM_Std=sd(Area_CWM),Area_CWM_Mean=mean(Area_CWM),Area_CWM_n=length(Area_CWM),
            Lifespan_CWM_Std=sd(Lifespan_CWM),Lifespan_CWM_Mean=mean(Lifespan_CWM),Lifespan_CWM_n=length(Lifespan_CWM),
            GrowthForm_CWM_Std=sd(GrowthForm_CWM),GrowthForm_CWM_Mean=mean(GrowthForm_CWM),GrowthForm_CWM_n=length(GrowthForm_CWM)
            )%>%
  mutate(Height_CWM_St_Error=Height_CWM_Std/sqrt(Height_CWM_n),
         PercentGreen_CWM_St_Error=PercentGreen_CWM_Std/sqrt(PercentGreen_CWM_n),
         LeafThickness_CWM_St_Error=LeafThickness_CWM_Std/sqrt(LeafThickness_CWM_n),
         LDMC_CWM_St_Error=LDMC_CWM_Std/sqrt(LDMC_CWM_n),
         Avg_SLA_CWM_St_Error=Avg_SLA_CWM_Std/sqrt(Avg_SLA_CWM_n),
         Area_CWM_St_Error=Area_CWM_Std/sqrt(Area_CWM_n),
         Lifespan_CWM_St_Error=Lifespan_CWM_Std/sqrt(Lifespan_CWM_n),
         GrowthForm_CWM_St_Error=GrowthForm_CWM_Std/sqrt(GrowthForm_CWM_n)) %>% 
  ungroup()


#### CWM of Height ####

#Fort Keogh all yearst 
Height_FK_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=Height_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5)+
  geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5)+
  geom_pointrange(aes(ymin=Height_CWM_Mean-Height_CWM_St_Error,ymax=Height_CWM_Mean+Height_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_alpha_manual(values=c(0,1,1,0))+
  #scale_linetype_manual(values=c("clear","solid","solid","clear"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(10,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.80),legend.key = element_rect(size=20), legend.key.size = unit(5.0, 'lines'))+
  annotate("text", x=10, y=20, label = "A. Height", size=20)

#Thunder Basin all years - not significant for any drought
Height_TB_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=Height_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=Height_CWM_Mean-Height_CWM_St_Error,ymax=Height_CWM_Mean+Height_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(5,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.80),legend.key = element_rect(size=20), legend.key.size = unit(5.0, 'lines'))+
  annotate("text", x=9, y=20, label = "A. Height", size=20)


#### CWM of PercentGreen ####

#Fort Keogh all years - significant for 2020 and 2021
PercentGreen_FK_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=PercentGreen_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2020), method='lm', se=FALSE,color="blue4",size=5)+
  geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5)+
  geom_pointrange(aes(ymin=PercentGreen_CWM_Mean-PercentGreen_CWM_St_Error,ymax=PercentGreen_CWM_Mean+PercentGreen_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(85,95))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=18, y=95, label = "B. Percent Green", size=20)

# Thunder Basin all years - significant in 2020 and 2022
PercentGreen_TB_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=PercentGreen_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2020), method='lm', se=FALSE,color="blue4",size=5,linetype="dashed")+
  geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5)+
  geom_pointrange(aes(ymin=PercentGreen_CWM_Mean-PercentGreen_CWM_St_Error,ymax=PercentGreen_CWM_Mean+PercentGreen_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(85,95))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=17, y=95, label = "B. Percent Green", size=20)


####CWM of LeafThickness ####

#Fort Keogh all years - no significance
LeafThickness_FK_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=LeafThickness_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=LeafThickness_CWM_Mean-LeafThickness_CWM_St_Error,ymax=LeafThickness_CWM_Mean+LeafThickness_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.3,0.45))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=0.45, label = "C. Leaf Thickness", size=20)

#Thunder Basin all years - no significance
LeafThickness_TB_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=LeafThickness_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=LeafThickness_CWM_Mean-LeafThickness_CWM_St_Error,ymax=LeafThickness_CWM_Mean+LeafThickness_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.25,0.4))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=20, y=0.4, label = "C. Leaf Thickness", size=20)


####CWM of LDMC ####

#Fort Keogh all years - no significance 
LDMC_FK_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=LDMC_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=LDMC_CWM_Mean-LDMC_CWM_St_Error,ymax=LDMC_CWM_Mean+LDMC_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.25,0.45))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=30, y=0.45, label = "D. Leaf Dry Matter Content", size=20)

# Thunder Basin all years - significant in 2020
LDMC_TB_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=LDMC_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=LDMC_CWM_Mean-LDMC_CWM_St_Error,ymax=LDMC_CWM_Mean+LDMC_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.20,0.7))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=27, y=0.7, label = "D. Leaf Dry Matter Content", size=20)

#### CWM of Avg_SLA #### 

#Fort Keogh all years - no significance 
Avg_SLA_FK_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=Avg_SLA_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=Avg_SLA_CWM_Mean-Avg_SLA_CWM_St_Error,ymax=Avg_SLA_CWM_Mean+Avg_SLA_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_y_continuous(labels=comma) + 
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0,1500))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=26, y=1500, label = "E. Specific Leaf Area", size=20)

# Thunder Basin all years - no significance
Avg_SLA_TB_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=Avg_SLA_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=Avg_SLA_CWM_Mean-Avg_SLA_CWM_St_Error,ymax=Avg_SLA_CWM_Mean+Avg_SLA_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_y_continuous(labels = comma)+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0,1500))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=23, y=1500, label = "E. Specific Leaf Area", size=20)


####CWM of LeafArea ####

#Fort Keogh all years - significant in 2020
LeafArea_FK_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=Area_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2020), method='lm', se=FALSE,color="blue4",size=5,linetype="solid")+
  geom_pointrange(aes(ymin=Area_CWM_Mean-Area_CWM_St_Error,ymax=Area_CWM_Mean+Area_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(1,3))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=12, y=3, label = "F. Leaf Area", size=20)

#Thunder Basin all years - significance in 2021
LeafArea_TB_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=Area_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5,linetype="dashed")+
  geom_pointrange(aes(ymin=Area_CWM_Mean-Area_CWM_St_Error,ymax=Area_CWM_Mean+Area_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(1,2.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=12, y=2.5, label = "F. Leaf Area", size=20)

####CWM of Lifespan ####

#Fort Keogh all years - significant in 2022
Lifespan_FK_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=Lifespan_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5)+
  geom_pointrange(aes(ymin=Lifespan_CWM_Mean-Lifespan_CWM_St_Error,ymax=Lifespan_CWM_Mean+Lifespan_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=13, y=1, label = "G. Lifespan", size=20)

#Thunder Basin all years - not significance
Lifespan_TB_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=Lifespan_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=Lifespan_CWM_Mean-Lifespan_CWM_St_Error,ymax=Lifespan_CWM_Mean+Lifespan_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.25,1.25))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=10, y=1.25, label = "G. Lifespan", size=20)


####CWM of GrowthForm ####
# Fort Keogh all years - no significance
GrowthForm_FK_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="FK"&year>=2019),aes(x=rainfall_reduction,y=GrowthForm_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=GrowthForm_CWM_Mean-GrowthForm_CWM_St_Error,ymax=GrowthForm_CWM_Mean+GrowthForm_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.5,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=16, y=1.5, label = "H. Growth Form", size=20)

# Thunder Basin all years - no significance 
GrowthForm_TB_ALL<-ggplot(subset(CWM_Collected_Data_avg,Site=="TB"&year>=2019),aes(x=rainfall_reduction,y=GrowthForm_CWM_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=GrowthForm_CWM_Mean-GrowthForm_CWM_St_Error,ymax=GrowthForm_CWM_Mean+GrowthForm_CWM_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.5,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=15, y=1.5, label = "H. Growth Form", size=20)

#### Create graph of all years for CWM ####

#FK
pushViewport(viewport(layout=grid.layout(4,2)))
print(Height_FK_ALL,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(PercentGreen_FK_ALL,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LeafThickness_FK_ALL,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_FK_ALL,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(Avg_SLA_FK_ALL,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(LeafArea_FK_ALL,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(Lifespan_FK_ALL,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(GrowthForm_FK_ALL,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#Save at 3500 x 4000  

#TB
pushViewport(viewport(layout=grid.layout(4,2)))
print(Height_TB_ALL,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(PercentGreen_TB_ALL,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LeafThickness_TB_ALL,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_TB_ALL,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(Avg_SLA_TB_ALL,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(LeafArea_TB_ALL,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(Lifespan_TB_ALL,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(GrowthForm_TB_ALL,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#Save at 3500 x 4000  

#### Bar Graphs for Grazing ####

#### CWM of Height FK Grazing ####
Height_Grazing_FK<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Height_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(5,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.9),legend.key = element_rect(size=30), legend.key.size = unit(7.0, 'lines'))+
  annotate("text", x=1.5, y=20, label = "A. Height", size=30)


#### CWM of Height TB Grazing ####
Height_Grazing_TB<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Height_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(5,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.9),legend.key = element_rect(size=30), legend.key.size = unit(7.0, 'lines'))+
  annotate("text", x=1.5, y=20, label = "A. Height", size=30)

#### CWM of Percent Green FK Grazing ####
Green_Grazing_FK<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=PercentGreen_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2, y=100, label = "B. Percent Green", size=30)


#### CWM of Green TB Grazing ####
Green_Grazing_TB<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=PercentGreen_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(80,100))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2, y=100, label = "B. Percent Green", size=30)

#### CWM of Leaf Thickness FK Grazing ####
Thickness_Grazing_FK<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=LeafThickness_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.25,0.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2.5, y=0.5, label = "C. Leaf Thickness", size=30)


#### CWM of Leaf Thickness TB Grazing ####
Thickness_Grazing_TB<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=LeafThickness_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.25,0.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2.5, y=0.5, label = "C. Leaf Thickness", size=30)

#### CWM of LDMC FK Grazing ####
LDMC_Grazing_FK<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=LDMC_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.2,0.6))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=3, y=0.6, label = "D. Leaf Dry Matter Content", size=30)


#### CWM of LDMC TB Grazing ####
LDMC_Grazing_TB<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=LDMC_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.2,0.6))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=3, y=0.6, label = "D. Leaf Dry Matter Content", size=30)

#### CWM of SLA FK Grazing ####
SLA_Grazing_FK<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Avg_SLA_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0,2000))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=3, y=2500, label = "E. Specific Leaf Area", size=30)


#### CWM of SLA TB Grazing ####
SLA_Grazing_TB<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Avg_SLA_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0,2000))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=3, y=2500, label = "E. Specific Leaf Area", size=30)

#### CWM of Leaf Area FK Grazing ####
LeafArea_Grazing_FK<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Area_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.5,4))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2.5, y=4, label = "F. Leaf Area", size=30)


#### CWM of Leaf Area TB Grazing ####
LeafArea_Grazing_TB<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Area_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
            ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.5,4))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2.5, y=4, label = "F. Leaf Area", size=30)

#### CWM of Lifespan FK Grazing ####
Lifespan_Grazing_FK<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Lifespan_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Year")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=2, y=1.5, label = "G. Lifespan", size=30)

#### CWM of Lifespan TB Grazing ####
Lifespan_Grazing_TB<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Lifespan_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=2, y=1.5, label = "G. Lifespan", size=30)

#### CWM of Growth Form FK Grazing ####
GrowthForm_Grazing_FK<-ggplot(subset(CWM_Collected_Data,Site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=GrowthForm_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.5,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=2, y=1.5, label = "H. Growth Form", size=30)

#### CWM of Growth Form TB Grazing ####
GrowthForm_Grazing_TB<-ggplot(subset(CWM_Collected_Data,Site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=GrowthForm_CWM,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.5,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=2, y=1.5, label = "H. Growth Form", size=30)

#### Create graph of all years for CWM Grazing ####

#FK
pushViewport(viewport(layout=grid.layout(4,2)))
print(Height_Grazing_FK,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Green_Grazing_FK,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Thickness_Grazing_FK,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_Grazing_FK,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(SLA_Grazing_FK,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(LeafArea_Grazing_FK,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(Lifespan_Grazing_FK,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(GrowthForm_Grazing_FK,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#Save at 3500 x 4000  

#TB
pushViewport(viewport(layout=grid.layout(4,2)))
print(Height_Grazing_TB,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Green_Grazing_TB,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Thickness_Grazing_TB,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_Grazing_TB,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(SLA_Grazing_TB,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(LeafArea_Grazing_TB,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(Lifespan_Grazing_TB,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(GrowthForm_Grazing_TB,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#Save at 3500 x 4000  

##### CWM Stats ####

#CWM Data: Stats - FK#
#Make a list of all p-values and use p.adjust to correct all p-values at once 

### CWM Height FK Stats #### 

#CWM of height for Fort Keogh 2019 - LMER
FK_Height_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Height_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Height_2019_LMER_slope, type = 3) #ns

#CWM of height for Fort Keogh 2020 - LMER
FK_Height_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), Height_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Height_2020_LMER_slope, type = 3) #ns

#CWM of height for Fort Keogh 2021 - LMER
FK_Height_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Height_2021_LMER_slope, type = 3) #drought (p=0.001088) , #GxD (p=0.05884)
#adjust drought p-value
p.adjust(0.001088, method = "BH", n=9) #0.00972
#adjust drought*grazing p-value
p.adjust(0.058844, method = "BH", n=9) #0.529596
summary(glht(FK_Height_2021_LMER_slope, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
      #75%-0% (p=0.002) , #75%-25% (p=0.0003)

#adjust drought p-value (4 traits + multidemensional)
p.adjust(0.001088, method = "BH", n=5) #0.00544
#adjust drought*grazing p-value
p.adjust(0.058844, method = "BH", n=5) #0.529596


#CWM of height for Fort Keogh 2022 - LMER
FK_Height_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Height_2022_LMER_slope, type = 3) #drought (p=0.02882)
#adjust drought p-value
p.adjust(0.02882, method = "BH", n=9) #0.25938
summary(glht(FK_Height_2022_LMER_slope, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#50%-25% (p=0.0234) , #75%-25% (p=0.0134), #99%-25% (p=0.0209)

#adjust drought p-value (4 traits + multidemensional)
p.adjust(0.02882, method = "BH", n=5) #0.1441


### CWM Percent Green FK Stats #### 

#CWM of PercentGreen for Fort Keogh 2019 - LMER
FK_PercentGreen_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), PercentGreen_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_PercentGreen_2019_LMER, type = 3) #ns

#CWM of PercentGreen for Fort Keogh 2020 - LMER
FK_PercentGreen_2020_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), PercentGreen_CWM_TF ~ Grazing_2020 *Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_PercentGreen_2020_LMER, type = 3) #drought (0.01242)
#adjust drought p-value
p.adjust(0.01242, method = "BH", n=9) #0.11178
summary(glht(FK_PercentGreen_2020_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#NS

#CWM of PercentGreen for Fort Keogh 2021 - LMER
FK_PercentGreen_2021_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_PercentGreen_2021_LMER, type = 3) #drought (0.003823)
#adjust drought p-value
p.adjust(0.003823, method = "BH", n=9) #0.034407
summary(glht(FK_PercentGreen_2021_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#NS

#CWM of PercentGreen for Fort Keogh 2022 - LMER
FK_PercentGreen_2022_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_PercentGreen_2022_LMER, type = 3)

### CWM Leaf Thickness FK Stats ####  

#CWM of LeafThickness for Fort Keogh 2019 - LMER
FK_LeafThickness_2019_LMER <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), LeafThickness_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LeafThickness_2019_LMER, type = 3) #ns

#CWM of LeafThickness for Fort Keogh 2020 - LMER
FK_LeafThickness_2020_LMER_slope_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), LeafThickness_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LeafThickness_2020_LMER_slope_slope, type = 3) #ns

#CWM of LeafThickness for Fort Keogh 2021 - LMER
FK_LeafThickness_2021_LMER_slope_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LeafThickness_2021_LMER_slope_slope, type = 3) #ns

#CWM of LeafThickness for Fort Keogh 2022 - LMER
FK_LeafThickness_2022_LMER_slope_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LeafThickness_2022_LMER_slope_slope, type = 3) #ns

#### CWM LDMC FK Stats ####

#CWM of LDMC for Fort Keogh 2019 - LMER
FK_LDMC_2019_LMER_slope_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), LDMC_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LDMC_2019_LMER_slope_slope, type = 3) #ns

#CWM of LDMC for Fort Keogh 2020 - LMER
FK_LDMC_2020_LMER_slope_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), LDMC_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LDMC_2020_LMER_slope_slope, type = 3) #grazing is marginally significant
#adjust grazing p-value
p.adjust(0.09863, method = "BH", n=9) #0.88767
summary(glht(FK_LDMC_2020_LMER_slope_slope, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) #medium-high (0.0293)

#CWM of LDMC for Fort Keogh 2021 - LMER
FK_LDMC_2021_LMER_slope_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LDMC_2021_LMER_slope_slope, type = 3) #ns
 
#CWM of LDMC for Fort Keogh 2022 - LMER
FK_LDMC_2022_LMER_slope_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LDMC_2022_LMER_slope_slope, type = 3) #ns

#### CWM SLA FK Stats ####  

#CWM of SLA for Fort Keogh 2019 - LMER
FK_SLA_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Avg_SLA_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_SLA_2019_LMER_slope, type = 3) #NS

#CWM of SLA for Fort Keogh 2020 - LMER
FK_SLA_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), Avg_SLA_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_SLA_2020_LMER_slope, type = 3) #grazing (0.02132)
#adjust grazing p-value
p.adjust(0.02132, method = "BH", n=9) #0.19188
summary(glht(FK_SLA_2020_LMER_slope, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) #NS

#CWM of SLA for Fort Keogh 2021 - LMER
FK_SLA_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_SLA_2021_LMER_slope, type = 3) #ns

#CWM of SLA for Fort Keogh 2022 - LMER
FK_SLA_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_SLA_2022_LMER_slope, type = 3) #ns

#### CWM Leaf Area FK Stats ####  

#CWM of Area for Fort Keogh 2019 - LMER
FK_Area_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Area_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Area_2019_LMER_slope, type = 3) #ns

#CWM of Area for Fort Keogh 2020 - LMER
FK_Area_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), Area_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Area_2020_LMER_slope, type = 3) #drought (0.02781)
#adjust drought p-value
p.adjust(0.02781, method = "BH", n=9) #0.25029
summary(glht(FK_Area_2020_LMER_slope, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#marginal significance between 75%-0% and 75%-50%

#adjust drought p-value (5)
p.adjust(0.02781, method = "BH", n=5) #0.13905

#CWM of Area for Fort Keogh 2021 - LMER
FK_Area_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Area_2021_LMER_slope, type = 3) #ns

#CWM of Area for Fort Keogh 2022 - LMER
FK_Area_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Area_2022_LMER_slope, type = 3) #ns

#### CWM Lifespan FK Stats ####  

#CWM of Lifespan for Fort Keogh 2019 - LMER
FK_Lifespan_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Lifespan_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Lifespan_2019_LMER_slope, type = 3) #ns

#CWM of Lifespan for Fort Keogh 2020 - LMER
FK_Lifespan_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), Lifespan_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Lifespan_2020_LMER_slope, type = 3) #ns

#CWM of Lifespan for Fort Keogh 2021 - LMER
FK_Lifespan_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Lifespan_2021_LMER_slope, type = 3) #ns

#CWM of Lifespan for Fort Keogh 2022 - LMER
FK_Lifespan_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Lifespan_2022_LMER_slope, type = 3) #drought (0.02106)
#adjust drought p-value
p.adjust(0.02106, method = "BH", n=9) #0.18954
summary(glht(FK_Lifespan_2022_LMER_slope, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
        #99%-0% (p=0.029) #99%-25% (p=0.029)



#### CWM Growth Form FK Stats ####  

#CWM of GrowthForm for Fort Keogh 2019 - LMER
FK_GrowthForm_2019_LMER_slope <- lmerTest:: lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), GrowthForm_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_GrowthForm_2019_LMER_slope, type = 3) #ns

#CWM of GrowthForm for Fort Keogh 2020 - LMER
FK_GrowthForm_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), GrowthForm_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_GrowthForm_2020_LMER_slope, type = 3) #ns

#CWM of GrowthForm for Fort Keogh 2021 - LMER
FK_GrowthForm_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_GrowthForm_2021_LMER_slope, type = 3) #ns

#CWM of GrowthForm for Fort Keogh 2022 - LMER
FK_GrowthForm_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_GrowthForm_2022_LMER_slope, type = 3) #ns

#### CWM Data: Stats - TB ####

### CWM Height TB Stats #### 

#CWM of height for Thunder Basin 2019 - LMER
TB_Height_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Height_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Height_2019_LMER_slope, type = 3) #ns

#CWM of height for Thunder Basin 2020 - LMER
TB_Height_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), Height_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Height_2020_LMER_slope, type = 3) #grazing (0.0009)
#adjust grazing p-value
p.adjust(0.0009077, method = "BH", n=9) #0.0081693
summary(glht(TB_Height_2020_LMER_slope, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) 
#NS

#adjust grazing p-value (5)
p.adjust(0.0009077, method = "BH", n=5) #0.0045385

#CWM of height for Thunder Basin 2021 - LMER
TB_Height_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Height_2021_LMER_slope, type = 3) #grazing (0.0008)
#adjust grazing p-value
p.adjust(0.0008097, method = "BH", n=9) #0.0072873
summary(glht(TB_Height_2021_LMER_slope, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
#NS

#adjust grazing p-value (5)
p.adjust(0.0008097, method = "BH", n=5) #0.0040485

#CWM of height for Thunder Basin 2022 - LMER
TB_Height_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), Height_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Height_2022_LMER_slope, type = 3) #grazing (0.0003598)
#adjust grazing p-value
p.adjust(0.0003598, method = "BH", n=9) #0.0032382
summary(glht(TB_Height_2022_LMER_slope, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
#NS
#adjust grazing p-value (5)
p.adjust(0.0003598, method = "BH", n=5) #0.001799

### CWM Percent Green TB Stats #### 

#CWM of PercentGreen for Thunder Basin 2019 - LMER
TB_PercentGreen_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), PercentGreen_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_PercentGreen_2019_LMER_slope, type = 3) #ns

#CWM of PercentGreen for Thunder Basin 2020 - LMER
TB_PercentGreen_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), PercentGreen_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_PercentGreen_2020_LMER_slope, type = 3) #GxD (p=0.05846)
#adjust GxD p-value
p.adjust(0.05846, method = "BH", n=9) #0.52614

#CWM of PercentGreen for Thunder Basin 2021 - LMER
TB_PercentGreen_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_PercentGreen_2021_LMER_slope, type = 3) #Grazing (0.04374)
#adjust grazing p-value
p.adjust(0.04274, method = "BH", n=9) #0.38466
summary(glht(TB_PercentGreen_2021_LMER_slope, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
#NS

#CWM of PercentGreen for Thunder Basin 2022 - LMER
TB_PercentGreen_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), PercentGreen_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_PercentGreen_2022_LMER_slope, type = 3) #grazing (0.01929) , #drought (0.03556)
#adjust grazing p-value
p.adjust(0.01929, method = "BH", n=9) #0.17361
#adjust drought p-value
p.adjust(0.03556, method = "BH", n=9) #0.32004
summary(glht(TB_PercentGreen_2022_LMER_slope, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
          #heavy-destock (0.0169), #stable-destock (0.0596)
summary(glht(TB_PercentGreen_2022_LMER_slope, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
          #NS


### CWM Leaf Thickness TB Stats ####  

#CWM of LeafThickness for Thunder Basin 2019 - LMER
TB_LeafThickness_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), LeafThickness_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LeafThickness_2019_LMER_slope, type = 3) #ns

#CWM of LeafThickness for Thunder Basin 2020 - LMER
TB_LeafThickness_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), LeafThickness_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LeafThickness_2020_LMER_slope, type = 3) #grazing (0.01723)
#adjust grazing p-value
p.adjust(0.01723, method = "BH", n=9) #0.15507
summary(glht(TB_LeafThickness_2020_LMER_slope, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) 
      #medium-high (p=0.0495)

#adjust grazing p-value
p.adjust(0.01723, method = "BH", n=5) #0.086

#CWM of LeafThickness for Thunder Basin 2021 - LMER
TB_LeafThickness_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LeafThickness_2021_LMER_slope, type = 3) #grazing (0.01115)
#adjust grazing p-value
p.adjust(0.01115, method = "BH", n=9) #0.10035
summary(glht(TB_LeafThickness_2021_LMER_slope, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
#marginal difference between stable-destock

#adjust grazing p-value
p.adjust(0.01115, method = "BH", n=5) #0.05575

#CWM of LeafThickness for Thunder Basin 2022 - LMER
TB_LeafThickness_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), LeafThickness_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LeafThickness_2022_LMER_slope, type = 3) #grazing (0.0002)
#adjust grazing p-value
p.adjust(0.0002094, method = "BH", n=9) #0.0018846
summary(glht(TB_LeafThickness_2022_LMER_slope, linfct = mcp(grazing_treatment= "Tukey")), test = adjusted(type = "BH")) 
#heavy-destock (0.00989) , #stable-destock (p=0.00174)

#adjust grazing p-value
p.adjust(0.0002094, method = "BH", n=5) #0.001047

#### CWM LDMC TB Stats #### 

#CWM of LDMC for TB 2019 - LMER
TB_LDMC_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), LDMC_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LDMC_2019_LMER_slope, type = 3) #ns

#CWM of LDMC for TB 2020 - LMER
TB_LDMC_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), LDMC_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LDMC_2020_LMER_slope, type = 3) #DxG (0.09019)
#adjust DxG p-value
p.adjust(0.09019, method = "BH", n=9) #0.81171


#CWM of LDMC for TB 2021 - LMER
TB_LDMC_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LDMC_2021_LMER_slope, type = 3) #ns

#CWM of LDMC for TB 2022 - LMER
TB_LDMC_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), LDMC_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LDMC_2022_LMER_slope, type = 3) #ns

#### CWM SLA TB Stats ####  

#CWM of SLA for TB 2019 - LMER
TB_SLA_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Avg_SLA_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_SLA_2019_LMER_slope, type = 3) #ns

#CWM of SLA for TB 2020 - LMER
TB_SLA_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), Avg_SLA_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_SLA_2020_LMER_slope, type = 3) #grazing (0.04202)
#adjust grazing p-value
p.adjust(0.04202, method = "BH", n=9) #0.37818
summary(glht(TB_SLA_2020_LMER_slope, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) 
#ns

#CWM of SLA for TB 2021 - LMER
TB_SLA_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_SLA_2021_LMER_slope, type = 3) #ns

#CWM of SLA for TB 2022 - LMER
TB_SLA_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), Avg_SLA_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_SLA_2022_LMER_slope, type = 3) #ns

#### CWM Leaf Area TB Stats ####  

#CWM of Area for Thunder Basin 2019 - LMER
TB_Area_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Area_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Area_2019_LMER_slope, type = 3) #ns

#CWM of Area for Thunder Basin 2020 - LMER
TB_Area_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), Area_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Area_2020_LMER_slope, type = 3) #ns

#CWM of Area for Thunder Basin 2021 - LMER
TB_Area_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Area_2021_LMER_slope, type = 3) #DxG (0.09875)
#adjust DxG p-value
p.adjust(0.09875, method = "BH", n=9) #0.88875

#CWM of Area for Thunder Basin 2022 - LMER
TB_Area_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), Area_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Area_2022_LMER_slope, type = 3)#ns

#### CWM Lifespan TB Stats ####  

#CWM of Lifespan for Thunder Basin 2019 - LMER
TB_Lifespan_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Lifespan_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Lifespan_2019_LMER_slope, type = 3) #ns

#CWM of Lifespan for Thunder Basin 2020 - LMER
TB_Lifespan_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), Lifespan_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Lifespan_2020_LMER_slope, type = 3) #ns

#CWM of Lifespan for Thunder Basin 2021 - LMER
TB_Lifespan_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Lifespan_2021_LMER_slope, type = 3) #ns

#CWM of Lifespan for Thunder Basin 2022 - LMER
TB_Lifespan_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), Lifespan_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Lifespan_2022_LMER_slope, type = 3) #ns

#### CWM Growth Form TB Stats ####  

#CWM of GrowthForm for Thunder Basin 2019 - LMER
TB_GrowthForm_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), GrowthForm_CWM_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_GrowthForm_2019_LMER_slope, type = 3) #ns

#CWM of GrowthForm for Thunder Basin 2020 - LMER
TB_GrowthForm_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), GrowthForm_CWM_TF ~ Grazing_2020*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_GrowthForm_2020_LMER_slope, type = 3) #grazing (0.03511)
#adjust grazing p-value
p.adjust(0.03511, method = "BH", n=9) #0.31599
summary(glht(TB_GrowthForm_2020_LMER_slope, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) 
#ns


#CWM of GrowthForm for Thunder Basin 2021 - LMER
TB_GrowthForm_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_GrowthForm_2021_LMER_slope, type = 3) #ns

#CWM of GrowthForm for Thunder Basin 2022 - LMER
TB_GrowthForm_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), GrowthForm_CWM_TF ~ grazing_treatment*Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_GrowthForm_2022_LMER_slope, type = 3) #ns

### CWM Multivariate Space ####

#Create seperate dataframes for each site and year for PCAs

CWM_Collected_Data_FK_19<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2019)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Trtm,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_FK_20<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2020)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_FK_21<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2021)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_FK_22<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2022)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

#TB

CWM_Collected_Data_TB_19<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2019) %>% 
  na.omit(Biomass_CWM)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Trtm,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_TB_20<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2020) %>% 
  na.omit(Biomass_CWM) %>% 
  na.omit(LDMC_CWM)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_TB_21<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2021) %>% 
  filter(!is.na(Biomass_CWM))%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_TB_22<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2022)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

#### PCA for FK 2019 ####
PCA_FK_19<-prcomp(CWM_Collected_Data_FK_19[,11:18],scale=TRUE)
PCA_FK_19
summary(PCA_FK_19)

axes_FK_19 <- predict(PCA_FK_19, newdata = CWM_Collected_Data_FK_19)
head(axes_FK_19, 4)

#put PCA axes with site and plot #   
PCA_FK_19_meta<-cbind(CWM_Collected_Data_FK_19,axes_FK_19)%>%
  dplyr::select(plot,block,slope,paddock,Rainfall_reduction_cat,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_19 <- get_pca_var(PCA_FK_19)
var_FK_19
head(var_FK_19$contrib, 13)


#### PCA for FK 2020 ####
PCA_FK_20<-prcomp(CWM_Collected_Data_FK_20[,12:19],scale=TRUE)
PCA_FK_20
summary(PCA_FK_20)

axes_FK_20 <- predict(PCA_FK_20, newdata = CWM_Collected_Data_FK_20)
head(axes_FK_20, 4)

#put PCA axes with site and plot #   
PCA_FK_20_meta<-cbind(CWM_Collected_Data_FK_20,axes_FK_20)%>%
  dplyr::select(plot,block,slope,paddock,Rainfall_reduction_cat,Grazing_2020,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_20 <- get_pca_var(PCA_FK_20)
var_FK_20
head(var_FK_20$contrib, 12)

#### PCA for FK 2021 ####
PCA_FK_21<-prcomp(CWM_Collected_Data_FK_21[,14:21],scale=TRUE)
PCA_FK_21
summary(PCA_FK_21)

axes_FK_21 <- predict(PCA_FK_21, newdata = CWM_Collected_Data_FK_21)
head(axes_FK_21, 4)

#put PCA axes with site and plot #   
PCA_FK_21_meta<-cbind(CWM_Collected_Data_FK_21,axes_FK_21)%>%
  dplyr::select(plot,block,paddock,slope,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_21 <- get_pca_var(PCA_FK_21)
var_FK_21
head(var_FK_21$contrib, 12)

#### PCA for FK 2022 ####
PCA_FK_22<-prcomp(CWM_Collected_Data_FK_22[,14:21],scale=TRUE)
PCA_FK_22
summary(PCA_FK_22)

axes_FK_22 <- predict(PCA_FK_22, newdata = CWM_Collected_Data_FK_22)
head(axes_FK_22, 4)

#put PCA axes with site and plot #   
PCA_FK_22_meta<-cbind(CWM_Collected_Data_FK_22,axes_FK_22)%>%
  dplyr::select(plot,block,paddock,slope,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_FK_22 <- get_pca_var(PCA_FK_22)
var_FK_22
head(var_FK_22$contrib, 12)

#### PCA for TB 2019 ####
PCA_TB_19<-prcomp(CWM_Collected_Data_TB_19[,11:18],scale=TRUE)
PCA_TB_19
summary(PCA_TB_19)

axes_TB_19 <- predict(PCA_TB_19, newdata = CWM_Collected_Data_TB_19)
head(axes_TB_19, 4)

#put PCA axes with site and plot #   
PCA_TB_19_meta<-cbind(CWM_Collected_Data_TB_19,axes_TB_19)%>%
  dplyr::select(plot,block,paddock,slope,Rainfall_reduction_cat,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_19 <- get_pca_var(PCA_TB_19)
var_TB_19
head(var_TB_19$contrib, 12)

#### PCA for TB 2020 ####
PCA_TB_20<-prcomp(CWM_Collected_Data_TB_20[,12:19],scale=TRUE)
PCA_TB_20
summary(PCA_TB_20)

axes_TB_20 <- predict(PCA_TB_20, newdata = CWM_Collected_Data_TB_20)
head(axes_TB_20, 4)

#put PCA axes with site and plot #   
PCA_TB_20_meta<-cbind(CWM_Collected_Data_TB_20,axes_TB_20)%>%
  dplyr::select(plot,block,paddock,slope,Rainfall_reduction_cat,Grazing_2020,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_20 <- get_pca_var(PCA_TB_20)
var_TB_20
head(var_TB_20$contrib, 12)

#### PCA for TB 2021 ####
PCA_TB_21<-prcomp(CWM_Collected_Data_TB_21[,14:21],scale=TRUE)
PCA_TB_21
summary(PCA_TB_21)

axes_TB_21 <- predict(PCA_TB_21, newdata = CWM_Collected_Data_TB_21)
head(axes_TB_21, 4)

#put PCA axes with site and plot #   
PCA_TB_21_meta<-cbind(CWM_Collected_Data_TB_21,axes_TB_21)%>%
  dplyr::select(plot,block,paddock,slope,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_21 <- get_pca_var(PCA_TB_21)
var_TB_21
head(var_TB_21$contrib, 11)

#### PCA for TB 2022 ####
PCA_TB_22<-prcomp(CWM_Collected_Data_TB_22[,14:21],scale=TRUE)
PCA_TB_22
summary(PCA_TB_22)

axes_TB_22 <- predict(PCA_TB_22, newdata = CWM_Collected_Data_TB_22)
head(axes_TB_22, 4)

#put PCA axes with site and plot #   
PCA_TB_22_meta<-cbind(CWM_Collected_Data_TB_22,axes_TB_22)%>%
  dplyr::select(plot,block,paddock,slope,Rainfall_reduction_cat,grazing_treatment,Trtm,PC1,PC2)

#find contributions of CW traits to PCA axes #
var_TB_22 <- get_pca_var(PCA_TB_22)
var_TB_22
head(var_TB_22$contrib, 12)

#### PCA Graphs #### 

#FK
PCA_FK_19<-autoplot(PCA_FK_19, data=CWM_Collected_Data_FK_19, scale=0, loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=F)

PCA_FK_20<-autoplot(PCA_FK_20, data=CWM_Collected_Data_FK_20, scale=0, colour="Grazing_2020", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=F)+
  theme(legend.position =c(0.1,0.9))

PCA_FK_21<-autoplot(PCA_FK_21, data=CWM_Collected_Data_FK_21, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=F)+
  theme(legend.position = c(0.1,0.9))

PCA_FK_22<-autoplot(PCA_FK_22, data=CWM_Collected_Data_FK_22, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=F)+
  theme(legend.position ="none")

#TB
PCA_TB_19<-autoplot(PCA_TB_19, data=CWM_Collected_Data_TB_19, scale=0, loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=F)+
  theme(legend.position ="none")

PCA_TB_20<-autoplot(PCA_TB_20, data=CWM_Collected_Data_TB_20, scale=0, colour="Grazing_2020", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=F)+
  theme(legend.position = c(0.1,0.9))

PCA_TB_21<-autoplot(PCA_TB_21, data=CWM_Collected_Data_TB_21, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=F)+
  theme(legend.position = c(0.1,0.9))

PCA_TB_22<-autoplot(PCA_TB_22, data=CWM_Collected_Data_TB_22, scale=0, colour="grazing_treatment", loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=F)+
  theme(legend.position ="none")

#Create graph of all years for PCAs
pushViewport(viewport(layout=grid.layout(4,2)))
print(PCA_FK_19,vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(PCA_FK_20,vp=viewport(layout.pos.row=2, layout.pos.col=1))
print(PCA_FK_21,vp=viewport(layout.pos.row=3, layout.pos.col=1))
print(PCA_FK_22,vp=viewport(layout.pos.row=4, layout.pos.col=1))
print(PCA_TB_19,vp=viewport(layout.pos.row=1, layout.pos.col=2))
print(PCA_TB_20,vp=viewport(layout.pos.row=2, layout.pos.col=2))
print(PCA_TB_21,vp=viewport(layout.pos.row=3, layout.pos.col=2))
print(PCA_TB_22,vp=viewport(layout.pos.row=4, layout.pos.col=2))
#Save at 2500 x 1500  

#### PCA Stats ####

#make two seperate dataframes for each year and site to have treatment data and trait data seperate
#adding 1 to all leaf thickness measures since they are negative numbers

## FK ##


#2019
CWM_FK_19_Trait<-CWM_Collected_Data_FK_19 %>% 
  dplyr::select(-year,-Site,-plot,-slope,-block,-paddock,-rainfall_reduction,-drought,-Rainfall_reduction_cat,-Trtm) %>% 
  mutate(LeafThickness_CWM_TF=1+LeafThickness_CWM_TF)

CWM_FK_19_Treatment<-CWM_Collected_Data_FK_19 %>% 
  dplyr::select(year,Site,plot,block,paddock,slope,rainfall_reduction,drought,Rainfall_reduction_cat,Trtm)

#2020
CWM_FK_20_Trait<-CWM_Collected_Data_FK_20 %>% 
  dplyr::select(-year,-Site,-plot,-block,-slope,-paddock,-rainfall_reduction,-drought,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=1+LeafThickness_CWM_TF)

CWM_FK_20_Treatment<-CWM_Collected_Data_FK_20 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2021
CWM_FK_21_Trait<-CWM_Collected_Data_FK_21 %>% 
  dplyr::select(-year,-Site,-plot,-block,-slope,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=1+LeafThickness_CWM_TF)

CWM_FK_21_Treatment<-CWM_Collected_Data_FK_21 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2022
CWM_FK_22_Trait<-CWM_Collected_Data_FK_22 %>% 
  dplyr::select(-year,-Site,-plot,-slope,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=1+LeafThickness_CWM_TF)

CWM_FK_22_Treatment<-CWM_Collected_Data_FK_22 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

## TB ##

#2019
CWM_TB_19_Trait<-CWM_Collected_Data_TB_19 %>% 
  dplyr::select(-year,-Site,-plot,-block,-paddock,-slope,-rainfall_reduction,-drought,-Rainfall_reduction_cat,-Trtm)%>% 
  mutate(LeafThickness_CWM_TF=2+LeafThickness_CWM_TF)

CWM_TB_19_Treatment<-CWM_Collected_Data_TB_19 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Trtm)

#2020
CWM_TB_20_Trait<-CWM_Collected_Data_TB_20 %>% 
  dplyr::select(-year,-Site,-plot,-block,-paddock,-slope,-rainfall_reduction,-drought,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=2+LeafThickness_CWM_TF)

CWM_TB_20_Treatment<-CWM_Collected_Data_TB_20 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2021
CWM_TB_21_Trait<-CWM_Collected_Data_TB_21 %>% 
  dplyr::select(-year,-Site,-plot,-block,-paddock,-slope,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=2+LeafThickness_CWM_TF)

CWM_TB_21_Treatment<-CWM_Collected_Data_TB_21 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2022
CWM_TB_22_Trait<-CWM_Collected_Data_TB_22 %>% 
  dplyr::select(-year,-Site,-plot,-block,-paddock,-slope,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=2+LeafThickness_CWM_TF)

CWM_TB_22_Treatment<-CWM_Collected_Data_TB_22 %>% 
  dplyr::select(year,Site,plot,slope,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#### PerMANOVA ####

# run PERMANOVA using adonis using trait dataframe as data to run adonis on and treatment dataframe as variables

## FK ##
#FK 2019
PERMANOVA_FK_19 <-adonis2(CWM_FK_19_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_FK_19_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_19) #N.S

#FK 2020
PERMANOVA_FK_20 <-adonis2(CWM_FK_20_Trait~Rainfall_reduction_cat*Grazing_2020 + (1|block/slope), data = CWM_FK_20_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_20) 

#FK 2021
PERMANOVA_FK_21 <-adonis2(CWM_FK_21_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/slope), data = CWM_FK_21_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_21) 
#adjust grazing p-value
p.adjust(0.09091, method = "BH", n=9) #0.81

#FK 2022
PERMANOVA_FK_22 <-adonis2(CWM_FK_22_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/slope), data = CWM_FK_22_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_22) 

##TB##
#TB 2019
PERMANOVA_TB_19 <-adonis2(CWM_TB_19_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_TB_19_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_19)  #NS

#TB 2020
PERMANOVA_TB_20 <-adonis2(CWM_TB_20_Trait~Rainfall_reduction_cat*Grazing_2020 + (1|block/slope), data = CWM_TB_20_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_20) #NS

#TB 2021
PERMANOVA_TB_21 <-adonis2(CWM_TB_21_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/slope), data = CWM_TB_21_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_21) #NS

#TB 2022
PERMANOVA_TB_22 <-adonis2(CWM_TB_22_Trait~Rainfall_reduction_cat*grazing_treatment + (1|block/slope), data = CWM_TB_22_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_22) #grazing (0.01299)
#adjust grazing p-value
p.adjust(0.01299, method = "BH", n=5) #0.11691

#### PermDISP ####

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

#### Simper FK ####
#no simper since there were no grazing treatments
#Run a SIMPER test comparing data 
#SIMPER_FK_19 <- with(CWM_FK_19_Treatment,simper(CWM_FK_19_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
#summary(SIMPER_FK_19)

#Run a SIMPER test comparing data 
SIMPER_FK_20 <- with(CWM_FK_20_Treatment,simper(CWM_FK_20_Trait,Grazing_2020))
#Print out a summary of the results
summary(SIMPER_FK_20)

#Run a SIMPER test comparing data 
SIMPER_FK_21 <- with(CWM_FK_21_Treatment,simper(CWM_FK_21_Trait,grazing_treatment))
#Print out a summary of the results
summary(SIMPER_FK_21)

#Run a SIMPER test comparing data 
SIMPER_FK_22 <- with(CWM_FK_22_Treatment,simper(CWM_FK_22_Trait,grazing_treatment))
#Print out a summary of the results
summary(SIMPER_FK_22)

#### Simper TB ####
#no simper since there were no grazing treatments
#Run a SIMPER test comparing data 
#SIMPER_TB_19 <- with(CWM_TB_19_Treatment,simper(CWM_TB_19_Trait,Rainfall_reduction_cat))
#Print out a summary of the results
#summary(SIMPER_TB_19)

#Run a SIMPER test comparing data 
SIMPER_TB_20 <- with(CWM_TB_20_Treatment,simper(CWM_TB_20_Trait,Grazing_2020))
#Print out a summary of the results
summary(SIMPER_TB_20)

#Run a SIMPER test comparing data 
SIMPER_TB_21 <- with(CWM_TB_21_Treatment,simper(CWM_TB_21_Trait,grazing_treatment))
#Print out a summary of the results
summary(SIMPER_TB_21)

#Run a SIMPER test comparing data 
SIMPER_TB_22 <- with(CWM_TB_22_Treatment,simper(CWM_TB_22_Trait,grazing_treatment))
#Print out a summary of the results
summary(SIMPER_TB_22)

#### Single Trait Functional Metrics ####

#### Height Diversity Metrics ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data_Height<-Avg_Traits_FK %>% 
  dplyr::select(Avg_height_cm) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_Height) <- c(1:33)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_Height<-Avg_Traits_FK %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_Height<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames_Height) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_Height<-Species_Comp_FK_Height %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_Height<-Species_Comp_FK_Wide_Height %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_Height<-Species_Comp_FK_Wide_Height %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_Height <- dbFD(Avg_Traits_FK_Data_Height, Species_Comp_FK_Wide_Data_Height,corr = "none")
summary(FK_FunctionalDiversity_Height)

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_Height<-Avg_Traits_TB%>% 
  dplyr::select(Avg_height_cm) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_Height) <- c(1:43)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_Height<-Avg_Traits_TB %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_Height<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames_Height) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_Height<-Species_Comp_TB_Height %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_Height<-Species_Comp_TB_Wide_Height %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_Height<-Species_Comp_TB_Wide_Height %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_Height <- dbFD(Avg_Traits_TB_Data_Height, Species_Comp_TB_Wide_Data_Height,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_Height<-as.data.frame(FK_FunctionalDiversity_Height) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_Height)

Functional_Diversity_TB_Height<-as.data.frame(TB_FunctionalDiversity_Height) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_Height)

Functional_Diversity_Height<-Functional_Diversity_FK_Height %>% 
  rbind(Functional_Diversity_TB_Height) %>% 
  separate(ID,c("year","site","plot"), sep = "_") %>% 
  dplyr::select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_Height=FDis)

#### percent_green Diversity Metrics ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data_percent_green<-Avg_Traits_FK %>% 
  dplyr::select(Avg_percent_green) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_percent_green) <- c(1:33)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_percent_green<-Avg_Traits_FK %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_percent_green<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames_percent_green) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_percent_green<-Species_Comp_FK_percent_green %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_percent_green<-Species_Comp_FK_Wide_percent_green %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_percent_green<-Species_Comp_FK_Wide_percent_green %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_percent_green <- dbFD(Avg_Traits_FK_Data_percent_green, Species_Comp_FK_Wide_Data_percent_green,corr = "none")

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_percent_green<-Avg_Traits_TB%>% 
  dplyr::select(Avg_percent_green) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_percent_green) <- c(1:43)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_percent_green<-Avg_Traits_TB %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_percent_green<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames_percent_green) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_percent_green<-Species_Comp_TB_percent_green %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_percent_green<-Species_Comp_TB_Wide_percent_green %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_percent_green<-Species_Comp_TB_Wide_percent_green %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_percent_green <- dbFD(Avg_Traits_TB_Data_percent_green, Species_Comp_TB_Wide_Data_percent_green,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_percent_green<-as.data.frame(FK_FunctionalDiversity_percent_green) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_percent_green)

Functional_Diversity_TB_percent_green<-as.data.frame(TB_FunctionalDiversity_percent_green) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_percent_green)

Functional_Diversity_percent_green<-Functional_Diversity_FK_percent_green %>% 
  rbind(Functional_Diversity_TB_percent_green) %>% 
  separate(ID,c("year","site","plot"), sep = "_") %>% 
  dplyr::select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_Green=FDis)

#### leaf_thickness_.mm. Diversity Metrics ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data_leaf_thickness_.mm._1<-Avg_Traits_FK %>% 
  dplyr::select(Avg_leaf_thickness,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:32))

Avg_Traits_FK_Data_leaf_thickness_.mm.<-Avg_Traits_FK_Data_leaf_thickness_.mm._1 %>% 
  dplyr::select(Avg_leaf_thickness) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_leaf_thickness_.mm.) <- c(1:32)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_leaf_thickness_.mm.<-Avg_Traits_FK_Data_leaf_thickness_.mm._1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_leaf_thickness_.mm.<-Species_Comp_FK_Wide_leaf_thickness_.mm. %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_leaf_thickness_.mm.<-Species_Comp_FK_Wide_leaf_thickness_.mm. %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_leaf_thickness_.mm. <- dbFD(Avg_Traits_FK_Data_leaf_thickness_.mm., Species_Comp_FK_Wide_Data_leaf_thickness_.mm.,corr = "none")

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_leaf_thickness_.mm._1<-Avg_Traits_TB %>% 
  dplyr::select(Avg_leaf_thickness,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:42))

Avg_Traits_TB_Data_leaf_thickness_.mm.<-Avg_Traits_TB_Data_leaf_thickness_.mm._1%>% 
  dplyr::select(Avg_leaf_thickness) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_leaf_thickness_.mm.) <- c(1:42)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_leaf_thickness_.mm.<-Avg_Traits_TB_Data_leaf_thickness_.mm._1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_leaf_thickness_.mm.<-Species_Comp_TB_Wide_leaf_thickness_.mm. %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_leaf_thickness_.mm.<-Species_Comp_TB_Wide_leaf_thickness_.mm. %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_leaf_thickness_.mm. <- dbFD(Avg_Traits_TB_Data_leaf_thickness_.mm., Species_Comp_TB_Wide_Data_leaf_thickness_.mm.,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_leaf_thickness_.mm.<-as.data.frame(FK_FunctionalDiversity_leaf_thickness_.mm.) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_leaf_thickness_.mm.)

Functional_Diversity_TB_leaf_thickness_.mm.<-as.data.frame(TB_FunctionalDiversity_leaf_thickness_.mm.) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_leaf_thickness_.mm.)

Functional_Diversity_leaf_thickness_.mm.<-Functional_Diversity_FK_leaf_thickness_.mm. %>% 
  rbind(Functional_Diversity_TB_leaf_thickness_.mm.) %>% 
  separate(ID,c("year","site","plot"), sep = "_") %>% 
  dplyr::select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_Thickness=FDis)

#### LDMC Diversity Metrics ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data_LDMC_1<-Avg_Traits_FK %>% 
  dplyr::select(Avg_LDMC,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:31))

Avg_Traits_FK_Data_LDMC<-Avg_Traits_FK_Data_LDMC_1 %>% 
  dplyr::select(Avg_LDMC) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_LDMC) <- c(1:31)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_LDMC<-Avg_Traits_FK_Data_LDMC_1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_LDMC<-Species_Comp_FK_Wide_LDMC %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_LDMC<-Species_Comp_FK_Wide_LDMC %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_LDMC <- dbFD(Avg_Traits_FK_Data_LDMC, Species_Comp_FK_Wide_Data_LDMC,corr = "none")

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_LDMC_1<-Avg_Traits_TB %>% 
  dplyr::select(Avg_LDMC,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:41))

Avg_Traits_TB_Data_LDMC<-Avg_Traits_TB_Data_LDMC_1%>% 
  dplyr::select(Avg_LDMC) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_LDMC) <- c(1:41)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_LDMC<-Avg_Traits_TB_Data_LDMC_1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_LDMC<-Species_Comp_TB_Wide_LDMC %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_LDMC<-Species_Comp_TB_Wide_LDMC %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_LDMC <- dbFD(Avg_Traits_TB_Data_LDMC, Species_Comp_TB_Wide_Data_LDMC,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_LDMC<-as.data.frame(FK_FunctionalDiversity_LDMC) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_LDMC)

Functional_Diversity_TB_LDMC<-as.data.frame(TB_FunctionalDiversity_LDMC) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_LDMC)

Functional_Diversity_LDMC<-Functional_Diversity_FK_LDMC %>% 
  rbind(Functional_Diversity_TB_LDMC) %>% 
  separate(ID,c("year","site","plot"), sep = "_") %>% 
  dplyr::select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_LDMC=FDis)

#### SLA Diversity Metrics ####

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_FK_Data_SLA_1<-Avg_Traits_FK %>% 
  dplyr::select(Avg_SLA,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:31))

Avg_Traits_FK_Data_SLA<-Avg_Traits_FK_Data_SLA_1 %>% 
  dplyr::select(Avg_SLA) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_SLA) <- c(1:31)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_SLA<-Avg_Traits_FK_Data_SLA_1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_SLA<-Species_Comp_FK_Wide_SLA %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_SLA<-Species_Comp_FK_Wide_SLA %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_SLA <- dbFD(Avg_Traits_FK_Data_SLA, Species_Comp_FK_Wide_Data_SLA,corr = "none")

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_SLA_1<-Avg_Traits_TB %>% 
  dplyr::select(Avg_SLA,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:36))

Avg_Traits_TB_Data_SLA<-Avg_Traits_TB_Data_SLA_1 %>% 
  dplyr::select(Avg_SLA) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_SLA) <- c(1:36)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_SLA<-Avg_Traits_TB_Data_SLA_1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_SLA<-Species_Comp_TB_Wide_SLA %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_SLA<-Species_Comp_TB_Wide_SLA %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_SLA <- dbFD(Avg_Traits_TB_Data_SLA, Species_Comp_TB_Wide_Data_SLA,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_SLA<-as.data.frame(FK_FunctionalDiversity_SLA) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_SLA)

Functional_Diversity_TB_SLA<-as.data.frame(TB_FunctionalDiversity_SLA) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_SLA)

Functional_Diversity_SLA<-Functional_Diversity_FK_SLA %>% 
  rbind(Functional_Diversity_TB_SLA) %>% 
  separate(ID,c("year","site","plot"), sep = "_") %>% 
  dplyr::select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_SLA=FDis)

#### Leaf Area Diversity Metrics ####

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_FK_Data_Area_1<-Avg_Traits_FK %>% 
  dplyr::select(Avg_Area,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:32))

Avg_Traits_FK_Data_Area<-Avg_Traits_FK_Data_Area_1 %>% 
  dplyr::select(Avg_Area) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_Area) <- c(1:32)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_Area<-Avg_Traits_FK_Data_Area_1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_Area<-Species_Comp_FK_Wide_Area %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_Area<-Species_Comp_FK_Wide_Area %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_Area <- dbFD(Avg_Traits_FK_Data_Area, Species_Comp_FK_Wide_Data_Area,corr = "none")


#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_Area_1<-Avg_Traits_TB %>% 
  dplyr::select(Avg_Area,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:37))

Avg_Traits_TB_Data_Area<-Avg_Traits_TB_Data_Area_1 %>% 
  dplyr::select(Avg_Area) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_Area) <- c(1:37)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_Area<-Avg_Traits_TB_Data_Area_1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_Area<-Species_Comp_TB_Wide_Area %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_Area<-Species_Comp_TB_Wide_Area %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_Area <- dbFD(Avg_Traits_TB_Data_Area, Species_Comp_TB_Wide_Data_Area,corr = "none")


#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_Area<-as.data.frame(FK_FunctionalDiversity_Area) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_Area)

Functional_Diversity_TB_Area<-as.data.frame(TB_FunctionalDiversity_Area) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_Area)

Functional_Diversity_Area<-Functional_Diversity_FK_Area %>% 
  rbind(Functional_Diversity_TB_Area) %>% 
  separate(ID,c("year","site","plot"), sep = "_") %>% 
  dplyr::select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_Area=FDis)

#### Lifespan Diversity Metrics ####

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_FK_Data_Lifespan_1<-Avg_Traits_FK %>% 
  dplyr::select(Avg_Lifespan,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:32))

Avg_Traits_FK_Data_Lifespan<-Avg_Traits_FK_Data_Lifespan_1 %>% 
  dplyr::select(Avg_Lifespan) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_Lifespan) <- c(1:32)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_Lifespan<-Avg_Traits_FK_Data_Lifespan_1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_Lifespan<-Species_Comp_FK_Wide_Lifespan %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_Lifespan<-Species_Comp_FK_Wide_Lifespan %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_Lifespan <- dbFD(Avg_Traits_FK_Data_Lifespan, Species_Comp_FK_Wide_Data_Lifespan,corr = "none")


#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_Lifespan_1<-Avg_Traits_TB %>% 
  dplyr::select(Avg_Lifespan,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:42))

Avg_Traits_TB_Data_Lifespan<-Avg_Traits_TB_Data_Lifespan_1 %>% 
  dplyr::select(Avg_Lifespan) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_Lifespan) <- c(1:42)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_Lifespan<-Avg_Traits_TB_Data_Lifespan_1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_Lifespan<-Species_Comp_TB_Wide_Lifespan %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_Lifespan<-Species_Comp_TB_Wide_Lifespan %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_Lifespan <- dbFD(Avg_Traits_TB_Data_Lifespan, Species_Comp_TB_Wide_Data_Lifespan,corr = "none")


#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_Lifespan<-as.data.frame(FK_FunctionalDiversity_Lifespan) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_Lifespan)

Functional_Diversity_TB_Lifespan<-as.data.frame(TB_FunctionalDiversity_Lifespan) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_Lifespan)

Functional_Diversity_Lifespan<-Functional_Diversity_FK_Lifespan %>% 
  rbind(Functional_Diversity_TB_Lifespan) %>% 
  separate(ID,c("year","site","plot"), sep = "_") %>% 
  dplyr::select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_Lifespan=FDis)

#### GrowthForm Diversity Metrics ####

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_FK_Data_GrowthForm_1<-Avg_Traits_FK %>% 
  dplyr::select(Avg_GrowthForm,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:32))

Avg_Traits_FK_Data_GrowthForm<-Avg_Traits_FK_Data_GrowthForm_1 %>% 
  dplyr::select(Avg_GrowthForm) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_GrowthForm) <- c(1:32)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_GrowthForm<-Avg_Traits_FK_Data_GrowthForm_1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_GrowthForm<-Species_Comp_FK_Wide_GrowthForm %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_GrowthForm<-Species_Comp_FK_Wide_GrowthForm %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_GrowthForm <- dbFD(Avg_Traits_FK_Data_GrowthForm, Species_Comp_FK_Wide_Data_GrowthForm,corr = "none")


#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data_GrowthForm_1<-Avg_Traits_TB %>% 
  dplyr::select(Avg_GrowthForm,Genus_Species_Correct,Sp_Num) %>% 
  na.omit() %>% 
  mutate(Sp_Num_2=c(1:42))

Avg_Traits_TB_Data_GrowthForm<-Avg_Traits_TB_Data_GrowthForm_1 %>% 
  dplyr::select(Avg_GrowthForm) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_GrowthForm) <- c(1:42)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_GrowthForm<-Avg_Traits_TB_Data_GrowthForm_1 %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num_2)

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
  dplyr::select(Sp_Num_2,Relative_Cover,ID) %>% 
  spread(key=Sp_Num_2,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_GrowthForm<-Species_Comp_TB_Wide_GrowthForm %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_GrowthForm<-Species_Comp_TB_Wide_GrowthForm %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_GrowthForm <- dbFD(Avg_Traits_TB_Data_GrowthForm, Species_Comp_TB_Wide_Data_GrowthForm,corr = "none")


#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_GrowthForm<-as.data.frame(FK_FunctionalDiversity_GrowthForm) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_GrowthForm)

Functional_Diversity_TB_GrowthForm<-as.data.frame(TB_FunctionalDiversity_GrowthForm) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_GrowthForm)

Functional_Diversity_GrowthForm<-Functional_Diversity_FK_GrowthForm %>% 
  rbind(Functional_Diversity_TB_GrowthForm) %>% 
  separate(ID,c("year","site","plot"), sep = "_") %>% 
  dplyr::select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_GrowthForm=FDis)

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
  dplyr::select(-Genus_Species_Correct,-Sp_Num,-Site) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data) <- c(1:33)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames<-Avg_Traits_FK %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  left_join(Avg_Traits_FK_SpNames) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide<-Species_Comp_FK %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data<-Species_Comp_FK_Wide %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData<-Species_Comp_FK_Wide %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

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
  dplyr::select(-Genus_Species_Correct,-Sp_Num,-Site) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data) <- c(1:43)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames<-Avg_Traits_TB %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB<- Species_Comp_RelCov_All %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  left_join(Avg_Traits_TB_SpNames) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide<-Species_Comp_TB %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data<-Species_Comp_TB_Wide %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData<-Species_Comp_TB_Wide %>% 
  mutate(ID_Num=c(1:270)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity <- dbFD(Avg_Traits_TB_Data, Species_Comp_TB_Wide_Data,corr = "none")

#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK<-as.data.frame(FK_FunctionalDiversity) %>% 
  cbind(Species_Comp_FK_Wide_PlotData)

Functional_Diversity_TB<-as.data.frame(TB_FunctionalDiversity) %>% 
  cbind(Species_Comp_TB_Wide_PlotData)

Functional_Diversity<-Functional_Diversity_FK %>% 
  rbind(Functional_Diversity_TB) %>% 
  separate(ID,c("year","site","plot"), sep = "_") %>% 
  dplyr::select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_All=FDis)

#### Distribution and Covariance of Dispersion ####

#make data frame with dispersion of all traits calculated above
Functional_Diversity_All<-Functional_Diversity %>% 
  dplyr::select(year,site,plot,block,paddock,slope,rainfall_reduction,Rainfall_reduction_cat,grazing_treatment,Grazing_2020,FDis_All) %>% 
  left_join(Functional_Diversity_Height) %>% 
  dplyr::select(year,site,plot,block,paddock,slope,rainfall_reduction,Rainfall_reduction_cat,grazing_treatment,Grazing_2020,FDis_All,FDis_Height) %>%
  left_join(Functional_Diversity_percent_green) %>% 
  dplyr::select(year,site,plot,block,paddock,slope,rainfall_reduction,Rainfall_reduction_cat,grazing_treatment,Grazing_2020,FDis_All,FDis_Height, FDis_Green) %>%
  left_join(Functional_Diversity_leaf_thickness_.mm.) %>% 
  dplyr::select(year,site,plot,block,paddock,slope,rainfall_reduction,Rainfall_reduction_cat,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness) %>%
  left_join(Functional_Diversity_LDMC) %>% 
  dplyr::select(year,site,plot,block,paddock,slope,rainfall_reduction,Rainfall_reduction_cat,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness,FDis_LDMC) %>%
  left_join(Functional_Diversity_SLA) %>% 
  dplyr::select(year,site,plot,block,paddock,slope,rainfall_reduction,Rainfall_reduction_cat,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness,FDis_LDMC,FDis_SLA) %>%
  left_join(Functional_Diversity_Area) %>% 
  dplyr::select(year,site,plot,block,paddock,slope,rainfall_reduction,Rainfall_reduction_cat,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness,FDis_LDMC,FDis_SLA,FDis_Area) %>%
  left_join(Functional_Diversity_Lifespan) %>% 
  dplyr::select(year,site,plot,block,paddock,slope,rainfall_reduction,Rainfall_reduction_cat,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness,FDis_LDMC,FDis_SLA,FDis_Area,FDis_Lifespan) %>% 
left_join(Functional_Diversity_GrowthForm) %>% 
  dplyr::select(year,site,plot,block,paddock,slope,rainfall_reduction,Rainfall_reduction_cat,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness,FDis_LDMC,FDis_SLA,FDis_Area,FDis_Lifespan,FDis_GrowthForm)

Functional_Diversity_All_FK<-Functional_Diversity_All %>% 
  filter(site=="FK") %>% 
  mutate(FDis_LDMC_TF=sqrt(FDis_LDMC)) %>% 
  mutate(FDis_SLA_TF=sqrt(FDis_SLA)) %>% 
  mutate(FDis_Area_TF=NA) %>% 
  mutate(FDis_Lifespan_TF=exp(FDis_Lifespan))%>% 
  mutate(FDis_GrowthForm_TF=NA)

Functional_Diversity_All_TB<-Functional_Diversity_All %>% 
  filter(site=="TB") %>% 
  mutate(FDis_LDMC_TF=log(FDis_LDMC)) %>% 
  mutate(FDis_SLA_TF=log(FDis_SLA)) %>% 
  mutate(FDis_Area_TF=log(FDis_Area)) %>% 
  mutate(FDis_Lifespan_TF=exp(FDis_Lifespan)) %>% 
  mutate(FDis_GrowthForm_TF=sqrt(FDis_GrowthForm))

Functional_Diversity_All<-Functional_Diversity_All_FK %>% 
  rbind(Functional_Diversity_All_TB)

#not transformed - FK
chart.Correlation(Functional_Diversity_All_FK[c(11,12,13,14,15,16,17,18,19)],pch="41", cex = 4, method="spearman", histogram = TRUE)

#transformed - FK
chart.Correlation(Functional_Diversity_All_FK[c(11,12,13,14,20,21,17,23,19)],pch="41", cex = 4, method="spearman", histogram = TRUE)

#not transformed -TB
chart.Correlation(Functional_Diversity_All_TB[c(11,12,13,14,15,16,17,18,19)],pch="41", cex = 4, method="spearman", histogram = TRUE)

#transformed - TB
chart.Correlation(Functional_Diversity_All_TB[c(11,12,13,14,20,21,22,23,24)],pch="41", cex = 4, method="spearman", histogram = TRUE)

####Height Functional Dispersion - Fort Keogh all years####

Functional_Diversity_Height_avg<-Functional_Diversity_Height %>% 
  group_by(site, year, Rainfall_reduction_cat)%>%
  summarize(FDis_Std=sd(FDis),FDis_Mean=mean(FDis),FDis_n=length(FDis))%>%
  mutate(FDis_St_Error=FDis_Std/sqrt(FDis_n)) %>% 
  ungroup()

####Height Functional Dispersion - FK all years####
Height_FDis_FK<-ggplot(subset(Functional_Diversity_Height_avg,site=="FK"&year>=2019),aes(x=Rainfall_reduction_cat,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke=6)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_linetype_manual(values=c("dashed","solid","dashed"),labels = c("2019", "2020","2021"), breaks = c("2019","2020","2021"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.8),legend.key = element_rect(size=20), legend.key.size = unit(5.0, 'lines'))+
  expand_limits(y=c(0.2,1.25))+
  annotate("text", x=1, y=1.25, label = "A. Height", size=20)

####Height Functional Dispersion - TB all years####
Height_FDis_TB<-ggplot(subset(Functional_Diversity_Height_avg,site=="TB"&year>=2019),aes(x=Rainfall_reduction_cat,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke=6)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.8),legend.key = element_rect(size=20), legend.key.size = unit(5.0, 'lines'))+
  expand_limits(y=c(0.2,1.25))+
  annotate("text", x=1, y=1.25, label = "A. Height", size=20)

####percent_green Functional Dispersion - Fort Keogh all years####
Functional_Diversity_percent_green_avg<-Functional_Diversity_percent_green %>% 
    group_by(site, year, rainfall_reduction)%>%
    summarize(FDis_Std=sd(FDis),FDis_Mean=mean(FDis),FDis_n=length(FDis))%>%
    mutate(FDis_St_Error=FDis_Std/sqrt(FDis_n)) %>% 
    ungroup()
  
percent_green_FDis_FK<-ggplot(subset(Functional_Diversity_percent_green_avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(Functional_Diversity_percent_green_avg,site=="FK"&year=="2019"), method='lm', se=FALSE,color="darkslateblue",size=5,linetype="dashed")+
  geom_smooth(data=subset(Functional_Diversity_percent_green_avg,site=="FK"&year=="2022"), method='lm', se=FALSE,color="darkgreen",size=5, linetype="dashed")+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=1, label = "B. Percent Green", size=20)

####percent_green Functional Dispersion - TB all years####
percent_green_FDis_TB<-ggplot(subset(Functional_Diversity_percent_green_avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(Functional_Diversity_percent_green_avg,site=="TB"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=1, label = "B. Percent Green", size=20)

#### Leaf Thickness Functional Dispersion - Fort Keogh all years####
Functional_Diversity_leaf_thickness_.mm._avg<-Functional_Diversity_leaf_thickness_.mm. %>% 
  group_by(site, year, rainfall_reduction)%>%
  summarize(FDis_Std=sd(FDis),FDis_Mean=mean(FDis),FDis_n=length(FDis))%>%
  mutate(FDis_St_Error=FDis_Std/sqrt(FDis_n)) %>% 
  ungroup()

LeafThickness_FDis_FK<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm._avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,0.8))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=0.8, label = "C. Leaf Thickness", size=20)

####Leaf Thickness Functional Dispersion - TB all years####
LeafThickness_FDis_TB<-ggplot(subset(Functional_Diversity_leaf_thickness_.mm._avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  expand_limits(y=c(0,0.8))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=0.8, label = "C. Leaf Thickness", size=20)


#### LDMC Functional Dispersion - Fort Keogh all years####
Functional_Diversity_LDMC_avg<-Functional_Diversity_LDMC %>% 
  group_by(site, year, rainfall_reduction)%>%
  summarize(FDis_Std=sd(FDis),FDis_Mean=mean(FDis),FDis_n=length(FDis))%>%
  mutate(FDis_St_Error=FDis_Std/sqrt(FDis_n)) %>% 
  ungroup()

LDMC_FDis_FK<-ggplot(subset(Functional_Diversity_LDMC_avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(Functional_Diversity_LDMC_avg,site=="FK"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5, linetype="dashed")+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,0.8))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=0.8, label = "D. Leaf Dry Matter Content", size=20)

####LDMC Functional Dispersion - TB all years####
LDMC_FDis_TB<-ggplot(subset(Functional_Diversity_LDMC_avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,0.8))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=0.8, label = "D. Leaf Dry Matter Content", size=20)

#### SLA Functional Dispersion - Fort Keogh all years####
Functional_Diversity_SLA_avg<-Functional_Diversity_SLA %>% 
  group_by(site, year, rainfall_reduction)%>%
  summarize(FDis_Std=sd(FDis),FDis_Mean=mean(FDis),FDis_n=length(FDis))%>%
  mutate(FDis_St_Error=FDis_Std/sqrt(FDis_n)) %>% 
  ungroup()

SLA_FDis_FK<-ggplot(subset(Functional_Diversity_SLA_avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(Functional_Diversity_SLA_avg,site=="FK"&year==2020), method='lm', se=FALSE,color="blue4",size=5)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.2))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position ="none")+
  annotate("text", x=1, y=1.2, label = "E. Specific Leaf Area", size=20)

####SLA Functional Dispersion - TB all years####
SLA_FDis_TB<-ggplot(subset(Functional_Diversity_SLA_avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.2))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position ="none")+
  annotate("text", x=1, y=1.2, label = "E. Specific Leaf Area", size=20)


#### Area Functional Dispersion - Fort Keogh all years####
Functional_Diversity_Area_avg<-Functional_Diversity_Area %>% 
  group_by(site, year, rainfall_reduction)%>%
  summarize(FDis_Std=sd(FDis),FDis_Mean=mean(FDis),FDis_n=length(FDis))%>%
  mutate(FDis_St_Error=FDis_Std/sqrt(FDis_n)) %>% 
  ungroup()

Area_FDis_FK<-ggplot(subset(Functional_Diversity_Area_avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(Functional_Diversity_Area_avg,site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5, linetype="dashed")+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,0.8))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=0.8, label = "F. Leaf Area", size=20)

#### Area Functional Dispersion - TB all years####
Area_FDis_TB<-ggplot(subset(Functional_Diversity_Area_avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(Functional_Diversity_Area_avg,site=="TB"&year==2019), method='lm', se=FALSE,color="darkslateblue",size=5, linetype="dashed")+
  geom_smooth(data=subset(Functional_Diversity_Area_avg,site=="TB"&year==2021), method='lm', se=FALSE,color="maroon4",size=5, linetype="dashed")+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,0.8))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=1, y=0.8, label = "F. Leaf Area", size=20)


#### Lifespan Functional Dispersion - Fort Keogh all years####
Functional_Diversity_Lifespan_avg<-Functional_Diversity_Lifespan %>% 
  group_by(site, year, rainfall_reduction)%>%
  summarize(FDis_Std=sd(FDis),FDis_Mean=mean(FDis),FDis_n=length(FDis))%>%
  mutate(FDis_St_Error=FDis_Std/sqrt(FDis_n)) %>% 
  ungroup()

Lifespan_FDis_FK<-ggplot(subset(Functional_Diversity_Lifespan_avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(Functional_Diversity_Lifespan_avg,site=="FK"&year==2019), method='lm', se=FALSE,color="darkslateblue",size=5, linetype="dashed")+
  geom_smooth(data=subset(Functional_Diversity_Lifespan_avg,site=="FK"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5, linetype="dashed")+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.25))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=1, y=1.25, label = "G. Lifespan", size=20)

#### Lifespan Functional Dispersion - TB all years####
Lifespan_FDis_TB<-ggplot(subset(Functional_Diversity_Lifespan_avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.25))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=1, y=1.25, label = "G. Lifespan", size=20)

#### GrowthForm Functional Dispersion - Fort Keogh all years####
Functional_Diversity_GrowthForm_avg<-Functional_Diversity_GrowthForm %>% 
  group_by(site, year, rainfall_reduction)%>%
  summarize(FDis_Std=sd(FDis),FDis_Mean=mean(FDis),FDis_n=length(FDis))%>%
  mutate(FDis_St_Error=FDis_Std/sqrt(FDis_n)) %>% 
  ungroup()

GrowthForm_FDis_FK<-ggplot(subset(Functional_Diversity_GrowthForm_avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_smooth(data=subset(Functional_Diversity_GrowthForm_avg,site=="FK"&year==2019), method='lm', se=FALSE,color="darkslateblue",size=5,linetype="dashed")+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.25))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=1, y=1.25, label = "H. Growth Form", size=20)

#### GrowthForm Functional Dispersion - TB all years####
GrowthForm_FDis_TB<-ggplot(subset(Functional_Diversity_GrowthForm_avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.25))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=1, y=1.25, label = "H. Growth Form", size=20)

#### Create graph of all years for FDis FK ####
pushViewport(viewport(layout=grid.layout(4,2)))
print(Height_FDis_FK,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(percent_green_FDis_FK,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LeafThickness_FDis_FK,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_FDis_FK,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(SLA_FDis_FK,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(Area_FDis_FK,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(Lifespan_FDis_FK,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(GrowthForm_FDis_FK,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#Save at 4000 x 4500  

#### Create graph of all years for FDis TB ####
pushViewport(viewport(layout=grid.layout(4,2)))
print(Height_FDis_TB,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(percent_green_FDis_TB,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(LeafThickness_FDis_TB,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(LDMC_FDis_TB,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(SLA_FDis_TB,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(Area_FDis_TB,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(Lifespan_FDis_TB,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(GrowthForm_FDis_TB,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#Save at 4000 x 4500  

#### Bar Graphs for Grazing ####

Functional_Diversity_Height_avg_G<-Functional_Diversity_Height %>% 
  group_by(site, year, grazing_treatment)%>%
  summarize(FDis_Std=sd(FDis),FDis_Mean=mean(FDis),FDis_n=length(FDis))%>%
  mutate(FDis_St_Error=FDis_Std/sqrt(FDis_n)) %>% 
  ungroup()

#### FDis of Height FK Grazing ####
FDis_Height_Grazing_FK<-ggplot(subset(Functional_Diversity_All,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Height,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.9),legend.key = element_rect(size=30), legend.key.size = unit(7.0, 'lines'))+
  annotate("text", x=1.5, y=1.5, label = "A. Height", size=30)


#### FDis of Height TB Grazing ####
FDis_Height_Grazing_TB<-ggplot(subset(Functional_Diversity_All,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Height,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.9),legend.key = element_rect(size=30), legend.key.size = unit(7.0, 'lines'))+
  annotate("text", x=1.5, y=1.5, label = "A. Height", size=30)

#### FDis of Percent Green FK Grazing ####
FDis_Green_Grazing_FK<-ggplot(subset(Functional_Diversity_All,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Green,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2, y=1.5, label = "B. Percent Green", size=30)


#### FDis of Green TB Grazing ####
FDis_Green_Grazing_TB<-ggplot(subset(Functional_Diversity_All,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Green,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2, y=1.5, label = "B. Percent Green", size=30)

#### FDis of Leaf Thickness FK Grazing ####
FDis_Thickness_Grazing_FK<-ggplot(subset(Functional_Diversity_All,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Thickness,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2.5, y=1.5, label = "C. Leaf Thickness", size=30)


#### FDis of Leaf Thickness TB Grazing ####
FDis_Thickness_Grazing_TB<-ggplot(subset(Functional_Diversity_All,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Thickness,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2.5, y=1.5, label = "C. Leaf Thickness", size=30)

#### FDis of LDMC FK Grazing ####
FDis_LDMC_Grazing_FK<-ggplot(subset(Functional_Diversity_All,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_LDMC,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=3, y=1.5, label = "D. Leaf Dry Matter Content", size=30)


#### FDis of LDMC TB Grazing ####
FDis_LDMC_Grazing_TB<-ggplot(subset(Functional_Diversity_All,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_LDMC,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=3, y=1.5, label = "D. Leaf Dry Matter Content", size=30)

#### FDis of SLA FK Grazing ####
FDis_SLA_Grazing_FK<-ggplot(subset(Functional_Diversity_All,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_SLA,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=3, y=1.5, label = "E. Specific Leaf Area", size=30)


#### FDis of SLA TB Grazing ####
FDis_SLA_Grazing_TB<-ggplot(subset(Functional_Diversity_All,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_SLA,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=3, y=1.5, label = "E. Specific Leaf Area", size=30)

#### FDis of Leaf Area FK Grazing ####
FDis_LeafArea_Grazing_FK<-ggplot(subset(Functional_Diversity_All,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Area,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2.5, y=1.5, label = "F. Leaf Area", size=30)


#### FDis of Leaf Area TB Grazing ####
FDis_LeafArea_Grazing_TB<-ggplot(subset(Functional_Diversity_All,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Area,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1.5))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  annotate("text", x=2.5, y=1.5, label = "F. Leaf Area", size=30)

#### FDis of Lifespan FK Grazing ####
FDis_Lifespan_Grazing_FK<-ggplot(subset(Functional_Diversity_All,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Lifespan,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Year")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=2, y=1.5, label = "G. Lifespan", size=30)

#### FDis of Lifespan TB Grazing ####
FDis_Lifespan_Grazing_TB<-ggplot(subset(Functional_Diversity_All,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Lifespan,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=2, y=1.5, label = "G. Lifespan", size=30)

#### FDis of Growth Form FK Grazing ####
FDis_GrowthForm_Grazing_FK<-ggplot(subset(Functional_Diversity_All,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_GrowthForm,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=2, y=1.5, label = "H. Growth Form", size=30)

#### FDis of Growth Form TB Grazing ####
FDis_GrowthForm_Grazing_TB<-ggplot(subset(Functional_Diversity_All,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_GrowthForm,color=factor(grazing_treatment,level=c("destock","stable","heavy"))))+
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")+
  annotate("text", x=2, y=1.5, label = "H. Growth Form", size=30)

#### Create graph of all years for FDis Grazing ####

#FK
pushViewport(viewport(layout=grid.layout(4,2)))
print(FDis_Height_Grazing_FK,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_Green_Grazing_FK,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_Thickness_Grazing_FK,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_LDMC_Grazing_FK,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(FDis_SLA_Grazing_FK,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(FDis_LeafArea_Grazing_FK,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(FDis_Lifespan_Grazing_FK,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(FDis_GrowthForm_Grazing_FK,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#Save at 3500 x 4000  

#TB
pushViewport(viewport(layout=grid.layout(4,2)))
print(FDis_Height_Grazing_TB,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_Green_Grazing_TB,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FDis_Thickness_Grazing_TB,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FDis_LDMC_Grazing_TB,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(FDis_SLA_Grazing_TB,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(FDis_LeafArea_Grazing_TB,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(FDis_Lifespan_Grazing_TB,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(FDis_GrowthForm_Grazing_TB,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#Save at 3500 x 4000  

#### Single Trait Functional Dispersion Stats ####

### Single Trait FK Stats FDis Height #### 

#FDis for Fort Keogh 2019 Height - LMER
FDis_Height_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_Height ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_Height_FK19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 Height - LMER
FDis_Height_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="FK"), FDis_Height ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_Height_FK20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 Height - LMER
FDis_Height_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="FK"), FDis_Height ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Height_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 Height - LMER
FDis_Height_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="FK"), FDis_Height ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Height_FK22_LMER, type = 3) #NS

### Single Trait FK Stats FDis Percent Green #### 

#FDis for Fort Keogh 2019 Percent Green - LMER
FDis_percent_green_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_Green ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_percent_green_FK19_LMER, type = 3) #Drought (p=0.06766)
#adjust drought p-value
p.adjust(0.06766, method = "BH", n=9) #0.60894
summary(glht(FDis_percent_green_FK19_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#75%-0% (0.0781), #99%-0% (0.0781)

#FDis for Fort Keogh 2020 Percent Green - LMER
FDis_percent_green_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="FK"), FDis_Green ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_percent_green_FK20_LMER, type = 3) #NS

#FDis for Fort Keogh 2021 Percent Green - LMER
FDis_percent_green_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="FK"), FDis_Green ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_percent_green_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 Percent Green - LMER
FDis_percent_green_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="FK"), FDis_Green ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_percent_green_FK22_LMER, type = 3) #Drought (0.07941)
#adjust grazing p-value
p.adjust(0.07941, method = "BH", n=9) #0.71469
summary(glht(FDis_percent_green_FK22_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) #ns

### Single Trait FK Stats FDis Leaf Thickness #### 

#FDis for Fort Keogh 2019 Thickness - LMER
FDis_leaf_thickness_.mm._FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_Thickness ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_leaf_thickness_.mm._FK19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 Thickness - LMER
FDis_leaf_thickness_.mm._FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="FK"), FDis_Thickness ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_leaf_thickness_.mm._FK20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 Thickness - LMER
FDis_leaf_thickness_.mm._FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="FK"), FDis_Thickness ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_leaf_thickness_.mm._FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 Thickness - LMER
FDis_leaf_thickness_.mm._FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="FK"), FDis_Thickness ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_leaf_thickness_.mm._FK22_LMER, type = 3) #NS

### Single Trait FK Stats FDis LDMC #### 

#FDis for Fort Keogh 2019 LDMC - LMER
FDis_LDMC_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_LDMC_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_LDMC_FK19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 LDMC - LMER
FDis_LDMC_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="FK"), FDis_LDMC_TF ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_LDMC_FK20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 LDMC - LMER
FDis_LDMC_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="FK"), FDis_LDMC_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_LDMC_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 LDMC - LMER
FDis_LDMC_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="FK"), FDis_LDMC_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_LDMC_FK22_LMER, type = 3) #Drought (p=0.09669)
#adjust drought p-value
p.adjust(0.09669, method = "BH", n=9) #0.87021
summary(glht(FDis_LDMC_FK22_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#75%-0% (0.0970), #99%-0% (0.0180)

### Single Trait FK Stats FDis SLA #### 

#FDis for Fort Keogh 2019 SLA - LMER
FDis_SLA_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_SLA_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_SLA_FK19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 SLA - LMER
FDis_SLA_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="FK"), FDis_SLA_TF ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_SLA_FK20_LMER, type = 3) #Drought (p=0.01225)
#adjust drought p-value
p.adjust(0.01225, method = "BH", n=9) #0.11025
summary(glht(FDis_SLA_FK20_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#ns

#FDis for Fort Keogh 2021 SLA - LMER
FDis_SLA_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="FK"), FDis_SLA_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_SLA_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 SLA - LMER
FDis_SLA_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="FK"), FDis_SLA_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_SLA_FK22_LMER, type = 3) #NS

### Single Trait FK Stats FDis Leaf Area #### 

#FDis for Fort Keogh 2019 Area - LMER
FDis_Area_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_Area ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_Area_FK19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 Area - LMER
FDis_Area_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="FK"), FDis_Area ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_Area_FK20_LMER, type = 3) #NS

#FDis for Fort Keogh 2021 Area - LMER
FDis_Area_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="FK"), FDis_Area ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Area_FK21_LMER, type = 3) #Drought (p=0.06742)
#adjust grazing p-value
p.adjust(0.06742, method = "BH", n=9) #0.60678
summary(glht(FDis_Area_FK21_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#ns

#adjust grazing p-value
p.adjust(0.06742, method = "BH", n=5) #0.3371

#FDis for Fort Keogh 2022 Area - LMER
FDis_Area_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="FK"), FDis_Area ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Area_FK22_LMER, type = 3) #NS

### Single Trait FK Stats FDis Lifespan #### 

#FDis for Fort Keogh 2019 Lifespan - LMER
FDis_Lifespan_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_Lifespan_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_Lifespan_FK19_LMER, type = 3) #Drought (p=0.06743)
#adjust drought p-value
p.adjust(0.06743, method = "BH", n=9) #0.60687
summary(glht(FDis_Lifespan_FK19_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#ns

#FDis for Fort Keogh 2020 Lifespan - LMER
FDis_Lifespan_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="FK"), FDis_Lifespan_TF ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_Lifespan_FK20_LMER, type = 3) #grazing (p=0.0959)
#adjust grazing p-value
p.adjust(0.0959, method = "BH", n=9) #0.8631
summary(glht(FDis_Lifespan_FK20_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#ns

#FDis for Fort Keogh 2021 Lifespan - LMER
FDis_Lifespan_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="FK"), FDis_Lifespan_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Lifespan_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 Lifespan - LMER
FDis_Lifespan_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="FK"), FDis_Lifespan_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Lifespan_FK22_LMER, type = 3) #NS

### Single Trait FK Stats FDis Growth Form #### 

#FDis for Thunder Basin 2019 GrowthForm - LMER
FDis_GrowthForm_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_GrowthForm ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_FK19_LMER, type = 3) #ns

#FDis for Thunder Basin 2020 GrowthForm - LMER
FDis_GrowthForm_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="FK"), FDis_GrowthForm ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_FK20_LMER, type = 3) #NS

#FDis for Thunder Basin 2021 GrowthForm - LMER
FDis_GrowthForm_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="FK"), FDis_GrowthForm ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_FK21_LMER, type = 3) #NS

#FDis for Thunder Basin 2022 GrowthForm - LMER
FDis_GrowthForm_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="FK"), FDis_GrowthForm ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_FK22_LMER, type = 3) #NS

### Single Trait TB Stats FDis Height #### 

#FDis for Thunder Basin 2019 Height - LMER
FDis_Height_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_Height ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_Height_TB19_LMER, type = 3) #NS

#FDis for Thunder Basin 2020 Height - LMER
FDis_Height_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="TB"), FDis_Height ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_Height_TB20_LMER, type = 3)  #Grazing (p=0.003726)
#adjust grazing p-value
p.adjust(0.003726, method = "BH", n=9) #0.033534
summary(glht(FDis_Height_TB20_LMER, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) 
#medium-high (0.0303)

#adjust grazing p-value
p.adjust(0.003726, method = "BH", n=5) #0.01863

#FDis for Thunder Basin 2021 Height - LMER
FDis_Height_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="TB"), FDis_Height ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Height_TB21_LMER, type = 3) #Grazing (p=0.04587)
#adjust grazing p-value
p.adjust(0.04587, method = "BH", n=9) #0.41283
summary(glht(FDis_Height_TB21_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
#ns

#adjust grazing p-value
p.adjust(0.04587, method = "BH", n=5) #0.41283

#FDis for Thunder Basin 2022 Height - LMER
FDis_Height_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="TB"), FDis_Height ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Height_TB22_LMER, type = 3) #Grazing (p=0.01032)
#adjust grazing p-value
p.adjust(0.01032, method = "BH", n=9) #0.09288
summary(glht(FDis_Height_TB22_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
#ns

#adjust grazing p-value
p.adjust(0.01032, method = "BH", n=5) #0.0516

### Single Trait TB Stats FDis Percent Green #### 

#FDis for Thunder Basin 2019 Percent Green - LMER
FDis_percent_green_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_Green ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_percent_green_TB19_LMER, type = 3) #NS

#FDis for Thunder Basin 2020 Percent Green - LMER
FDis_percent_green_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="TB"), FDis_Green ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_percent_green_TB20_LMER, type = 3) #NS

#FDis for Thunder Basin 2021 Percent Green - LMER
FDis_percent_green_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="TB"), FDis_Green ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_percent_green_TB21_LMER, type = 3) #NS

#FDis for Thunder Basin 2022 Percent Green - LMER
FDis_percent_green_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="TB"), FDis_Green ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_percent_green_TB22_LMER, type = 3) # grazing (p=0.01323)
#adjust grazing p-value
p.adjust(0.01323, method = "BH", n=9) #0.11907
summary(glht(FDis_percent_green_TB22_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
#ns

### Single Trait TB Stats FDis Leaf Thickness #### 

#FDis for Thunder Basin 2019 Thickness - LMER
FDis_leaf_thickness_.mm._TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_Thickness ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_leaf_thickness_.mm._TB19_LMER, type = 3) #NS

#FDis for Thunder Basin 2020 Thickness - LMER
FDis_leaf_thickness_.mm._TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="TB"), FDis_Thickness ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_leaf_thickness_.mm._TB20_LMER, type = 3)  #Grazing (p=0.08078)
#adjust grazing p-value
p.adjust(0.08078, method = "BH", n=9) #0.72702
summary(glht(FDis_percent_green_TB22_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#ns

#FDis for Thunder Basin 2021 Thickness - LMER
FDis_leaf_thickness_.mm._TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="TB"), FDis_Thickness ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_leaf_thickness_.mm._TB21_LMER, type = 3) #NS

#FDis for Thunder Basin 2022 Thickness - LMER
FDis_leaf_thickness_.mm._TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="TB"), FDis_Thickness ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_leaf_thickness_.mm._TB22_LMER, type = 3) #NS

### Single Trait TB Stats FDis LDMC #### 

#FDis for Thunder Basin 2019 LDMC - LMER
FDis_LDMC_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_LDMC_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_LDMC_TB19_LMER, type = 3) #NS

#FDis for Thunder Basin 2020 LDMC - LMER
FDis_LDMC_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="TB"), FDis_LDMC_TF ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_LDMC_TB20_LMER, type = 3)  #Grazing (p=0.0045)
#adjust grazing p-value
p.adjust(0.0045, method = "BH", n=9) #0.0405
summary(glht(FDis_LDMC_TB20_LMER, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) 
#medium-high (p=0.00118)

#adjust grazing p-value
p.adjust(0.0045, method = "BH", n=5) #0.0225

#FDis for Thunder Basin 2021 LDMC - LMER
FDis_LDMC_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="TB"), FDis_LDMC_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_LDMC_TB21_LMER, type = 3) #NS

#FDis for Thunder Basin 2022 LDMC - LMER
FDis_LDMC_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="TB"), FDis_LDMC_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_LDMC_TB22_LMER, type = 3) #grazing (0.04931)
#adjust grazing p-value
p.adjust(0.04931, method = "BH", n=9) #0.44279
summary(glht(FDis_LDMC_TB22_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
#stable-destock (0.0349), #stable-heavy(0.0465)


### Single Trait TB Stats FDis SLA #### 

#FDis for Thunder Basin 2019 SLA - LMER
FDis_SLA_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_SLA_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_SLA_TB19_LMER, type = 3) #NS

#FDis for Thunder Basin 2020 SLA - LMER
FDis_Height_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="TB"), FDis_SLA_TF ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_Height_TB20_LMER, type = 3) #grazing (p=0.0961)
#adjust grazing p-value
p.adjust(0.0961, method = "BH", n=9) #0.8649
summary(glht(FDis_Height_TB20_LMER, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) 
#NS


#FDis for Thunder Basin 2021 SLA - LMER
FDis_SLA_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="TB"), FDis_SLA_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_SLA_TB21_LMER, type = 3) #ns

#FDis for Thunder Basin 2022 SLA - LMER
FDis_SLA_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="TB"), FDis_SLA_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_SLA_TB22_LMER, type = 3) #Grazing (p=0.05026)
#adjust grazing p-value
p.adjust(0.05026, method = "BH", n=9) #0.45234
summary(glht(FDis_SLA_TB22_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
#NS

### Single Trait TB Stats FDis Leaf Area #### 

#FDis for Thunder Basin 2019 Area - LMER
FDis_Area_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_Area_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_Area_TB19_LMER, type = 3) #Drought (p=0.09573)
#adjust drought p-value
p.adjust(0.09573, method = "BH", n=9) #0.86157
summary(glht(FDis_Area_TB19_LMER, linfct = mcp(Rainfall_reduction_cat = "Tukey")), test = adjusted(type = "BH")) 
#75%-0% (0.705), #75%-25% (0.0705)

#FDis for Thunder Basin 2020 Area - LMER
FDis_Area_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="TB"), FDis_Area_TF ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_Area_TB20_LMER, type = 3) #grazing (p=0.06299)
#adjust grazing p-value
p.adjust(0.06299, method = "BH", n=9) #0.56691
summary(glht(FDis_Area_TB20_LMER, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) 
#NS

#FDis for Thunder Basin 2021 Area - LMER
FDis_Area_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="TB"), FDis_Area_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Area_TB21_LMER, type = 3) #Drought (p=0.08919)
#adjust grazing p-value
p.adjust(0.08919, method = "BH", n=9) #0.80271
summary(glht(FDis_Area_TB21_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
#NS

#FDis for Thunder Basin 2022 Area - LMER
FDis_Area_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="TB"), FDis_Area_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Area_TB22_LMER, type = 3) #NS

### Single Trait TB Stats FDis Lifespan #### 

#FDis for Thunder Basin 2019 Lifespan - LMER
FDis_Lifespan_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_Lifespan_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_Lifespan_TB19_LMER, type = 3) #NS

#FDis for Thunder Basin 2020 Lifespan - LMER
FDis_Lifespan_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="TB"), FDis_Lifespan_TF ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_Lifespan_TB20_LMER, type = 3) #NS

#FDis for Thunder Basin 2021 Lifespan - LMER
FDis_Lifespan_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="TB"), FDis_Lifespan_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Lifespan_TB21_LMER, type = 3) #NS

#FDis for Thunder Basin 2022 Lifespan - LMER
FDis_Lifespan_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="TB"), FDis_Lifespan_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Lifespan_TB22_LMER, type = 3) #NS

### Single Trait TB Stats FDis Growth Form #### 

#FDis for Thunder Basin 2019 GrowthForm - LMER
FDis_GrowthForm_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_GrowthForm_TF ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_TB19_LMER, type = 3) #NS

#FDis for Thunder Basin 2020 GrowthForm - LMER
FDis_GrowthForm_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2020&site=="TB"), FDis_GrowthForm_TF ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_TB20_LMER, type = 3) #grazing (p=0.0808)
#adjust grazing p-value
p.adjust(0.0808, method = "BH", n=9) #0.7272
summary(glht(FDis_GrowthForm_TB20_LMER, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) 
#NS

#FDis for Thunder Basin 2021 GrowthForm - LMER
FDis_GrowthForm_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2021&site=="TB"), FDis_GrowthForm_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_TB21_LMER, type = 3) #NS

#FDis for Thunder Basin 2022 GrowthForm - LMER
FDis_GrowthForm_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2022&site=="TB"), FDis_GrowthForm_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_TB22_LMER, type = 3) #grazing (p=0.0872)
#adjust grazing p-value
p.adjust(0.0872, method = "BH", n=9) #0.7848
summary(glht(FDis_GrowthForm_TB21_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) 
#NS

####Multivariate Functional Dispersion - Fort Keogh all years####

Functional_Dispersion_avg<-Functional_Diversity %>% 
  group_by(site, year, rainfall_reduction,grazing_treatment)%>%
  summarize(FDis_Std=sd(FDis),FDis_Mean=mean(FDis),FDis_n=length(FDis))%>%
  mutate(FDis_St_Error=FDis_Std/sqrt(FDis_n)) %>% 
  ungroup()

#Drought x FDis
Multivariate_FDis_FK<-ggplot(subset(Functional_Dispersion_avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=10, stroke =4)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 2)+
  geom_smooth(data=subset(Functional_Dispersion_avg,site=="FK"&year==2019), method='lm', se=FALSE,color="darkslateblue",size=5,linetype="dashed")+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,0.25))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none",legend.key = element_rect(size=10), legend.key.size = unit(4.0, 'lines')) #+
  #annotate("text", x=17, y=0.25, label = "A. Montana Site", size=20)

#### Multivariate Functional Dispersion - Thunder Basin all years####
Multivariate_FDis_TB<-ggplot(subset(Functional_Dispersion_avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDis_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=10, stroke =4)+
  geom_pointrange(aes(ymin=FDis_Mean-FDis_St_Error,ymax=FDis_Mean+FDis_St_Error),linewidth = 2)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,0.25))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = c(0.2,0.9)) #+
  #annotate("text", x=15, y=0.25, label = "B. Wyoming Site", size=20)

#Drought x FRic
Multivariate_FRic_FK<-ggplot(subset(Functional_Richness_avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FRic_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=10, stroke =4)+
  geom_pointrange(aes(ymin=FRic_Mean-FRic_St_Error,ymax=FRic_Mean+FRic_St_Error),linewidth = 2)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,8e-06))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none",legend.key = element_rect(size=10), legend.key.size = unit(4.0, 'lines')) #+
  #annotate("text", x=17, y=8e-06, label = "A. FRic Montana Site", size=20)

#### Multivariate Functional Richness - Thunder Basin all years####
Multivariate_FRic_TB<-ggplot(subset(Functional_Richness_avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FRic_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=10, stroke =4)+
  geom_pointrange(aes(ymin=FRic_Mean-FRic_St_Error,ymax=FRic_Mean+FRic_St_Error),linewidth = 2)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,0.05))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none") #+
  #annotate("text", x=15, y=0.05, label = "B. Wyoming Site", size=20)

####Multivariate Functional Evenness - Fort Keogh all years####

Functional_Evenness_avg<-Functional_Diversity %>% 
  group_by(site, year, rainfall_reduction,grazing_treatment)%>%
  summarize(FEve_Std=sd(FEve),FEve_Mean=mean(FEve),FEve_n=length(FEve))%>%
  mutate(FEve_St_Error=FEve_Std/sqrt(FEve_n)) %>% 
  ungroup()

#Drought x FEve
Multivariate_FEve_FK<-ggplot(subset(Functional_Evenness_avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FEve_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=10, stroke =4)+
  geom_pointrange(aes(ymin=FEve_Mean-FEve_St_Error,ymax=FEve_Mean+FEve_St_Error),linewidth = 2)+
  geom_smooth(data=subset(Functional_Evenness_avg,site=="FK"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none",legend.key = element_rect(size=10), legend.key.size = unit(4.0, 'lines')) #+
  #annotate("text", x=17, y=8e-06, label = "A. FEve Montana Site", size=20)

#### Multivariate Functional Evenness - Thunder Basin all years####
Multivariate_FEve_TB<-ggplot(subset(Functional_Evenness_avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FEve_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=10, stroke =4)+
  geom_pointrange(aes(ymin=FEve_Mean-FEve_St_Error,ymax=FEve_Mean+FEve_St_Error),linewidth = 2)+
  geom_smooth(data=subset(Functional_Evenness_avg,site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5,linetype="dashed")+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none") #+
  #annotate("text", x=15, y=0.05, label = "B. Wyoming Site", size=20)

####Multivariate Functional Diversity - Fort Keogh all years####

Functional_Diversity_avg<-Functional_Diversity %>% 
  group_by(site, year, rainfall_reduction,grazing_treatment)%>%
  summarize(FDiv_Std=sd(FDiv),FDiv_Mean=mean(FDiv),FDiv_n=length(FDiv))%>%
  mutate(FDiv_St_Error=FDiv_Std/sqrt(FDiv_n)) %>% 
  ungroup()

#Drought x FDiv
Multivariate_FDiv_FK<-ggplot(subset(Functional_Diversity_avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=FDiv_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=10, stroke =4)+
  geom_pointrange(aes(ymin=FDiv_Mean-FDiv_St_Error,ymax=FDiv_Mean+FDiv_St_Error),linewidth = 2)+
  geom_smooth(data=subset(Functional_Diversity_avg,site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Diversity")+
  expand_limits(y=c(0.5,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none",legend.key = element_rect(size=10), legend.key.size = unit(4.0, 'lines')) #+
  #annotate("text", x=17, y=8e-06, label = "A. FDiv Montana Site", size=20)

#### Multivariate Functional Diversity - Thunder Basin all years####
Multivariate_FDiv_TB<-ggplot(subset(Functional_Diversity_avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=FDiv_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=10, stroke =4)+
  geom_pointrange(aes(ymin=FDiv_Mean-FDiv_St_Error,ymax=FDiv_Mean+FDiv_St_Error),linewidth = 2)+
  geom_smooth(data=subset(Functional_Diversity_avg,site=="FK"&year==2020), method='lm', se=FALSE,color="blue4",size=5)+
  geom_smooth(data=subset(Functional_Diversity_avg,site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  xlab("Rainfall Reduction (%)")+
  ylab("Functional Diversity")+
  expand_limits(y=c(0.5,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")#+
  #annotate("text", x=15, y=0.05, label = "B. Wyoming Site", size=20)

#### Create graph of multivariate ALL drought ####
pushViewport(viewport(layout=grid.layout(4,2)))
print(Multivariate_FDis_FK,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Multivariate_FDis_TB,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Multivariate_FRic_FK,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Multivariate_FRic_TB,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(Multivariate_FEve_FK,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(Multivariate_FEve_TB,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(Multivariate_FDiv_FK,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(Multivariate_FDiv_TB,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#save at 3500 x 2000

#### Multivariate Dispersion Richness - Grazing Fort Keogh all years####
FDis_Grazing_FK<-ggplot(subset(Functional_Dispersion_avg,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Mean,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  #annotate('rect', xmin = c('2020.5'), xmax = c('2021.5'),ymin=-Inf, ymax=Inf, alpha=0.2, fill="white")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  #scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0.05,0.25))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.95)) #+
  #annotate("text", x=2.5, y=0.25, label = "A. Montana Site", size=30)

#### Multivariate Dispersion Richness - Grazing Thunder Basin all years####
FDis_Grazing_TB<-ggplot(subset(Functional_Dispersion_avg,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDis_Mean,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  #annotate('rect', xmin = c('2020.5'), xmax = c('2021.5'),ymin=-Inf, ymax=Inf, alpha=0.2, fill="white")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  #scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Functional Dispersion")+
  expand_limits(y=c(0,0.2))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")#+
  #annotate("text", x=2.5, y=0.25, label = "B. Wyoming Site", size=30)

#### Multivariate Functional Richness - Grazing Fort Keogh all years####
FRic_Grazing_FK<-ggplot(subset(Functional_Richness_avg,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FRic_Mean,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  #annotate('rect', xmin = c('2020.5'), xmax = c('2021.5'),ymin=-Inf, ymax=Inf, alpha=0.2, fill="white")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  #scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Functional Richness")+
  expand_limits(y=c(0,5e-06))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none") #+
  #annotate("text", x=2.5, y=5e-06, label = "A. Montana Site", size=30)

#### Multivariate Functional Richness - Grazing Thunder Basin all years####
FRic_Grazing_TB<-ggplot(subset(Functional_Richness_avg,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FRic_Mean,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  #annotate('rect', xmin = c('2020.5'), xmax = c('2021.5'),ymin=-Inf, ymax=Inf, alpha=0.2, fill="white")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  #scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0,0.04))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")#+
  #annotate("text", x=2.5, y=5e-06, label = "B. Wyoming Site", size=30)

#### Multivariate Functional Evenness - Grazing Fort Keogh all years####
FEve_Grazing_FK<-ggplot(subset(Functional_Evenness_avg,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FEve_Mean,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  #annotate('rect', xmin = c('2020.5'), xmax = c('2021.5'),ymin=-Inf, ymax=Inf, alpha=0.2, fill="white")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  #scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Functional Evenness")+
  expand_limits(y=c(0.2,0.8))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = "none") #+
  #annotate("text", x=2.5, y=5e-06, label = "A. Montana Site", size=30)

#### Multivariate Functional Evenness - Grazing Thunder Basin all years####
FEve_Grazing_TB<-ggplot(subset(Functional_Evenness_avg,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FEve_Mean,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  #annotate('rect', xmin = c('2020.5'), xmax = c('2021.5'),ymin=-Inf, ymax=Inf, alpha=0.2, fill="white")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  #scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.2,0.8))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")#+
  #annotate("text", x=2.5, y=5e-06, label = "B. Wyoming Site", size=30)


#### Multivariate Functional Diversity - Grazing Fort Keogh all years####
FDiv_Grazing_FK<-ggplot(subset(Functional_Diversity_avg,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDiv_Mean,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  #annotate('rect', xmin = c('2020.5'), xmax = c('2021.5'),ymin=-Inf, ymax=Inf, alpha=0.2, fill="white")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  #scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Functional Diversity")+
  expand_limits(y=c(0.4,1))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "none") #+
  #annotate("text", x=2.5, y=5e-06, label = "A. Montana Site", size=30)

#### Multivariate Functional Diversity - Grazing Thunder Basin all years####
FDiv_Grazing_TB<-ggplot(subset(Functional_Diversity_avg,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=FDiv_Mean,color=factor(grazing_treatment,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  #annotate('rect', xmin = c('2020.5'), xmax = c('2021.5'),ymin=-Inf, ymax=Inf, alpha=0.2, fill="white")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"))+
  #scale_y_continuous(labels = label_number(accuracy = 0.001))+
  xlab("Grazing Treatment")+
  ylab("Community Weighted Mean")+
  expand_limits(y=c(0.4,1))+
    theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "none")#+
  #annotate("text", x=2.5, y=5e-06, label = "B. Wyoming Site", size=30)

#### Create graph of multivariate ALL grazing ####
pushViewport(viewport(layout=grid.layout(4,2)))
print(FDis_Grazing_FK,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(FDis_Grazing_TB,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(FRic_Grazing_FK,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(FRic_Grazing_TB,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(FEve_Grazing_FK,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(FEve_Grazing_TB,vp=viewport(layout.pos.row=3, layout.pos.col =2))
print(FDiv_Grazing_FK,vp=viewport(layout.pos.row=4, layout.pos.col =1))
print(FDiv_Grazing_TB,vp=viewport(layout.pos.row=4, layout.pos.col =2))
#save at 3500 x 2000

#### Multivariate Functional Dispersion Stats ####

#FDis for Fort Keogh 2018 - LMER
FDis_FK18_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK18_LMER, type = 3) #NS

#FDis for Fort Keogh 2019 - LMER
FDis_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&site=="FK"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK19_LMER, type = 3) #Drought (p=0.06858)
#adjust drought p-value
p.adjust(0.06858, method = "BH", n=9) #0.61722
summary(glht(FDis_FK19_LMER, linfct = mcp(Rainfall_reduction_cat= "Tukey")), test = adjusted(type = "BH")) #75%-0% marginally significant

#FDis for Fort Keogh 2020 - LMER
FDis_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&site=="FK"), FDis ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_FK20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&site=="FK"), FDis ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&site=="FK"), FDis ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_FK22_LMER, type = 3) #NS


### Thunder Basin 

#FDis for Fort Keogh 2018 - LMER
FDis_TB18_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB18_LMER, type = 3) #NS

#FDis for Thunder Basin 2019 - LMER
FDis_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&site=="TB"), FDis ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB19_LMER, type = 3) #NS

#FDis for Thunder Basin 2020 - LMER
FDis_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&site=="TB"), FDis ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDis_TB20_LMER, type = 3)  #Grazing (p=0.01034)
#adjust grazing p-value
p.adjust(0.01034, method = "BH", n=9) #0.09396
summary(glht(FDis_TB20_LMER, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) #NS

#adjust grazing p-value (5)
p.adjust(0.01034, method = "BH", n=5) #0.052

#FDis for Thunder Basin 2021 - LMER
FDis_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&site=="TB"), FDis ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_TB21_LMER, type = 3) #NS

#FDis for Thunder Basin 2022 - LMER
FDis_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&site=="TB"), FDis ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_TB22_LMER, type = 3) #Grazing (p=0.01044)
#adjust grazing p-value
p.adjust(0.01044, method = "BH", n=9) #0.09396
#post hoc test for lmer test on drought
summary(glht(FDis_TB22_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) #NS

#adjust grazing p-value (5)
p.adjust(0.01044, method = "BH", n=5) #0.0522

#### Multivariate Functional Richness Stats ####

#FRic for Fort Keogh 2018 - LMER
FRic_FK18_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&site=="FK"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FRic_FK18_LMER, type = 3) #NS

#FRic for Fort Keogh 2019 - LMER
FRic_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&site=="FK"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FRic_FK19_LMER, type = 3) #NS

#FRic for Fort Keogh 2020 - LMER
FRic_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&site=="FK"), FRic ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FRic_FK20_LMER, type = 3)  #NS

#FRic for Fort Keogh 2021 - LMER
FRic_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&site=="FK"), FRic ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FRic_FK21_LMER, type = 3) #NS

#FRic for Fort Keogh 2022 - LMER
FRic_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&site=="FK"), FRic ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FRic_FK22_LMER, type = 3) #NS


### Thunder Basin 

#FRic for Thunder Basin 2018 - LMER
FRic_TB18_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&site=="TB"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FRic_TB18_LMER, type = 3) #NS

#FRic for Thunder Basin 2019 - LMER
FRic_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&site=="TB"), FRic ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FRic_TB19_LMER, type = 3) #NS

#FRic for Thunder Basin 2020 - LMER
FRic_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&site=="TB"), FRic ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FRic_TB20_LMER, type = 3)  #Grazing (p=0.005486)
summary(glht(FRic_TB20_LMER, linfct = mcp(Grazing_2020 = "Tukey")), test = adjusted(type = "BH")) #medium and high are significantly different

#FRic for Thunder Basin 2021 - LMER
FRic_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&site=="TB"), FRic ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FRic_TB21_LMER, type = 3) #grazing (0.04517)
summary(glht(FRic_TB21_LMER, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) #ns

#FRic for Thunder Basin 2022 - LMER
FRic_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&site=="TB"), FRic ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FRic_TB22_LMER, type = 3) #NS


#### Multivariate Functional Evenness Stats ####

#FEve for Fort Keogh 2018 - LMER
FEve_FK18_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&site=="FK"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FEve_FK18_LMER, type = 3) #NS

#FEve for Fort Keogh 2019 - LMER
FEve_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&site=="FK"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FEve_FK19_LMER, type = 3) #NS

#FEve for Fort Keogh 2020 - LMER
FEve_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&site=="FK"), FEve ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FEve_FK20_LMER, type = 3)  #NS

#FEve for Fort Keogh 2021 - LMER
FEve_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&site=="FK"), FEve ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FEve_FK21_LMER, type = 3) #Grazing (0.066)

#FEve for Fort Keogh 2022 - LMER
FEve_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&site=="FK"), FEve ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FEve_FK22_LMER, type = 3) #Drought (0.022), grazing (0.09)


### Thunder Basin 

#FEve for Thunder Basin 2018 - LMER
FEve_TB18_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&site=="TB"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FEve_TB18_LMER, type = 3) #NS

#FEve for Thunder Basin 2019 - LMER
FEve_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&site=="TB"), FEve ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FEve_TB19_LMER, type = 3) #NS

#FEve for Thunder Basin 2020 - LMER
FEve_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&site=="TB"), FEve ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FEve_TB20_LMER, type = 3)  #NS

#FEve for Thunder Basin 2021 - LMER
FEve_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&site=="TB"), FEve ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FEve_TB21_LMER, type = 3) #DxG (0.01591)

#FEve for Thunder Basin 2022 - LMER
FEve_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&site=="TB"), FEve ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FEve_TB22_LMER, type = 3) #NS

#### Multivariate Functional Diversity Stats ####

#FDiv for Fort Keogh 2018 - LMER
FDiv_FK18_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&site=="FK"), FDiv ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDiv_FK18_LMER, type = 3) #NS

#FDiv for Fort Keogh 2019 - LMER
FDiv_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&site=="FK"), FDiv ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDiv_FK19_LMER, type = 3) #NS

#FDiv for Fort Keogh 2020 - LMER
FDiv_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&site=="FK"), FDiv ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDiv_FK20_LMER, type = 3)  #NS

#FDiv for Fort Keogh 2021 - LMER
FDiv_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&site=="FK"), FDiv ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDiv_FK21_LMER, type = 3) #Drought (0.047)

#FDiv for Fort Keogh 2022 - LMER
FDiv_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&site=="FK"), FDiv ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDiv_FK22_LMER, type = 3) #NS


### Thunder Basin 

#FDiv for Thunder Basin 2018 - LMER
FDiv_TB18_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2018&site=="TB"), FDiv ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDiv_TB18_LMER, type = 3) #NS

#FDiv for Thunder Basin 2019 - LMER
FDiv_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&site=="TB"), FDiv ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDiv_TB19_LMER, type = 3) #NS

#FDiv for Thunder Basin 2020 - LMER
FDiv_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&site=="TB"), FDiv ~ Rainfall_reduction_cat*Grazing_2020 + (1|block) + (1|block:slope))
anova(FDiv_TB20_LMER, type = 3)  #Drought (0.039)

#FDiv for Thunder Basin 2021 - LMER
FDiv_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&site=="TB"), FDiv ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDiv_TB21_LMER, type = 3) #Drought (0.037) and DxG (0.02)

#FDiv for Thunder Basin 2022 - LMER
FDiv_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&site=="TB"), FDiv ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDiv_TB22_LMER, type = 3) #NS

#### 2018 & 2019 Data with Grazing - CWM ####

#CWM of height for, Fort Keogh 2018 - LMER
FK_Height_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), Height_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_Height_2018_LMER_CHECK, type = 3) #grazing (0.03598)
#adjust grazing p-value
p.adjust(0.03598, method = "BH", n=9) #0.32382

#CWM of height for Fort Keogh 2019 - LMER
FK_Height_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Height_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_Height_2019_LMER_CHECK, type = 3) #ns

#CWM of Green for Fort Keogh 2018 - LMER
FK_PercentGreen_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), PercentGreen_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_PercentGreen_2018_LMER_CHECK, type = 3) #drought (0.01511)
#adjust grazing p-value
p.adjust(0.01511, method = "BH", n=9) #0.13599
#adjust grazing p-value
p.adjust(0.09212, method = "BH", n=9) #0.83908


#CWM of Green for Fort Keogh 2019 - LMER
FK_PercentGreen_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), PercentGreen_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_PercentGreen_2019_LMER_CHECK, type = 3) #ns

#CWM of thickness for Fort Keogh 2018 - LMER
FK_LeafThickness_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), LeafThickness_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_LeafThickness_2018_LMER_CHECK, type = 3) #ns

#CWM of thickness for Fort Keogh 2019 - LMER
FK_LeafThickness_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), LeafThickness_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_LeafThickness_2019_LMER_CHECK, type = 3) #ns

#CWM of LDMC for Fort Keogh 2018 - LMER
FK_LDMC_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), LDMC_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_LDMC_2018_LMER_CHECK, type = 3) #ns

#CWM of LDMC for Fort Keogh 2019 - LMER
FK_LDMC_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), LDMC_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_LDMC_2019_LMER_CHECK, type = 3) #ns

#CWM of SLA for Fort Keogh 2018 - LMER
FK_SLA_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), Avg_SLA_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_SLA_2018_LMER_CHECK, type = 3) #grazing (p=0.08565)
#adjust grazing p-value
p.adjust(0.08565, method = "BH", n=9) #0.77085

#CWM of SLA for Fort Keogh 2019 - LMER
FK_SLA_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Avg_SLA_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_SLA_2019_LMER_CHECK, type = 3) #ns

#CWM of Area for Fort Keogh 2018 - LMER
FK_Area_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), Area_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_Area_2018_LMER_CHECK, type = 3) #ns

#CWM of area for Fort Keogh 2019 - LMER
FK_Area_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Area_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_Area_2019_LMER_CHECK, type = 3) #ns

#CWM of Lifespan for Fort Keogh 2018 - LMER
FK_Lifespan_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), Lifespan_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_Lifespan_2018_LMER_CHECK, type = 3) #ns

#CWM of Lifespan  for Fort Keogh 2019 - LMER
FK_Lifespan_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), Lifespan_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_Lifespan_2019_LMER_CHECK, type = 3) #ns

#CWM of GrowthForm for Fort Keogh 2018 - LMER
FK_GrowthForm_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="FK"), GrowthForm_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_GrowthForm_2018_LMER_CHECK, type = 3) #drought (0.09796)
#adjust grazing p-value
p.adjust(0.09796, method = "BH", n=9) #0.88164

#CWM of GrowthForm for Fort Keogh 2019 - LMER
FK_GrowthForm_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), GrowthForm_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_GrowthForm_2019_LMER_CHECK, type = 3) #ns

#CWM of height for, Thunder Basin 2018 - LMER
TB_Height_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), Height_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_Height_2018_LMER_CHECK, type = 3) #grazing (0.00276)
#adjust grazing p-value
p.adjust(0.00276, method = "BH", n=5) #0.02484

#CWM of height for Thunder Basin 2019 - LMER
TB_Height_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Height_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_Height_2019_LMER_CHECK, type = 3) #grazing (0.0006692)
#adjust grazing p-value
p.adjust(0.0006692, method = "BH", n=9) #0.0060228
summary(glht(TB_Height_2019_LMER_CHECK, linfct = mcp(grazing_treatment = "Tukey")), test = adjusted(type = "BH")) #NS

#CWM of Green for Thunder Basin 2018 - LMER
TB_PercentGreen_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), PercentGreen_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_PercentGreen_2018_LMER_CHECK, type = 3) #ns

#CWM of Green for Thunder Basin 2019 - LMER
TB_PercentGreen_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), PercentGreen_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_PercentGreen_2019_LMER_CHECK, type = 3) #ns

#CWM of thickness for Thunder Basin 2018 - LMER
TB_LeafThickness_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), LeafThickness_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_LeafThickness_2018_LMER_CHECK, type = 3) #grazing (5x10-6)
#adjust grazing p-value
p.adjust(5.819e-6, method = "BH", n=5) #5.2371x10-5

#CWM of thickness for Thunder Basin 2019 - LMER
TB_LeafThickness_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), LeafThickness_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_LeafThickness_2019_LMER_CHECK, type = 3) #grazing (8.3x10-5)
#adjust grazing p-value
p.adjust(8.3e-05, method = "BH", n=9) #0.000747

#CWM of LDMC for Thunder Basin 2018 - LMER
TB_LDMC_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), LDMC_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_LDMC_2018_LMER_CHECK, type = 3) #ns

#CWM of LDMC for Thunder Basin 2019 - LMER
TB_LDMC_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), LDMC_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_LDMC_2019_LMER_CHECK, type = 3) #ns

#CWM of SLA for Thunder Basin 2018 - LMER
TB_SLA_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), Avg_SLA_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_SLA_2018_LMER_CHECK, type = 3) #grazing (p=0.008989)
#adjust grazing p-value
p.adjust(0.008989, method = "BH", n=9) #0.080901

#CWM of SLA for Thunder Basin 2019 - LMER
TB_SLA_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Avg_SLA_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_SLA_2019_LMER_CHECK, type = 3) #ns

#CWM of Area for Thunder Basin 2018 - LMER
TB_Area_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), Area_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_Area_2018_LMER_CHECK, type = 3) #ns

#CWM of area for Thunder Basin 2019 - LMER
TB_Area_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Area_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_Area_2019_LMER_CHECK, type = 3) #ns

#CWM of Lifespan for Thunder Basin 2018 - LMER
TB_Lifespan_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), Lifespan_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_Lifespan_2018_LMER_CHECK, type = 3) #grazing (0.03742)
#adjust grazing p-value
p.adjust(0.03743, method = "BH", n=9) #0.33687

#CWM of Lifespan  for Thunder Basin 2019 - LMER
TB_Lifespan_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), Lifespan_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_Lifespan_2019_LMER_CHECK, type = 3) #ns

#CWM of GrowthForm for Thunder Basin 2018 - LMER
TB_GrowthForm_2018_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2018&Site=="TB"), GrowthForm_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_GrowthForm_2018_LMER_CHECK, type = 3) #grazing (0.0003849)
#adjust grazing p-value
p.adjust(0.0003849, method = "BH", n=9) #0.0034631

#CWM of GrowthForm for Thunder Basin 2019 - LMER
TB_GrowthForm_2019_LMER_CHECK <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), GrowthForm_CWM_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_GrowthForm_2019_LMER_CHECK, type = 3) #grazing (0.004832)
#adjust grazing p-value
p.adjust(0.004832, method = "BH", n=9) #0.043488

#### 2018 & 2019 Data with Grazing - FDis ####

#FDis for Fort Keogh 2018 Height - LMER
FDis_Height_FK18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="FK"), FDis_Height ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Height_FK18_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2019 Height - LMER
FDis_Height_FK19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_Height ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Height_FK19_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2018 Green - LMER
FDis_Green_FK18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="FK"), FDis_Green ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Green_FK18_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2019 Green - LMER
FDis_Green_FK19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_Green ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Green_FK19_LMER_CHECK, type = 3) #drought (0.07025)
#adjust grazing p-value
p.adjust(0.07025, method = "BH", n=9) #0.63225

#FDis for Fort Keogh 2018 Thickness - LMER
FDis_Thickness_FK18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="FK"), FDis_Thickness ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Thickness_FK18_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2019 Thickness - LMER
FDis_Thickness_FK19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_Thickness ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Thickness_FK19_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2018 _LDMC_TF - LMER
FDis__LDMC_FK18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="FK"), FDis_LDMC_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis__LDMC_FK18_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2019 _LDMC_TF - LMER
FDis_LDMC_FK19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_LDMC_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_LDMC_FK19_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2018 _SLA_TF - LMER
FDis_SLA_FK18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="FK"), FDis_SLA_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_SLA_FK18_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2019 _SLA_TF - LMER
FDis_SLA_FK19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_SLA_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_SLA_FK19_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2018 _Area_TF - LMER
FDis_Area_FK18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="FK"), FDis_Area ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Area_FK18_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2019 _Area_TF - LMER
FDis_Area_FK19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_Area ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Area_FK19_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2018 FDis_Lifespan_TF - LMER
FDis_Lifespan_FK18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="FK"), FDis_Lifespan_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Lifespan_FK18_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2019 FDis_Lifespan_TF - LMER
FDis_Lifespan_FK19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_Lifespan_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Lifespan_FK19_LMER_CHECK, type = 3) #drought (0.05084), #grazing (0.02650)
#adjust drought p-value
p.adjust(0.05084, method = "BH", n=9) #0.45746
#adjust grazing p-value
p.adjust(0.01044, method = "BH", n=9) #0.09396

#FDis for Fort Keogh 2018 _GrowthForm_TF - LMER
FDis_GrowthForm_FK18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="FK"), FDis_GrowthForm ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_FK18_LMER_CHECK, type = 3) #NS

#FDis for Fort Keogh 2019 _GrowthForm_TF - LMER
FDis_GrowthForm_FK19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="FK"), FDis_GrowthForm ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_FK19_LMER_CHECK, type = 3) #NS

#FDis for Thunder Basin 2018 Height - LMER
FDis_Height_TB18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="TB"), FDis_Height ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Height_TB18_LMER_CHECK, type = 3) #Grazing (0.00116)
#adjust grazing p-value
p.adjust(0.00116, method = "BH", n=9) #0.01044

#FDis for Thunder Basin 2019 Height - LMER
FDis_Height_TB19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_Height ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Height_TB19_LMER_CHECK, type = 3) #Grazing (0.003442)
#adjust grazing p-value
p.adjust(0.003442, method = "BH", n=9) #0.030978

#FDis for Thunder Basin 2018 Green - LMER
FDis_Green_TB18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="TB"), FDis_Green ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Green_TB18_LMER_CHECK, type = 3) #Grazing (0.0007559)
#adjust grazing p-value
p.adjust(0.0007559, method = "BH", n=9) #0.0068031

#FDis for Thunder Basin 2019 Green - LMER
FDis_Green_TB19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_Green ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Green_TB19_LMER_CHECK, type = 3) #ns

#FDis for Thunder Basin 2018 Thickness - LMER
FDis_Thickness_TB18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="TB"), FDis_Thickness ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Thickness_TB18_LMER_CHECK, type = 3) #grazing (0.03642) , #DxG(0.02554)
#adjust grazing p-value
p.adjust(0.03642, method = "BH", n=9) #0.32778
#adjust DXG p-value
p.adjust(0.02554, method = "BH", n=9) #0.22986

#FDis for Thunder Basin 2019 Thickness - LMER
FDis_Thickness_TB19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_Thickness ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Thickness_TB19_LMER_CHECK, type = 3) #NS

#FDis for Thunder Basin 2018 _LDMC_TF - LMER
FDis__LDMC_TB18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="TB"), FDis_LDMC_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis__LDMC_TB18_LMER_CHECK, type = 3) #NS

#FDis for Thunder Basin 2019 _LDMC_TF - LMER
FDis_LDMC_TB19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_LDMC_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_LDMC_TB19_LMER_CHECK, type = 3) #NS

#FDis for Thunder Basin 2018 _SLA_TF - LMER
FDis_SLA_TB18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="TB"), FDis_SLA_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_SLA_TB18_LMER_CHECK, type = 3) #grazing (0.008203)
#adjust grazing p-value
p.adjust(0.008203, method = "BH", n=9) #0.073827

#FDis for Thunder Basin 2019 _SLA_TF - LMER
FDis_SLA_TB19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_SLA_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_SLA_TB19_LMER_CHECK, type = 3) #NS

#FDis for Thunder Basin 2018 _Area_TF - LMER
FDis_Area_TB18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="TB"), FDis_Area_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Area_TB18_LMER_CHECK, type = 3) #DxG(0.0586)
#adjust grazing p-value
p.adjust(0.0586, method = "BH", n=9) #0.5274

#FDis for Thunder Basin 2019 _Area_TF - LMER
FDis_Area_TB19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_Area_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Area_TB19_LMER_CHECK, type = 3) #NS

#FDis for Thunder Basin 2018 FDis_Lifespan_TF - LMER
FDis_Lifespan_TB18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="TB"), FDis_Lifespan_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Lifespan_TB18_LMER_CHECK, type = 3) #grazing (0.06769)
#adjust grazing p-value
p.adjust(0.06769, method = "BH", n=9) #0.60921

#FDis for Thunder Basin 2019 FDis_Lifespan_TF - LMER
FDis_Lifespan_TB19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_Lifespan_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_Lifespan_TB19_LMER_CHECK, type = 3) #grazing(0.05811)
#adjust grazing p-value
p.adjust(0.05811, method = "BH", n=9) #0.52299

#FDis for Thunder Basin 2018 _GrowthForm_TF - LMER
FDis_GrowthForm_TB18_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2018&site=="TB"), FDis_GrowthForm_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_TB18_LMER_CHECK, type = 3) #grazing (0.0001979)
#adjust grazing p-value
p.adjust(0.0001979, method = "BH", n=9) #0.0017811

#FDis for Thunder Basin 2019 _GrowthForm_TF - LMER
FDis_GrowthForm_TB19_LMER_CHECK <- lmerTest::lmer(data = subset(Functional_Diversity_All,year==2019&site=="TB"), FDis_GrowthForm_TF ~ Rainfall_reduction_cat*grazing_treatment + (1|block) + (1|block:slope))
anova(FDis_GrowthForm_TB19_LMER_CHECK, type = 3) #Grazing (0.02606)
#adjust grazing p-value
p.adjust(0.02606, method = "BH", n=9) #0.23454
