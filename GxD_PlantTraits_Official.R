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
  dplyr::select(site,block,paddock,plot,rainfall_reduction,drought,grazing_category,grazing_treatment,livestock_util_2019,livestock_util_2020,livestock_util_2021)
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

#combine FK and TB data together
CWM_Collected_Data<-CWM_Collected_Data_FK %>% 
  rbind(CWM_Collected_Data_TB)


#### Plot CWM data ####


#### CWM of Height ####

#Fort Keogh all years
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

#Thunder Basin all years
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


#### CWM of PercentGreen ####

#Fort Keogh all years
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

# Thunder Basin all years
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


####CWM of LeafThickness ####

#Fort Keogh all years
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

#Thunder Basin all years
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


####CWM of LDMC ####

#Fort Keogh all years
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

# Thunder Basin all years
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

#### CWM of Avg_SLA #### 

#Fort Keogh all years
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

# Thunder Basin all years
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


####CWM of LeafArea ####

#Fort Keogh all years
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

#Thunder Basin all years
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

####CWM of Lifespan ####

#Fort Keogh all years
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

#Thunder Basin all years
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


####CWM of GrowthForm ####
# Fort Keogh all years
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

# Thunder Basin all years
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
#Save at 3000 x 4000  

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
#Save at 3000 x 4000  

#### CWM Data: Stats ####

### CWM Multivariate Space ####

#Create seperate dataframes for each site and year for PCAs

#FK
CWM_Collected_Data_FK_18<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2018) %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_FK_19<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2019)%>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_FK_20<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2020)%>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_FK_21<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2021)%>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_FK_22<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2022)%>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

#TB

CWM_Collected_Data_TB_18<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2018) %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_TB_19<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2019) %>% 
  na.omit(Biomass_CWM)%>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_TB_20<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2020) %>% 
  na.omit(Biomass_CWM) %>% 
  na.omit(LDMC_CWM)%>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_TB_21<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2021) %>% 
  filter(!is.na(Biomass_CWM))%>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)

CWM_Collected_Data_TB_22<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2022)%>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020,Height_CWM_TF,PercentGreen_CWM_TF,LeafThickness_CWM_TF,LDMC_CWM_TF,Avg_SLA_CWM_TF,Lifespan_CWM_TF,GrowthForm_CWM_TF,Area_CWM_TF)


#### PCA for FK 2018 ####
PCA_FK_18<-prcomp(CWM_Collected_Data_FK_18[,13:20],scale=TRUE)
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
PCA_FK_19<-prcomp(CWM_Collected_Data_FK_19[,13:20],scale=TRUE)
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
PCA_FK_20<-prcomp(CWM_Collected_Data_FK_20[,13:20],scale=TRUE)
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
PCA_FK_21<-prcomp(CWM_Collected_Data_FK_21[,13:20],scale=TRUE)
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
PCA_FK_22<-prcomp(CWM_Collected_Data_FK_22[,13:20],scale=TRUE)
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
PCA_TB_18<-prcomp(CWM_Collected_Data_TB_18[,13:20],scale=TRUE)
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
head(var_TB_18$contrib, 13)

#### PCA for TB 2019 ####
PCA_TB_19<-prcomp(CWM_Collected_Data_TB_19[,13:20],scale=TRUE)
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
PCA_TB_20<-prcomp(CWM_Collected_Data_TB_20[,13:20],scale=TRUE)
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
PCA_TB_21<-prcomp(CWM_Collected_Data_TB_21[,13:20],scale=TRUE)
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
PCA_TB_22<-prcomp(CWM_Collected_Data_TB_22[,13:20],scale=TRUE)
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
#adding 1 to all leaf thickness measures since they are negative numbers

## FK ##

#2018
CWM_FK_18_Trait<-CWM_Collected_Data_FK_18 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020) %>% 
  mutate(LeafThickness_CWM_TF=1+LeafThickness_CWM_TF)

CWM_FK_18_Treatment<-CWM_Collected_Data_FK_18 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2019
CWM_FK_19_Trait<-CWM_Collected_Data_FK_19 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020) %>% 
  mutate(LeafThickness_CWM_TF=1+LeafThickness_CWM_TF)

CWM_FK_19_Treatment<-CWM_Collected_Data_FK_19 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2020
CWM_FK_20_Trait<-CWM_Collected_Data_FK_20 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=1+LeafThickness_CWM_TF)

CWM_FK_20_Treatment<-CWM_Collected_Data_FK_20 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2021
CWM_FK_21_Trait<-CWM_Collected_Data_FK_21 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=1+LeafThickness_CWM_TF)

CWM_FK_21_Treatment<-CWM_Collected_Data_FK_21 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2022
CWM_FK_22_Trait<-CWM_Collected_Data_FK_22 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=1+LeafThickness_CWM_TF)

CWM_FK_22_Treatment<-CWM_Collected_Data_FK_22 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

## TB ##

#2018
CWM_TB_18_Trait<-CWM_Collected_Data_TB_18 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020) %>% 
  mutate(LeafThickness_CWM_TF=2+LeafThickness_CWM_TF)

CWM_TB_18_Treatment<-CWM_Collected_Data_TB_18 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2019
CWM_TB_19_Trait<-CWM_Collected_Data_TB_19 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=2+LeafThickness_CWM_TF)

CWM_TB_19_Treatment<-CWM_Collected_Data_TB_19 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2020
CWM_TB_20_Trait<-CWM_Collected_Data_TB_20 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=2+LeafThickness_CWM_TF)

CWM_TB_20_Treatment<-CWM_Collected_Data_TB_20 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2021
CWM_TB_21_Trait<-CWM_Collected_Data_TB_21 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=2+LeafThickness_CWM_TF)

CWM_TB_21_Treatment<-CWM_Collected_Data_TB_21 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#2022
CWM_TB_22_Trait<-CWM_Collected_Data_TB_22 %>% 
  select(-year,-Site,-plot,-block,-paddock,-rainfall_reduction,-drought,-grazing_category,-grazing_treatment,-Rainfall_reduction_cat,-Trtm,-Grazing_2020)%>% 
  mutate(LeafThickness_CWM_TF=2+LeafThickness_CWM_TF)

CWM_TB_22_Treatment<-CWM_Collected_Data_TB_22 %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,drought,grazing_category,grazing_treatment,Rainfall_reduction_cat,Trtm,Grazing_2020)

#### PerMANOVA ####

# run PERMANOVA using adonis using trait dataframe as data to run adonis on and treatment dataframe as variables

## FK ##
#FK 2018
PERMANOVA_FK_18 <-adonis2(CWM_FK_18_Trait~Rainfall_reduction_cat + (1|block/paddock), data = CWM_FK_18_Treatment, 
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
PERMANOVA_TB_18 <-adonis2(CWM_TB_18_Trait~Rainfall_reduction_cat + (1|block/paddock), data = CWM_TB_18_Treatment, 
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

#### PermDISP ####

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
anova(Dispersion_TB_18_graze) 

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
SIMPER_FK_18 <- with(CWM_FK_18_Treatment,simper(CWM_FK_18_Trait,grazing_treatment))
#Print out a summary of the results
summary(SIMPER_FK_18)

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

#Run a SIMPER test comparing data
SIMPER_TB_18 <- with(CWM_TB_18_Treatment,simper(CWM_TB_18_Trait,grazing_treatment))
#Print out a summary of the results
summary(SIMPER_TB_18)

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
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_Height=FDis)

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
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_Green=FDis)

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
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_Thickness=FDis)

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
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_LDMC=FDis)

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
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_SLA=FDis)

#### Leaf Area Diversity Metrics ####

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
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_Area=FDis)

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
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_Lifespan=FDis)

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
  mutate(Grazing_2020=ifelse(grazing_category=="MMMMM","medium",ifelse(grazing_category=="HHMMM","high",ifelse(grazing_category=="MLLMM","medium",grazing_category)))) %>% 
  mutate(FDis_All=FDis)

#### Distribution and Covariance of Dispersion ####

#make data frame with dispersion of all traits calculated above
Functional_Diversity_All<-Functional_Diversity %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,grazing_treatment,Grazing_2020,FDis_All) %>% 
  left_join(Functional_Diversity_Height) %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,grazing_treatment,Grazing_2020,FDis_All,FDis_Height) %>%
  left_join(Functional_Diversity_percent_green) %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,grazing_treatment,Grazing_2020,FDis_All,FDis_Height, FDis_Green) %>%
  left_join(Functional_Diversity_leaf_thickness_.mm.) %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness) %>%
  left_join(Functional_Diversity_LDMC) %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness,FDis_LDMC) %>%
  left_join(Functional_Diversity_SLA) %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness,FDis_LDMC,FDis_SLA) %>%
  left_join(Functional_Diversity_Area) %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness,FDis_LDMC,FDis_SLA,FDis_Area) %>%
  left_join(Functional_Diversity_Lifespan) %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness,FDis_LDMC,FDis_SLA,FDis_Area,FDis_Lifespan) %>% 
left_join(Functional_Diversity_GrowthForm) %>% 
  select(year,Site,plot,block,paddock,rainfall_reduction,grazing_treatment,Grazing_2020,FDis_All,FDis_Height,FDis_Green, FDis_Thickness,FDis_LDMC,FDis_SLA,FDis_Area,FDis_Lifespan,FDis_GrowthForm)

Functional_Diversity_All_FK<-Functional_Diversity_All %>% 
  filter(Site=="FK") %>% 
  mutate(FDis_LDMC_TF=sqrt(FDis_LDMC)) %>% 
  mutate(FDis_SLA_TF=sqrt(FDis_SLA)) %>% 
  mutate(FDis_Lifespan_TF=exp(FDis_Lifespan))

Functional_Diversity_All_TB<-Functional_Diversity_All %>% 
  filter(Site=="TB") %>% 
  mutate(FDis_LDMC_TF=log(FDis_LDMC)) %>% 
  mutate(FDis_SLA_TF=log(FDis_SLA)) %>% 
  mutate(FDis_Area_TF=log(FDis_Area)) %>% 
  mutate(FDis_Lifespan_TF=exp(FDis_Lifespan)) %>% 
  mutate(FDis_GrowthForm_TF=sqrt(FDis_GrowthForm))
  

#not transformed - FK
chart.Correlation(Functional_Diversity_All_FK[c(9,10,11,12,13,14,15,16,17)],pch="41", cex = 4, method="spearman", histogram = TRUE)

#transformed - FK
chart.Correlation(Functional_Diversity_All_FK[c(9,10,11,12,18,19,15,20,17)],pch="41", cex = 4, method="spearman", histogram = TRUE)

#not transformed -TB
chart.Correlation(Functional_Diversity_All_TB[c(9,10,11,12,13,14,15,16,17)],pch="41", cex = 4, method="spearman", histogram = TRUE)

#transformed - TB
chart.Correlation(Functional_Diversity_All_TB[c(9,10,11,12,18,19,20,21,22)],pch="41", cex = 4, method="spearman", histogram = TRUE)


