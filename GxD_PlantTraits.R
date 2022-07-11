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
#install.packages("FD")
library(FD)
library(tidyverse) 


#### Set Working Directory ####
#Bloodworth - Mac
setwd("~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data")

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
  select(Site,DxG_block, paddock, genus_species, species_code, height_cm, emerging_leaves, developed_leaves, scenesced_leaves, flower_heads, open_flowers, percent_green, Date, Season, people, comments)
Lab_Traits<-read.csv("DxG_Plant_Traits/2022_DxG_CWM_LabTraits.csv") %>% 
  rename(Site=site) %>% 
  rename(DxG_block=block) %>% 
  rename(Season=season) %>% 
  rename(comments_lab=comments) %>% 
  rename(date_lab=date) %>% 
  rename(people_lab=personelle) %>% 
  #removing genus species and season from this dataframe to avoid spelling issues and inconsistancies with data entered
  select(-genus_species,-Season)

#merge trait dataframes
Traits<-Field_Traits %>% 
  left_join(Lab_Traits)

#Trait Database
#Trait_Database<-read_csv("DxG_Plant_Traits/sCoRRE categorical trait data_11302021.csv")

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
plot_layoutK<-read.csv("DxG_Plant_Traits/GMDR_site_plot_metadata.csv") %>% 
  select(site,block,paddock,plot,rainfall_reduction,drought,grazing_category,grazing_treatment,livestock_util_2019,livestock_util_2020,livestock_util_2021)

plot_layoutK$plot<-as.factor(plot_layoutK$plot)

#Soil moisture data  - bring in and keep only what we need for this study and take average SM data for all months
SM_data<-read.csv("DxG_Plant_Traits/SM_FK_TB_2019-2021.csv") %>% 
  filter(Site=="FK") %>% 
  filter(Year==2019) %>%
  group_by(Block,Paddock,Plot,Drought,Grazing) %>% 
  summarise(Avg_SM=mean(Soil_Moisture)) %>% 
  rename(plot="Plot")

#### Clean Up Species Comp Data and Calculate Relative Cover ####

#get dataframe with just total cover per plot for each year
#FK - 2018
Aerial_Cover_2018_FK<-FK_SpComp_2018 %>% 
  filter(aerial_basal!="Basal")

#Create Long dataframe from wide dataframe
Long_Cov_2018_FK<-gather(Aerial_Cover_2018_FK,key="species","cover",18:117) %>% 
  select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("Oenotherea.suffrutescens.1","STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii","CRCE.LELU.Penstemon.sp","Oenothera.","Unknown..7.baby.guara.","UNKN8.basal.rosette.lancroiati","Unk..3.Basal.Rosette","Unk..3.soft.point.leaf.KW.photo","Unkn..10.small.linear.leaf.KW.photo","Oneothera.n.","Rock","Moss.Lichen.Bogr.overlap")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2018_FK<-Long_Cov_2018_FK%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  select(year,site,plot,species,Relative_Cover)

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
  select(-observers,-date)

#Calculate Relative Cover
Relative_Cover_2019_FK<-Species_Cover_2019_FK%>%
  #Make a new column named "Treatment"
  mutate(Treatment=paste(block,plot,sep="_"))%>%
  #Add Total_Cover data into the Relative_Cover data sheet
  left_join(Total_Cover_2019_FK)%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(Species_Cover/Total_Cover)*100) %>% 
  select(-Species_Cover,-basal_cover,-Total_Cover) %>% 
  mutate(Relative_Cover=replace_na(Relative_Cover,0)) %>% 
  mutate(year=2019)  %>% 
  rename(species="genus_species") %>% 
  select(year,site,plot,species,Relative_Cover)

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
  select(-observers,-date)

#Calculate Relative Cover
Relative_Cover_2020_FK<-Species_Cover_2020_FK%>%
  #Make a new column named "Treatment"
  mutate(Treatment=paste(block,plot,sep="_"))%>%
  #Add Total_Cover data into the Relative_Cover data sheet
  left_join(Total_Cover_2020_FK)%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(Species_Cover/Total_Cover)*100) %>% 
  select(-Species_Cover,-basal_cover,-Total_Cover) %>% 
  mutate(Relative_Cover=replace_na(Relative_Cover,0)) %>% 
  mutate(year=2020) %>% 
  rename(species="genus_species") %>% 
  select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2020_FK$plot<-as.factor(Relative_Cover_2020_FK$plot)

#FK - 2021
Aerial_Cover_2021_FK<-FK_SpComp_2021 %>% 
  filter(aerial_basal!="basal")

#Create Long dataframe from wide dataframe
Long_Cov_2021_FK<-gather(Aerial_Cover_2021_FK,key="species","cover",20:61) %>% 
  select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("Poa_diseased_Kwpic.","Linear_leaf_hairy_red_stem_KWpic.")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2021_FK<-Long_Cov_2021_FK%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2021_FK$plot<-as.factor(Relative_Cover_2021_FK$plot)

#TB- 2018
Aerial_Cover_2018_TB<-TB_SpComp_2018 %>% 
  filter(aerial_basal!="Basal")

#Create Long dataframe from wide dataframe
Long_Cov_2018_TB<-gather(Aerial_Cover_2018_TB,key="species","cover",18:113) %>% 
  select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("Oenotherea.suffrutescens.1", "STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii",  "CRCE.LELU.Penstemon.sp","Oenothera.","Unknown..7.baby.guara.","UNKN8.basal.rosette.lancroiati","Unk..3.Basal.Rosette","Unk..3.soft.point.leaf.KW.photo","Unkn..10.small.linear.leaf.KW.photo")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2018_TB<-Long_Cov_2018_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2018_TB$plot<-as.factor(Relative_Cover_2018_TB$plot)

#TB- 2019
Aerial_Cover_2019_TB<-TB_SpComp_2019 %>% 
  filter(aerial_basal!="Basal")

#Create Long dataframe from wide dataframe
Long_Cov_2019_TB<-gather(Aerial_Cover_2019_TB,key="species","cover",18:114) %>% 
  select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii","Penstemon.sp.","CRCE.LELU.Penstemon.sp","Oenothera.","Unknown..7.baby.guara.","UNKN8.basal.rosette.lancroiati","Unk..3.Basal.Rosette","Unk..3.soft.point.leaf.KW.photo","Unkn..10.small.linear.leaf.KW.photo")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

Long_Cov_2019_TB$cover<-as.numeric(Long_Cov_2019_TB$cover)

#Calculate Relative Cover
Relative_Cover_2019_TB<-Long_Cov_2019_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2019_TB$plot<-as.factor(Relative_Cover_2019_TB$plot)

#TB- 2020
Aerial_Cover_2020_TB<-TB_SpComp_2020 %>% 
  filter(aerial_basal!="Basal")

#Create Long dataframe from wide dataframe
Long_Cov_2020_TB<-gather(Aerial_Cover_2020_TB,key="species","cover",18:114) %>% 
  select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii","Penstemon.sp.","CRCE.LELU.Penstemon.sp","Oenothera.","Unknown..7.baby.guara.","UNKN8.basal.rosette.lancroiati","Unk..3.Basal.Rosette","Unk..3.soft.point.leaf.KW.photo","Unkn..10.small.linear.leaf.KW.photo")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0) %>% 
  filter(cover!="<0.5")

Long_Cov_2020_TB$cover<-as.numeric(Long_Cov_2020_TB$cover)

#Calculate Relative Cover
Relative_Cover_2020_TB<-Long_Cov_2020_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2020_TB$plot<-as.factor(Relative_Cover_2020_TB$plot)

#TB- 2021
Aerial_Cover_2021_TB<-TB_SpComp_2021 %>% 
  filter(aerial_basal!="basal")

#Create Long dataframe from wide dataframe
Long_Cov_2021_TB<-gather(Aerial_Cover_2021_TB,key="species","cover",21:79) %>% 
  select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("Unk_baby_forb_opp.")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0) %>% 
  filter(cover!="<0.5")

#Calculate Relative Cover
Relative_Cover_2021_TB<-Long_Cov_2021_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2021_TB$plot<-as.factor(Relative_Cover_2021_TB$plot)

####Merge all TB and FK Data Frames together
Species_Comp_RelCov_All<- 
  full_join(Relative_Cover_2018_TB,Relative_Cover_2019_TB) %>% 
  full_join(Relative_Cover_2020_TB) %>% 
  full_join(Relative_Cover_2021_TB) %>% 
  full_join(Relative_Cover_2018_FK) %>% 
  full_join(Relative_Cover_2019_FK) %>% 
  full_join(Relative_Cover_2020_FK) %>% 
  full_join(Relative_Cover_2021_FK) %>% 
  mutate(Genus_Species=ifelse(species=="Oenothera_suffruticosa","Oenothera.suffrutescens",ifelse(species=="Oenotherea.suffrutescens","Oenothera.suffrutescens",ifelse(species=="OESU","Oenothera.suffrutescens",ifelse(species=="OPPO","Opuntia.polyacantha",ifelse(species=="Opuntia_polycantha","Opuntia.polyacantha",ifelse(species=="Pascopyrum_smithii","Pascopyrum.smithii",ifelse(species=="PASM","Pascopyrum_smithii",ifelse(species=="Pediomelum_esculenta","Pediomelum.esculentum",ifelse(species=="pediomelum_esculentum","Pediomelum.esculentum",ifelse(species=="Pediomelum_esculentum","Pediomelum.esculentum",ifelse(species=="PHHO","Phlox.hoodii",ifelse(species=="Plantago_patagonica","Plantago.patagonica",ifelse(species=="PLPA","Plantago.patagonica",ifelse(species=="Poa_secunda","Poa.secunda",ifelse(species=="POSE","Poa.secunda",ifelse(species=="PSTE","Psoralidium.tenuiflorum",ifelse(species=="SPCO","Sphaeralcea.coccinea",ifelse(species=="Sphaeralcea_coccinea","Sphaeralcea.coccinea",ifelse(species=="Sporobolus_cryptandrus","Sporobolus.cryptandrus",ifelse(species=="TAOF","Taraxacum.officinale",ifelse(species=="Taraxacum_officinale","Taraxacum.officinale",ifelse(species=="Tragopogon_dubius","Tragopogon.dubius",ifelse(species=="TRDU","Tragopogon.dubius",ifelse(species=="VIAM","Vicia.americana",ifelse(species=="Vicia_americana","Vicia.americana",ifelse(species=="Vulpia_octoflora","Vulpia.octoflora",ifelse(species=="VUOC","Vulpia.octoflora",ifelse(species=="ALDE","Alyssum.desertorum",ifelse(species=="Allysum_desetorum","Alyssum.desertorum",ifelse(species=="ALTE","Allium.textile",ifelse(species=="Alyssum_desertorum","Alyssum.desertorum",ifelse(species=="Alyssum.desertorum","Alyssum.desertorum",ifelse(species=="Androsace_occidentalis","Androsace.occidentalis",ifelse(species=="ARCA","Artemisia.cana",ifelse(species=="ARDR","Artemisia.dracunculus",ifelse(species=="ARFR","Artemisia.frigida",ifelse(species=="Aristida_purpurea","Aristida.purpurea",ifelse(species=="ARPU","Aristida.purpurea",ifelse(species=="Artemisia_cana","Artemisia.cana",ifelse(species=="Artemisia_dracunculus","Artemisia.dracunculus",ifelse(species=="Artemisia_frigida","Artemisia.frigida",ifelse(species=="ARTR","Artemisia.tridentata",ifelse(species=="BODA","Bouteloua.dactyloides",ifelse(species=="BOGR" ,"Bouteloua.gracilis",ifelse(species=="Bouteloua_dactyloides","Bouteloua.dactyloides",ifelse(species=="Bouteloua_gracilis","Bouteloua.gracilis",ifelse(species=="BRAR","Bromus.arvensis",ifelse(species=="Bromus_arvensis","Bromus.arvensis",ifelse(species=="Bromus_tectorum","Bromus.tectorum",species)))))))))))))))))))))))))))))))))))))))))))))))))) %>% 
  mutate(Genus_Species_Correct=ifelse(Genus_Species=="BRTE","Bromus.tectorum",ifelse(Genus_Species=="CADU","Carex.duriuscula",ifelse(Genus_Species=="CAFI","Carex.filifolia",ifelse(Genus_Species=="Carex_durescula","Carex.duriuscula",ifelse(Genus_Species=="Carex_duriuscula","Carex.duriuscula",ifelse(Genus_Species=="conyza_canadensis","Conyza.canadensis",ifelse(Genus_Species=="Conyza_canadensis","Conyza.canadensis",ifelse(Genus_Species=="Coryphanthus_vivipara","Coryphantha.viviparus",ifelse(Genus_Species=="Coryphantha_viviparus","Coryphantha.viviparus",ifelse(Genus_Species=="COVI","Coryphantha.viviparus",ifelse(Genus_Species=="DEPI","Descurainia.pinnata",ifelse(Genus_Species=="ERHO","Eremogone.hookeri",ifelse(Genus_Species=="GUSA","Gutierrezia.sarothrae",ifelse(Genus_Species=="HECO","Hesperostipa.comata",ifelse(Genus_Species=="Hesperostipa_comata","Hesperostipa.comata",ifelse(Genus_Species=="Hedeoma_hispida","Hedeoma.hispida",ifelse(Genus_Species=="Koeleria_macrantha","Koeleria.macrantha",ifelse(Genus_Species=="KOMA","Koeleria.macrantha",ifelse(Genus_Species=="Lithospermum_incisum","Lithospermum.incisum",ifelse(Genus_Species=="LOAR","Logfia.arvensis",ifelse(Genus_Species=="Logfia_arvensis","Logfia.arvensis",ifelse(Genus_Species=="LYJU","Lygodesmia.juncea",ifelse(Genus_Species=="MUDI","Musineon.divaricatum",ifelse(Genus_Species=="NAVI","Nassella.viridula",ifelse(Genus_Species=="Oenothera_suffrutescens","Oenothera.suffrutescens",ifelse(Genus_Species=="oenothera_suffruticosa","Oenothera.suffrutescens",ifelse(Genus_Species=="Carex_filifolia","Carex.filifolia", ifelse(Genus_Species=="Liatrus_punctata","Liatris.punctata",ifelse(Genus_Species== "LOFO","Lomatium.foeniculaceum",ifelse(Genus_Species=="Pascopyrum_smithii","Pascopyrum.smithii",ifelse(Genus_Species=="Lygodesmia_juncea","Lygodesmia.juncea",ifelse(Genus_Species=="Linum_rigidum","Linum.rigidum",ifelse(Genus_Species=="Asclepias_stenophylla","Asclepias.stenophylla",ifelse(Genus_Species=="Lepidium_densiflorum","Lepidium.densiflorum",ifelse(Genus_Species=="Astragalus_gracilis","Astragalus.gracilis",ifelse(Genus_Species== "Euphorbia_nutans","Euphorbia.nutans",ifelse(Genus_Species=="Liatris_punctata","Liatris.punctata",ifelse(Genus_Species=="Astragalus_purshii","Astragalus.purshii",ifelse(Genus_Species=="Lactuca_serriola","Lactuca.serriola",ifelse(Genus_Species=="COLI","Collomia.linearis",Genus_Species))))))))))))))))))))))))))))))))))))))))) %>% 
  select(-species,-Genus_Species)


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

Trait_Species_Unique<-Species_Cover_90_all %>% 
  select(-Relative_Cover,-Total_Percent,-plot,-year) %>% 
  unique() 

#save as a csv
write.csv(Trait_Species_Unique,"DxG_Plant_Traits/Trait_Species_FK_TB.csv", row.names = FALSE)

#### Clean up trait data ####


##make dataframes match up

#remove all NAs from height column to remove any plants not collected/measured but to avoid removing plants where percent green was not collected  by accident
Traits_Clean <- Traits [complete.cases(Traits[ , 6]),] %>% 
  filter(comments_lab!="not BRTE - did not measure, remove from data") %>% 
  filter(comments_lab!="maybe KOMA?") %>% 
  mutate(wet_leaf_weight_g=as.numeric(ifelse(wet_leaf_weight_g=="<0.0001","0.00001",ifelse(wet_leaf_weight_g=="0..0233",0.0233, wet_leaf_weight_g))))

#Changing ARTR to ARFR based on comments on lab traits
Traits_Clean[602, "genus_species"] <- "Artemisia_frigida"
Traits_Clean[602, "species_code"] <- "ARFR"

#changing LIPU to LIIN based on comments on lab traits
Traits_Clean[427, "genus_species"] <- "Lithospermum_incisum"
Traits_Clean[427, "species_code"] <- "LIIN"
  
#changing MUDI to PIOP based on comments on lab traits
Traits_Clean[249, "genus_species"] <- "Picradeniopsis_oppositifolia"
Traits_Clean[249, "species_code"] <- "PIOP"

#changing LIIN to LIPU based on comments on lab traits
Traits_Clean[504, "genus_species"] <- "Liatris_punctata"
Traits_Clean[504, "species_code"] <- "LIPU"

#changing KOMA to PASM based on comments on lab traits
Traits_Clean[475, "genus_species"] <- "Pascopyrum_smithii"
Traits_Clean[475, "species_code"] <- "PASM"


### Check how many plants have trait data compared to how many still needed for 90%

#FK Species Done
Species_FK<-Traits_Clean %>% 
  filter(Site=="FK")
Species_TB<-Traits_Clean %>% 
  filter(Site=="TB")

Trait_Species_Unique_FK <- Trait_Species_Unique %>% 
  filter(site=="FK") %>% 
  unique()

Trait_Species_Unique_TB <- Trait_Species_Unique %>% 
  filter(site=="TB")%>% 
  unique()

#put a 1 next to any species that has been already measured at FK
Trait_Species_Done_FK<-Trait_Species_Unique_FK %>% 
  mutate(Done=ifelse(Genus_Species_Correct=="Alyssum.desertorum",1,ifelse(Genus_Species_Correct=="Androsace.occidentalis",1,ifelse(Genus_Species_Correct=="Astragalus.purshii",1,ifelse(Genus_Species_Correct=="Astragalus.gracilis",1,ifelse(Genus_Species_Correct=="Bromus.arvensis",1,ifelse(Genus_Species_Correct=="Bromus.tectorum",1,ifelse(Genus_Species_Correct=="Carex.duriuscula",1,ifelse(Genus_Species_Correct=="Carex.filifolia",1,ifelse(Genus_Species_Correct=="Conyza.canadensis",1,ifelse(Genus_Species_Correct=="Hedeoma.hispida",1,ifelse(Genus_Species_Correct=="Hesperostipa.comata",1,ifelse(Genus_Species_Correct=="Koeleria.macrantha",1,ifelse(Genus_Species_Correct=="Lepidium.densiflorum",1,ifelse(Genus_Species_Correct=="Liatris.punctata",1,ifelse(Genus_Species_Correct=="Lithospermum.incisum",1,ifelse(Genus_Species_Correct=="Logfia.arvensis",1,ifelse(Genus_Species_Correct=="Lygodesmia.juncea",1,ifelse(Genus_Species_Correct=="Pascopyrum.smithii",1,ifelse(Genus_Species_Correct=="Pediomelum.esculentum",1,ifelse(Genus_Species_Correct=="Plantago.patagonica",1,ifelse(Genus_Species_Correct=="Poa.secunda",1,ifelse(Genus_Species_Correct=="Sphaeralcea.coccinea",1,ifelse(Genus_Species_Correct=="Taraxacum.officinale",1,ifelse(Genus_Species_Correct=="Tragopogon.dubius",1,ifelse(Genus_Species_Correct=="Vulpia.octoflora",1,ifelse(Genus_Species_Correct=="Linum.rigidum",1,ifelse(Genus_Species_Correct=="Aristida.purpurea",1,ifelse(Genus_Species_Correct=="Artemisia.cana",1,ifelse(Genus_Species_Correct=="Artemisia.dracunculus",1,ifelse(Genus_Species_Correct=="Artemisia.frigida",1,ifelse(Genus_Species_Correct=="Bouteloua.dactyloides",1,ifelse(Genus_Species_Correct=="Bouteloua.gracilis",1,ifelse(Genus_Species_Correct=="Sporobolus.cryptandrus",1,0))))))))))))))))))))))))))))))))))

#put a 1 next to any species that has been already measured at TB
Trait_Species_Done_TB<-Trait_Species_Unique_TB %>% 
  mutate(Done=ifelse(Genus_Species_Correct=="Allium.textile",1,ifelse(Genus_Species_Correct=="Alyssum.desertorum",1,ifelse(Genus_Species_Correct=="Antennaria.parvifolia",1,ifelse(Genus_Species_Correct=="Astragalus.bisulcatus",1,ifelse(Genus_Species_Correct=="Bromus.arvensis",1,ifelse(Genus_Species_Correct=="Bromus.tectorum",1,ifelse(Genus_Species_Correct=="Carex.duriuscula",1,ifelse(Genus_Species_Correct=="Carex.filifolia",1,ifelse(Genus_Species_Correct=="Cirsium.undulatum",1,ifelse(Genus_Species_Correct=="Collomia.linearis",1,ifelse(Genus_Species_Correct=="Descurainia.pinnata",1,ifelse(Genus_Species_Correct=="Draba.reptans",1,ifelse(Genus_Species_Correct=="Eremogone.hookeri",1,ifelse(Genus_Species_Correct=="Erigeron.canus",1,ifelse(Genus_Species_Correct=="Erigeron.pumilus",1,ifelse(Genus_Species_Correct=="Hedeoma.hispida",1,ifelse(Genus_Species_Correct=="Hesperostipa.comata",1,ifelse(Genus_Species_Correct=="Koeleria.macrantha",1,ifelse(Genus_Species_Correct=="Lepidium.densiflorum",1,ifelse(Genus_Species_Correct=="Lithospermum.incisum",1,ifelse(Genus_Species_Correct=="Logfia.arvensis",1,ifelse(Genus_Species_Correct=="Lomatium.foeniculaceum",1,ifelse(Genus_Species_Correct=="Musineon.divaricatum",1,ifelse(Genus_Species_Correct=="Nassella.viridula",1,ifelse(Genus_Species_Correct=="Nothocalais.cuspidate",1,ifelse(Genus_Species_Correct=="Oenothera.suffrtescuns",1,ifelse(Genus_Species_Correct=="Pascopyrum.smithii",1,ifelse(Genus_Species_Correct=="Phlox.hoodii",1,ifelse(Genus_Species_Correct=="Picradeniopsis.oppositifolia",1,ifelse(Genus_Species_Correct=="Plantago.patagonica",1,ifelse(Genus_Species_Correct=="Poa.secunda",1,ifelse(Genus_Species_Correct=="Psoralidium.tenuiflorum",1,ifelse(Genus_Species_Correct=="Sphaeralcea.coccinea",1,ifelse(Genus_Species_Correct=="Taraxacum.officinale",1,ifelse(Genus_Species_Correct=="Tetraneuris.acaulis",1,ifelse(Genus_Species_Correct=="Tragopogon.dubius",1,ifelse(Genus_Species_Correct=="Vulpia.octoflora",1,ifelse(Genus_Species_Correct=="Vicia.americana",1,ifelse(Genus_Species_Correct=="Elymus.elymoides",1,ifelse(Genus_Species_Correct=="Aristida.purpurea",1,ifelse(Genus_Species_Correct=="Artemisia.frigida",1,ifelse(Genus_Species_Correct=="Artemisia.tridentata",1,ifelse(Genus_Species_Correct=="Bouteloua.gracilis",1,ifelse(Genus_Species_Correct=="Gutierrezia.sarothrae",1,0)))))))))))))))))))))))))))))))))))))))))))))

Trait_Species_Done<-Trait_Species_Done_FK %>% 
  rbind(Trait_Species_Done_TB)

#### Look at Trait Database Data and compare to species needed for this project ####
#Database_Data<-Trait_Database %>% 
  #separate(species_matched,c("Genus","Species"), sep = " ")%>%
  #mutate(Genus_Species_Correct=paste(Genus,Species,sep = "."))

#merge FK/TB traits with trait database
#Ground_Database_Traits <-Trait_Species_Done %>% 
 # left_join(Database_Data)

#### Look at differences in Trait Database Traits across community weighted means ####

#Calculate CWM
CWM_Database_Data<- Species_Comp_RelCov_All %>% 
  left_join(plot_layoutK) %>% 
  left_join(Ground_Database_Traits) %>% 
  group_by(block,plot,year,site)

CWM_Database<-functcomp(CWM_Database_Data$trait[, 1:3], CWM_Database_Data$Relative_Cover, CWM.type = "all")


#calculate CWM using tidyr function, removing NAs for now until more data are collected
summarise(PhotosyntheticPathway_CWM=weighted.mean(photosynthetic_pathway,Relative_Cover,na.rm = T))

#### Look at differences in collected traits for CWM 

#Clean up leaf traits and calculate SLA
AverageTraits<-Traits %>% 
  group_by(year,site,block,Genus_Species_Correct) %>% 
  summarise(
    Avg_height_cm=mean(height_cm),
    Avg_biomass_mg=mean(biomass_mg),
    Avg_percent_green=mean(percent_green),
    Avg_emerging_leaves=mean(emerging_leaves),
    Avg_developed_leaves=mean(developed_leaves),
    Avg_scenesced_leaves=mean(scenesced_leaves),
  ) %>% 
  ungroup()

CWM_Collected_Data<- Species_Comp_RelCov_All %>% 
  left_join(plot_layoutK) %>% 
  left_join(AverageTraits) %>% 
  filter(year!=2018) %>%
  filter(year!=2021) %>% #until 2021 data is entered
  group_by(year,site,plot,block,rainfall_reduction,drought,grazing_category,grazing_treatment) %>% 
  #calculate CWM using tidyr function, removing NAs for now until more data are collected
  summarise(
    Height_CWM=weighted.mean(Avg_height_cm,Relative_Cover,na.rm = T),
    Biomass_CWM=weighted.mean(Avg_biomass_mg,Relative_Cover,na.rm=T),
    PercentGreen_CWM=weighted.mean(Avg_percent_green,Relative_Cover,na.rm=T),
    EmergingLeaves_CWM=weighted.mean(Avg_emerging_leaves,Relative_Cover,na.rm=T),
    DevelopedLeaves_CWM=weighted.mean(Avg_developed_leaves,Relative_Cover,na.rm=T),
    ScenescedLeaves_CWM=weighted.mean(Avg_scenesced_leaves,Relative_Cover,na.rm=T),
  ) %>% 
  ungroup()


#### Plot the data ####
####CWM - Height Plots ####
#Figure looking at CWM of height - 2019 
ggplot(subset(CWM_Collected_Data,year==2019),aes(x=rainfall_reduction,y=Height_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community-weighted Mean of Height (cm)")+
  expand_limits(y=30)+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = "NONE")+
  geom_text(x=10, y=30, label="2019",size=20)
#save at 1100 x 1000

#### proposal ####
#Figure looking at CWM of height - 2020 and FK
ggplot(subset(CWM_Collected_Data,year==2020&site=="FK"),aes(x=rainfall_reduction,y=Height_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("No grazing", "Low Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("No grazing", "Low Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("No grazing", "Low Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community-weighted Mean of Height (cm)")+
  expand_limits(y=30)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.text=element_text(size=55),legend.title=element_text(size=55),legend.position = c(0.75,0.80))+
  annotate("text", x=1, y=30, label = "a.", size=20)
#save at 1600 x 1600


####CWM - Biomass Plots ####
#2019
ggplot(subset(CWM_Collected_Data,year==2019),aes(x=rainfall_reduction,y=Biomass_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community-weighted Mean of Biomass (mg)")+
  expand_limits(y=400)+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = "NONE")+
  geom_text(x=10, y=400, label="2019",size=20)
#save at 1000 x 1000

#2020 - FK 
ggplot(subset(CWM_Collected_Data,year==2020&site=="FK"),aes(x=rainfall_reduction,y=Biomass_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=10, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE, size=4)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Low grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","orange3","maroon4"),labels = c("Low grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("twodash","twodash","twodash"),labels = c("Low grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  expand_limits(y=1000)+
  theme(axis.text.y=element_text(size=80),axis.text.x=element_text(size=80),axis.title.y=element_blank(),axis.title.x=element_text(size=80),legend.text=element_text(size=80),legend.title=element_text(size=80),legend.position ="NONE")+
  annotate("text", x=38, y=1000, label = "d. Biomass (mg)", size=35)
#save at 1600 x 1600

####CWM - Percent Green ####
#2019
ggplot(subset(CWM_Collected_Data,year==2019),aes(x=rainfall_reduction,y=PercentGreen_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community-weighted Mean of Greenness (%)")+
  expand_limits(y=150)+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = "NONE")+
  geom_text(x=10, y=150, label="2019",size=20)
#save at 1000 x 1000

#2020 - FK
ggplot(subset(CWM_Collected_Data,year==2020&site=="FK"),aes(x=rainfall_reduction,y=PercentGreen_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=10, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE,size=4)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Low grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","orange3","maroon4"),labels = c("Low grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("twodash","twodash","twodash"),labels = c("Low grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community-weighted Mean")+
  expand_limits(y=80)+
  theme(axis.text.y=element_text(size=80),axis.text.x=element_text(size=80),axis.title.y=element_text(size=80),axis.title.x=element_text(size=80),legend.text=element_text(size=80),legend.title=element_text(size=80),legend.position ="NONE")+
  annotate("text", x=42, y=80, label = "c. Percent Green", size=35)
#save at 1600 x 1600

ggplot(subset(CWM_Collected_Data,year==2020&site=="FK"),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=10, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE,size=4)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("No grazing", "Low Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","orange3","maroon4"),labels = c("No grazing", "Low Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("twodash","twodash","twodash"),labels = c("No grazing", "Low Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  ylab("Community-weighted Mean")+
  expand_limits(y=3)+
  theme(axis.text.y=element_text(size=80),axis.text.x=element_blank(),axis.title.y=element_text(size=80),axis.title.x=element_blank(),legend.text=element_text(size=70),legend.title=element_text(size=70),legend.position =c(0.70,0.74))+
  annotate("text", x=50, y=3, label = "a. Developed Leaves", size=40)
#save at 1600 X 1600

####CWM - Emerging Leaves ####
#2019
ggplot(subset(CWM_Collected_Data,year==2019),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM of Emerging Leaves")+
  expand_limits(y=6)+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = "NONE")+
  geom_text(x=10, y=6, label="2019",size=20)
#save at 1000 x 1000

#2020 - FK
ggplot(subset(CWM_Collected_Data,year==2020&site=="FK"),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("No grazing", "Low Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("No grazing", "Low Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("No grazing", "Low Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community-weighted Mean of Emerging Leaves")+
  expand_limits(y=1)+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = "NONE")+
  annotate("text", x=1, y=1, label = "b.", size=20)
#save at 1000 x 1000


####CWM - Developed Leaves ####
#2019
ggplot(subset(CWM_Collected_Data,year==2019),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM of Developed Leaves")+
  expand_limits(y=15)+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = "NONE")+
  geom_text(x=10, y=15, label="2019",size=20)
#save at 1000 x 1000

#2020 - FK
ggplot(subset(CWM_Collected_Data,year==2020&site=="FK"),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=6, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Low Grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkseagreen2","blue4","maroon4"),labels = c("Low Grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("solid","twodash","dotted"),labels = c("Low Grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  ylab("Community-weighted Mean")+
  expand_limits(y=3)+
  theme(axis.text.y=element_text(size=70),axis.text.x=element_blank(),axis.title.y=element_text(size=70),axis.title.x=element_blank(),legend.text=element_text(size=70),legend.title=element_text(size=70),legend.position =c(0.70,0.75))+
  annotate("text", x=50, y=3, label = "a. Developed Leaves", size=35)
#save at 1600 X 1600

####CWM - Scenesced Leaves ####
#2019
ggplot(subset(CWM_Collected_Data,year==2019),aes(x=rainfall_reduction,y=ScenescedLeaves_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM of Scenesced Leaves")+
  expand_limits(y=7)+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = "NONE")+
  geom_text(x=10, y=7, label="2019",size=20)
#save at 1000 x 1000

#2020
ggplot(subset(CWM_Collected_Data,year==2020&site=="FK"),aes(x=rainfall_reduction,y=ScenescedLeaves_CWM,color=grazing_treatment,linetype=grazing_treatment,shape=grazing_treatment)) +  
  geom_point(size=10, stroke =2)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE,size=4)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  labs(color  = "Grazing Treatment", linetype = "Grazing Treatment", shape = "Grazing Treatment")+
  scale_shape_manual(values=c(15,16,17),labels = c("Low grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_color_manual(values=c("darkgreen","orange3","maroon4"),labels = c("Low grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  scale_linetype_manual(values=c("twodash","twodash","twodash"),labels = c("Low grazing", "Medium Grazing","High Grazing"), breaks = c("destock","stable","heavy"),name="Grazing Treatment")+
  xlab("Rainfall Reduction (%)")+
  ylab("Community-weighted Mean of Senesced Leaves")+
  expand_limits(y=6)+
  theme(axis.text.y=element_text(size=80),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position ="NONE")+
  annotate("text", x=43, y=6, label = "b.Senesced Leaves", size=35)


#### Statistics for FK 2020 ###
#CWM of height
Height_CWM_AOV_model <- aov(Height_CWM ~ grazing_treatment*drought + (1|block), data = subset(CWM_Collected_Data,year==2020&site=="FK")) 
summary(Height_CWM_AOV_model)
model.tables(Height_CWM_AOV_model)

#CWM of biomass
Biomass_CWM_AOV_model <- aov(Biomass_CWM ~ grazing_treatment*drought + (1|block), data = subset(CWM_Collected_Data,year==2020&site=="FK")) 
summary(Biomass_CWM_AOV_model)
model.tables(Biomass_CWM_AOV_model)

#CWM of percent green
PercentGreen_CWM_AOV_model <- aov(PercentGreen_CWM ~ grazing_treatment*drought + (1|block), data = subset(CWM_Collected_Data,year==2020&site=="FK")) 
summary(PercentGreen_CWM_AOV_model)
model.tables(PercentGreen_CWM_AOV_model)
#%green is significant across grazing treatments
TukeyHSD(PercentGreen_CWM_AOV_model)

#CWM of emerging leaves
EmergingLeaves_CWM_AOV_model <- aov(EmergingLeaves_CWM ~ grazing_treatment*drought + (1|block), data = subset(CWM_Collected_Data,year==2020&site=="FK")) 
summary(EmergingLeaves_CWM_AOV_model)
model.tables(EmergingLeaves_CWM_AOV_model)

#CWM of developed leaves
DevelopedLeaves_CWM_AOV_model <- aov(DevelopedLeaves_CWM ~ grazing_treatment*drought + (1|block), data = subset(CWM_Collected_Data,year==2020&site=="FK")) 
summary(DevelopedLeaves_CWM_AOV_model)
model.tables(DevelopedLeaves_CWM_AOV_model)

#CWM of scenesced leaves
ScenescedLeaves_CWM_AOV_model <- aov(ScenescedLeaves_CWM ~ grazing_treatment*drought + (1|block), data = subset(CWM_Collected_Data,year==2020&site=="FK")) 
summary(ScenescedLeaves_CWM_AOV_model)
model.tables(ScenescedLeaves_CWM_AOV_model)
#senesced is significant across grazing treatments
TukeyHSD(ScenescedLeaves_CWM_AOV_model)



