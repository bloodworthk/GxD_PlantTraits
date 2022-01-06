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

#Trait Database
Trait_Database<-read.csv("DxG_Plant_Traits/sCoRRE categorical trait data_11302021.csv")

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
All_Traits_2019<- All_Traits_2019[!apply(is.na(All_Traits_2019) |All_Traits_2019 == "", 1, all),]
All_Traits_2019_fixed<-All_Traits_2019 %>% 
  select(-X,-X.1,-plant) %>% 
  mutate(year=2019) %>% 
  mutate(wet_leaf_weight=as.numeric(ifelse(wet_weight_g=="<0.001","0.0001",wet_weight_g))) %>% 
  mutate(dry_leaf_weight=as.numeric(ifelse(leaf_mg=="no leaf",NA,ifelse(leaf_mg=="",NA,leaf_mg))))
All_Traits_2019_fixed<-All_Traits_2019_fixed[, c(24,1,4,2,3,5,6,7,8,9,10,11,12,13,14,15,17,16,18,19,20,21,22,23)]

#merge field and lab traits
All_Traits_2020<-Leaf_Traits_2020 %>% 
  mutate(wet_leaf_weight=as.numeric(ifelse(wet_leaf_weight_g=="<0.0001","0.00001",wet_leaf_weight_g))) %>% 
  mutate(dry_leaf_weight=as.numeric(ifelse(dry_leaf_weight_g=="<0.0001","0.00001",dry_leaf_weight_g))) %>% 
  select(-paddock,-plant,-date,-comments,-dry_leaf_weight_g,-wet_leaf_weight_g) %>% 
  left_join(Field_Traits_2020) %>% 
  mutate(biomass_g=dry_plant_weight_g+dry_leaf_weight) %>% 
  mutate(biomass_mg=biomass_g*1000) %>% 
  select(-biomass_g)

All_Traits_2020_fixed<-All_Traits_2020 %>% 
  rename(open_flowers="open_.flowers") %>% 
  select(-observer,-date,-plant)%>% 
  mutate(year=2020) %>% 
  rename(leaf_thickness_mm="leaf_thickness_.mm.") %>% 
  mutate(leaf_area_cm=NA) %>% 
  select(-dry_plant_weight_g)
All_Traits_2020_fixed<-All_Traits_2020_fixed[, c(23,1,4,2,3,8,9,10,11,12,13,14,15,16,17,18,19,20,22,5,24,6,7,21)]

#### add in once 2021 data is done ####
#merge field and lab traits 2021
All_Traits_2021<-Leaf_Traits_2021 %>% 
  select(-paddock,-plant,-date,-comments) %>% 
  left_join(Field_Traits_2021) %>% 
  mutate(biomass_g=dry_plant_weight_g+dry_leaf_weight) %>% 
  mutate(biomass_mg=biomass_g*1000) %>% 
  select(-biomass_g)

All_Traits_2021_fixed<-All_Traits_2021 %>% 
  select(-paddock,-X,-X.1,-X.2,-X.3,-X.4,-X.5)%>% 
  rename(block="DxG_block") %>% 
  rename(open_flowers="open_.flowers") %>% 
  mutate(year=2021)
All_Traits_2021_fixed<-All_Traits_2021_fixed[, c()]

#join all traits together
Traits<-All_Traits_2019_fixed %>% 
  full_join(All_Traits_2020_fixed) %>% 
  separate(genus_species,c("Genus","Species"), sep = "([_ ])")%>%
  mutate(Genus_Species_Correct=paste(Genus,Species,sep = "."))


### Check how many plants have trait data compared to how many still needed for 90%

#FK Species Done
Species_FK<-Traits %>% 
  filter(site=="FK")
Species_TB<-Traits %>% 
  filter(site=="TB")

Trait_Species_Unique_FK <- Trait_Species_Unique %>% 
  filter(site=="FK") %>% 
  unique()

Trait_Species_Unique_TB <- Trait_Species_Unique %>% 
  filter(site=="TB")%>% 
  unique()

#put a 1 next to any species that has been already measured at FK
Trait_Species_Done_FK<-Trait_Species_Unique_FK %>% 
  mutate(Done=ifelse(Genus_Species_Correct=="Aristida.purpurea",1,ifelse(Genus_Species_Correct=="Bromus.arvensis",1,ifelse(Genus_Species_Correct=="Bromus.tectorum",1,ifelse(Genus_Species_Correct=="Hesperostipa.comata",1,ifelse(Genus_Species_Correct=="Koeleria.macrantha",1,ifelse(Genus_Species_Correct=="Logfia.arvensis",1,ifelse(Genus_Species_Correct=="Pediomelum.esculentum",1,ifelse(Genus_Species_Correct=="Sphaeralcea.coccinea",1,ifelse(Genus_Species_Correct=="Tragopogon.dubius",1,0))))))))))

#put a 1 next to any species that has been already measured at TB
Trait_Species_Done_TB<-Trait_Species_Unique_TB %>% 
  mutate(Done=ifelse(Genus_Species_Correct=="Bouteloua.gracilis",1,ifelse(Genus_Species_Correct=="Vicia.americana",1,ifelse(Genus_Species_Correct== "Koeleria.macrantha",1,ifelse(Genus_Species_Correct=="Pascopyrum.smithii",1,ifelse(Genus_Species_Correct=="Logfia.arvensis",1,ifelse(Genus_Species_Correct=="Bromus.tectorum",1,ifelse(Genus_Species_Correct=="Carex.filifolia",1,ifelse(Genus_Species_Correct=="Psoralidium.tenuiflorum",1,ifelse(Genus_Species_Correct=="Phlox.hoodii",1,ifelse(Genus_Species_Correct=="Plantago.patagonica",1,ifelse(Genus_Species_Correct=="Comandra.umbellata",1,0))))))))))))

Trait_Species_Done<-Trait_Species_Done_FK %>% 
  rbind(Trait_Species_Done_TB)


#save as a csv
write.csv(Trait_Species_Done,"DxG_Plant_Traits/Trait_Species_Done.csv", row.names = FALSE)

#### Look at Trait Database Data and compare to species needed for this project ####
Database_Data<-Trait_Database %>% 
  separate(species_matched,c("Genus","Species"), sep = " ")%>%
  mutate(Genus_Species_Correct=paste(Genus,Species,sep = "."))

#merge FK/TB traits with trait database
Ground_Database_Traits <-Trait_Species_Done %>% 
  left_join(Database_Data)
  
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

#Figure looking at CWM of height - 2020
ggplot(subset(CWM_Collected_Data,year==2020),aes(x=rainfall_reduction,y=Height_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community-weighted Mean of Height (cm)")+
  expand_limits(y=30)+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = c(0.8,0.89))+
  geom_text(x=6, y=30, label="2020",size=20)
#save at 1000 x 1000

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

#2020
ggplot(subset(CWM_Collected_Data,year==2020),aes(x=rainfall_reduction,y=Biomass_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community-weighted Mean of Biomass (mg)")+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = c(0.8,0.89))+
  expand_limits(y=1000)+
  geom_text(x=6, y=1000, label="2020",size=20)
#save at 1000 x 1000

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

#2020
ggplot(subset(CWM_Collected_Data,year==2020),aes(x=rainfall_reduction,y=PercentGreen_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("Community-weighted Mean of Greenness (%)")+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = c(0.8,0.89))+
  expand_limits(y=150)+
  geom_text(x=6, y=150, label="2020",size=20)
#save at 1000 x 1000

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

#2020
ggplot(subset(CWM_Collected_Data,year==2020),aes(x=rainfall_reduction,y=EmergingLeaves_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM of Emerging Leaves")+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = c(0.8,0.89))+
  expand_limits(y=6)+
  geom_text(x=6, y=6, label="2020",size=20)
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

#2020
ggplot(subset(CWM_Collected_Data,year==2020),aes(x=rainfall_reduction,y=DevelopedLeaves_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM of Developed Leaves")+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = c(0.8,0.89))+
  expand_limits(y=15)+
  geom_text(x=6, y=15, label="2020",size=20)
#save at 1000 x 1000

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
ggplot(subset(CWM_Collected_Data,year==2020),aes(x=rainfall_reduction,y=ScenescedLeaves_CWM,fill=grazing_treatment)) +  
  geom_point(size=2, shape=23)+
  geom_smooth(aes(linetype=grazing_treatment),method='lm', se=FALSE)+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  xlab("Rainfall Reduction (%)")+
  ylab("CWM of Scenesced Leaves")+
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),axis.title.y=element_text(size=24),axis.title.x=element_text(size=24),legend.text=element_text(size=25),legend.title=element_text(size=30),legend.position = c(0.8,0.89))+
  expand_limits(y=7)+
  geom_text(x=6, y=7, label="2020",size=20)
#save at 1000 x 1000


#### Statistics
#CWM of height
Height_CWM_AOV_model <- aov(Height_CWM ~ grazing_treatment*drought*year, data = CWM_Collected_Data) 

summary(Height_CWM_AOV_model)

model.tables(Height_CWM_AOV_model)

#CWM of biomass
Biomass_CWM_AOV_model <- aov(Biomass_CWM ~ grazing_treatment*drought*year, data = CWM_Collected_Data) 

summary(Biomass_CWM_AOV_model)

model.tables(Biomass_CWM_AOV_model)

#CWM of percent green
PercentGreen_CWM_AOV_model <- aov(PercentGreen_CWM ~ grazing_treatment*drought*year, data = CWM_Collected_Data) 

summary(PercentGreen_CWM_AOV_model)

model.tables(PercentGreen_CWM_AOV_model)

#CWM of emerging leaves
EmergingLeaves_CWM_AOV_model <- aov(EmergingLeaves_CWM ~ grazing_treatment*drought*year, data = CWM_Collected_Data) 

summary(EmergingLeaves_CWM_AOV_model)

model.tables(EmergingLeaves_CWM_AOV_model)

#CWM of developed leaves
DevelopedLeaves_CWM_AOV_model <- aov(DevelopedLeaves_CWM ~ grazing_treatment*drought*year, data = CWM_Collected_Data) 

summary(DevelopedLeaves_CWM_AOV_model)

model.tables(DevelopedLeaves_CWM_AOV_model)

#CWM of scenesced leaves
ScenescedLeaves_CWM_AOV_model <- aov(ScenescedLeaves_CWM ~ grazing_treatment*drought*year, data = CWM_Collected_Data) 

summary(ScenescedLeaves_CWM_AOV_model)

model.tables(ScenescedLeaves_CWM_AOV_model)



