##########################################################################################################
#Project: Plant Species Composition in MGP with Drought x Grazing 

##########################################################################################################

#### Load Libraries ####
library(tidyverse) 

#### Set Working Directory ####
#Bloodworth - Mac
setwd("~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data")

#### Read in Data ####
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


#Create Long dataframe from wide dataframe and fix species issues
Long_Cov_2018_FK<-gather(FK_SpComp_2018,key="species","cover",18:117) %>% 
  dplyr::select(year,site,plot,aerial_basal,added_total_excel,species,cover) %>% 
  filter(!species %in% c("STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii","Rock","Moss.Lichen.Bogr.overlap")) %>%
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2018_FK<-Long_Cov_2018_FK%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,aerial_basal,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2018_FK$plot<-as.factor(Relative_Cover_2018_FK$plot)

#FK - 2019
# get dataframe with just aerial total cover per plot
Total_Cover_2019_FK<-FK_SpComp_2019 %>%
  #only keep species to calculate added total
  filter(!genus_species %in% c("Added_Total","Estimated_Total", "Rock", "Litter", "Bare Ground","overlap" ,"Overlap" ,"opuntia_pads" ,"Dung","Lichen" ,"Moss" ,"Mushroom")) %>% 
  mutate(basal_cover=ifelse(basal_cover=="<0.6",0.25,as.numeric(basal_cover))) %>% 
  group_by(site,block,plot) %>% 
  summarise(Total_Cover_Aerial=sum(aerial_cover,na.rm=T), Total_Cover_Basal=sum(basal_cover,na.rm=T)) %>% 
  ungroup() 

#make dataframe with necessary information for relative cover calculation
Species_Cover_2019_FK<-FK_SpComp_2019 %>% 
  #take out all 'species' that are not actually plant species
  filter(!is.na(genus_species)) %>% 
  filter(genus_species!="") %>% 
  filter(!genus_species %in% c("Added_Total","Estimated_Total" ,"Rock","Litter", "Bare Ground","overlap","Overlap", "Dung","Lichen","Moss","Mushroom","opuntia_pads")) %>%
  dplyr::select(-c(observers,date,notes)) %>% 
  filter(!is.na(aerial_cover)) %>%
  filter(!is.na(basal_cover)) %>%
  filter(aerial_cover!="0") %>% 
  mutate(basal_cover=ifelse(basal_cover=="<0.6",0.25,ifelse(basal_cover==0,0.25, as.numeric(basal_cover))))


#Calculate Relative Cover
Relative_Cover_2019_FK_1<-Species_Cover_2019_FK%>%
  #Make a new column named "Treatment"
  mutate(Treatment=paste(block,plot,sep="_"))%>%
  #Add Total_Cover data into the Relative_Cover data sheet
  left_join(Total_Cover_2019_FK)%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(aerial_Relative_Cover=(aerial_cover/Total_Cover_Aerial)*100,basal_Relative_Cover=(basal_cover/Total_Cover_Basal)*100) %>%
  mutate(year=2019)  %>% 
  rename(species="genus_species") %>% 
  dplyr::select(year,site,plot,species,aerial_Relative_Cover,basal_Relative_Cover)
  

####stopped here ####
# need to gather so that aerial and basal are in the same column like remainder of dataframes
# already double checked TB data and got aerial and basal rel cov for TB -- need to do that for the rest of FK though
# then need to clean up species names, etc. before merge step so that it's cleaned for other usage as well
Relative_Cover_2019_FK<-Relative_Cover_2019_FK_1 %>% 
  mutate(mutate(Treatment=paste(c(year,site,plot,species,sep="_")))) %>%
  gather(Relative_Cover_2019_FK_1,key=1:4,5:6)

#make plot a factor not an integer
Relative_Cover_2019_FK$plot<-as.factor(Relative_Cover_2019_FK$plot)

#FK - 2020
#get dataframe with just total cover per plot
Total_Cover_2020_FK<-FK_SpComp_2020%>%
  #only keep species to calculate added tota
  filter(!is.na(genus_species)) %>% 
  filter(genus_species!="" & genus_species!="Added_total" & genus_species!="Estimated_total" & genus_species!="Rock" & genus_species!="Litter" & genus_species!="Bareground" &  genus_species!="Dung" & genus_species!="Lichen" & genus_species!="Moss" & genus_species!="Mushroom") %>% 
  group_by(block,plot) %>% 
  summarise(Total_Cover_Aerial=sum(aerial_cover,na.rm = T)) %>% 
  ungroup()

#make dataframe with necessary information for relative cover calculation
Species_Cover_2020_FK<-FK_SpComp_2020 %>% 
  #take out all 'species' that are not actually plant species
  filter(!is.na(genus_species)) %>% 
  filter(genus_species!="") %>% 
  filter(!genus_species %in% c("Added_total","Estimated_total" ,"Rock","Litter", "Bareground","overlap","Overlap", "Dung","ASER_Like_Woody","Lichen","Moss","Mushroom")) %>% 
  filter(!is.na(aerial_cover)) %>% 
  filter(aerial_cover!=0) %>% 
  dplyr::select(-observers,-date)

#Calculate Relative Cover
Relative_Cover_2020_FK<-Species_Cover_2020_FK%>%
  #Make a new column named "Treatment"
  mutate(Treatment=paste(block,plot,sep="_"))%>%
  #Add Total_Cover data into the Relative_Cover data sheet
  left_join(Total_Cover_2020_FK) %>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(aerial_cover/Total_Cover_Aerial)*100) %>% 
  dplyr::select(-aerial_cover,-basal_cover,-Total_Cover_Aerial) %>% 
  mutate(year=2020) %>% 
  rename(species="genus_species") %>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2020_FK$plot<-as.factor(Relative_Cover_2020_FK$plot)

#FK - 2021
Aerial_Cover_2021_FK<-FK_SpComp_2021 %>% 
  filter(aerial_basal!="basal")

#Create Long dataframe from wide dataframe
Long_Cov_2021_FK<-gather(Aerial_Cover_2021_FK,key="species","cover",20:61,na.rm=T) %>% 
  dplyr::select(year,site,plot,added_total_excel,species,cover) 

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
Long_Cov_2022_FK<-gather(Aerial_Cover_2022_FK,key="species","cover",18:68, na.rm=T) %>% 
  dplyr::select(year,site,plot,added_total_excel,species,cover) %>% 
  filter(!species %in% c("final_total","final_total_excel")) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2022_FK<-Long_Cov_2022_FK%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2022_FK$plot<-as.factor(Relative_Cover_2022_FK$plot)

#TB- 2018
#Create Long dataframe from wide dataframe
Long_Cov_2018_TB<-gather(TB_SpComp_2018,key="species","cover",18:113) %>% 
  dplyr::select(year,site,plot,aerial_basal,added_total_excel,species,cover) %>% 
  filter(!species %in% c("STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2018_TB<-Long_Cov_2018_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  dplyr::select(year,site,plot,aerial_basal,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2018_TB$plot<-as.factor(Relative_Cover_2018_TB$plot)

#TB- 2019

#Create Long dataframe from wide dataframe
Long_Cov_2019_TB<-gather(TB_SpComp_2019 ,key="species","cover",18:114) %>% 
  dplyr::select(year,site,plot,aerial_basal,added_total_excel,species,cover) %>% 
  filter(!species %in% c("STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0) %>% 
  #removing typo -- this should have been a zero
  filter(cover!=".")

Long_Cov_2019_TB$cover<-as.numeric(Long_Cov_2019_TB$cover)

#Calculate Relative Cover
Relative_Cover_2019_TB<-Long_Cov_2019_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  dplyr::select(year,site,plot,aerial_basal,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2019_TB$plot<-as.factor(Relative_Cover_2019_TB$plot)

#TB- 2020

#Create Long dataframe from wide dataframe
Long_Cov_2020_TB<-gather(TB_SpComp_2020,key="species","cover",18:114) %>% 
  dplyr::select(year,site,plot,aerial_basal,added_total_excel,species,cover) %>% 
  filter(!species %in% c("STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0) %>% 
  filter(cover!="") %>% 
  mutate(cover=ifelse(cover=="<0.5",0.25,cover))

Long_Cov_2020_TB$cover<-as.numeric(Long_Cov_2020_TB$cover)

#Calculate Relative Cover
Relative_Cover_2020_TB<-Long_Cov_2020_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  dplyr::select(year,site,plot,aerial_basal,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2020_TB$plot<-as.factor(Relative_Cover_2020_TB$plot)

#TB- 2021

#Create Long dataframe from wide dataframe
Long_Cov_2021_TB<-gather(TB_SpComp_2021,key="species","cover",21:81) %>% 
  dplyr::select(year,site,plot,aerial_basal,added_total_excel,species,cover) %>%  
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2021_TB<-Long_Cov_2021_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100)%>% 
  dplyr::select(year,site,plot,aerial_basal,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_2021_TB$plot<-as.factor(Relative_Cover_2021_TB$plot)

#TB - 2022

#Create Long dataframe from wide dataframe
Long_Cov_2022_TB<-gather(TB_SpComp_2022,key="species","cover",18:85) %>% 
  dplyr::select(year,site,plot,aerial_basal,added_total_excel,species,cover) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2022_TB<-Long_Cov_2022_TB%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,aerial_basal,species,Relative_Cover)

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
