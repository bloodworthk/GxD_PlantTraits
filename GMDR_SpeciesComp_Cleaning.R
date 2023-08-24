##########################################################################################################
#Project: Plant Species Composition in MGP with Drought x Grazing
#Cleaning Step

##########################################################################################################

#### Load Libraries ####
library(tidyverse) 
#install.packages("codyn")
library(codyn)
library(vegan)

#### Set Working Directory ####
#Bloodworth - Mac
setwd("~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data")

#Bloodworth - PC
setwd("/Users/kjbloodw/Box/Projects/Dissertation/Data")

#### Read in Data ####
#Read in Species Comp Data and fix species names
FK_SpComp_2018<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2018.csv") 
FK_SpComp_2018$plot<-as.factor(FK_SpComp_2018$plot)
FK_SpComp_2019<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2019.csv")
FK_SpComp_2020<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2020.csv")
FK_SpComp_2021<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2021.csv")
FK_SpComp_2022<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2022.csv")
FK_SpComp_2023<-read.csv("DxG_Plant_Traits/DxG_spcomp_FK_2023.csv")
TB_SpComp_2018<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2018.csv")
TB_SpComp_2019<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2019.csv")
TB_SpComp_2020<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2020.csv")
TB_SpComp_2021<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2021.csv")
TB_SpComp_2022<-read.csv("DxG_Plant_Traits/DxG_spcomp_TB_2022.csv")

#Read in Plot Data
plot_layoutK<-read.csv("DxG_Plant_Traits/GMDR_site_plot_metadata.csv") %>% 
  dplyr::select(site,block,paddock,plot,slope,rainfall_reduction,drought,grazing_category,grazing_treatment,livestock_util_2019,livestock_util_2020,livestock_util_2021)
plot_layoutK$plot<-as.factor(plot_layoutK$plot)

#Read in functional group data
Functional_Group_RelCov<-read.csv("RelCov_FunctionalGroups.csv")
Functional_Group_RelCov$plot<-as.factor(Functional_Group_RelCov$plot)

#### Clean Up Species Comp Data and Calculate Relative Cover ####

#### FK - 2018 - Relative Cover ####
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
  dplyr::select(year,site,plot,species,aerial_basal,Relative_Cover) %>% 
  mutate(aerial_basal=ifelse(aerial_basal=="aerial","Aerial",aerial_basal))

#make plot a factor not an integer
Relative_Cover_2018_FK$plot<-as.factor(Relative_Cover_2018_FK$plot)

#### FK - 2019 - Relative Cover ####
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

Relative_Cover_2019_FK<-gather(Relative_Cover_2019_FK_1, "aerial_basal","Relative_Cover",5:6) %>% 
  mutate(aerial_basal=ifelse(aerial_basal=="aerial_Relative_Cover","Aerial","Basal"))

#make plot a factor not an integer
Relative_Cover_2019_FK$plot<-as.factor(Relative_Cover_2019_FK$plot)

#### FK - 2020 - Relative Cover ####
#get dataframe with just total cover per plot
Total_Cover_2020_FK<-FK_SpComp_2020%>%
  #only keep species to calculate added tota
  filter(!is.na(genus_species)) %>% 
  filter(!genus_species %in% c("","Added_total","Estimated_total","Rock","Litter","Bareground","Dung","Lichen","Moss","Mushroom")) %>% 
  group_by(block,plot) %>% 
  summarise(Total_Cover_Aerial=sum(aerial_cover,na.rm = T),Total_Cover_Basal=sum(basal_cover,na.rm = T)) %>% 
  ungroup()

#make dataframe with necessary information for relative cover calculation
Species_Cover_2020_FK<-FK_SpComp_2020 %>% 
  #take out all 'species' that are not actually plant species
  filter(!is.na(genus_species)) %>% 
  filter(!genus_species %in% c("" ,"Added_total","Estimated_total" ,"Rock","Litter", "Bareground","overlap","Overlap", "Dung","ASER_Like_Woody","Lichen","Moss","Mushroom")) %>% 
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
  mutate(Relative_Cover_Aerial=(aerial_cover/Total_Cover_Aerial)*100) %>% 
  mutate(Relative_Cover_Basal=(basal_cover/Total_Cover_Basal)*100) %>% 
  mutate(year=2020) %>% 
  rename(species="genus_species") %>% 
  dplyr::select(year,site,plot,species,Relative_Cover_Aerial,Relative_Cover_Basal) %>% 
  gather("aerial_basal","Relative_Cover",5:6) %>% 
  mutate(aerial_basal=ifelse(aerial_basal=="Relative_Cover_Aerial","Aerial","Basal"))

#make plot a factor not an integer
Relative_Cover_2020_FK$plot<-as.factor(Relative_Cover_2020_FK$plot)

#### FK - 2021 - Relative Cover ####
#Create Long dataframe from wide dataframe
Long_Cov_2021_FK<-gather(FK_SpComp_2021,key="species","cover",20:61,na.rm=T) %>% 
  dplyr::select(year,site,plot,species,aerial_basal,cover,added_total_excel) 

#Calculate Relative Cover
Relative_Cover_2021_FK<-Long_Cov_2021_FK%>%
  group_by(year, site, plot, aerial_basal) %>% 
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,aerial_basal,Relative_Cover) %>% 
  mutate(aerial_basal=ifelse(aerial_basal=="aerial","Aerial","Basal")) %>% 
  ungroup()

#make plot a factor not an integer
Relative_Cover_2021_FK$plot<-as.factor(Relative_Cover_2021_FK$plot)

#### FK - 2022 - Relative Cover ####
#Create Long dataframe from wide dataframe
Long_Cov_2022_FK<-gather(FK_SpComp_2022,key="species","cover",20:70, na.rm=T) %>% 
  dplyr::select(year,site,plot,species,aerial_basal,cover,added_total_excel) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2022_FK<-Long_Cov_2022_FK%>%
  group_by(year, site, plot, aerial_basal) %>% 
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,aerial_basal,Relative_Cover) %>% 
  mutate(aerial_basal=ifelse(aerial_basal=="aerial","Aerial","Basal")) %>% 
  ungroup()

#make plot a factor not an integer
Relative_Cover_2022_FK$plot<-as.factor(Relative_Cover_2022_FK$plot)

#### FK - 2023 - Relative Cover ####
# get dataframe with just aerial total cover per plot
Total_Cover_2023_FK<-FK_SpComp_2023 %>%
  #only keep species to calculate added total
  filter(!genus_species %in% c("Added_total","Estimated_total", "Rock", "Litter", "LItter","Bareground","OPPO_Pads" ,"Dung","Lichen" ,"Moss" ,"Mushroom")) %>% 
  na.omit(aerial_cover) %>% 
  group_by(site,block,plot) %>% 
  summarise(Total_Cover_Aerial=sum(aerial_cover,na.rm=T), Total_Cover_Basal=sum(basal_cover,na.rm=T)) %>%
  ungroup() 

#make dataframe with necessary information for relative cover calculation
Species_Cover_2023_FK<-FK_SpComp_2023 %>% 
  #take out all 'species' that are not actually plant species
  #only keep species to calculate added total
  filter(!genus_species %in% c("Added_total","Estimated_total", "Rock", "Litter","LItter", "Bareground","OPPO_Pads" ,"Dung","Lichen" ,"Moss" ,"Mushroom")) %>% 
  na.omit(aerial_cover) %>% 
  dplyr::select(-c(observers,date,notes))

#Calculate Relative Cover
Relative_Cover_2023_FK_1<-Species_Cover_2023_FK%>%
  #Make a new column named "Treatment"
  mutate(Treatment=paste(block,plot,sep="_"))%>%
  #Add Total_Cover data into the Relative_Cover data sheet
  left_join(Total_Cover_2023_FK)%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(aerial_Relative_Cover=(aerial_cover/Total_Cover_Aerial)*100,basal_Relative_Cover=(basal_cover/Total_Cover_Basal)*100) %>%
  mutate(year=2023)  %>% 
  rename(species="genus_species") %>% 
  dplyr::select(year,site,plot,species,aerial_Relative_Cover,basal_Relative_Cover)

Relative_Cover_2023_FK<-gather(Relative_Cover_2023_FK_1, "aerial_basal","Relative_Cover",5:6) %>% 
  mutate(aerial_basal=ifelse(aerial_basal=="aerial_Relative_Cover","Aerial","Basal"))

#make plot a factor not an integer
Relative_Cover_2023_FK$plot<-as.factor(Relative_Cover_2023_FK$plot)

#### TB - 2018 - Relative Cover ####
#Create Long dataframe from wide dataframe
Long_Cov_2018_TB<-gather(TB_SpComp_2018,key="species","cover",18:115) %>% 
  dplyr::select(year,site,plot,aerial_basal,added_total_excel,species,cover) %>% 
  filter(!species %in% c("STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2018_TB<-Long_Cov_2018_TB%>%
  group_by(year, site, plot, aerial_basal) %>% 
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,aerial_basal,Relative_Cover) %>% 
  ungroup()
#make plot a factor not an integer
Relative_Cover_2018_TB$plot<-as.factor(Relative_Cover_2018_TB$plot)

#### TB - 2019 - Relative Cover ####
#Create Long dataframe from wide dataframe
Long_Cov_2019_TB<-gather(TB_SpComp_2019 ,key="species","cover",18:115) %>% 
  dplyr::select(year,site,plot,aerial_basal,added_total_excel,species,cover) %>% 
  filter(!species %in% c("STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0) %>% 
  #removing typo -- this should have been a zero
  filter(cover!=".")

Long_Cov_2019_TB$cover<-as.numeric(Long_Cov_2019_TB$cover)

#Calculate Relative Cover
Relative_Cover_2019_TB<-Long_Cov_2019_TB%>%
  group_by(year, site, plot, aerial_basal) %>% 
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,aerial_basal,Relative_Cover) %>% 
  ungroup()

#make plot a factor not an integer
Relative_Cover_2019_TB$plot<-as.factor(Relative_Cover_2019_TB$plot)

#### TB - 2020 - Relative Cover ####
#Create Long dataframe from wide dataframe
Long_Cov_2020_TB<-gather(TB_SpComp_2020,key="species","cover",18:115) %>% 
  dplyr::select(year,site,plot,aerial_basal,added_total_excel,species,cover) %>% 
  filter(!species %in% c("STANDING.DEADArtemisia.tridentata","STANDING.DEAD.Bromus.arvensis","STANDING.DEAD.Bromus.tectorum","STANDING.DEAD.Logfia.arvensis","STANDING.DEAD.Pascopyrum.smithii")) %>% 
  na.omit(cover) %>% 
  filter(cover!=0) %>% 
  filter(cover!="") %>% 
  mutate(cover=ifelse(cover=="<0.5",0.25,cover))

Long_Cov_2020_TB$cover<-as.numeric(Long_Cov_2020_TB$cover)

#Calculate Relative Cover
Relative_Cover_2020_TB<-Long_Cov_2020_TB%>%
  group_by(year, site, plot, aerial_basal) %>% 
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,aerial_basal,Relative_Cover) %>% 
  ungroup()

#make plot a factor not an integer
Relative_Cover_2020_TB$plot<-as.factor(Relative_Cover_2020_TB$plot)

#### TB - 2021 - Relative Cover ####
#Create Long dataframe from wide dataframe
Long_Cov_2021_TB<-gather(TB_SpComp_2021,key="species","cover",21:81) %>% 
  dplyr::select(year,site,plot,aerial_basal,added_total_excel,species,cover) %>%  
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2021_TB<-Long_Cov_2021_TB%>%
  group_by(year, site, plot, aerial_basal) %>% 
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,aerial_basal,Relative_Cover) %>% 
  mutate(aerial_basal=ifelse(aerial_basal=="aerial","Aerial","Basal")) %>% 
  ungroup()

#make plot a factor not an integer
Relative_Cover_2021_TB$plot<-as.factor(Relative_Cover_2021_TB$plot)

#### TB - 2022 - Relative Cover ####
#Create Long dataframe from wide dataframe
Long_Cov_2022_TB<-gather(TB_SpComp_2022,key="species","cover",18:85) %>% 
  dplyr::select(year,site,plot,aerial_basal,added_total_excel,species,cover) %>% 
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_2022_TB<-Long_Cov_2022_TB%>%
  group_by(year, site, plot, aerial_basal) %>% 
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(year,site,plot,species,aerial_basal,Relative_Cover) %>% 
  mutate(aerial_basal=ifelse(aerial_basal=="aerial","Aerial","Basal")) %>% 
  ungroup()

#make plot a factor not an integer
Relative_Cover_2022_TB$plot<-as.factor(Relative_Cover_2022_TB$plot)

#### Merge Relative Cover Data Frames Together ####
Species_Comp_RelCov_All<- Relative_Cover_2018_TB %>% 
  rbind(Relative_Cover_2019_TB) %>% 
  rbind(Relative_Cover_2020_TB) %>% 
  rbind(Relative_Cover_2021_TB) %>% 
  rbind(Relative_Cover_2022_TB) %>% 
  rbind(Relative_Cover_2018_FK) %>% 
  rbind(Relative_Cover_2019_FK) %>% 
  rbind(Relative_Cover_2020_FK) %>% 
  rbind(Relative_Cover_2021_FK) %>% 
  rbind(Relative_Cover_2022_FK) %>% 
  rbind(Relative_Cover_2023_FK) 

#### Clean Species Names Up to Match ####
Species_Comp_RelCov_All$species <- gsub("_",".",Species_Comp_RelCov_All$species)

Species_Comp_RelCov_Clean<-Species_Comp_RelCov_All %>% 
  #change species codes to full species names
  mutate(Genus_Species_1=ifelse(species=="ALDE","Alyssum.desertorum",
                         ifelse(species=="ALTE","Allium.textile",
                         ifelse(species=="ANOC","Androsace.occidentalis",
                         ifelse(species=="ANPA","Antennaria.parvifolia",
                         ifelse(species=="ARCA","Artemisia.cana",
                         ifelse(species=="ARDR","Artemisia.dracunculus",
                         ifelse(species=="ARFR","Artemisia.frigida",
                         ifelse(species=="ARPU","Aristida.purpurea",
                         ifelse(species=="ARTR","Artemisia.tridentata",
                         ifelse(species=="ASBI","Astragalus.bisulcatus",
                         ifelse(species=="ASGR","Astragalus.gracilis",
                         ifelse(species=="ASPU","Astragalus.purshii",
                         ifelse(species=="BODA","Bouteloua.dactyloides",
                         ifelse(species=="BOGR" ,"Bouteloua.gracilis",
                         ifelse(species=="BRAR","Bromus.arvensis",
                         ifelse(species=="BRTE","Bromus.tectorum",
                         ifelse(species=="CADU","Carex.duriuscula",
                         ifelse(species=="CAFI","Carex.filifolia",
                         ifelse(species=="CAMI","Camelina.microcarpa",
                         ifelse(species=="CHPR","Chenopudium.pratericola",
                         ifelse(species=="CIUN","Cirsium.undulatum",
                         ifelse(species=="COCA","Conyza.canadensis",
                         ifelse(species=="COLI","Collomia.linearis",
                         ifelse(species=="COVI","Coryphantha.viviparus",
                         ifelse(species=="DEPI","Descurainia.pinnata",
                         ifelse(species=="ERHO","Eremogone.hookeri",
   ifelse(species=="GUSA","Gutierrezia.sarothrae",
    ifelse(species=="HECO","Hesperostipa.comata",
     ifelse(species=="VIAM","Vicia.americana",
      ifelse(species=="VUOC","Vulpia.octoflora",
       ifelse(species=="KOMA","Koeleria.macrantha",
        ifelse(species=="LOAR","Logfia.arvensis",
   ifelse(species=="LYJU","Lygodesmia.juncea",
    ifelse(species=="MUDI","Musineon.divaricatum",
     ifelse(species=="NAVI","Nassella.viridula",
      ifelse(species== "LOFO","Lomatium.foeniculaceum",
       ifelse(species=="DRRE","Draba.reptans",
        ifelse(species=="ANPA","Antennaria.parvifolia",
         ifelse(species=="ERCA.","Erigeron.canus",
    ifelse(species=="ERPU","Erigeron.pumilus",
     ifelse(species=="ERPU.","Erigeron.pumilus",
      ifelse(species=="HEHI","Hedeoma.hispida",
       ifelse(species=="LEDE","Lepidium.densiflorum",
        ifelse(species=="LIIN","Lithospermum.incisum",
         ifelse(species=="LIPU","Liatris.punctata",
          ifelse(species=="MODI","Musineon.divaricatum",
     ifelse(species=="NOCU","Nothocalais.cuspidata",
      ifelse(species=="PEES","Pediomelum.esculentum",
       ifelse(species=="PIOP","Picradeniopsis.oppositifolia",
        species)))))))))))))))))))))))))))))))))))))))))))))))))) %>% 
  mutate(Genus_Species_2=ifelse(Genus_Species_1=="POAV","Polygonum.aviculare",
        ifelse(Genus_Species_1=="VEPE","Veronica.peregrina",
         ifelse(Genus_Species_1=="ZIVE","Zigadenus.venenosus",
          ifelse(Genus_Species_1=="LIRI","Linum.rigidum",
     ifelse(Genus_Species_1=="MAGR","Machaeranthera.grindelioides",
      ifelse(Genus_Species_1=="PEAL","Penstemon.albidus",
       ifelse(Genus_Species_1=="SPCR","Sporobolus.cryptandrus",
        ifelse(Genus_Species_1=="ERDI","Erigeron.divergens.ochroleucus",
         ifelse(Genus_Species_1=="GUSA.1","Gutierrezia.sarothrae",
          ifelse(Genus_Species_1=="LEMO","Leucocrinum.montanum",
           ifelse(Genus_Species_1=="LOFO.1","Lomatium.foeniculaceum",
      ifelse(Genus_Species_1=="SCLA","Scorzonera.laciniata",
       ifelse(Genus_Species_1=="SCPA","Schedonnardus.paniculatus",
        ifelse(Genus_Species_1=="TROC","Tradescantia.occidentalis",
         ifelse(Genus_Species_1=="VINU","Viola.nuttallii",
          ifelse(Genus_Species_1=="PEAN","Penstamom.angus",
           ifelse(Genus_Species_1=="PHLO","Phlox.longifoli",
            ifelse(Genus_Species_1=="CRMI","Cryptans.minima",
 ifelse(Genus_Species_1=="DRNE","Draba.nemorosa",
  ifelse(Genus_Species_1=="MILI","Mirabilis.linearis",
   ifelse(Genus_Species_1=="OEAL","Oenothera.albicaulis",
    ifelse(Genus_Species_1=="OOMU","Oonopsis.multicaulis",
     ifelse(Genus_Species_1=="POSE","Poa.secunda",
      ifelse(Genus_Species_1=="PSTE","Psoralidium.tenuiflorum",
       ifelse(Genus_Species_1=="SPCO","Sphaeralcea.coccinea",
  ifelse(Genus_Species_1=="TRDU","Tragopogon.dubius",
   ifelse(Genus_Species_1=="TAOF","Taraxacum.officinale",
    ifelse(Genus_Species_1=="OESU","Oenothera.suffrutescens",
     ifelse(Genus_Species_1=="OPPO","Opuntia.polyacantha",
      ifelse(Genus_Species_1=="PASM","Pascopyrum.smithii",
       ifelse(Genus_Species_1=="PLPA","Plantago.patagonica",
        ifelse(Genus_Species_1=="PHHO","Phlox.hoodii",
   ifelse(Genus_Species_1=="ZIDI","Zigadenus.venenosus",
    ifelse(species=="ASCR","Astragalus.crassicarpus",
     Genus_Species_1))))))))))))))))))))))))))))))))))) %>% 
  #fix spelling errors
  mutate(Genus_Species_3=ifelse(Genus_Species_2=="Oenotherea.suffrutescens","Oenothera.suffrutescens",
       ifelse(Genus_Species_2=="Opuntia.polycantha","Opuntia.polyacantha",
       ifelse(Genus_Species_2=="Pediomelum.esculenta","Pediomelum.esculentum",
       ifelse(Genus_Species_2=="pediomelum.esculentum","Pediomelum.esculentum", 
       ifelse(Genus_Species_2=="Allysum.desetorum","Alyssum.desertorum", 
       ifelse(Genus_Species_2=="Carex.durescula","Carex.duriuscula",
       ifelse(Genus_Species_2=="conyza.canadensis","Conyza.canadensis",
        ifelse(Genus_Species_2=="Coryphanthus_vivipara","Coryphantha.viviparus",
         ifelse(Genus_Species_2=="Liatrus_punctata","Liatris.punctata",
          ifelse(Genus_Species_2=="oenothera_suffruticosa","Oenothera.suffrutescens",
           ifelse(Genus_Species_2=="Coryphantha.viviparus","Coryphantha.vivipara",
      ifelse(Genus_Species_2=="Oenotherea.suffrutescens.1","Oenothera.suffrutescens",
       ifelse(Genus_Species_2=="Liatrus.punctata","Liatris.punctata",
        ifelse(Genus_Species_2=="oenothera.suffruticosa","Oenothera.suffrutescens",
         ifelse(Genus_Species_2=="Oenothera.suffruticosa","Oenothera.suffrutescens",
          Genus_Species_2)))))))))))))))) %>% 
  #Give unknowns more cohesive name
  mutate(Genus_Species_4=ifelse(Genus_Species_3=="Antennaria.KW.pic.unknown","Antennaria.UNKWN1",
      ifelse(Genus_Species_3=="Antennaria.spp.unknown","Antennaria.UNKWN2",
      ifelse(Genus_Species_3=="Artemisia.look.alike.no.smell.KW.pic.unknown","Artemisia.UNKWN3",
      ifelse(Genus_Species_3=="ASER.Like.Woody","ASER.UNKWN4",
      ifelse(Genus_Species_3=="Astragalus.KW.pic.unknown","Astragalus.UNKWN5",
      ifelse(Genus_Species_3=="basal.aster.KW.pic.unknown","BasalAster_UNKWN6",
      ifelse(Genus_Species_3=="basal.rosette","BasalRosette_UNKWN7",
      ifelse(Genus_Species_3=="Basal.rosette","BasalRosette_UNKWN8",
      ifelse(Genus_Species_3=="basal.rosette.2020.KW.pic.unknown","BasalRosette_UNKWN9",
      ifelse(Genus_Species_3=="Cirsium.sp.","CirsiumSp.UNKWN10",
      ifelse(Genus_Species_3=="CIUN.CIOC.","CIUNCIOC.UNKWN11",
      ifelse(Genus_Species_3=="CRCE.LELU.Penstemon.sp","CRCELELUPenstemon.UNKWN12",
      ifelse(Genus_Species_3=="dead.mustard.unk","DeadMustard.UNKWN13",
      ifelse(Genus_Species_3=="Erigeron.divergens.ochroleucus","Erigeron.divergensOchroleucus",
      ifelse(Genus_Species_3=="flat.spikelet.panicle.KW.pic.unknown","FlatSpikeletPanicle.UNKWN14",
      ifelse(Genus_Species_3=="Flesur.linear.leaves.in.bunch..KW.pic.","FlesurLinearLeavesBunch.UNKWN15",
      ifelse(Genus_Species_3=="lanceolate.KW.pic.unknown","Lanceolate.UNKWN16",
      ifelse(Genus_Species_3=="Linear.leaf.hairy.red.stem.KWpic." ,"LinearLeafHairyRedStem.UNKWN16",
      ifelse(Genus_Species_3=="Lithospermum.KW.pic.unknown" ,"Lithospermum.UNKWN17",
      ifelse(Genus_Species_3=="long.pods.no.leaves.KW.pic.unknown", "LongPodsNoLeaves.UNKWN18",
      ifelse(Genus_Species_3=="Lygo.deomia.1","Lygodesmia_UNKNWN19",
      ifelse(Genus_Species_3=="Lygo.deomia","Lygodesmia_UNKNWN19",
      ifelse(Genus_Species_3=="oenothera?.basal.rossetta","OenotheraBasalRosetta.UNKNWN20",
      ifelse(Genus_Species_3=="oenothera?.basal.rossette","OenotheraBasalRosetta.UNKNWN20",
      ifelse(Genus_Species_3=="Oenothera.","Oenothera.UNKNWN21",
      ifelse(Genus_Species_3=="Oenothera.waxy.leaves","OenotheraWaxyLeaves.UNKNWN22",
      ifelse(Genus_Species_3=="Oneothera.n.", "Oenothera.UNKNWN23",
      ifelse(Genus_Species_3=="PEAL.LP.pic.","PEAL.UNKNWN24",
      ifelse(Genus_Species_3=="Penstemon.sp.","Penstemon.UNKWN25",
      ifelse(Genus_Species_3=="Poa.diseased.Kwpic.","Poa.UNKWN26", 
      ifelse(Genus_Species_3=="Pointy.petals..Rhear.leaves", "PointyPetalRhearLeaves.UNKWN27",
      ifelse(Genus_Species_3=="silver.stuff.unk3","SilverStuff.UNKWN28",
      ifelse(Genus_Species_3=="Skinny.leaf.fuzzy.bottom","SkinnyLeafFuzzyBottom.UNKWN29",  
      ifelse(Genus_Species_3=="Sponge.leaf..KW.pic....FRUN.PIOP.46......" ,"SpongeLeafFRUNPIOO.UNKWN30",
      ifelse(Genus_Species_3=="Unk..3.Basal.Rosette","BasalRosette.UNKWN31",
      ifelse(Genus_Species_3=="Unk..3.soft.point.leaf.KW.photo" ,"SoftPointLeaf.UNKWN32",
      ifelse(Genus_Species_3=="Unk.baby.forb.opp.","BabyForbOPP.UNKWN33",
      ifelse(Genus_Species_3=="Unkn..10.small.linear.leaf.KW.photo","SmallLinear.UNKWN34",
      ifelse(Genus_Species_3=="UNKN8.basal.rosette.lancroiati","BasalRosetteLancroiati.UNKWN35",
      ifelse(Genus_Species_3=="Unknown..7.baby.guara.","BabyGuara.UNKWN36",
      ifelse(Genus_Species_3=="Unknown1.2021.no.sp.name.datasheet" ,"NoName.UNKWN37",
      ifelse(Genus_Species_3=="CHLE","CHLE.UNKWN38",
      ifelse(Genus_Species_3=="DECA","Dalea.candida",
      ifelse(Genus_Species_3=="Chamaesyce.nutans","Oenothera.nutans",
      ifelse(Genus_Species_3=="Pediomelum.esculentum","Psoralea.esculenta",
      ifelse(Genus_Species_3=="Phlox.longifoli","Phlox.longifolia",
      ifelse(Genus_Species_3=="Penstamom.angus","Penstemon.angustifolius",
      ifelse(Genus_Species_3=="Oenothera.suffrutescens","Oenotherea.suffrutescens",
      ifelse(Genus_Species_3=="Oenothera.nutans","Oenothera.nuttallii",
      Genus_Species_3)))))))))))))))))))))))))))))))))))))))))))))))))) %>% 
  mutate(Genus_Species=ifelse(Genus_Species_4=="Cryptans.minima","Cryptantha.minima",   
     ifelse(Genus_Species_4=="Chenopudium.pratericola","Chenopodium.pratericola",
     ifelse(Genus_Species_4=="Coryphanthus.vivipara","Coryphantha.vivipara",
     Genus_Species_4)))) %>% 
  dplyr::select(year,site,plot,aerial_basal,Genus_Species,Relative_Cover) %>% 
  unique()

#write CSV to save Species_Comp_RelCov_Clean
write.csv(Species_Comp_RelCov_Clean,"Species_Comp_RelCov_Clean.csv")

#### Match Functional Groups
Functional_Groups<-read.csv("FunctionalGroups.csv")

#Merge with Relative Cover data
RelCov_FunctionalGroups<-Species_Comp_RelCov_Clean %>% 
  full_join(Functional_Groups, relationship="many-to-many")

#write CSV to save RelCov_FunctionalGroups
write.csv(RelCov_FunctionalGroups,"RelCov_FunctionalGroups.csv")

#### Absolute Cover ####
FK_2018 <- Long_Cov_2018_FK %>% 
  filter(aerial_basal=="Aerial") %>% 
  rename(aerial_cover=cover) %>%  
  dplyr::select(year,site,plot,species,aerial_cover)

FK_2019 <- Species_Cover_2019_FK %>% 
  mutate(year=2019) %>% 
  rename(species=genus_species) %>%
  dplyr::select(year,site,plot,species,aerial_cover)

FK_2020 <-Species_Cover_2020_FK %>% 
  mutate(year=2020) %>% 
  rename(species=genus_species) %>%
  dplyr::select(year,site,plot,species,aerial_cover)

FK_2021 <- Long_Cov_2021_FK %>% 
  filter(aerial_basal=="Aerial") %>% 
  rename(aerial_cover=cover) %>% 
  dplyr::select(year,site,plot,species,aerial_cover)

FK_2022 <- Long_Cov_2022_FK %>% 
  filter(aerial_basal=="Aerial") %>% 
  rename(aerial_cover=cover) %>% 
  dplyr::select(year,site,plot,species,aerial_cover)

FK_2023 <- Species_Cover_2023_FK %>% 
  mutate(year=2023) %>% 
  rename(species=genus_species) %>%
  dplyr::select(year,site,plot,species,aerial_cover)

TB_2018 <- Long_Cov_2018_TB %>% 
  filter(aerial_basal=="Aerial") %>% 
  rename(aerial_cover=cover) %>% 
  dplyr::select(year,site,plot,species,aerial_cover)

TB_2019 <- Long_Cov_2019_TB %>% 
  filter(aerial_basal=="Aerial") %>% 
  rename(aerial_cover=cover) %>% 
  dplyr::select(year,site,plot,species,aerial_cover)

TB_2020 <-Long_Cov_2020_TB %>% 
  filter(aerial_basal=="Aerial") %>% 
  rename(aerial_cover=cover) %>% 
  dplyr::select(year,site,plot,species,aerial_cover)

TB_2021 <-Long_Cov_2021_TB %>% 
  filter(aerial_basal=="Aerial") %>% 
  rename(aerial_cover=cover) %>% 
  dplyr::select(year,site,plot,species,aerial_cover)

TB_2022 <-Long_Cov_2022_TB %>% 
  filter(aerial_basal=="Aerial") %>% 
  rename(aerial_cover=cover) %>% 
  dplyr::select(year,site,plot,species,aerial_cover)

#### Merge Absolute Cover Data Frames Together ####
Absolute_Species_Comp<- FK_2018 %>% 
  rbind(FK_2019) %>% 
  rbind(FK_2020) %>% 
  rbind(FK_2021) %>% 
  rbind(FK_2022) %>% 
  rbind(FK_2023) %>% 
  rbind(TB_2018) %>% 
  rbind(TB_2019) %>% 
  rbind(TB_2020) %>% 
  rbind(TB_2021) %>% 
  rbind(TB_2022)   


#### Clean Species Names Up to Match ####
Absolute_Species_Comp$species <- gsub("_",".",Absolute_Species_Comp$species)

Absolute_Species_Comp_Clean<-Absolute_Species_Comp %>% 
  #change species codes to full species names
  mutate(Genus_Species_1=ifelse(species=="ALDE","Alyssum.desertorum",
                                ifelse(species=="ALTE","Allium.textile",
                                       ifelse(species=="ANOC","Androsace.occidentalis",
                                              ifelse(species=="ANPA","Antennaria.parvifolia",
                                                     ifelse(species=="ARCA","Artemisia.cana",
                                                            ifelse(species=="ARDR","Artemisia.dracunculus",
                                                                   ifelse(species=="ARFR","Artemisia.frigida",
                                                                          ifelse(species=="ARPU","Aristida.purpurea",
                                                                                 ifelse(species=="ARTR","Artemisia.tridentata",
                                                                                        ifelse(species=="ASBI","Astragalus.bisulcatus",
                                                                                               ifelse(species=="ASGR","Astragalus.gracilis",
                                                                                                      ifelse(species=="ASPU","Astragalus.purshii",
                                                                                                             ifelse(species=="BODA","Bouteloua.dactyloides",
                                                                                                                    ifelse(species=="BOGR" ,"Bouteloua.gracilis",
                                                                                                                           ifelse(species=="BRAR","Bromus.arvensis",
                                                                                                                                  ifelse(species=="BRTE","Bromus.tectorum",
                                                                                                                                         ifelse(species=="CADU","Carex.duriuscula",
                                                                                                                                                ifelse(species=="CAFI","Carex.filifolia",
                                                                                                                                                       ifelse(species=="CAMI","Camelina.microcarpa",
                                                                                                                                                              ifelse(species=="CHPR","Chenopudium.pratericola",
                                                                                                                                                                     ifelse(species=="CIUN","Cirsium.undulatum",
                                                                                                                                                                            ifelse(species=="COCA","Conyza.canadensis",
                                                                                                                                                                                   ifelse(species=="COLI","Collomia.linearis",
                                                                                                                                                                                          ifelse(species=="COVI","Coryphantha.viviparus",
                                                                                                                                                                                                 ifelse(species=="DEPI","Descurainia.pinnata",
                                                                                                                                                                                                        ifelse(species=="ERHO","Eremogone.hookeri",
                                                                                                                                                                                                               ifelse(species=="GUSA","Gutierrezia.sarothrae",
                                                                                                                                                                                                                      ifelse(species=="HECO","Hesperostipa.comata",
                                                                                                                                                                                                                             ifelse(species=="VIAM","Vicia.americana",
                                                                                                                                                                                                                                    ifelse(species=="VUOC","Vulpia.octoflora",
                                                                                                                                                                                                                                           ifelse(species=="KOMA","Koeleria.macrantha",
                                                                                                                                                                                                                                                  ifelse(species=="LOAR","Logfia.arvensis",
                                                                                                                                                                                                                                                         ifelse(species=="LYJU","Lygodesmia.juncea",
                                                                                                                                                                                                                                                                ifelse(species=="MUDI","Musineon.divaricatum",
                                                                                                                                                                                                                                                                       ifelse(species=="NAVI","Nassella.viridula",
                                                                                                                                                                                                                                                                              ifelse(species== "LOFO","Lomatium.foeniculaceum",
                                                                                                                                                                                                                                                                                     ifelse(species=="DRRE","Draba.reptans",
                                                                                                                                                                                                                                                                                            ifelse(species=="ANPA","Antennaria.parvifolia",
                                                                                                                                                                                                                                                                                                   ifelse(species=="ERCA.","Erigeron.canus",
                                                                                                                                                                                                                                                                                                          ifelse(species=="ERPU","Erigeron.pumilus",
                                                                                                                                                                                                                                                                                                                 ifelse(species=="ERPU.","Erigeron.pumilus",
                                                                                                                                                                                                                                                                                                                        ifelse(species=="HEHI","Hedeoma.hispida",
                                                                                                                                                                                                                                                                                                                               ifelse(species=="LEDE","Lepidium.densiflorum",
                                                                                                                                                                                                                                                                                                                                      ifelse(species=="LIIN","Lithospermum.incisum",
                                                                                                                                                                                                                                                                                                                                             ifelse(species=="LIPU","Liatris.punctata",
                                                                                                                                                                                                                                                                                                                                                    ifelse(species=="MODI","Musineon.divaricatum",
                                                                                                                                                                                                                                                                                                                                                           ifelse(species=="NOCU","Nothocalais.cuspidata",
                                                                                                                                                                                                                                                                                                                                                                  ifelse(species=="PEES","Pediomelum.esculentum",
                                                                                                                                                                                                                                                                                                                                                                         ifelse(species=="PIOP","Picradeniopsis.oppositifolia",
                                                                                                                                                                                                                                                                                                                                                                                species)))))))))))))))))))))))))))))))))))))))))))))))))) %>% 
  mutate(Genus_Species_2=ifelse(Genus_Species_1=="POAV","Polygonum.aviculare",
                                ifelse(Genus_Species_1=="VEPE","Veronica.peregrina",
                                       ifelse(Genus_Species_1=="ZIVE","Zigadenus.venenosus",
                                              ifelse(Genus_Species_1=="LIRI","Linum.rigidum",
                                                     ifelse(Genus_Species_1=="MAGR","Machaeranthera.grindelioides",
                                                            ifelse(Genus_Species_1=="PEAL","Penstemon.albidus",
                                                                   ifelse(Genus_Species_1=="SPCR","Sporobolus.cryptandrus",
                                                                          ifelse(Genus_Species_1=="ERDI","Erigeron.divergens.ochroleucus",
                                                                                 ifelse(Genus_Species_1=="GUSA.1","Gutierrezia.sarothrae",
                                                                                        ifelse(Genus_Species_1=="LEMO","Leucocrinum.montanum",
                                                                                               ifelse(Genus_Species_1=="LOFO.1","Lomatium.foeniculaceum",
                                                                                                      ifelse(Genus_Species_1=="SCLA","Scorzonera.laciniata",
                                                                                                             ifelse(Genus_Species_1=="SCPA","Schedonnardus.paniculatus",
                                                                                                                    ifelse(Genus_Species_1=="TROC","Tradescantia.occidentalis",
                                                                                                                           ifelse(Genus_Species_1=="VINU","Viola.nuttallii",
                                                                                                                                  ifelse(Genus_Species_1=="PEAN","Penstamom.angus",
                                                                                                                                         ifelse(Genus_Species_1=="PHLO","Phlox.longifoli",
                                                                                                                                                ifelse(Genus_Species_1=="CRMI","Cryptans.minima",
                                                                                                                                                       ifelse(Genus_Species_1=="DRNE","Draba.nemorosa",
                                                                                                                                                              ifelse(Genus_Species_1=="MILI","Mirabilis.linearis",
                                                                                                                                                                     ifelse(Genus_Species_1=="OEAL","Oenothera.albicaulis",
                                                                                                                                                                            ifelse(Genus_Species_1=="OOMU","Oonopsis.multicaulis",
                                                                                                                                                                                   ifelse(Genus_Species_1=="POSE","Poa.secunda",
                                                                                                                                                                                          ifelse(Genus_Species_1=="PSTE","Psoralidium.tenuiflorum",
                                                                                                                                                                                                 ifelse(Genus_Species_1=="SPCO","Sphaeralcea.coccinea",
                                                                                                                                                                                                        ifelse(Genus_Species_1=="TRDU","Tragopogon.dubius",
                                                                                                                                                                                                               ifelse(Genus_Species_1=="TAOF","Taraxacum.officinale",
                                                                                                                                                                                                                      ifelse(Genus_Species_1=="OESU","Oenothera.suffrutescens",
                                                                                                                                                                                                                             ifelse(Genus_Species_1=="OPPO","Opuntia.polyacantha",
                                                                                                                                                                                                                                    ifelse(Genus_Species_1=="PASM","Pascopyrum.smithii",
                                                                                                                                                                                                                                           ifelse(Genus_Species_1=="PLPA","Plantago.patagonica",
                                                                                                                                                                                                                                                  ifelse(Genus_Species_1=="PHHO","Phlox.hoodii",
                                                                                                                                                                                                                                                         ifelse(Genus_Species_1=="ZIDI","Zigadenus.venenosus",
                                                                                                                                                                                                                                                                ifelse(species=="ASCR","Astragalus.crassicarpus",
                                                                                                                                                                                                                                                                       Genus_Species_1))))))))))))))))))))))))))))))))))) %>% 
  #fix spelling errors
  mutate(Genus_Species_3=ifelse(Genus_Species_2=="Oenotherea.suffrutescens","Oenothera.suffrutescens",
                                ifelse(Genus_Species_2=="Opuntia.polycantha","Opuntia.polyacantha",
                                       ifelse(Genus_Species_2=="Pediomelum.esculenta","Pediomelum.esculentum",
                                              ifelse(Genus_Species_2=="pediomelum.esculentum","Pediomelum.esculentum", 
                                                     ifelse(Genus_Species_2=="Allysum.desetorum","Alyssum.desertorum", 
                                                            ifelse(Genus_Species_2=="Carex.durescula","Carex.duriuscula",
                                                                   ifelse(Genus_Species_2=="conyza.canadensis","Conyza.canadensis",
                                                                          ifelse(Genus_Species_2=="Coryphanthus_vivipara","Coryphantha.viviparus",
                                                                                 ifelse(Genus_Species_2=="Liatrus_punctata","Liatris.punctata",
                                                                                        ifelse(Genus_Species_2=="oenothera_suffruticosa","Oenothera.suffrutescens",
                                                                                               ifelse(Genus_Species_2=="Coryphantha.viviparus","Coryphantha.vivipara",
                                                                                                      ifelse(Genus_Species_2=="Oenotherea.suffrutescens.1","Oenothera.suffrutescens",
                                                                                                             ifelse(Genus_Species_2=="Liatrus.punctata","Liatris.punctata",
                                                                                                                    ifelse(Genus_Species_2=="oenothera.suffruticosa","Oenothera.suffrutescens",
                                                                                                                           ifelse(Genus_Species_2=="Oenothera.suffruticosa","Oenothera.suffrutescens",
                                                                                                                                  Genus_Species_2)))))))))))))))) %>% 
  #Give unknowns more cohesive name
  mutate(Genus_Species_4=ifelse(Genus_Species_3=="Antennaria.KW.pic.unknown","Antennaria.UNKWN1",
                                ifelse(Genus_Species_3=="Antennaria.spp.unknown","Antennaria.UNKWN2",
                                       ifelse(Genus_Species_3=="Artemisia.look.alike.no.smell.KW.pic.unknown","Artemisia.UNKWN3",
                                              ifelse(Genus_Species_3=="ASER.Like.Woody","ASER.UNKWN4",
                                                     ifelse(Genus_Species_3=="Astragalus.KW.pic.unknown","Astragalus.UNKWN5",
                                                            ifelse(Genus_Species_3=="basal.aster.KW.pic.unknown","BasalAster_UNKWN6",
                                                                   ifelse(Genus_Species_3=="basal.rosette","BasalRosette_UNKWN7",
                                                                          ifelse(Genus_Species_3=="Basal.rosette","BasalRosette_UNKWN8",
                                                                                 ifelse(Genus_Species_3=="basal.rosette.2020.KW.pic.unknown","BasalRosette_UNKWN9",
                                                                                        ifelse(Genus_Species_3=="Cirsium.sp.","CirsiumSp.UNKWN10",
                                                                                               ifelse(Genus_Species_3=="CIUN.CIOC.","CIUNCIOC.UNKWN11",
                                                                                                      ifelse(Genus_Species_3=="CRCE.LELU.Penstemon.sp","CRCELELUPenstemon.UNKWN12",
                                                                                                             ifelse(Genus_Species_3=="dead.mustard.unk","DeadMustard.UNKWN13",
                                                                                                                    ifelse(Genus_Species_3=="Erigeron.divergens.ochroleucus","Erigeron.divergensOchroleucus",
                                                                                                                           ifelse(Genus_Species_3=="flat.spikelet.panicle.KW.pic.unknown","FlatSpikeletPanicle.UNKWN14",
                                                                                                                                  ifelse(Genus_Species_3=="Flesur.linear.leaves.in.bunch..KW.pic.","FlesurLinearLeavesBunch.UNKWN15",
                                                                                                                                         ifelse(Genus_Species_3=="lanceolate.KW.pic.unknown","Lanceolate.UNKWN16",
                                                                                                                                                ifelse(Genus_Species_3=="Linear.leaf.hairy.red.stem.KWpic." ,"LinearLeafHairyRedStem.UNKWN16",
                                                                                                                                                       ifelse(Genus_Species_3=="Lithospermum.KW.pic.unknown" ,"Lithospermum.UNKWN17",
                                                                                                                                                              ifelse(Genus_Species_3=="long.pods.no.leaves.KW.pic.unknown", "LongPodsNoLeaves.UNKWN18",
                                                                                                                                                                     ifelse(Genus_Species_3=="Lygo.deomia.1","Lygodesmia_UNKNWN19",
                                                                                                                                                                            ifelse(Genus_Species_3=="Lygo.deomia","Lygodesmia_UNKNWN19",
                                                                                                                                                                                   ifelse(Genus_Species_3=="oenothera?.basal.rossetta","OenotheraBasalRosetta.UNKNWN20",
                                                                                                                                                                                          ifelse(Genus_Species_3=="oenothera?.basal.rossette","OenotheraBasalRosetta.UNKNWN20",
                                                                                                                                                                                                 ifelse(Genus_Species_3=="Oenothera.","Oenothera.UNKNWN21",
                                                                                                                                                                                                        ifelse(Genus_Species_3=="Oenothera.waxy.leaves","OenotheraWaxyLeaves.UNKNWN22",
                                                                                                                                                                                                               ifelse(Genus_Species_3=="Oneothera.n.", "Oenothera.UNKNWN23",
                                                                                                                                                                                                                      ifelse(Genus_Species_3=="PEAL.LP.pic.","PEAL.UNKNWN24",
                                                                                                                                                                                                                             ifelse(Genus_Species_3=="Penstemon.sp.","Penstemon.UNKWN25",
                                                                                                                                                                                                                                    ifelse(Genus_Species_3=="Poa.diseased.Kwpic.","Poa.UNKWN26", 
                                                                                                                                                                                                                                           ifelse(Genus_Species_3=="Pointy.petals..Rhear.leaves", "PointyPetalRhearLeaves.UNKWN27",
                                                                                                                                                                                                                                                  ifelse(Genus_Species_3=="silver.stuff.unk3","SilverStuff.UNKWN28",
                                                                                                                                                                                                                                                         ifelse(Genus_Species_3=="Skinny.leaf.fuzzy.bottom","SkinnyLeafFuzzyBottom.UNKWN29",  
                                                                                                                                                                                                                                                                ifelse(Genus_Species_3=="Sponge.leaf..KW.pic....FRUN.PIOP.46......" ,"SpongeLeafFRUNPIOO.UNKWN30",
                                                                                                                                                                                                                                                                       ifelse(Genus_Species_3=="Unk..3.Basal.Rosette","BasalRosette.UNKWN31",
                                                                                                                                                                                                                                                                              ifelse(Genus_Species_3=="Unk..3.soft.point.leaf.KW.photo" ,"SoftPointLeaf.UNKWN32",
                                                                                                                                                                                                                                                                                     ifelse(Genus_Species_3=="Unk.baby.forb.opp.","BabyForbOPP.UNKWN33",
                                                                                                                                                                                                                                                                                            ifelse(Genus_Species_3=="Unkn..10.small.linear.leaf.KW.photo","SmallLinear.UNKWN34",
                                                                                                                                                                                                                                                                                                   ifelse(Genus_Species_3=="UNKN8.basal.rosette.lancroiati","BasalRosetteLancroiati.UNKWN35",
                                                                                                                                                                                                                                                                                                          ifelse(Genus_Species_3=="Unknown..7.baby.guara.","BabyGuara.UNKWN36",
                                                                                                                                                                                                                                                                                                                 ifelse(Genus_Species_3=="Unknown1.2021.no.sp.name.datasheet" ,"NoName.UNKWN37",
                                                                                                                                                                                                                                                                                                                        ifelse(Genus_Species_3=="CHLE","CHLE.UNKWN38",
                                                                                                                                                                                                                                                                                                                               ifelse(Genus_Species_3=="DECA","Dalea.candida",
                                                                                                                                                                                                                                                                                                                                      ifelse(Genus_Species_3=="Chamaesyce.nutans","Oenothera.nutans",
                                                                                                                                                                                                                                                                                                                                             ifelse(Genus_Species_3=="Pediomelum.esculentum","Psoralea.esculenta",
                                                                                                                                                                                                                                                                                                                                                    ifelse(Genus_Species_3=="Phlox.longifoli","Phlox.longifolia",
                                                                                                                                                                                                                                                                                                                                                           ifelse(Genus_Species_3=="Penstamom.angus","Penstemon.angustifolius",
                                                                                                                                                                                                                                                                                                                                                                  ifelse(Genus_Species_3=="Oenothera.suffrutescens","Oenotherea.suffrutescens",
                                                                                                                                                                                                                                                                                                                                                                         ifelse(Genus_Species_3=="Oenothera.nutans","Oenothera.nuttallii",
                                                                                                                                                                                                                                                                                                                                                                                Genus_Species_3)))))))))))))))))))))))))))))))))))))))))))))))))) %>% 
  mutate(Genus_Species=ifelse(Genus_Species_4=="Cryptans.minima","Cryptantha.minima",   
                              ifelse(Genus_Species_4=="Chenopudium.pratericola","Chenopodium.pratericola",
                                     ifelse(Genus_Species_4=="Coryphanthus.vivipara","Coryphantha.vivipara",
                                            Genus_Species_4)))) %>% 
  dplyr::select(year,site,plot,Genus_Species,aerial_cover) %>% 
  unique()


#write CSV to save Species_Comp_RelCov_Clean
write.csv(Absolute_Species_Comp_Clean,"Species_Comp_Absolute_Clean.csv")


#Merge with Relative Cover data
Absolute_FunctionalGroups<-Absolute_Species_Comp_Clean %>% 
  full_join(Functional_Groups, relationship="many-to-many")

#write CSV to save RelCov_FunctionalGroups
write.csv(Absolute_FunctionalGroups,"Absolute_FunctionalGroups.csv")
