##########################################################################################################
#Project: Plant Species Composition in MGP with Drought x Grazing 

##########################################################################################################

#### Load Libraries ####
library(tidyverse) 
#install.packages("codyn")
library(codyn)
library(olsrr)
library(car)
library(patchwork)
library(vegan)

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
             legend.text=element_text(size=40))

#### Read in Data ####
#Read in Species Comp Data and fix species names
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

#### FK - 2022 - Relative Cover
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

#### TB- 2018 - Relative Cover ####
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

#### TB- 2019 - Relative Cover ####
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

#### TB- 2020 - Relative Cover ####
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

#### TB- 2021 - Relative Cover ####
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
  rbind(Relative_Cover_2022_FK)

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
                        ifelse(Genus_Species_1=="PEAL","Pediomelum.esculentum",
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
                       ifelse(Genus_Species_3=="DECA","Daleac.andida",
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
  select(year,site,plot,aerial_basal,Genus_Species,Relative_Cover) %>% 
  unique()
             
                                                                    
#### Calculate Community Metrics ####
# uses codyn package and finds shannon's diversity 

#FK Diversity
Diversity_FK_Aerial <- community_diversity(df = subset(Species_Comp_RelCov_Clean, site == "FK" & aerial_basal=="Aerial"),
                                  time.var = "year",
                                  replicate.var = "plot",
                                  abundance.var = "Relative_Cover") %>% 
  mutate(site="FK")
Diversity_FK_Basal <- community_diversity(df = subset(Species_Comp_RelCov_Clean, site == "FK" & aerial_basal=="Basal"),
                                           time.var = "year",
                                           replicate.var = "plot",
                                           abundance.var = "Relative_Cover")  %>% 
  mutate(site="FK")
#TB Diversity
Diversity_TB_Aerial <- community_diversity(df = subset(Species_Comp_RelCov_Clean, site == "TB" & aerial_basal=="Aerial"),
                                           time.var = "year",
                                           replicate.var = "plot",
                                           abundance.var = "Relative_Cover")  %>% 
  mutate(site="TB")
Diversity_TB_Basal <- community_diversity(df = subset(Species_Comp_RelCov_Clean, site == "TB" & aerial_basal=="Basal"),
                                          time.var = "year",
                                          replicate.var = "plot",
                                          abundance.var = "Relative_Cover")%>% 
  mutate(site="TB") 
#Merge Site Data-  aerial
Diversity_Aerial<-Diversity_FK_Aerial %>% 
  full_join(Diversity_TB_Aerial)

#Merge Site Data - basal
Diversity_Basal<-Diversity_FK_Basal %>% 
  full_join(Diversity_TB_Basal)

#FK Community Structure
#FK Diversity
Structure_FK_Aerial <- community_structure(df = subset(Species_Comp_RelCov_Clean, site == "FK" & aerial_basal=="Aerial"),
                                           time.var = "year",
                                           replicate.var = "plot",
                                           abundance.var = "Relative_Cover",
                                           metric = "Evar")  %>% 
  mutate(site="FK")
Structure_FK_Basal <- community_structure(df = subset(Species_Comp_RelCov_Clean, site == "FK" & aerial_basal=="Basal"),
                                          time.var = "year",
                                          replicate.var = "plot",
                                          abundance.var = "Relative_Cover",
                                          metric = "Evar")   %>% 
  mutate(site="FK")
#TB Community Structure
Structure_TB_Aerial <- community_structure(df = subset(Species_Comp_RelCov_Clean, site == "TB" & aerial_basal=="Aerial"),
                                           time.var = "year",
                                           replicate.var = "plot",
                                           abundance.var = "Relative_Cover",
                                           metric = "Evar")   %>% 
  mutate(site="TB")
Structure_TB_Basal <- community_structure(df = subset(Species_Comp_RelCov_Clean, site == "TB" & aerial_basal=="Basal"),
                                          time.var = "year",
                                          replicate.var = "plot",
                                          abundance.var = "Relative_Cover",
                                           metric = "Evar")  %>% 
  mutate(site="TB")   

#Merge Site Data-  aerial
Structure_Aerial<-Structure_FK_Aerial %>% 
  full_join(Structure_TB_Aerial)

#Merge Site Data - basal
Structure_Basal<-Structure_FK_Basal %>% 
  full_join(Structure_TB_Basal)

#join the datasets
CommunityMetrics_Aerial <- Diversity_Aerial %>%
  full_join(Structure_Aerial) %>% 
  full_join(plot_layoutK) %>%
  mutate(drought = ifelse(drought == 1, 0, ifelse(drought==2,0, drought))) %>%
  #create column that has all grazing treatments in it for a given year
  mutate(grazing_treatment_fig=ifelse(grazing_category=="MMMMM" &year==2020,"stable",ifelse(grazing_category=="HHMMM" &year==2020, "heavy",ifelse(grazing_category=="MLLMM" &year==2020, "stable",ifelse(year==2019,NA,grazing_treatment))))) %>% 
  #create a column for graphing grazing shannons,richness, and evar without 2019 data
  mutate(richness_fig=ifelse(year==2019,NA,richness)) %>% 
  mutate(Shannon_fig=ifelse(year==2019,NA,Shannon)) %>% 
  mutate(Evar_fig=ifelse(year==2019,NA,Evar)) %>% 
  mutate(livestock_util_2019 = as.factor(livestock_util_2019)) %>%
  mutate(livestock_util_2020 = as.factor(livestock_util_2020)) %>% 
  mutate(livestock_util_2021 = as.factor(livestock_util_2021)) %>% 
  mutate(grazing_treatment_fig = as.factor(grazing_treatment_fig)) %>%
  select(year,site,block,slope,plot,rainfall_reduction,grazing_treatment,grazing_treatment_fig, livestock_util_2019,richness,richness_fig,Shannon,Shannon_fig,Evar,Evar_fig)

CommunityMetrics_Basal <- Diversity_Basal %>%
  full_join(Structure_Basal) %>% 
  full_join(plot_layoutK) %>%
  mutate(drought = ifelse(drought == 1, 0, ifelse(drought==2,0, drought))) %>%
  #create column that has all grazing treatments in it for a given year
  mutate(grazing_treatment_fig=ifelse(grazing_category=="MMMMM" &year==2020,"stable",ifelse(grazing_category=="HHMMM" &year==2020, "heavy",ifelse(grazing_category=="MLLMM" &year==2020, "stable",ifelse(year==2019,NA,grazing_treatment))))) %>% 
  #create a column for graphing grazing shannons,richness, and evar without 2019 data
  mutate(richness_fig=ifelse(year==2019,NA,richness)) %>% 
  mutate(Shannon_fig=ifelse(year==2019,NA,Shannon)) %>% 
  mutate(Evar_fig=ifelse(year==2019,NA,Evar)) %>% 
  mutate(livestock_util_2019 = as.factor(livestock_util_2019)) %>%
  mutate(livestock_util_2020 = as.factor(livestock_util_2020)) %>% 
  mutate(livestock_util_2021 = as.factor(livestock_util_2021)) %>% 
  mutate(grazing_treatment_fig = as.factor(grazing_treatment_fig)) %>%
  select(year,site,block,slope,plot,rainfall_reduction,grazing_treatment,grazing_treatment_fig, livestock_util_2019,richness,richness_fig,Shannon,Shannon_fig,Evar,Evar_fig)

#### Check for Normality ####

####still need to check for homoscedacity but levene test can't be used with quantitative explanatory variables 

#### Normality: Fort Keogh Aerial + Basal - Richness ####
#FK - Aerial - Richness: 2018 
#non transformed data
Norm_FK_18_Richness_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "FK"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_Richness_Ar) 
ols_test_normality(Norm_FK_18_Richness_Ar) #normal

#FK - Aerial - Richness: 2019 
#non transformed data
Norm_FK_19_Richness_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "FK"), richness  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_Richness_Ar) 
ols_test_normality(Norm_FK_19_Richness_Ar) #normal

#FK - Aerial - Richness: 2020
#non transformed data
Norm_FK_20_Richness_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "FK"), richness  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_Richness_Ar) 
ols_test_normality(Norm_FK_20_Richness_Ar) #normal

#FK - Aerial - Richness: 2021
#non transformed data
Norm_FK_21_Richness_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "FK"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_Richness_Ar) 
ols_test_normality(Norm_FK_21_Richness_Ar) #normal

#FK - Aerial - Richness: 2022
#non transformed data
Norm_FK_22_Richness_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "FK"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_Richness_Ar) 
ols_test_normality(Norm_FK_22_Richness_Ar) #normal

#FK - Basal - Richness: 2018 
#non transformed data
Norm_FK_18_Richness_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2018 & site== "FK"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_Richness_Ba) 
ols_test_normality(Norm_FK_18_Richness_Ba) #normal

#FK - Basal - Richness: 2019 
#non transformed data
Norm_FK_19_Richness_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2019 & site== "FK"), richness  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_Richness_Ba) 
ols_test_normality(Norm_FK_19_Richness_Ba) #normal

#FK - Basal - Richness: 2020
#non transformed data
Norm_FK_20_Richness_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2020 & site== "FK"), richness  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_Richness_Ba) 
ols_test_normality(Norm_FK_20_Richness_Ba) #normal

#FK - Basal - Richness: 2021
#non transformed data
Norm_FK_21_Richness_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2021 & site== "FK"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_Richness_Ba) 
ols_test_normality(Norm_FK_21_Richness_Ba) #normal

#FK - Basal - Richness: 2022
#non transformed data
Norm_FK_22_Richness_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2022 & site== "FK"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_Richness_Ba) 
ols_test_normality(Norm_FK_22_Richness_Ba) #normal

#### Normality: Thunder Basin Aerial + Basal - Richness ####
#TB - Aerial - Richness: 2018 
#non transformed data
Norm_TB_18_Richness_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "TB"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_Richness_Ar) 
ols_test_normality(Norm_TB_18_Richness_Ar) #normal

#TB - Aerial - Richness: 2019 
#non transformed data
Norm_TB_19_Richness_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "TB"), richness  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_Richness_Ar) 
ols_test_normality(Norm_TB_19_Richness_Ar) #non transformed is best option

#TB - Aerial - Richness: 2020
#non transformed data
Norm_TB_20_Richness_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "TB"), richness  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_Richness_Ar) 
ols_test_normality(Norm_TB_20_Richness_Ar) #normal

#TB - Aerial - Richness: 2021
#non transformed data
Norm_TB_21_Richness_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "TB"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_Richness_Ar) 
ols_test_normality(Norm_TB_21_Richness_Ar) #normal

#TB - Aerial - Richness: 2022
#non transformed data
Norm_TB_22_Richness_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "TB"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_Richness_Ar) 
ols_test_normality(Norm_TB_22_Richness_Ar) #normal

#TB - Basal - Richness: 2018 
#non transformed data
Norm_TB_18_Richness_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2018 & site== "TB"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_Richness_Ba) 
ols_test_normality(Norm_TB_18_Richness_Ba) #normal

#TB - Basal - Richness: 2019 
#non transformed data
Norm_TB_19_Richness_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2019 & site== "TB"), richness  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_Richness_Ba) 
ols_test_normality(Norm_TB_19_Richness_Ba) #non transformeed is best option


#TB - Basal - Richness: 2020
#non transformed data
Norm_TB_20_Richness_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2020 & site== "TB"), richness  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_Richness_Ba) 
ols_test_normality(Norm_TB_20_Richness_Ba) #normal

#TB - Basal - Richness: 2021
#non transformed data
Norm_TB_21_Richness_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2021 & site== "TB"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_Richness_Ba) 
ols_test_normality(Norm_TB_21_Richness_Ba) #normal

#TB - Basal - Richness: 2022
#non transformed data
Norm_TB_22_Richness_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2022 & site== "TB"), richness  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_Richness_Ba) 
ols_test_normality(Norm_TB_22_Richness_Ba) #normal

#### Stats: Fort Keogh Aerial + Basal - Richness ####

#FK 2018 - checking drought and grazing
FK_18_Richness_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "FK"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Richness_Aerial, type = 3) #NS

#FK 2019 - just drought
FK_19_Richness_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "FK"), richness ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_Richness_Aerial, type = 3) #p=0.007433

#FK 2020 - droughtxgrazing
FK_20_Richness_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "FK"), richness ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_Richness_Aerial, type = 3) #drought (p=0.0662)

#FK 2021- droughtxgrazing
FK_21_Richness_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "FK"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_Richness_Aerial, type = 3) #drought (p=0.04036)

#FK 2022- droughtxgrazing
FK_22_Richness_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "FK"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_Richness_Aerial, type = 3) #NS

#Basal 
#FK 2018 - checking drought and grazing
FK_18_Richness_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2018 & site== "FK"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Richness_Basal, type = 3) #NS

#FK 2019 - just drought
FK_19_Richness_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2019 & site== "FK"), richness ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_Richness_Basal, type = 3) #p=0.007433

#FK 2020 - droughtxgrazing
FK_20_Richness_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2020 & site== "FK"), richness ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_Richness_Basal, type = 3) #drought (p=0.0662)

#FK 2021- droughtxgrazing
FK_21_Richness_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2021 & site== "FK"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_Richness_Basal, type = 3) #drought (p=0.04036)

#FK 2022- droughtxgrazing
FK_22_Richness_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2022 & site== "FK"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_Richness_Basal, type = 3) #NS

#### Stats: Thunder Basin Aerial + Basal - Richness  ####

#TB 2018 - checking drought and grazing
TB_18_Richness_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "TB"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_Richness_Aerial, type = 3) #NS

#TB 2019 - just drought
TB_19_Richness_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "TB"), richness ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_Richness_Aerial, type = 3) #NS

#TB 2020 - droughtxgrazing
TB_20_Richness_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "TB"), richness ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_Richness_Aerial, type = 3) #NS

#TB 2021- droughtxgrazing
TB_21_Richness_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "TB"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_Richness_Aerial, type = 3) #NS

#TB 2022- droughtxgrazing
TB_22_Richness_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "TB"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_Richness_Aerial, type = 3) #NS

#Basal#

#TB 2018 - checking drought and grazing
TB_18_Richness_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2018 & site== "TB"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_Richness_Basal, type = 3) #NS

#TB 2019 - just drought
TB_19_Richness_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2019 & site== "TB"), richness ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_Richness_Basal, type = 3) #NS

#TB 2020 - droughtxgrazing
TB_20_Richness_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2020 & site== "TB"), richness ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_Richness_Basal, type = 3) #NS

#TB 2021- droughtxgrazing
TB_21_Richness_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2021 & site== "TB"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_Richness_Basal, type = 3) #NS

#TB 2022- droughtxgrazing
TB_22_Richness_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2022 & site== "TB"), richness ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_Richness_Basal, type = 3) #NS

#### Figure: Aerial - Richness ####
#make dataframe with averages
CommunityMetrics_Aerial_Avg<-CommunityMetrics_Aerial %>% 
  group_by(year, site, rainfall_reduction)%>%
  summarize(Richness_Std=sd(richness),Richness_Mean=mean(richness),Richness_n=length(richness),
            Shannon_Std=sd(Shannon),Shannon_Mean=mean(Shannon),Shannon_n=length(Shannon),
            Evar_Std=sd(Evar),Evar_Mean=mean(Evar),Evar_n=length(Evar))%>%
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n),
         Shannon_St_Error=Shannon_Std/sqrt(Shannon_n),
         Evar_St_Error=Evar_Std/sqrt(Evar_n)) %>% 
  ungroup()

#color palettes
droughtColor <- c('#6baed6', '#6baed6', '#fdbe85', '#fd8d3c', '#e6550d', '#a63603') #from 0 to 99 ##change this to be blue at 0 to red at 99
grazingColor <- c('#ABDEFF', '#469BEC', '#6D882B') #from HHMMM to MMMMM to MLLMM

#FK: richness and drought
#Fort Keogh all years
Richness_FK_ALL_Aerial_Drought<-ggplot(subset(CommunityMetrics_Aerial_Avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=Richness_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+ #2019 2021
  geom_smooth(data=subset(CommunityMetrics_Aerial_Avg,site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5)+
  geom_smooth(data=subset(CommunityMetrics_Aerial_Avg,site=="FK"&year==2019), method='lm', se=FALSE,color="darkslateblue",size=5)+
  geom_pointrange(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","cadetblue","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_alpha_manual(values=c(0,1,1,0))+
  #scale_linetype_manual(values=c("clear","solid","solid","clear"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Plant Species Richness")+
  expand_limits(y=c(0,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.1,0.25),legend.key = element_rect(size=20), legend.key.size = unit(5.0, 'lines'))+
  annotate("text", x=20, y=20, label = "A. Montana Site", size=20)

#FThunder Basin all years
Richness_TB_ALL_Aerial_Drought<-ggplot(subset(CommunityMetrics_Aerial_Avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=Richness_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  #geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5)+
  #geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5)+
  geom_pointrange(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","cadetblue","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_alpha_manual(values=c(0,1,1,0))+
  #scale_linetype_manual(values=c("clear","solid","solid","clear"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Plant Species Richness")+
  expand_limits(y=c(0,20))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=20,y=20, label = "B. Wyoming Site", size=20)

#### Create RichnessxDrought Figure ####
Richness_FK_ALL_Aerial_Drought+
  Richness_TB_ALL_Aerial_Drought+
  plot_layout(ncol = 1,nrow = 2)
#save at 1500x2000

## FK Grazing ##
Richness_FK_ALL_Aerial_Grazing<-ggplot(subset(CommunityMetrics_Aerial,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=richness_fig,color=factor(grazing_treatment_fig,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c('#6D882B','#469BEC',"#ABDEFF"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment",drop = FALSE)+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"),drop = FALSE)+
  #scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Year")+
  ylab("Plant Species Richness")+
  expand_limits(y=c(0,20))+
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = c(0.15,0.1),legend.key = element_rect(size=40), legend.key.size = unit(10.0, 'lines'))+
  annotate("text", x=2.2, y=29, label = "C. Montana Site", size=30)

## TB Grazing ##
Richness_TB_ALL_Aerial_Grazing<-ggplot(subset(CommunityMetrics_Aerial,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=richness_fig,color=factor(grazing_treatment_fig,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c('#6D882B','#469BEC',"#ABDEFF"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment",drop = FALSE)+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"),drop = FALSE)+
  #scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Year")+
  ylab("Plant Species Richness")+
  expand_limits(y=c(0,20))+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=2.2, y=29, label = "D. Wyoming Site", size=30)

#### Create RichnessXGrazing Figure ####

  Richness_FK_ALL_Aerial_Grazing+
  Richness_TB_ALL_Aerial_Grazing+
  plot_layout(ncol = 1,nrow = 2)
#Save at 2000x2000

#### Create GMDR Bullitin Figure ####
Richness_FK_ALL_Aerial_Drought+
  Richness_FK_ALL_Aerial_Grazing+
  Richness_TB_ALL_Aerial_Drought+
  Richness_TB_ALL_Aerial_Grazing+
  plot_layout(ncol = 2,nrow = 2)
#save at 3000x2000

#### Normality: Fort Keogh Aerial + Basal - Evar ####
#FK - Aerial - Evar: 2018 
#non transformed data
Norm_FK_18_Evar_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "FK"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_Evar_Ar) 
ols_test_normality(Norm_FK_18_Evar_Ar) #normal

#FK - Aerial - Evar: 2019 
#non transformed data
Norm_FK_19_Evar_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "FK"), Evar  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_Evar_Ar) 
ols_test_normality(Norm_FK_19_Evar_Ar) #not normal

CommunityMetrics_Aerial<-CommunityMetrics_Aerial %>% 
  mutate(Evar_FK_19_TF=1/sqrt(Evar))

Norm_FK_19_Evar_Ar_TF <- lm(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "FK"), Evar_FK_19_TF  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_Evar_Ar_TF) 
ols_test_normality(Norm_FK_19_Evar_Ar_TF) #1/sqrt works best

#FK - Aerial - Evar: 2020
#non transformed data
Norm_FK_20_Evar_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "FK"), Evar  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_Evar_Ar) 
ols_test_normality(Norm_FK_20_Evar_Ar) #normal

#FK - Aerial - Evar: 2021
#non transformed data
Norm_FK_21_Evar_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "FK"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_Evar_Ar) 
ols_test_normality(Norm_FK_21_Evar_Ar) #normal

#FK - Aerial - Evar: 2022
#non transformed data
Norm_FK_22_Evar_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "FK"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_Evar_Ar) 
ols_test_normality(Norm_FK_22_Evar_Ar) #normal

#FK - Basal - Evar: 2018 
#non transformed data
Norm_FK_18_Evar_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2018 & site== "FK"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_Evar_Ba) 
ols_test_normality(Norm_FK_18_Evar_Ba) #normal

#FK - Basal - Evar: 2019 
#non transformed data
Norm_FK_19_Evar_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2019 & site== "FK"), Evar  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_Evar_Ba) 
ols_test_normality(Norm_FK_19_Evar_Ba) #normal

#FK - Basal - Evar: 2020
#non transformed data
Norm_FK_20_Evar_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2020 & site== "FK"), Evar  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_Evar_Ba) 
ols_test_normality(Norm_FK_20_Evar_Ba) #normal

#FK - Basal - Evar: 2021
#non transformed data
Norm_FK_21_Evar_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2021 & site== "FK"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_Evar_Ba) 
ols_test_normality(Norm_FK_21_Evar_Ba) #normal

#FK - Basal - Evar: 2022
#non transformed data
Norm_FK_22_Evar_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2022 & site== "FK"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_Evar_Ba) 
ols_test_normality(Norm_FK_22_Evar_Ba) #normal

#### Normality: Thunder Basin Aerial + Basal - Evar ####
#TB - Aerial - Evar: 2018 
#non transformed data
Norm_TB_18_Evar_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "TB"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_Evar_Ar) 
ols_test_normality(Norm_TB_18_Evar_Ar) #normal

#TB - Aerial - Evar: 2019 
#non transformed data
Norm_TB_19_Evar_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "TB"), Evar  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_Evar_Ar) 
ols_test_normality(Norm_TB_19_Evar_Ar) #normal

#TB - Aerial - Evar: 2020
#non transformed data
Norm_TB_20_Evar_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "TB"), Evar  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_Evar_Ar) 
ols_test_normality(Norm_TB_20_Evar_Ar) #normal

#TB - Aerial - Evar: 2021
#non transformed data
Norm_TB_21_Evar_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "TB"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_Evar_Ar) 
ols_test_normality(Norm_TB_21_Evar_Ar) #normal

#TB - Aerial - Evar: 2022
#non transformed data
Norm_TB_22_Evar_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "TB"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_Evar_Ar) 
ols_test_normality(Norm_TB_22_Evar_Ar) #not normal


CommunityMetrics_Aerial<-CommunityMetrics_Aerial %>% 
  mutate(Evar_TB_22_TF=1/sqrt(Evar))

Norm_TB_22_Evar_Ar_TF <- lm(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "FK"), Evar_TB_22_TF  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_22_Evar_Ar_TF) 
ols_test_normality(Norm_TB_22_Evar_Ar_TF) #1/sqrt works best


#TB - Basal - Evar: 2018 
#non transformed data
Norm_TB_18_Evar_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2018 & site== "TB"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_Evar_Ba) 
ols_test_normality(Norm_TB_18_Evar_Ba) #normal

#TB - Basal - Evar: 2019 
#non transformed data
Norm_TB_19_Evar_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2019 & site== "TB"), Evar  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_Evar_Ba) 
ols_test_normality(Norm_TB_19_Evar_Ba) #not normal

CommunityMetrics_Basal<-CommunityMetrics_Basal %>% 
  mutate(Evar_TB_19_TF=sqrt(Evar))

Norm_TB_19_Evar_Ba_TF <- lm(data = subset(CommunityMetrics_Basal, year == 2019 & site== "FK"), Evar_TB_19_TF  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_Evar_Ba_TF) 
ols_test_normality(Norm_TB_19_Evar_Ba_TF) #sqrt works best


#TB - Basal - Evar: 2020
#non transformed data
Norm_TB_20_Evar_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2020 & site== "TB"), Evar  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_Evar_Ba) 
ols_test_normality(Norm_TB_20_Evar_Ba) #not normal

CommunityMetrics_Basal<-CommunityMetrics_Basal %>% 
  mutate(Evar_TB_20_TF=sqrt(Evar))

Norm_TB_20_Evar_Ba_TF <- lm(data = subset(CommunityMetrics_Basal, year == 2020 & site== "FK"), Evar_TB_20_TF  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_20_Evar_Ba_TF) 
ols_test_normality(Norm_TB_20_Evar_Ba_TF) #sqrt works best

#TB - Basal - Evar: 2021
#non transformed data
Norm_TB_21_Evar_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2021 & site== "TB"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_Evar_Ba) 
ols_test_normality(Norm_TB_21_Evar_Ba) #normal

#TB - Basal - Evar: 2022
#non transformed data
Norm_TB_22_Evar_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2022 & site== "TB"), Evar  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_Evar_Ba) 
ols_test_normality(Norm_TB_22_Evar_Ba) #not normal

CommunityMetrics_Basal<-CommunityMetrics_Basal %>% 
  mutate(Evar_TB_22_TF=1/sqrt(Evar))

Norm_TB_22_Evar_Ba_TF <- lm(data = subset(CommunityMetrics_Basal, year == 2022 & site== "FK"), Evar_TB_22_TF  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_22_Evar_Ba_TF) 
ols_test_normality(Norm_TB_22_Evar_Ba_TF) #1/sqrt works best


#### Stats: Fort Keogh Aerial + Basal - Evar ####

#FK 2018 - checking drought and grazing
FK_18_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "FK"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Evar_Aerial, type = 3) #NS

#FK 2019 - just drought
FK_19_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "FK"), Evar_FK_19_TF ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_Evar_Aerial, type = 3) #NS

#FK 2020 - droughtxgrazing
FK_20_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "FK"), Evar ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_Evar_Aerial, type = 3) #NS

#FK 2021- droughtxgrazing
FK_21_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "FK"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_Evar_Aerial, type = 3) #drought (p=0.006575), grazing (p=0.088628)

#FK 2022- droughtxgrazing
FK_22_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "FK"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_Evar_Aerial, type = 3) #grazing (0.007961), interaction (0.080160)

#Basal
#FK 2018 - checking drought and grazing
FK_18_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2018 & site== "FK"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Evar_Basal, type = 3) #grazing (0.07809)

#FK 2019 - just drought
FK_19_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2019 & site== "FK"), Evar ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_Evar_Basal, type = 3) #NS

#FK 2020 - droughtxgrazing
FK_20_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2020 & site== "FK"), Evar ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_Evar_Basal, type = 3) #NS

#FK 2021- droughtxgrazing
FK_21_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2021 & site== "FK"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_Evar_Basal, type = 3) #drought (p=0.0002416)

#FK 2022- droughtxgrazing
FK_22_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2022 & site== "FK"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_Evar_Basal, type = 3) #Drought(0.02057)

#### Stats:  Thunder Basin Aerial + Basal - Evar ####

#TB 2018 - checking drought and grazing
TB_18_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "TB"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_Evar_Aerial, type = 3) #NS

#TB 2019 - just drought
TB_19_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "TB"), Evar ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_Evar_Aerial, type = 3) #NS

#TB 2020 - droughtxgrazing
TB_20_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "TB"), Evar ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_Evar_Aerial, type = 3) #grazing (0.02589)

#TB 2021- droughtxgrazing
TB_21_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "TB"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_Evar_Aerial, type = 3) #drought (0.003804)

#TB 2022- droughtxgrazing
TB_22_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "TB"), Evar_TB_22_TF ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_Evar_Aerial, type = 3) #NS

#basal
#TB 2018 - checking drought and grazing
TB_18_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2018 & site== "TB"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_Evar_Basal, type = 3) #NS

#TB 2019 - just drought
TB_19_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2019 & site== "TB"), Evar_TB_19_TF ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_Evar_Basal, type = 3) #NS

#TB 2020 - droughtxgrazing
TB_20_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2020 & site== "TB"), Evar_TB_20_TF ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_Evar_Basal, type = 3) #grazing (0.02589)

#TB 2021- droughtxgrazing
TB_21_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2021 & site== "TB"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_Evar_Basal, type = 3) #drought (0.08525)

#TB 2022- droughtxgrazing
TB_22_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2022 & site== "TB"), Evar_TB_22_TF ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_Evar_Basal, type = 3) #grazing (0.005449)

#### Figure: Aerial Evenness ####

#FK: Evar and drought
#Fort Keogh all years
Evar_FK_ALL_Aerial_Drought<-ggplot(subset(CommunityMetrics_Aerial_Avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=Evar_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  #geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5)+
  #geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5)+
  geom_pointrange(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_alpha_manual(values=c(0,1,1,0))+
  #scale_linetype_manual(values=c("clear","solid","solid","clear"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Plant Species Evenness")+
  expand_limits(y=c(0,0.6))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.1,0.2),legend.key = element_rect(size=20), legend.key.size = unit(5.0, 'lines'))+
  annotate("text", x=29, y=0.6, label = "A. MT Aerial Evenness", size=20)

#Thunder Basin all years
Evar_TB_ALL_Aerial_Drought<-ggplot(subset(CommunityMetrics_Aerial_Avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=Evar_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  #geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5)+
  #geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5)+
  geom_pointrange(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_alpha_manual(values=c(0,1,1,0))+
  #scale_linetype_manual(values=c("clear","solid","solid","clear"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Plant Species Evenness")+
  expand_limits(y=c(0,0.6))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=29, y=0.6, label = "B. WY Aerial Evenness", size=20)

#### Create EvarxDrought Figure ####
Evar_FK_ALL_Aerial_Drought+
  Evar_TB_ALL_Aerial_Drought+
  plot_layout(ncol = 1,nrow = 2)
#save at 1500x2000

## FK Grazing ##
Evar_FK_ALL_Aerial_Grazing<-ggplot(subset(CommunityMetrics_Aerial,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Evar_fig,color=factor(grazing_treatment_fig,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment",drop = FALSE)+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"),drop = FALSE)+
  #scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Plant Species Diveristy")+
  expand_limits(y=c(0,4))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.9),legend.key = element_rect(size=40), legend.key.size = unit(10.0, 'lines'))+
  annotate("text", x=2.8, y=3.9, label = "C. MT Aerial Evenness", size=30)

## TB Grazing ##
Evar_TB_ALL_Aerial_Grazing<-ggplot(subset(CommunityMetrics_Aerial,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Evar_fig,color=factor(grazing_treatment_fig,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment",drop = FALSE)+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"),drop = FALSE)+
  #scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Plant Species Evenness")+
  expand_limits(y=c(0,4))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=2.8, y=3.9, label = "D. WY Aerial Evenness", size=30)

#### Create EvarXGrazing Figure ####

Evar_FK_ALL_Aerial_Grazing+
  Evar_TB_ALL_Aerial_Grazing+
  plot_layout(ncol = 1,nrow = 2)
#Save at 2000x2000


#### Normality: Fort Keogh Aerial + Basal - Shannon ####
#FK - Aerial - Shannon: 2018 
#non transformed data
Norm_FK_18_Shannon_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "FK"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_Shannon_Ar) 
ols_test_normality(Norm_FK_18_Shannon_Ar) #normal

#FK - Aerial - Shannon: 2019 
#non transformed data
Norm_FK_19_Shannon_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "FK"), Shannon  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_Shannon_Ar) 
ols_test_normality(Norm_FK_19_Shannon_Ar) #normal

#FK - Aerial - Shannon: 2020
#non transformed data
Norm_FK_20_Shannon_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "FK"), Shannon  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_Shannon_Ar) 
ols_test_normality(Norm_FK_20_Shannon_Ar) #not normal

CommunityMetrics_Aerial<- CommunityMetrics_Aerial %>% 
  mutate(Shannon_20_FK_TF=exp(Shannon))

Norm_FK_20_Shannon_Ar_TF <- lm(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "FK"), Shannon_20_FK_TF  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_Shannon_Ar_TF) 
ols_test_normality(Norm_FK_20_Shannon_Ar_TF) #best with exponential transformation

#FK - Aerial - Shannon: 2021
#non transformed data
Norm_FK_21_Shannon_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "FK"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_Shannon_Ar) 
ols_test_normality(Norm_FK_21_Shannon_Ar) #normalish

#FK - Aerial - Shannon: 2022
#non transformed data
Norm_FK_22_Shannon_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "FK"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_Shannon_Ar) 
ols_test_normality(Norm_FK_22_Shannon_Ar) #normal

#FK - Basal - Shannon: 2018 
#non transformed data
Norm_FK_18_Shannon_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2018 & site== "FK"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_Shannon_Ba) 
ols_test_normality(Norm_FK_18_Shannon_Ba) #normal

#FK - Basal - Shannon: 2019 
#non transformed data
Norm_FK_19_Shannon_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2019 & site== "FK"), Shannon  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_Shannon_Ba) 
ols_test_normality(Norm_FK_19_Shannon_Ba) #normalish

#FK - Basal - Shannon: 2020
#non transformed data
Norm_FK_20_Shannon_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2020 & site== "FK"), Shannon  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_Shannon_Ba) 
ols_test_normality(Norm_FK_20_Shannon_Ba) #normal

#FK - Basal - Shannon: 2021
#non transformed data
Norm_FK_21_Shannon_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2021 & site== "FK"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_Shannon_Ba) 
ols_test_normality(Norm_FK_21_Shannon_Ba) #not normal

CommunityMetrics_Basal<- CommunityMetrics_Basal %>% 
  mutate(Shannon_21_FK_TF=exp(Shannon))

Norm_FK_21_Shannon_Ba_TF <- lm(data = subset(CommunityMetrics_Basal, year == 2021 & site== "FK"), Shannon_21_FK_TF  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_Shannon_Ba_TF) 
ols_test_normality(Norm_FK_21_Shannon_Ba_TF) #exponential is best transformation

#FK - Basal - Shannon: 2022
#non transformed data
Norm_FK_22_Shannon_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2022 & site== "FK"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_Shannon_Ba) 
ols_test_normality(Norm_FK_22_Shannon_Ba) #normal

#### Normality: Thunder Basin Aerial + Basal - Shannon ####
#TB - Aerial - Shannon: 2018 
#non transformed data
Norm_TB_18_Shannon_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "TB"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_Shannon_Ar) 
ols_test_normality(Norm_TB_18_Shannon_Ar) #not normal

CommunityMetrics_Aerial<- CommunityMetrics_Aerial %>% 
  mutate(Shannon_18_TB_TF=exp(Shannon))

Norm_TB_18_Shannon_Ar_TF <- lm(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "FK"), Shannon_18_TB_TF  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_Shannon_Ar_TF) 
ols_test_normality(Norm_TB_18_Shannon_Ar_TF) #exponential is best transformation

#TB - Aerial - Shannon: 2019 
#non transformed data
Norm_TB_19_Shannon_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "TB"), Shannon  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_Shannon_Ar) 
ols_test_normality(Norm_TB_19_Shannon_Ar) #normal

#TB - Aerial - Shannon: 2020
#non transformed data
Norm_TB_20_Shannon_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "TB"), Shannon  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_Shannon_Ar) 
ols_test_normality(Norm_TB_20_Shannon_Ar) #normal

#TB - Aerial - Shannon: 2021
#non transformed data
Norm_TB_21_Shannon_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "TB"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_Shannon_Ar) 
ols_test_normality(Norm_TB_21_Shannon_Ar) #normal

#TB - Aerial - Shannon: 2022
#non transformed data
Norm_TB_22_Shannon_Ar <- lm(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "TB"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_Shannon_Ar) 
ols_test_normality(Norm_TB_22_Shannon_Ar) #normal

#TB - Basal - Shannon: 2018 
#non transformed data
Norm_TB_18_Shannon_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2018 & site== "TB"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_Shannon_Ba) 
ols_test_normality(Norm_TB_18_Shannon_Ba) #normal

#TB - Basal - Shannon: 2019 
#non transformed data
Norm_TB_19_Shannon_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2019 & site== "TB"), Shannon  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_Shannon_Ba) 
ols_test_normality(Norm_TB_19_Shannon_Ba) # normal

#TB - Basal - Shannon: 2020
#non transformed data
Norm_TB_20_Shannon_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2020 & site== "TB"), Shannon  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_Shannon_Ba) 
ols_test_normality(Norm_TB_20_Shannon_Ba) #normal

#TB - Basal - Shannon: 2021
#non transformed data
Norm_TB_21_Shannon_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2021 & site== "TB"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_Shannon_Ba) 
ols_test_normality(Norm_TB_21_Shannon_Ba) #normal

#TB - Basal - Shannon: 2022
#non transformed data
Norm_TB_22_Shannon_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2022 & site== "TB"), Shannon  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_Shannon_Ba) 
ols_test_normality(Norm_TB_22_Shannon_Ba) #not normal

CommunityMetrics_Basal<- CommunityMetrics_Basal %>% 
  mutate(Shannon_22_TB_TF=exp(Shannon))

Norm_TB_22_Shannon_Ar_TF <- lm(data = subset(CommunityMetrics_Basal, year == 2022 & site== "FK"), Shannon_22_TB_TF  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_Shannon_Ar_TF) 
ols_test_normality(Norm_TB_22_Shannon_Ar_TF) #exponential is best transformation


#### Stats: Fort Keogh Aerial + Basal - Shannon's ####

#FK 2018 - checking drought and grazing
FK_18_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "FK"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Shannon_Aerial, type = 3) #NS

#FK 2019 - just drought
FK_19_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "FK"), Shannon ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_Shannon_Aerial, type = 3) #drought (0.008147)

#FK 2020 - droughtxgrazing
FK_20_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "FK"), Shannon_20_FK_TF ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_Shannon_Aerial, type = 3) #Grazing (0.09032)

#FK 2021- droughtxgrazing
FK_21_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "FK"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_Shannon_Aerial, type = 3) #NS

#FK 2022- droughtxgrazing
FK_22_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "FK"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_Shannon_Aerial, type = 3) #grazing (0.01847)

#basal
#FK 2018 - checking drought and grazing
FK_18_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2018 & site== "FK"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Shannon_Basal, type = 3) #NS

#FK 2019 - just drought
FK_19_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2019 & site== "FK"), Shannon ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_Shannon_Basal, type = 3) #drought (0.0006638)

#FK 2020 - droughtxgrazing
FK_20_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2020 & site== "FK"), Shannon ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_Shannon_Basal, type = 3) #drought (0.06), grazing (0.062)

#FK 2021- droughtxgrazing
FK_21_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2021 & site== "FK"), Shannon_21_FK_TF ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_Shannon_Basal, type = 3) #NS

#FK 2022- droughtxgrazing
FK_22_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2022 & site== "FK"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_Shannon_Basal, type = 3) #grazing (0.008034), interaction (0.054988)

#### Stats: Thunder  Basin Aerial + Basal - Shannon's####

#TB 2018 - checking drought and grazing
TB_18_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "TB"), Shannon_18_TB_TF ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_Shannon_Aerial, type = 3) #grazing (0.008932)

#TB 2019 - just drought
TB_19_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "TB"), Shannon ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_Shannon_Aerial, type = 3) #NS

#TB 2020 - droughtxgrazing
TB_20_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "TB"), Shannon ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_Shannon_Aerial, type = 3) #grazing (0.01341)

#TB 2021- droughtxgrazing
TB_21_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2021 & site== "TB"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_Shannon_Aerial, type = 3) #NS

#TB 2022- droughtxgrazing
TB_22_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "TB"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_Shannon_Aerial, type = 3) #grazing (0.0308)

#basal
#TB 2018 - checking drought and grazing
TB_18_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2018 & site== "TB"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_Shannon_Basal, type = 3) #grazing (0.002954)

#TB 2019 - just drought
TB_19_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2019 & site== "TB"), Shannon ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_Shannon_Basal, type = 3) #NS

#TB 2020 - droughtxgrazing
TB_20_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2020 & site== "TB"), Shannon ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_Shannon_Basal, type = 3) #grazing (0.04664)

#TB 2021- droughtxgrazing
TB_21_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2021 & site== "TB"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_Shannon_Basal, type = 3) #NS

#TB 2022- droughtxgrazing
TB_22_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2022 & site== "TB"), Shannon_22_TB_TF ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_Shannon_Basal, type = 3) #grazing (0.02883)

#### Figure: Aerial Diversity ####

#FK: Shannon and drought
#Fort Keogh all years
Shannon_FK_ALL_Aerial_Drought<-ggplot(subset(CommunityMetrics_Aerial_Avg,site=="FK"&year>=2019),aes(x=rainfall_reduction,y=Shannon_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  #geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5)+
  #geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5)+
  geom_pointrange(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_alpha_manual(values=c(0,1,1,0))+
  #scale_linetype_manual(values=c("clear","solid","solid","clear"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Plant Species Diversity")+
  expand_limits(y=c(0,4))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.89,0.77),legend.key = element_rect(size=20), legend.key.size = unit(5.0, 'lines'))+
  annotate("text", x=28, y=4, label = "A. MT Aerial Diversity", size=20)

#Thunder Basin all years
Shannon_TB_ALL_Aerial_Drought<-ggplot(subset(CommunityMetrics_Aerial_Avg,site=="TB"&year>=2019),aes(x=rainfall_reduction,y=Shannon_Mean,color=as.factor(year),shape=as.factor(year))) +  
  geom_point(size=14, stroke =6)+
  #geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2021), method='lm', se=FALSE,color="maroon4",size=5)+
  #geom_smooth(data=subset(CWM_Collected_Data_avg,Site=="FK"&year==2022), method='lm', se=FALSE,color="darkgreen",size=5)+
  geom_pointrange(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),linewidth = 4)+
  labs(color  = "Year", linetype = "Year", shape = "Year")+
  scale_shape_manual(values=c(15,16,17,18),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  scale_color_manual(values=c("darkslateblue","blue4","maroon4","darkgreen"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_alpha_manual(values=c(0,1,1,0))+
  #scale_linetype_manual(values=c("clear","solid","solid","clear"),labels = c("2019", "2020","2021","2022"), breaks = c("2019","2020","2021","2022"),name="Year")+
  #scale_y_continuous(labels = label_number(accuracy = 0.01))+
  xlab("Rainfall Reduction (%)")+
  ylab("Plant Species Diversity")+
  expand_limits(y=c(0,4))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=28, y=4, label = "B. WY Aerial Diversity", size=20)

#### Create ShannonxDrought Figure ####
Shannon_FK_ALL_Aerial_Drought+
  Shannon_TB_ALL_Aerial_Drought+
  plot_layout(ncol = 1,nrow = 2)
#save at 1500x2000

## FK Grazing ##
Shannon_FK_ALL_Aerial_Grazing<-ggplot(subset(CommunityMetrics_Aerial,site=="FK"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Shannon_fig,color=factor(grazing_treatment_fig,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment",drop = FALSE)+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"),drop = FALSE)+
  #scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Plant Species Diveristy")+
  expand_limits(y=c(0,4))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_blank(),axis.title.y=element_text(size=55),axis.title.x=element_blank(),legend.position = c(0.9,0.9),legend.key = element_rect(size=40), legend.key.size = unit(10.0, 'lines'))+
  annotate("text", x=2.8, y=3.9, label = "C. MT Aerial Diversity", size=30)

## TB Grazing ##
Shannon_TB_ALL_Aerial_Grazing<-ggplot(subset(CommunityMetrics_Aerial,site=="TB"&year>=2020),aes(x=factor(year,level=c(2020,2021,2022)),y=Shannon_fig,color=factor(grazing_treatment_fig,level=c("destock","stable","heavy")))) +
  annotate('rect', xmin = c('2019.5','2021.5'), xmax = c('2020.5','2022.5'), 
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey")+
  geom_boxplot(lwd=2,position=position_dodge(2))+
  theme(legend.key.height = unit(1, 'cm'),legend.key.width= unit(2, 'cm'))+
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment",drop = FALSE)+
  scale_x_discrete(labels = c("2020","2021","2022"), breaks = c("2020","2021","2022"),drop = FALSE)+
  #scale_y_continuous(labels = label_number(accuracy = 0.1))+
  xlab("Grazing Treatment")+
  ylab("Plant Species Diversity")+
  expand_limits(y=c(0,4))+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.position = "NONE")+
  annotate("text", x=2.8, y=3.9, label = "D. WY Aerial Diversity", size=30)

#### Create ShannonXGrazing Figure ####

Shannon_FK_ALL_Aerial_Grazing+
  Shannon_TB_ALL_Aerial_Grazing+
  plot_layout(ncol = 1,nrow = 2)
#Save at 2000x2000


#### NMDS ####

#Create wide relative cover dataframe
RelCov_Clean<-Species_Comp_RelCov_Clean %>% 
  full_join(plot_layoutK) %>%
  mutate(drought = ifelse(drought == 1, 0, ifelse(drought==2,0, drought))) %>%
  #create column that has all grazing treatments in it for a given year
  mutate(grazing_treatment_fig=ifelse(grazing_category=="MMMMM" &year==2020,"stable",ifelse(grazing_category=="HHMMM" &year==2020, "heavy",ifelse(grazing_category=="MLLMM" &year==2020, "stable",ifelse(year==2019,NA,grazing_treatment))))) %>% 
  spread(key=Genus_Species,value=Relative_Cover, fill=0) 

#### Bray Curtis FK Aerial 2018 ####

#Species Comp FK: Aerial 2018
Wide_FK_AR_18<-RelCov_Clean%>%
  filter(site=="FK" & aerial_basal=="Aerial" & year=="2018") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp FK: Aerial 
BC_FK_AR_18<-metaMDS(Wide_FK_AR_18[,16:152])
#look at species signiciance driving NMDS 
intrinsics_FK_AR_18 <- envfit(BC_FK_AR_18, Wide_FK_AR_18, permutations = 999,na.rm=T)
head(intrinsics_FK_AR_18)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_FK_AR_18 <- 1:nrow(Wide_FK_AR_18)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_FK_AR_18 <- Wide_FK_AR_18[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2019,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_FK_AR_18 = data.frame(MDS1 = BC_FK_AR_18$points[,1], MDS2 = BC_FK_AR_18$points[,2],group=BC_Meta_Data_FK_AR_18$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_FK_AR_18 <- cbind(BC_Meta_Data_FK_AR_18,BC_NMDS_FK_AR_18)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_FK_AR_18<-ordiellipse(BC_FK_AR_18, BC_Meta_Data_FK_AR_18$Yr_Dr_Gr, display = "sites",
                               kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_FK_AR_18 <- data.frame()

#Use the vegan ellipse function to make ellipses           
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_FK_AR_18$group)){
  BC_Ellipses_FK_AR_18 <- rbind(BC_Ellipses_FK_AR_18, cbind(as.data.frame(with(BC_NMDS_FK_AR_18[BC_NMDS_FK_AR_18$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_FK_AR_18[[g]]$cov,BC_Ord_Ellipses_FK_AR_18[[g]]$center,BC_Ord_Ellipses_FK_AR_18[[g]]$scale)))
                                              ,group=g))
}

#### Bray Curtis FK Aerial 2019 ####

#Species Comp FK: Aerial 2019
Wide_FK_AR_19<-RelCov_Clean%>%
  filter(site=="FK" & aerial_basal=="Aerial" & year=="2019") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp FK: Aerial 
BC_FK_AR_19<-metaMDS(Wide_FK_AR_19[,16:152])
#look at species significance driving NMDS 
intrinsics_FK_AR_19 <- envfit(BC_FK_AR_19, Wide_FK_AR_19, permutations = 999,na.rm=T)
head(intrinsics_FK_AR_19)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_FK_AR_19 <- 1:nrow(Wide_FK_AR_19)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_FK_AR_19 <- Wide_FK_AR_19[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2019,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_FK_AR_19 = data.frame(MDS1 = BC_FK_AR_19$points[,1], MDS2 = BC_FK_AR_19$points[,2],group=BC_Meta_Data_FK_AR_19$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_FK_AR_19 <- cbind(BC_Meta_Data_FK_AR_19,BC_NMDS_FK_AR_19)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_FK_AR_19<-ordiellipse(BC_FK_AR_19, BC_Meta_Data_FK_AR_19$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_FK_AR_19 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_FK_AR_19$group)){
  BC_Ellipses_FK_AR_19 <- rbind(BC_Ellipses_FK_AR_19, cbind(as.data.frame(with(BC_NMDS_FK_AR_19[BC_NMDS_FK_AR_19$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_FK_AR_19[[g]]$cov,BC_Ord_Ellipses_FK_AR_19[[g]]$center,BC_Ord_Ellipses_FK_AR_19[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis FK Aerial 2020 ####

#Species Comp FK: Aerial 2020
Wide_FK_AR_20<-RelCov_Clean%>%
  filter(site=="FK" & aerial_basal=="Aerial" & year=="2020") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp FK: Aerial 
BC_FK_AR_20<-metaMDS(Wide_FK_AR_20[,16:152])
#look at species signiciance driving NMDS 
intrinsics_FK_AR_20 <- envfit(BC_FK_AR_20, Wide_FK_AR_20, permutations = 999,na.rm=T)
head(intrinsics_FK_AR_20)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_FK_AR_20 <- 1:nrow(Wide_FK_AR_20)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_FK_AR_20 <- Wide_FK_AR_20[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2020,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_FK_AR_20 = data.frame(MDS1 = BC_FK_AR_20$points[,1], MDS2 = BC_FK_AR_20$points[,2],group=BC_Meta_Data_FK_AR_20$Yr_Dr_Gr_20)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_FK_AR_20 <- cbind(BC_Meta_Data_FK_AR_20,BC_NMDS_FK_AR_20)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_FK_AR_20<-ordiellipse(BC_FK_AR_20, BC_Meta_Data_FK_AR_20$Yr_Dr_Gr_20, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_FK_AR_20 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_FK_AR_20$group)){
  BC_Ellipses_FK_AR_20 <- rbind(BC_Ellipses_FK_AR_20, cbind(as.data.frame(with(BC_NMDS_FK_AR_20[BC_NMDS_FK_AR_20$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_FK_AR_20[[g]]$cov,BC_Ord_Ellipses_FK_AR_20[[g]]$center,BC_Ord_Ellipses_FK_AR_20[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis FK Aerial 2021 ####

#Species Comp FK: Aerial 2021
Wide_FK_AR_21<-RelCov_Clean%>%
  filter(site=="FK" & aerial_basal=="Aerial" & year=="2021") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp FK: Aerial 
BC_FK_AR_21<-metaMDS(Wide_FK_AR_21[,16:152])
#look at species signiciance driving NMDS 
intrinsics_FK_AR_21 <- envfit(BC_FK_AR_21, Wide_FK_AR_21, permutations = 999,na.rm=T)
head(intrinsics_FK_AR_21)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_FK_AR_21 <- 1:nrow(Wide_FK_AR_21)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_FK_AR_21 <- Wide_FK_AR_21[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_21=paste(year,rainfall_reduction,livestock_util_2021,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_FK_AR_21 = data.frame(MDS1 = BC_FK_AR_21$points[,1], MDS2 = BC_FK_AR_21$points[,2],group=BC_Meta_Data_FK_AR_21$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_FK_AR_21 <- cbind(BC_Meta_Data_FK_AR_21,BC_NMDS_FK_AR_21)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_FK_AR_21<-ordiellipse(BC_FK_AR_21, BC_Meta_Data_FK_AR_21$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_FK_AR_21 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_FK_AR_21$group)){
  BC_Ellipses_FK_AR_21 <- rbind(BC_Ellipses_FK_AR_21, cbind(as.data.frame(with(BC_NMDS_FK_AR_21[BC_NMDS_FK_AR_21$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_FK_AR_21[[g]]$cov,BC_Ord_Ellipses_FK_AR_21[[g]]$center,BC_Ord_Ellipses_FK_AR_21[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis FK Aerial 2022 ####

#Species Comp FK: Aerial 2022
Wide_FK_AR_22<-RelCov_Clean%>%
  filter(site=="FK" & aerial_basal=="Aerial" & year=="2022") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp FK: Aerial 
BC_FK_AR_22<-metaMDS(Wide_FK_AR_22[,16:152])
#look at species signiciance driving NMDS 
intrinsics_FK_AR_22 <- envfit(BC_FK_AR_22, Wide_FK_AR_22, permutations = 999,na.rm=T)
head(intrinsics_FK_AR_22)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_FK_AR_22 <- 1:nrow(Wide_FK_AR_22)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_FK_AR_22 <- Wide_FK_AR_22[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_22=paste(year,rainfall_reduction,livestock_util_2020,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_FK_AR_22 = data.frame(MDS1 = BC_FK_AR_22$points[,1], MDS2 = BC_FK_AR_22$points[,2],group=BC_Meta_Data_FK_AR_22$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_FK_AR_22 <- cbind(BC_Meta_Data_FK_AR_22,BC_NMDS_FK_AR_22)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_FK_AR_22<-ordiellipse(BC_FK_AR_22, BC_Meta_Data_FK_AR_22$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_FK_AR_22 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_FK_AR_22$group)){
  BC_Ellipses_FK_AR_22 <- rbind(BC_Ellipses_FK_AR_22, cbind(as.data.frame(with(BC_NMDS_FK_AR_22[BC_NMDS_FK_AR_22$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_FK_AR_22[[g]]$cov,BC_Ord_Ellipses_FK_AR_22[[g]]$center,BC_Ord_Ellipses_FK_AR_22[[g]]$scale)))
                                                            ,group=g))
}


#### Bray Curtis FK Basal 2018 ####

#Species Comp FK: Basal 2018
Wide_FK_BA_18<-RelCov_Clean%>%
  filter(site=="FK" & aerial_basal=="Basal" & year=="2018") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp FK: Basal 
BC_FK_BA_18<-metaMDS(Wide_FK_BA_18[,16:152])
#look at species signiciance driving NMDS 
intrinsics_FK_BA_18 <- envfit(BC_FK_BA_18, Wide_FK_BA_18, permutations = 999,na.rm=T)
head(intrinsics_FK_BA_18)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_FK_BA_18 <- 1:nrow(Wide_FK_BA_18)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_FK_BA_18 <- Wide_FK_BA_18[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2019,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_FK_BA_18 = data.frame(MDS1 = BC_FK_BA_18$points[,1], MDS2 = BC_FK_BA_18$points[,2],group=BC_Meta_Data_FK_BA_18$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_FK_BA_18 <- cbind(BC_Meta_Data_FK_BA_18,BC_NMDS_FK_BA_18)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_FK_BA_18<-ordiellipse(BC_FK_BA_18, BC_Meta_Data_FK_BA_18$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_FK_BA_18 <- data.frame()


#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_FK_BA_18$group)){
  BC_Ellipses_FK_BA_18 <- rbind(BC_Ellipses_FK_BA_18, cbind(as.data.frame(with(BC_NMDS_FK_BA_18[BC_NMDS_FK_BA_18$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_FK_BA_18[[g]]$cov,BC_Ord_Ellipses_FK_BA_18[[g]]$center,BC_Ord_Ellipses_FK_BA_18[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis FK Basal 2019 ####

#Species Comp FK: Basal 2019
Wide_FK_BA_19<-RelCov_Clean%>%
  filter(site=="FK" & aerial_basal=="Basal" & year=="2019") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp FK: Basal 
BC_FK_BA_19<-metaMDS(Wide_FK_BA_19[,16:152])
#look at species significance driving NMDS 
intrinsics_FK_BA_19 <- envfit(BC_FK_BA_19, Wide_FK_BA_19, permutations = 999,na.rm=T)
head(intrinsics_FK_BA_19)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_FK_BA_19 <- 1:nrow(Wide_FK_BA_19)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_FK_BA_19 <- Wide_FK_BA_19[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2019,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_FK_BA_19 = data.frame(MDS1 = BC_FK_BA_19$points[,1], MDS2 = BC_FK_BA_19$points[,2],group=BC_Meta_Data_FK_BA_19$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_FK_BA_19 <- cbind(BC_Meta_Data_FK_BA_19,BC_NMDS_FK_BA_19)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_FK_BA_19<-ordiellipse(BC_FK_BA_19, BC_Meta_Data_FK_BA_19$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_FK_BA_19 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_FK_BA_19$group)){
  BC_Ellipses_FK_BA_19 <- rbind(BC_Ellipses_FK_BA_19, cbind(as.data.frame(with(BC_NMDS_FK_BA_19[BC_NMDS_FK_BA_19$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_FK_BA_19[[g]]$cov,BC_Ord_Ellipses_FK_BA_19[[g]]$center,BC_Ord_Ellipses_FK_BA_19[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis FK Basal 2020 ####

#Species Comp FK: Basal 2020
Wide_FK_BA_20<-RelCov_Clean%>%
  filter(site=="FK" & aerial_basal=="Basal" & year=="2020") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp FK: Basal 
BC_FK_BA_20<-metaMDS(Wide_FK_BA_20[,16:152])
#look at species signiciance driving NMDS 
intrinsics_FK_BA_20 <- envfit(BC_FK_BA_20, Wide_FK_BA_20, permutations = 999,na.rm=T)
head(intrinsics_FK_BA_20)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_FK_BA_20 <- 1:nrow(Wide_FK_BA_20)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_FK_BA_20 <- Wide_FK_BA_20[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2020,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_FK_BA_20 = data.frame(MDS1 = BC_FK_BA_20$points[,1], MDS2 = BC_FK_BA_20$points[,2],group=BC_Meta_Data_FK_BA_20$Yr_Dr_Gr_20)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_FK_BA_20 <- cbind(BC_Meta_Data_FK_BA_20,BC_NMDS_FK_BA_20)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_FK_BA_20<-ordiellipse(BC_FK_BA_20, BC_Meta_Data_FK_BA_20$Yr_Dr_Gr_20, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_FK_BA_20 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_FK_BA_20$group)){
  BC_Ellipses_FK_BA_20 <- rbind(BC_Ellipses_FK_BA_20, cbind(as.data.frame(with(BC_NMDS_FK_BA_20[BC_NMDS_FK_BA_20$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_FK_BA_20[[g]]$cov,BC_Ord_Ellipses_FK_BA_20[[g]]$center,BC_Ord_Ellipses_FK_BA_20[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis FK Basal 2021 ####

#Species Comp FK: Basal 2021
Wide_FK_BA_21<-RelCov_Clean%>%
  filter(site=="FK" & aerial_basal=="Basal" & year=="2021") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp FK: Basal 
BC_FK_BA_21<-metaMDS(Wide_FK_BA_21[,16:152])
#look at species signiciance driving NMDS 
intrinsics_FK_BA_21 <- envfit(BC_FK_BA_21, Wide_FK_BA_21, permutations = 999,na.rm=T)
head(intrinsics_FK_BA_21)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_FK_BA_21 <- 1:nrow(Wide_FK_BA_21)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_FK_BA_21 <- Wide_FK_BA_21[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_21=paste(year,rainfall_reduction,livestock_util_2021,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_FK_BA_21 = data.frame(MDS1 = BC_FK_BA_21$points[,1], MDS2 = BC_FK_BA_21$points[,2],group=BC_Meta_Data_FK_BA_21$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_FK_BA_21 <- cbind(BC_Meta_Data_FK_BA_21,BC_NMDS_FK_BA_21)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_FK_BA_21<-ordiellipse(BC_FK_BA_21, BC_Meta_Data_FK_BA_21$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_FK_BA_21 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_FK_BA_21$group)){
  BC_Ellipses_FK_BA_21 <- rbind(BC_Ellipses_FK_BA_21, cbind(as.data.frame(with(BC_NMDS_FK_BA_21[BC_NMDS_FK_BA_21$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_FK_BA_21[[g]]$cov,BC_Ord_Ellipses_FK_BA_21[[g]]$center,BC_Ord_Ellipses_FK_BA_21[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis FK Basal 2022 ####

#Species Comp FK: Basal 2022
Wide_FK_BA_22<-RelCov_Clean%>%
  filter(site=="FK" & aerial_basal=="Basal" & year=="2022") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp FK: Basal 
BC_FK_BA_22<-metaMDS(Wide_FK_BA_22[,16:152])
#look at species signiciance driving NMDS 
intrinsics_FK_BA_22 <- envfit(BC_FK_BA_22, Wide_FK_BA_22, permutations = 999,na.rm=T)
head(intrinsics_FK_BA_22)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_FK_BA_22 <- 1:nrow(Wide_FK_BA_22)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_FK_BA_22 <- Wide_FK_BA_22[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_22=paste(year,rainfall_reduction,livestock_util_2020,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_FK_BA_22 = data.frame(MDS1 = BC_FK_BA_22$points[,1], MDS2 = BC_FK_BA_22$points[,2],group=BC_Meta_Data_FK_BA_22$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_FK_BA_22 <- cbind(BC_Meta_Data_FK_BA_22,BC_NMDS_FK_BA_22)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_FK_BA_22<-ordiellipse(BC_FK_BA_22, BC_Meta_Data_FK_BA_22$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_FK_BA_22 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_FK_BA_22$group)){
  BC_Ellipses_FK_BA_22 <- rbind(BC_Ellipses_FK_BA_22, cbind(as.data.frame(with(BC_NMDS_FK_BA_22[BC_NMDS_FK_BA_22$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_FK_BA_22[[g]]$cov,BC_Ord_Ellipses_FK_BA_22[[g]]$center,BC_Ord_Ellipses_FK_BA_22[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis TB Aerial 2018 ####

#Species Comp TB: Aerial 2018
Wide_TB_AR_18<-RelCov_Clean%>%
  filter(site=="TB" & aerial_basal=="Aerial" & year=="2018") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp TB: Aerial 
BC_TB_AR_18<-metaMDS(Wide_TB_AR_18[,16:152])
#look at species signiciance driving NMDS 
intrinsics_TB_AR_18 <- envfit(BC_TB_AR_18, Wide_TB_AR_18, permutations = 999,na.rm=T)
head(intrinsics_TB_AR_18)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_TB_AR_18 <- 1:nrow(Wide_TB_AR_18)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_TB_AR_18 <- Wide_TB_AR_18[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2019,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_TB_AR_18 = data.frame(MDS1 = BC_TB_AR_18$points[,1], MDS2 = BC_TB_AR_18$points[,2],group=BC_Meta_Data_TB_AR_18$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_TB_AR_18 <- cbind(BC_Meta_Data_TB_AR_18,BC_NMDS_TB_AR_18)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_TB_AR_18<-ordiellipse(BC_TB_AR_18, BC_Meta_Data_TB_AR_18$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_TB_AR_18 <- data.frame()


#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_TB_AR_18$group)){
  BC_Ellipses_TB_AR_18 <- rbind(BC_Ellipses_TB_AR_18, cbind(as.data.frame(with(BC_NMDS_TB_AR_18[BC_NMDS_TB_AR_18$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_TB_AR_18[[g]]$cov,BC_Ord_Ellipses_TB_AR_18[[g]]$center,BC_Ord_Ellipses_TB_AR_18[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis TB Aerial 2019 ####

#Species Comp TB: Aerial 2019
Wide_TB_AR_19<-RelCov_Clean%>%
  filter(site=="TB" & aerial_basal=="Aerial" & year=="2019") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp TB: Aerial 
BC_TB_AR_19<-metaMDS(Wide_TB_AR_19[,16:152])
#look at species significance driving NMDS 
intrinsics_TB_AR_19 <- envfit(BC_TB_AR_19, Wide_TB_AR_19, permutations = 999,na.rm=T)
head(intrinsics_TB_AR_19)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_TB_AR_19 <- 1:nrow(Wide_TB_AR_19)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_TB_AR_19 <- Wide_TB_AR_19[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2019,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_TB_AR_19 = data.frame(MDS1 = BC_TB_AR_19$points[,1], MDS2 = BC_TB_AR_19$points[,2],group=BC_Meta_Data_TB_AR_19$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_TB_AR_19 <- cbind(BC_Meta_Data_TB_AR_19,BC_NMDS_TB_AR_19)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_TB_AR_19<-ordiellipse(BC_TB_AR_19, BC_Meta_Data_TB_AR_19$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_TB_AR_19 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_TB_AR_19$group)){
  BC_Ellipses_TB_AR_19 <- rbind(BC_Ellipses_TB_AR_19, cbind(as.data.frame(with(BC_NMDS_TB_AR_19[BC_NMDS_TB_AR_19$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_TB_AR_19[[g]]$cov,BC_Ord_Ellipses_TB_AR_19[[g]]$center,BC_Ord_Ellipses_TB_AR_19[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis TB Aerial 2020 ####

#Species Comp TB: Aerial 2020
Wide_TB_AR_20<-RelCov_Clean%>%
  filter(site=="TB" & aerial_basal=="Aerial" & year=="2020") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp TB: Aerial 
BC_TB_AR_20<-metaMDS(Wide_TB_AR_20[,16:152])
#look at species signiciance driving NMDS 
intrinsics_TB_AR_20 <- envfit(BC_TB_AR_20, Wide_TB_AR_20, permutations = 999,na.rm=T)
head(intrinsics_TB_AR_20)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_TB_AR_20 <- 1:nrow(Wide_TB_AR_20)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_TB_AR_20 <- Wide_TB_AR_20[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2020,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_TB_AR_20 = data.frame(MDS1 = BC_TB_AR_20$points[,1], MDS2 = BC_TB_AR_20$points[,2],group=BC_Meta_Data_TB_AR_20$Yr_Dr_Gr_20)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_TB_AR_20 <- cbind(BC_Meta_Data_TB_AR_20,BC_NMDS_TB_AR_20)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_TB_AR_20<-ordiellipse(BC_TB_AR_20, BC_Meta_Data_TB_AR_20$Yr_Dr_Gr_20, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_TB_AR_20 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_TB_AR_20$group)){
  BC_Ellipses_TB_AR_20 <- rbind(BC_Ellipses_TB_AR_20, cbind(as.data.frame(with(BC_NMDS_TB_AR_20[BC_NMDS_TB_AR_20$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_TB_AR_20[[g]]$cov,BC_Ord_Ellipses_TB_AR_20[[g]]$center,BC_Ord_Ellipses_TB_AR_20[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis TB Aerial 2021 ####

#Species Comp TB: Aerial 2021
Wide_TB_AR_21<-RelCov_Clean%>%
  filter(site=="TB" & aerial_basal=="Aerial" & year=="2021") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp TB: Aerial 
BC_TB_AR_21<-metaMDS(Wide_TB_AR_21[,16:152])
#look at species signiciance driving NMDS 
intrinsics_TB_AR_21 <- envfit(BC_TB_AR_21, Wide_TB_AR_21, permutations = 999,na.rm=T)
head(intrinsics_TB_AR_21)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_TB_AR_21 <- 1:nrow(Wide_TB_AR_21)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_TB_AR_21 <- Wide_TB_AR_21[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_21=paste(year,rainfall_reduction,livestock_util_2021,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_TB_AR_21 = data.frame(MDS1 = BC_TB_AR_21$points[,1], MDS2 = BC_TB_AR_21$points[,2],group=BC_Meta_Data_TB_AR_21$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_TB_AR_21 <- cbind(BC_Meta_Data_TB_AR_21,BC_NMDS_TB_AR_21)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_TB_AR_21<-ordiellipse(BC_TB_AR_21, BC_Meta_Data_TB_AR_21$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_TB_AR_21 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_TB_AR_21$group)){
  BC_Ellipses_TB_AR_21 <- rbind(BC_Ellipses_TB_AR_21, cbind(as.data.frame(with(BC_NMDS_TB_AR_21[BC_NMDS_TB_AR_21$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_TB_AR_21[[g]]$cov,BC_Ord_Ellipses_TB_AR_21[[g]]$center,BC_Ord_Ellipses_TB_AR_21[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis TB Aerial 2022 ####

#Species Comp TB: Aerial 2022
Wide_TB_AR_22<-RelCov_Clean%>%
  filter(site=="TB" & aerial_basal=="Aerial" & year=="2022") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp TB: Aerial 
BC_TB_AR_22<-metaMDS(Wide_TB_AR_22[,16:152])
#look at species signiciance driving NMDS 
intrinsics_TB_AR_22 <- envfit(BC_TB_AR_22, Wide_TB_AR_22, permutations = 999,na.rm=T)
head(intrinsics_TB_AR_22)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_TB_AR_22 <- 1:nrow(Wide_TB_AR_22)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_TB_AR_22 <- Wide_TB_AR_22[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_22=paste(year,rainfall_reduction,livestock_util_2020,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_TB_AR_22 = data.frame(MDS1 = BC_TB_AR_22$points[,1], MDS2 = BC_TB_AR_22$points[,2],group=BC_Meta_Data_TB_AR_22$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_TB_AR_22 <- cbind(BC_Meta_Data_TB_AR_22,BC_NMDS_TB_AR_22)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_TB_AR_22<-ordiellipse(BC_TB_AR_22, BC_Meta_Data_TB_AR_22$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_TB_AR_22 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_TB_AR_22$group)){
  BC_Ellipses_TB_AR_22 <- rbind(BC_Ellipses_TB_AR_22, cbind(as.data.frame(with(BC_NMDS_TB_AR_22[BC_NMDS_TB_AR_22$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_TB_AR_22[[g]]$cov,BC_Ord_Ellipses_TB_AR_22[[g]]$center,BC_Ord_Ellipses_TB_AR_22[[g]]$scale)))
                                                            ,group=g))
}


#### Bray Curtis TB Basal 2018 ####

#Species Comp TB: Basal 2018
Wide_TB_BA_18<-RelCov_Clean%>%
  filter(site=="TB" & aerial_basal=="Basal" & year=="2018") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp TB: Basal 
BC_TB_BA_18<-metaMDS(Wide_TB_BA_18[,16:152])
#look at species signiciance driving NMDS 
intrinsics_TB_BA_18 <- envfit(BC_TB_BA_18, Wide_TB_BA_18, permutations = 999,na.rm=T)
head(intrinsics_TB_BA_18)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_TB_BA_18 <- 1:nrow(Wide_TB_BA_18)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_TB_BA_18 <- Wide_TB_BA_18[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2019,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_TB_BA_18 = data.frame(MDS1 = BC_TB_BA_18$points[,1], MDS2 = BC_TB_BA_18$points[,2],group=BC_Meta_Data_TB_BA_18$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_TB_BA_18 <- cbind(BC_Meta_Data_TB_BA_18,BC_NMDS_TB_BA_18)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_TB_BA_18<-ordiellipse(BC_TB_BA_18, BC_Meta_Data_TB_BA_18$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_TB_BA_18 <- data.frame()


#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_TB_BA_18$group)){
  BC_Ellipses_TB_BA_18 <- rbind(BC_Ellipses_TB_BA_18, cbind(as.data.frame(with(BC_NMDS_TB_BA_18[BC_NMDS_TB_BA_18$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_TB_BA_18[[g]]$cov,BC_Ord_Ellipses_TB_BA_18[[g]]$center,BC_Ord_Ellipses_TB_BA_18[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis TB Basal 2019 ####

#Species Comp TB: Basal 2019
Wide_TB_BA_19<-RelCov_Clean%>%
  filter(site=="TB" & aerial_basal=="Basal" & year=="2019") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp TB: Basal 
BC_TB_BA_19<-metaMDS(Wide_TB_BA_19[,16:152])
#look at species significance driving NMDS 
intrinsics_TB_BA_19 <- envfit(BC_TB_BA_19, Wide_TB_BA_19, permutations = 999,na.rm=T)
head(intrinsics_TB_BA_19)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_TB_BA_19 <- 1:nrow(Wide_TB_BA_19)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_TB_BA_19 <- Wide_TB_BA_19[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2019,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_TB_BA_19 = data.frame(MDS1 = BC_TB_BA_19$points[,1], MDS2 = BC_TB_BA_19$points[,2],group=BC_Meta_Data_TB_BA_19$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_TB_BA_19 <- cbind(BC_Meta_Data_TB_BA_19,BC_NMDS_TB_BA_19)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_TB_BA_19<-ordiellipse(BC_TB_BA_19, BC_Meta_Data_TB_BA_19$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_TB_BA_19 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_TB_BA_19$group)){
  BC_Ellipses_TB_BA_19 <- rbind(BC_Ellipses_TB_BA_19, cbind(as.data.frame(with(BC_NMDS_TB_BA_19[BC_NMDS_TB_BA_19$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_TB_BA_19[[g]]$cov,BC_Ord_Ellipses_TB_BA_19[[g]]$center,BC_Ord_Ellipses_TB_BA_19[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis TB Basal 2020 ####

#Species Comp TB: Basal 2020
Wide_TB_BA_20<-RelCov_Clean%>%
  filter(site=="TB" & aerial_basal=="Basal" & year=="2020") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp TB: Basal 
BC_TB_BA_20<-metaMDS(Wide_TB_BA_20[,16:152])
#look at species signiciance driving NMDS 
intrinsics_TB_BA_20 <- envfit(BC_TB_BA_20, Wide_TB_BA_20, permutations = 999,na.rm=T)
head(intrinsics_TB_BA_20)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_TB_BA_20 <- 1:nrow(Wide_TB_BA_20)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_TB_BA_20 <- Wide_TB_BA_20[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_20=paste(year,rainfall_reduction,livestock_util_2020,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_TB_BA_20 = data.frame(MDS1 = BC_TB_BA_20$points[,1], MDS2 = BC_TB_BA_20$points[,2],group=BC_Meta_Data_TB_BA_20$Yr_Dr_Gr_20)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_TB_BA_20 <- cbind(BC_Meta_Data_TB_BA_20,BC_NMDS_TB_BA_20)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_TB_BA_20<-ordiellipse(BC_TB_BA_20, BC_Meta_Data_TB_BA_20$Yr_Dr_Gr_20, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_TB_BA_20 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_TB_BA_20$group)){
  BC_Ellipses_TB_BA_20 <- rbind(BC_Ellipses_TB_BA_20, cbind(as.data.frame(with(BC_NMDS_TB_BA_20[BC_NMDS_TB_BA_20$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_TB_BA_20[[g]]$cov,BC_Ord_Ellipses_TB_BA_20[[g]]$center,BC_Ord_Ellipses_TB_BA_20[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis TB Basal 2021 ####

#Species Comp TB: Basal 2021
Wide_TB_BA_21<-RelCov_Clean%>%
  filter(site=="TB" & aerial_basal=="Basal" & year=="2021") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp TB: Basal 
BC_TB_BA_21<-metaMDS(Wide_TB_BA_21[,16:152])
#look at species signiciance driving NMDS 
intrinsics_TB_BA_21 <- envfit(BC_TB_BA_21, Wide_TB_BA_21, permutations = 999,na.rm=T)
head(intrinsics_TB_BA_21)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_TB_BA_21 <- 1:nrow(Wide_TB_BA_21)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_TB_BA_21 <- Wide_TB_BA_21[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_21=paste(year,rainfall_reduction,livestock_util_2021,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_TB_BA_21 = data.frame(MDS1 = BC_TB_BA_21$points[,1], MDS2 = BC_TB_BA_21$points[,2],group=BC_Meta_Data_TB_BA_21$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_TB_BA_21 <- cbind(BC_Meta_Data_TB_BA_21,BC_NMDS_TB_BA_21)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_TB_BA_21<-ordiellipse(BC_TB_BA_21, BC_Meta_Data_TB_BA_21$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_TB_BA_21 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_TB_BA_21$group)){
  BC_Ellipses_TB_BA_21 <- rbind(BC_Ellipses_TB_BA_21, cbind(as.data.frame(with(BC_NMDS_TB_BA_21[BC_NMDS_TB_BA_21$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_TB_BA_21[[g]]$cov,BC_Ord_Ellipses_TB_BA_21[[g]]$center,BC_Ord_Ellipses_TB_BA_21[[g]]$scale)))
                                                            ,group=g))
}

#### Bray Curtis TB Basal 2022 ####

#Species Comp TB: Basal 2022
Wide_TB_BA_22<-RelCov_Clean%>%
  filter(site=="TB" & aerial_basal=="Basal" & year=="2022") 

#### Make new data frame called BC_Data and run an NMDS for each grouping

#Species Comp TB: Basal 
BC_TB_BA_22<-metaMDS(Wide_TB_BA_22[,16:152])
#look at species signiciance driving NMDS 
intrinsics_TB_BA_22 <- envfit(BC_TB_BA_22, Wide_TB_BA_22, permutations = 999,na.rm=T)
head(intrinsics_TB_BA_22)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites_TB_BA_22 <- 1:nrow(Wide_TB_BA_22)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-15
BC_Meta_Data_TB_BA_22 <- Wide_TB_BA_22[,1:15] %>% 
  mutate(Yr_Dr_Gr=paste(year,rainfall_reduction,grazing_treatment,sep=".")) %>% 
  mutate(Yr_Dr_Gr_22=paste(year,rainfall_reduction,livestock_util_2020,sep="."))


## Create NMDS numbers Graph ##
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_TB_BA_22 = data.frame(MDS1 = BC_TB_BA_22$points[,1], MDS2 = BC_TB_BA_22$points[,2],group=BC_Meta_Data_TB_BA_22$Yr_Dr_Gr)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_Graph_TB_BA_22 <- cbind(BC_Meta_Data_TB_BA_22,BC_NMDS_TB_BA_22)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_TB_BA_22<-ordiellipse(BC_TB_BA_22, BC_Meta_Data_TB_BA_22$Yr_Dr_Gr, display = "sites",
                                      kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_TB_BA_22 <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_TB_BA_22$group)){
  BC_Ellipses_TB_BA_22 <- rbind(BC_Ellipses_TB_BA_22, cbind(as.data.frame(with(BC_NMDS_TB_BA_22[BC_NMDS_TB_BA_22$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_TB_BA_22[[g]]$cov,BC_Ord_Ellipses_TB_BA_22[[g]]$center,BC_Ord_Ellipses_TB_BA_22[[g]]$scale)))
                                                            ,group=g))
}


#### PERMANOVA FK Aerial 2018 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_FK_AR_18 <- Wide_FK_AR_18[,16:ncol(Wide_FK_AR_18)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_FK_AR_18 <- Wide_FK_AR_18[,1:15]

Environment_Matrix_FK_AR_18$block=as.numeric(Environment_Matrix_FK_AR_18$block)
Environment_Matrix_FK_AR_18$slope=as.numeric(Environment_Matrix_FK_AR_18$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_FK_AR_18 <- adonis2(formula = Species_Matrix_FK_AR_18~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_FK_AR_18,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_FK_AR_18) #NS

#### PERMDISP FK Aerial 2018 ####

FK_AR_18<-Wide_FK_AR_18 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_AR_18 <- vegdist(Species_Matrix_FK_AR_18)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_AR_18_Dr <- betadisper(BC_Distance_Matrix_FK_AR_18,FK_AR_18$rainfall_reduction)
permutest(Dispersion_FK_AR_18_Dr,pairwise = T, permutations = 999) #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_FK_AR_18_GR <- betadisper(BC_Distance_Matrix_FK_AR_18,FK_AR_18$grazing_treatment)
permutest(Dispersion_FK_AR_18_GR,pairwise = T, permutations = 999) #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_FK_AR_18_DR_GR <- betadisper(BC_Distance_Matrix_FK_AR_18,FK_AR_18$Dr_Gr)
permutest(Dispersion_FK_AR_18_GR,pairwise = T, permutations = 999) #ns


#### PERMANOVA FK Aerial 2019 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_FK_AR_19 <- Wide_FK_AR_19[,16:ncol(Wide_FK_AR_19)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_FK_AR_19 <- Wide_FK_AR_19[,1:15]

Environment_Matrix_FK_AR_19$block=as.numeric(Environment_Matrix_FK_AR_19$block)
Environment_Matrix_FK_AR_19$slope=as.numeric(Environment_Matrix_FK_AR_19$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_FK_AR_19 <- adonis2(formula = Species_Matrix_FK_AR_19~rainfall_reduction + (1|block) + (1|block/slope) , data=Environment_Matrix_FK_AR_19,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_FK_AR_19) #NS

#### PERMDISP FK Aerial 2019 ####

FK_AR_19<-Wide_FK_AR_19 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_AR_19 <- vegdist(Species_Matrix_FK_AR_19)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_AR_19_Dr <- betadisper(BC_Distance_Matrix_FK_AR_19,FK_AR_19$rainfall_reduction)
permutest(Dispersion_FK_AR_19_Dr,pairwise = T, permutations = 999) 

#### PERMANOVA FK Aerial 2020 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_FK_AR_20 <- Wide_FK_AR_20[,16:ncol(Wide_FK_AR_20)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_FK_AR_20 <- Wide_FK_AR_20[,1:15]

Environment_Matrix_FK_AR_20$block=as.numeric(Environment_Matrix_FK_AR_20$block)
Environment_Matrix_FK_AR_20$slope=as.numeric(Environment_Matrix_FK_AR_20$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_FK_AR_20 <- adonis2(formula = Species_Matrix_FK_AR_20~rainfall_reduction*livestock_util_2019 + (1|block) + (1|block/slope) , data=Environment_Matrix_FK_AR_20,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_FK_AR_20) #NS

#### PERMDISP FK Aerial 2020 ####

FK_AR_20<-Wide_FK_AR_20 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_")) %>% 
  mutate(Dr_Gr19=paste(rainfall_reduction,livestock_util_2019,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_AR_20 <- vegdist(Species_Matrix_FK_AR_20)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_AR_20_Dr <- betadisper(BC_Distance_Matrix_FK_AR_20,FK_AR_20$rainfall_reduction)
permutest(Dispersion_FK_AR_20_Dr,pairwise = T, permutations = 999) #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_FK_AR_20_GR <- betadisper(BC_Distance_Matrix_FK_AR_20,FK_AR_20$livestock_util_2019)
permutest(Dispersion_FK_AR_20_GR,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_FK_AR_20_DR_GR <- betadisper(BC_Distance_Matrix_FK_AR_20,FK_AR_20$Dr_Gr19)
permutest(Dispersion_FK_AR_20_GR,pairwise = T, permutations = 999)  #ns

#### PERMANOVA FK Aerial 2021 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_FK_AR_21 <- Wide_FK_AR_21[,16:ncol(Wide_FK_AR_21)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_FK_AR_21 <- Wide_FK_AR_21[,1:15]

Environment_Matrix_FK_AR_21$block=as.numeric(Environment_Matrix_FK_AR_21$block)
Environment_Matrix_FK_AR_21$slope=as.numeric(Environment_Matrix_FK_AR_21$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_FK_AR_21 <- adonis2(formula = Species_Matrix_FK_AR_21~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_FK_AR_21,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_FK_AR_21) #drought (0.02)

#### PERMDISP FK Aerial 2021 ####

FK_AR_21<-Wide_FK_AR_21 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_AR_21 <- vegdist(Species_Matrix_FK_AR_21)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_AR_21_Dr <- betadisper(BC_Distance_Matrix_FK_AR_21,FK_AR_21$rainfall_reduction)
permutest(Dispersion_FK_AR_21_Dr,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_FK_AR_21_GR <- betadisper(BC_Distance_Matrix_FK_AR_21,FK_AR_21$grazing_treatment)
permutest(Dispersion_FK_AR_21_GR,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_FK_AR_21_DR_GR <- betadisper(BC_Distance_Matrix_FK_AR_21,FK_AR_21$Dr_Gr)
permutest(Dispersion_FK_AR_21_GR,pairwise = T, permutations = 999)  #ns


#### PERMANOVA FK Aerial 2022 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_FK_AR_22 <- Wide_FK_AR_22[,16:ncol(Wide_FK_AR_22)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_FK_AR_22 <- Wide_FK_AR_22[,1:15]

Environment_Matrix_FK_AR_22$block=as.numeric(Environment_Matrix_FK_AR_22$block)
Environment_Matrix_FK_AR_22$slope=as.numeric(Environment_Matrix_FK_AR_22$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_FK_AR_22 <- adonis2(formula = Species_Matrix_FK_AR_22~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_FK_AR_22,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_FK_AR_22) #drought (0.004)

#### PERMDISP FK Aerial 2022 ####

FK_AR_22<-Wide_FK_AR_22 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_AR_22 <- vegdist(Species_Matrix_FK_AR_22)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_AR_22_Dr <- betadisper(BC_Distance_Matrix_FK_AR_22,FK_AR_22$rainfall_reduction)
permutest(Dispersion_FK_AR_22_Dr,pairwise = T, permutations = 999) #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_FK_AR_22_GR <- betadisper(BC_Distance_Matrix_FK_AR_22,FK_AR_22$grazing_treatment)
permutest(Dispersion_FK_AR_22_GR,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_FK_AR_22_DR_GR <- betadisper(BC_Distance_Matrix_FK_AR_22,FK_AR_22$Dr_Gr)
permutest(Dispersion_FK_AR_22_GR,pairwise = T, permutations = 999)  #ns


#### PERMANOVA FK Basal 2018 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_FK_BA_18 <- Wide_FK_BA_18[,16:ncol(Wide_FK_BA_18)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_FK_BA_18 <- Wide_FK_BA_18[,1:15]

Environment_Matrix_FK_BA_18$block=as.numeric(Environment_Matrix_FK_BA_18$block)
Environment_Matrix_FK_BA_18$slope=as.numeric(Environment_Matrix_FK_BA_18$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_FK_BA_18 <- adonis2(formula = Species_Matrix_FK_BA_18~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_FK_BA_18,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_FK_BA_18) #NS

#### PERMDISP FK Basal 2018 ####

FK_BA_18<-Wide_FK_BA_18 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_BA_18 <- vegdist(Species_Matrix_FK_BA_18)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_BA_18_Dr <- betadisper(BC_Distance_Matrix_FK_BA_18,FK_BA_18$rainfall_reduction)
permutest(Dispersion_FK_BA_18_Dr,pairwise = T, permutations = 999)  #NS

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_FK_BA_18_GR <- betadisper(BC_Distance_Matrix_FK_BA_18,FK_BA_18$grazing_treatment)
permutest(Dispersion_FK_BA_18_GR,pairwise = T, permutations = 999)  #NS

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_FK_BA_18_DR_GR <- betadisper(BC_Distance_Matrix_FK_BA_18,FK_BA_18$Dr_Gr)
permutest(Dispersion_FK_BA_18_GR,pairwise = T, permutations = 999)  #NS


#### PERMANOVA FK Basal 2019 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_FK_BA_19 <- Wide_FK_BA_19[,16:ncol(Wide_FK_BA_19)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_FK_BA_19 <- Wide_FK_BA_19[,1:15]

Environment_Matrix_FK_BA_19$block=as.numeric(Environment_Matrix_FK_BA_19$block)
Environment_Matrix_FK_BA_19$slope=as.numeric(Environment_Matrix_FK_BA_19$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_FK_BA_19 <- adonis2(formula = Species_Matrix_FK_BA_19~rainfall_reduction + (1|block) + (1|block/slope) , data=Environment_Matrix_FK_BA_19,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_FK_BA_19) #NS

#### PERMDISP FK Basal 2019 ####

FK_BA_19<-Wide_FK_BA_19 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_BA_19 <- vegdist(Species_Matrix_FK_BA_19)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_BA_19_Dr <- betadisper(BC_Distance_Matrix_FK_BA_19,FK_BA_19$rainfall_reduction)
permutest(Dispersion_FK_BA_19_Dr,pairwise = T, permutations = 999) #NS

#### PERMANOVA FK Basal 2020 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_FK_BA_20 <- Wide_FK_BA_20[,16:ncol(Wide_FK_BA_20)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_FK_BA_20 <- Wide_FK_BA_20[,1:15]

Environment_Matrix_FK_BA_20$block=as.numeric(Environment_Matrix_FK_BA_20$block)
Environment_Matrix_FK_BA_20$slope=as.numeric(Environment_Matrix_FK_BA_20$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_FK_BA_20 <- adonis2(formula = Species_Matrix_FK_BA_20~rainfall_reduction*livestock_util_2019 + (1|block) + (1|block/slope) , data=Environment_Matrix_FK_BA_20,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_FK_BA_20) #NS

#### PERMDISP FK Basal 2020 ####

FK_BA_20<-Wide_FK_BA_20 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_")) %>% 
  mutate(Dr_Gr19=paste(rainfall_reduction,livestock_util_2019,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_BA_20 <- vegdist(Species_Matrix_FK_BA_20)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_BA_20_Dr <- betadisper(BC_Distance_Matrix_FK_BA_20,FK_BA_20$rainfall_reduction)
permutest(Dispersion_FK_BA_20_Dr,pairwise = T, permutations = 999) #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_FK_BA_20_GR <- betadisper(BC_Distance_Matrix_FK_BA_20,FK_BA_20$livestock_util_2019)
permutest(Dispersion_FK_BA_20_GR,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_FK_BA_20_DR_GR <- betadisper(BC_Distance_Matrix_FK_BA_20,FK_BA_20$Dr_Gr19)
permutest(Dispersion_FK_BA_20_GR,pairwise = T, permutations = 999)  #ns

#### PERMANOVA FK Basal 2021 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_FK_BA_21 <- Wide_FK_BA_21[,16:ncol(Wide_FK_BA_21)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_FK_BA_21 <- Wide_FK_BA_21[,1:15]

Environment_Matrix_FK_BA_21$block=as.numeric(Environment_Matrix_FK_BA_21$block)
Environment_Matrix_FK_BA_21$slope=as.numeric(Environment_Matrix_FK_BA_21$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_FK_BA_21 <- adonis2(formula = Species_Matrix_FK_BA_21~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_FK_BA_21,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_FK_BA_21) #drought (0.008)

#### PERMDISP FK Basal 2021 ####

FK_BA_21<-Wide_FK_BA_21 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilBAity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_BA_21 <- vegdist(Species_Matrix_FK_BA_21)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_BA_21_Dr <- betadisper(BC_Distance_Matrix_FK_BA_21,FK_BA_21$rainfall_reduction)
permutest(Dispersion_FK_BA_21_Dr,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_FK_BA_21_GR <- betadisper(BC_Distance_Matrix_FK_BA_21,FK_BA_21$grazing_treatment)
permutest(Dispersion_FK_BA_21_GR,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_FK_BA_21_DR_GR <- betadisper(BC_Distance_Matrix_FK_BA_21,FK_BA_21$Dr_Gr)
permutest(Dispersion_FK_BA_21_GR,pairwise = T, permutations = 999)  #ns


#### PERMANOVA FK Basal 2022 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_FK_BA_22 <- Wide_FK_BA_22[,16:ncol(Wide_FK_BA_22)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_FK_BA_22 <- Wide_FK_BA_22[,1:15]

Environment_Matrix_FK_BA_22$block=as.numeric(Environment_Matrix_FK_BA_22$block)
Environment_Matrix_FK_BA_22$slope=as.numeric(Environment_Matrix_FK_BA_22$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_FK_BA_22 <- adonis2(formula = Species_Matrix_FK_BA_22~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_FK_BA_22,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_FK_BA_22) #drought (0.001)

#### PERMDISP FK Basal 2022 ####

FK_BA_22<-Wide_FK_BA_22 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilBAity of the Species_Matrix dataframe
BC_Distance_Matrix_FK_BA_22 <- vegdist(Species_Matrix_FK_BA_22)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_FK_BA_22_Dr <- betadisper(BC_Distance_Matrix_FK_BA_22,FK_BA_22$rainfall_reduction)
permutest(Dispersion_FK_BA_22_Dr,pairwise = T, permutations = 999) #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_FK_BA_22_GR <- betadisper(BC_Distance_Matrix_FK_BA_22,FK_BA_22$grazing_treatment)
permutest(Dispersion_FK_BA_22_GR,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_FK_BA_22_DR_GR <- betadisper(BC_Distance_Matrix_FK_BA_22,FK_BA_22$Dr_Gr)
permutest(Dispersion_FK_BA_22_GR,pairwise = T, permutations = 999)  #ns

#### PERMANOVA TB Aerial 2018 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_TB_AR_18 <- Wide_TB_AR_18[,16:ncol(Wide_TB_AR_18)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_TB_AR_18 <- Wide_TB_AR_18[,1:15]

Environment_Matrix_TB_AR_18$block=as.numeric(Environment_Matrix_TB_AR_18$block)
Environment_Matrix_TB_AR_18$slope=as.numeric(Environment_Matrix_TB_AR_18$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_TB_AR_18 <- adonis2(formula = Species_Matrix_TB_AR_18~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_TB_AR_18,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_TB_AR_18) #grazing (0.002)

#### PERMDISP TB Aerial 2018 ####

TB_AR_18<-Wide_TB_AR_18 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_AR_18 <- vegdist(Species_Matrix_TB_AR_18)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_AR_18_Dr <- betadisper(BC_Distance_Matrix_TB_AR_18,TB_AR_18$rainfall_reduction)
permutest(Dispersion_TB_AR_18_Dr,pairwise = T, permutations = 999) #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_TB_AR_18_GR <- betadisper(BC_Distance_Matrix_TB_AR_18,TB_AR_18$grazing_treatment)
permutest(Dispersion_TB_AR_18_GR,pairwise = T, permutations = 999)  #grazing (0.002)

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_TB_AR_18_DR_GR <- betadisper(BC_Distance_Matrix_TB_AR_18,TB_AR_18$Dr_Gr)
permutest(Dispersion_TB_AR_18_GR,pairwise = T, permutations = 999)  #DxG (0.001)


#### PERMANOVA TB Aerial 2019 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_TB_AR_19 <- Wide_TB_AR_19[,16:ncol(Wide_TB_AR_19)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_TB_AR_19 <- Wide_TB_AR_19[,1:15]

Environment_Matrix_TB_AR_19$block=as.numeric(Environment_Matrix_TB_AR_19$block)
Environment_Matrix_TB_AR_19$slope=as.numeric(Environment_Matrix_TB_AR_19$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_TB_AR_19 <- adonis2(formula = Species_Matrix_TB_AR_19~rainfall_reduction + (1|block) + (1|block/slope) , data=Environment_Matrix_TB_AR_19,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_TB_AR_19) #NS

#### PERMDISP TB Aerial 2019 ####

TB_AR_19<-Wide_TB_AR_19 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_AR_19 <- vegdist(Species_Matrix_TB_AR_19)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_AR_19_Dr <- betadisper(BC_Distance_Matrix_TB_AR_19,TB_AR_19$rainfall_reduction)
permutest(Dispersion_TB_AR_19_Dr,pairwise = T, permutations = 999) #ns


#### PERMANOVA TB Aerial 2020 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_TB_AR_20 <- Wide_TB_AR_20[,16:ncol(Wide_TB_AR_20)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_TB_AR_20 <- Wide_TB_AR_20[,1:15]

Environment_Matrix_TB_AR_20$block=as.numeric(Environment_Matrix_TB_AR_20$block)
Environment_Matrix_TB_AR_20$slope=as.numeric(Environment_Matrix_TB_AR_20$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_TB_AR_20 <- adonis2(formula = Species_Matrix_TB_AR_20~rainfall_reduction*livestock_util_2019 + (1|block) + (1|block/slope) , data=Environment_Matrix_TB_AR_20,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_TB_AR_20) #NS

#### PERMDISP TB Aerial 2020 ####

TB_AR_20<-Wide_TB_AR_20 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_")) %>% 
  mutate(Dr_Gr19=paste(rainfall_reduction,livestock_util_2019,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_AR_20 <- vegdist(Species_Matrix_TB_AR_20)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_AR_20_Dr <- betadisper(BC_Distance_Matrix_TB_AR_20,TB_AR_20$rainfall_reduction)
permutest(Dispersion_TB_AR_20_Dr,pairwise = T, permutations = 999) #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_TB_AR_20_GR <- betadisper(BC_Distance_Matrix_TB_AR_20,TB_AR_20$livestock_util_2019)
permutest(Dispersion_TB_AR_20_GR,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_TB_AR_20_DR_GR <- betadisper(BC_Distance_Matrix_TB_AR_20,TB_AR_20$Dr_Gr19)
permutest(Dispersion_TB_AR_20_GR,pairwise = T, permutations = 999)  #ns

#### PERMANOVA TB Aerial 2021 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_TB_AR_21 <- Wide_TB_AR_21[,16:ncol(Wide_TB_AR_21)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_TB_AR_21 <- Wide_TB_AR_21[,1:15]

Environment_Matrix_TB_AR_21$block=as.numeric(Environment_Matrix_TB_AR_21$block)
Environment_Matrix_TB_AR_21$slope=as.numeric(Environment_Matrix_TB_AR_21$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_TB_AR_21 <- adonis2(formula = Species_Matrix_TB_AR_21~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_TB_AR_21,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_TB_AR_21) #grazing (0.019)

#### PERMDISP TB Aerial 2021 ####

TB_AR_21<-Wide_TB_AR_21 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_AR_21 <- vegdist(Species_Matrix_TB_AR_21)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_AR_21_Dr <- betadisper(BC_Distance_Matrix_TB_AR_21,TB_AR_21$rainfall_reduction)
permutest(Dispersion_TB_AR_21_Dr,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_TB_AR_21_GR <- betadisper(BC_Distance_Matrix_TB_AR_21,TB_AR_21$grazing_treatment)
permutest(Dispersion_TB_AR_21_GR,pairwise = T, permutations = 999)  #0.025

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_TB_AR_21_DR_GR <- betadisper(BC_Distance_Matrix_TB_AR_21,TB_AR_21$Dr_Gr)
permutest(Dispersion_TB_AR_21_GR,pairwise = T, permutations = 999)  #0.017


#### PERMANOVA TB Aerial 2022 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_TB_AR_22 <- Wide_TB_AR_22[,16:ncol(Wide_TB_AR_22)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_TB_AR_22 <- Wide_TB_AR_22[,1:15]

Environment_Matrix_TB_AR_22$block=as.numeric(Environment_Matrix_TB_AR_22$block)
Environment_Matrix_TB_AR_22$slope=as.numeric(Environment_Matrix_TB_AR_22$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_TB_AR_22 <- adonis2(formula = Species_Matrix_TB_AR_22~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_TB_AR_22,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_TB_AR_22) #grazing (0.002)

#### PERMDISP TB Aerial 2022 ####

TB_AR_22<-Wide_TB_AR_22 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_AR_22 <- vegdist(Species_Matrix_TB_AR_22)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_AR_22_Dr <- betadisper(BC_Distance_Matrix_TB_AR_22,TB_AR_22$rainfall_reduction)
permutest(Dispersion_TB_AR_22_Dr,pairwise = T, permutations = 999) #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_TB_AR_22_GR <- betadisper(BC_Distance_Matrix_TB_AR_22,TB_AR_22$grazing_treatment)
permutest(Dispersion_TB_AR_22_GR,pairwise = T, permutations = 999)  #0.006

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_TB_AR_22_DR_GR <- betadisper(BC_Distance_Matrix_TB_AR_22,TB_AR_22$Dr_Gr)
permutest(Dispersion_TB_AR_22_GR,pairwise = T, permutations = 999)  #0.003


#### PERMANOVA TB Basal 2018 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_TB_BA_18 <- Wide_TB_BA_18[,16:ncol(Wide_TB_BA_18)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_TB_BA_18 <- Wide_TB_BA_18[,1:15]

Environment_Matrix_TB_BA_18$block=as.numeric(Environment_Matrix_TB_BA_18$block)
Environment_Matrix_TB_BA_18$slope=as.numeric(Environment_Matrix_TB_BA_18$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_TB_BA_18 <- adonis2(formula = Species_Matrix_TB_BA_18~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_TB_BA_18,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_TB_BA_18) #grazing (0.001)

#### PERMDISP TB Basal 2018 ####

TB_BA_18<-Wide_TB_BA_18 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_BA_18 <- vegdist(Species_Matrix_TB_BA_18)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_BA_18_Dr <- betadisper(BC_Distance_Matrix_TB_BA_18,TB_BA_18$rainfall_reduction)
permutest(Dispersion_TB_BA_18_Dr,pairwise = T, permutations = 999)  #NS

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_TB_BA_18_GR <- betadisper(BC_Distance_Matrix_TB_BA_18,TB_BA_18$grazing_treatment)
permutest(Dispersion_TB_BA_18_GR,pairwise = T, permutations = 999)  #0.001

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_TB_BA_18_DR_GR <- betadisper(BC_Distance_Matrix_TB_BA_18,TB_BA_18$Dr_Gr)
permutest(Dispersion_TB_BA_18_GR,pairwise = T, permutations = 999)  #0.001


#### PERMANOVA TB Basal 2019 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_TB_BA_19 <- Wide_TB_BA_19[,16:ncol(Wide_TB_BA_19)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_TB_BA_19 <- Wide_TB_BA_19[,1:15]

Environment_Matrix_TB_BA_19$block=as.numeric(Environment_Matrix_TB_BA_19$block)
Environment_Matrix_TB_BA_19$slope=as.numeric(Environment_Matrix_TB_BA_19$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_TB_BA_19 <- adonis2(formula = Species_Matrix_TB_BA_19~rainfall_reduction + (1|block) + (1|block/slope) , data=Environment_Matrix_TB_BA_19,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_TB_BA_19) #NS

#### PERMDISP TB Basal 2019 ####

TB_BA_19<-Wide_TB_BA_19 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_BA_19 <- vegdist(Species_Matrix_TB_BA_19)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_BA_19_Dr <- betadisper(BC_Distance_Matrix_TB_BA_19,TB_BA_19$rainfall_reduction)
permutest(Dispersion_TB_BA_19_Dr,pairwise = T, permutations = 999) #NS

#### PERMANOVA TB Basal 2020 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_TB_BA_20 <- Wide_TB_BA_20[,16:ncol(Wide_TB_BA_20)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_TB_BA_20 <- Wide_TB_BA_20[,1:15]

Environment_Matrix_TB_BA_20$block=as.numeric(Environment_Matrix_TB_BA_20$block)
Environment_Matrix_TB_BA_20$slope=as.numeric(Environment_Matrix_TB_BA_20$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_TB_BA_20 <- adonis2(formula = Species_Matrix_TB_BA_20~rainfall_reduction*livestock_util_2019 + (1|block) + (1|block/slope) , data=Environment_Matrix_TB_BA_20,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_TB_BA_20) #NS

#### PERMDISP TB Basal 2020 ####

TB_BA_20<-Wide_TB_BA_20 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_")) %>% 
  mutate(Dr_Gr19=paste(rainfall_reduction,livestock_util_2019,sep="_"))

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_BA_20 <- vegdist(Species_Matrix_TB_BA_20)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_BA_20_Dr <- betadisper(BC_Distance_Matrix_TB_BA_20,TB_BA_20$rainfall_reduction)
permutest(Dispersion_TB_BA_20_Dr,pairwise = T, permutations = 999) #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_TB_BA_20_GR <- betadisper(BC_Distance_Matrix_TB_BA_20,TB_BA_20$livestock_util_2019)
permutest(Dispersion_TB_BA_20_GR,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_TB_BA_20_DR_GR <- betadisper(BC_Distance_Matrix_TB_BA_20,TB_BA_20$Dr_Gr19)
permutest(Dispersion_TB_BA_20_GR,pairwise = T, permutations = 999)  #ns

#### PERMANOVA TB Basal 2021 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_TB_BA_21 <- Wide_TB_BA_21[,16:ncol(Wide_TB_BA_21)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_TB_BA_21 <- Wide_TB_BA_21[,1:15]

Environment_Matrix_TB_BA_21$block=as.numeric(Environment_Matrix_TB_BA_21$block)
Environment_Matrix_TB_BA_21$slope=as.numeric(Environment_Matrix_TB_BA_21$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_TB_BA_21 <- adonis2(formula = Species_Matrix_TB_BA_21~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_TB_BA_21,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_TB_BA_21) #grazing (0.016)

 #### PERMDISP TB Basal 2021 ####

TB_BA_21<-Wide_TB_BA_21 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilBAity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_BA_21 <- vegdist(Species_Matrix_TB_BA_21)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_BA_21_Dr <- betadisper(BC_Distance_Matrix_TB_BA_21,TB_BA_21$rainfall_reduction)
permutest(Dispersion_TB_BA_21_Dr,pairwise = T, permutations = 999)  #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_TB_BA_21_GR <- betadisper(BC_Distance_Matrix_TB_BA_21,TB_BA_21$grazing_treatment)
permutest(Dispersion_TB_BA_21_GR,pairwise = T, permutations = 999)  #0.02

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_TB_BA_21_DR_GR <- betadisper(BC_Distance_Matrix_TB_BA_21,TB_BA_21$Dr_Gr)
permutest(Dispersion_TB_BA_21_GR,pairwise = T, permutations = 999)  #0.021


#### PERMANOVA TB Basal 2022 ####

#Make a new dataframe with the data from Wide_Relative_Cover all columns
Species_Matrix_TB_BA_22 <- Wide_TB_BA_22[,16:ncol(Wide_TB_BA_22)] %>% 
  select_if(colSums(.) != 0)
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_TB_BA_22 <- Wide_TB_BA_22[,1:15]

Environment_Matrix_TB_BA_22$block=as.numeric(Environment_Matrix_TB_BA_22$block)
Environment_Matrix_TB_BA_22$slope=as.numeric(Environment_Matrix_TB_BA_22$slope)

#run a perMANOVA -- had to change "(1|block:slope)" to "(1|block/slope)" to make this work
PerMANOVA_TB_BA_22 <- adonis2(formula = Species_Matrix_TB_BA_22~rainfall_reduction*grazing_treatment + (1|block) + (1|block/slope) , data=Environment_Matrix_TB_BA_22,permutations = 999, method = "bray",type=3)
#give a print out of the PermMANOVA
print(PerMANOVA_TB_BA_22) #gazing (0.003)

#### PERMDISP TB Basal 2022 ####

TB_BA_22<-Wide_TB_BA_22 %>% 
  mutate(Dr_Gr=paste(rainfall_reduction,grazing_treatment,sep="_"))

#Make a new dataframe and calculate the dissimilBAity of the Species_Matrix dataframe
BC_Distance_Matrix_TB_BA_22 <- vegdist(Species_Matrix_TB_BA_22)
#Run a dissimilarity matrix (PermDisp) comparing drought
Dispersion_TB_BA_22_Dr <- betadisper(BC_Distance_Matrix_TB_BA_22,TB_BA_22$rainfall_reduction)
permutest(Dispersion_TB_BA_22_Dr,pairwise = T, permutations = 999) #ns

#Run a dissimilarity matrix (PermDisp) comparing grazing
Dispersion_TB_BA_22_GR <- betadisper(BC_Distance_Matrix_TB_BA_22,TB_BA_22$grazing_treatment)
permutest(Dispersion_TB_BA_22_GR,pairwise = T, permutations = 999)  #0.001

#Run a dissimilarity matrix (PermDisp) comparing grazing*Drought
Dispersion_TB_BA_22_DR_GR <- betadisper(BC_Distance_Matrix_TB_BA_22,TB_BA_22$Dr_Gr)
permutest(Dispersion_TB_BA_22_GR,pairwise = T, permutations = 999)  #0.002


#### Relative Cover of Functional Group ####

FG_RelCov<-Functional_Group_RelCov %>% 
  left_join(plot_layoutK) %>% 
  mutate(Relative_Cover=Relative_Cover/100)

#### Normality: FK Forbs ####

RelCov_Forb<-FG_RelCov %>% 
  filter(Functional_Group=="Forb")

#FK - Aerial - Relative_Cover: 2018 
#non transformed data
Norm_FK_18_RelCov_Forb_Ar <- lm(data = subset(RelCov_Forb, year == 2018 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_Forb_Ar) 
ols_test_normality(Norm_FK_18_RelCov_Forb_Ar) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_18_FK_AR=1/asin(Relative_Cover))

Norm_FK_18_RelCov_Forb_Ar_TF <- lm(data = subset(RelCov_Forb, year == 2018 & site== "FK"& aerial_basal=="Aerial"), RelCov_18_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_Forb_Ar_TF) 
ols_test_normality(Norm_FK_18_RelCov_Forb_Ar_TF) #still not normal but better

#FK - Aerial - Relative_Cover: 2019 
#non transformed data
Norm_FK_19_RelCov_Forb_Ar <- lm(data = subset(RelCov_Forb, year == 2019 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_Forb_Ar) 
ols_test_normality(Norm_FK_19_RelCov_Forb_Ar) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_19_FK_AR=log10(Relative_Cover))

Norm_FK_19_RelCov_Forb_Ar_TF <- lm(data = subset(RelCov_Forb, year == 2019 & site== "FK"& aerial_basal=="Aerial"), RelCov_19_FK_AR  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_Forb_Ar_TF) 
ols_test_normality(Norm_FK_19_RelCov_Forb_Ar_TF) #still not normal but better


#FK - Aerial - Relative_Cover: 2020 
#non transformed data
Norm_FK_20_RelCov_Forb_Ar <- lm(data = subset(RelCov_Forb, year == 2020 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_Forb_Ar) 
ols_test_normality(Norm_FK_20_RelCov_Forb_Ar) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_20_FK_AR=1/asin(Relative_Cover))

Norm_FK_20_RelCov_Forb_Ar_TF <- lm(data = subset(RelCov_Forb, year == 2020 & site== "FK"& aerial_basal=="Aerial"), RelCov_20_FK_AR  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_Forb_Ar_TF ) 
ols_test_normality(Norm_FK_20_RelCov_Forb_Ar_TF ) #asin makes data somewhat normal

#FK - Aerial - Relative_Cover: 2021 
#non transformed data
Norm_FK_21_RelCov_Forb_Ar <- lm(data = subset(RelCov_Forb, year == 2021 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_Forb_Ar) 
ols_test_normality(Norm_FK_21_RelCov_Forb_Ar) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_21_FK_AR=1/sqrt(Relative_Cover))

Norm_FK_21_RelCov_Forb_Ar_TF  <- lm(data = subset(RelCov_Forb, year == 2021 & site== "FK"& aerial_basal=="Aerial"), RelCov_21_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_Forb_Ar_TF) 
ols_test_normality(Norm_FK_21_RelCov_Forb_Ar_TF) #normal

#FK - Aerial - Relative_Cover: 2022 
#non transformed data
Norm_FK_22_RelCov_Forb_Ar <- lm(data = subset(RelCov_Forb, year == 2022 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_Forb_Ar) 
ols_test_normality(Norm_FK_22_RelCov_Forb_Ar) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_22_FK_AR=1/asin(Relative_Cover))

Norm_FK_22_RelCov_Forb_Ar_TF  <- lm(data = subset(RelCov_Forb, year == 2022 & site== "FK"& aerial_basal=="Aerial"), RelCov_22_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_Forb_Ar_TF) 
ols_test_normality(Norm_FK_22_RelCov_Forb_Ar_TF) #still not normal but better

#FK - Basal - Relative_Cover: 2018 
#non transformed data
Norm_FK_18_RelCov_Forb_Ba <- lm(data = subset(RelCov_Forb, year == 2018 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_Forb_Ba) 
ols_test_normality(Norm_FK_18_RelCov_Forb_Ba) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_18_FK_Ba=1/asin(Relative_Cover))

Norm_FK_18_RelCov_Forb_Ba_TF <- lm(data = subset(RelCov_Forb, year == 2018 & site== "FK"& aerial_basal=="Basal"), RelCov_18_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_Forb_Ba_TF) 
ols_test_normality(Norm_FK_18_RelCov_Forb_Ba_TF) #still not normal but better

#FK - Basal - Relative_Cover: 2019 
#non transformed data
Norm_FK_19_RelCov_Forb_Ba <- lm(data = subset(RelCov_Forb, year == 2019 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_Forb_Ba) 
ols_test_normality(Norm_FK_19_RelCov_Forb_Ba) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_19_FK_Ba=1/asin(Relative_Cover))

Norm_FK_19_RelCov_Forb_Ba_TF <- lm(data = subset(RelCov_Forb, year == 2019 & site== "FK"& aerial_basal=="Basal"), RelCov_19_FK_Ba  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_Forb_Ba_TF) 
ols_test_normality(Norm_FK_19_RelCov_Forb_Ba_TF) #still not normal but better


#FK - Basal - Relative_Cover: 2020 
#non transformed data
Norm_FK_20_RelCov_Forb_Ba <- lm(data = subset(RelCov_Forb, year == 2020 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_Forb_Ba) 
ols_test_normality(Norm_FK_20_RelCov_Forb_Ba) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_20_FK_Ba=1/asin(Relative_Cover))

Norm_FK_20_RelCov_Forb_Ba_TF <- lm(data = subset(RelCov_Forb, year == 2020 & site== "FK"& aerial_basal=="Basal"), RelCov_20_FK_Ba  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_Forb_Ba_TF) 
ols_test_normality(Norm_FK_20_RelCov_Forb_Ba_TF) #still not normal but better

#FK - Basal - Relative_Cover: 2021 
#non transformed data
Norm_FK_21_RelCov_Forb_Ba <- lm(data = subset(RelCov_Forb, year == 2021 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_Forb_Ba) 
ols_test_normality(Norm_FK_21_RelCov_Forb_Ba) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_21_FK_Ba=1/sqrt(Relative_Cover))

Norm_FK_21_RelCov_Forb_Ba_TF  <- lm(data = subset(RelCov_Forb, year == 2021 & site== "FK"& aerial_basal=="Basal"), RelCov_21_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_Forb_Ba_TF) 
ols_test_normality(Norm_FK_21_RelCov_Forb_Ba_TF) #normalish

#FK - Basal - Relative_Cover: 2022 
#non transformed data
Norm_FK_22_RelCov_Forb_Ba <- lm(data = subset(RelCov_Forb, year == 2022 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_Forb_Ba) 
ols_test_normality(Norm_FK_22_RelCov_Forb_Ba) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_22_FK_Ba=1/asin(Relative_Cover))

Norm_FK_22_RelCov_Forb_Ba_TF  <- lm(data = subset(RelCov_Forb, year == 2022 & site== "FK"& aerial_basal=="Basal"), RelCov_22_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_Forb_Ba_TF) 
ols_test_normality(Norm_FK_22_RelCov_Forb_Ba_TF) #still not normal but better

#### Stats: FK Forbs ####

#FK 2018 - checking drought and grazing
FK_18_Forb_Ar <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2018 & site== "FK" & aerial_basal=="Aerial"), RelCov_18_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Forb_Ar, type = 3) #ns

#FK 2019 - just drought
FK_19_Forb_Ar <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2019 & site== "FK" & aerial_basal=="Aerial"), RelCov_19_FK_AR ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_Forb_Ar, type = 3) #ns

#FK 2020 - droughtxgrazing
FK_20_Forb_Ar <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2020 & site== "FK" & aerial_basal=="Aerial"), RelCov_20_FK_AR ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_Forb_Ar, type = 3) #ns

#FK 2021- droughtxgrazing
FK_21_Forb_Ar <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2021 & site== "FK" & aerial_basal=="Aerial"), RelCov_21_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_Forb_Ar, type = 3) #drought (0.05), grazing (0.05)

#FK 2022- droughtxgrazing
FK_22_Forb_Ar <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2022 & site== "FK" & aerial_basal=="Aerial"), RelCov_22_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_Forb_Ar, type = 3) #DxG (0.004)

#Basal 
#FK 2018 - checking drought and grazing
FK_18_Forb_Ba <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2018 & site== "FK" & aerial_basal=="Basal"), RelCov_18_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Forb_Ba, type = 3) #grazing (0.04)

#FK 2019 - just drought
FK_19_Forb_Ba <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2019 & site== "FK" & aerial_basal=="Basal"), RelCov_19_FK_Ba ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_Forb_Ba, type = 3) #ns

#FK 2020 - droughtxgrazing
FK_20_Forb_Ba <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2020 & site== "FK" & aerial_basal=="Basal"), RelCov_20_FK_Ba ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_Forb_Ba, type = 3) #ns

#FK 2021- droughtxgrazing
FK_21_Forb_Ba <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2021 & site== "FK" & aerial_basal=="Basal"), RelCov_21_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_Forb_Ba, type = 3) # drought (3.5x10-6), grazing (0.0011)

#FK 2022- droughtxgrazing
FK_22_Forb_Ba <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2022 & site== "FK" & aerial_basal=="Basal"), RelCov_22_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_Forb_Ba, type = 3)  #  drought (0.05), grazing (0.04), DxG (2.394x10-6)


#### Normality: TB Forbs ####

#TB - Aerial - Relative_Cover: 2018 
#non transformed data
Norm_TB_18_RelCov_Forb_Ar <- lm(data = subset(RelCov_Forb, year == 2018 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_Forb_Ar) 
ols_test_normality(Norm_TB_18_RelCov_Forb_Ar) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_18_TB_AR=log10(Relative_Cover))

Norm_TB_18_RelCov_Forb_Ar_TF <- lm(data = subset(RelCov_Forb, year == 2018 & site== "TB"& aerial_basal=="Aerial"), RelCov_18_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_Forb_Ar_TF) 
ols_test_normality(Norm_TB_18_RelCov_Forb_Ar_TF) #still not normal but better

#TB - Aerial - Relative_Cover: 2019 
#non transformed data
Norm_TB_19_RelCov_Forb_Ar <- lm(data = subset(RelCov_Forb, year == 2019 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_Forb_Ar) 
ols_test_normality(Norm_TB_19_RelCov_Forb_Ar) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_19_TB_AR=1/(Relative_Cover))

Norm_TB_19_RelCov_Forb_Ar_TF <- lm(data = subset(RelCov_Forb, year == 2019 & site== "TB"& aerial_basal=="Aerial"), RelCov_19_TB_AR  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_Forb_Ar_TF) 
ols_test_normality(Norm_TB_19_RelCov_Forb_Ar_TF) #still not normal but better


#TB - Aerial - Relative_Cover: 2020 
#non transformed data
Norm_TB_20_RelCov_Forb_Ar <- lm(data = subset(RelCov_Forb, year == 2020 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_Forb_Ar) 
ols_test_normality(Norm_TB_20_RelCov_Forb_Ar) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_20_TB_AR=1/(Relative_Cover))

Norm_TB_20_RelCov_Forb_Ar_TF <- lm(data = subset(RelCov_Forb, year == 2020 & site== "TB"& aerial_basal=="Aerial"), RelCov_20_TB_AR  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_Forb_Ar_TF ) 
ols_test_normality(Norm_TB_20_RelCov_Forb_Ar_TF ) #not normal but better

#TB - Aerial - Relative_Cover: 2021 
#non transformed data
Norm_TB_21_RelCov_Forb_Ar <- lm(data = subset(RelCov_Forb, year == 2021 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_Forb_Ar) 
ols_test_normality(Norm_TB_21_RelCov_Forb_Ar) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_21_TB_AR=1/asin(Relative_Cover))

Norm_TB_21_RelCov_Forb_Ar_TF  <- lm(data = subset(RelCov_Forb, year == 2021 & site== "TB"& aerial_basal=="Aerial"), RelCov_21_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_Forb_Ar_TF) 
ols_test_normality(Norm_TB_21_RelCov_Forb_Ar_TF) #not normal but better

#TB - Aerial - Relative_Cover: 2022 
#non transformed data
Norm_TB_22_RelCov_Forb_Ar <- lm(data = subset(RelCov_Forb, year == 2022 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_Forb_Ar) 
ols_test_normality(Norm_TB_22_RelCov_Forb_Ar) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_22_TB_AR=1/asin(Relative_Cover))

Norm_TB_22_RelCov_Forb_Ar_TF  <- lm(data = subset(RelCov_Forb, year == 2022 & site== "TB"& aerial_basal=="Aerial"), RelCov_22_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_Forb_Ar_TF) 
ols_test_normality(Norm_TB_22_RelCov_Forb_Ar_TF) #still not normal but better

#TB - Basal - Relative_Cover: 2018 
#non transformed data
Norm_TB_18_RelCov_Forb_Ba <- lm(data = subset(RelCov_Forb, year == 2018 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_Forb_Ba) 
ols_test_normality(Norm_TB_18_RelCov_Forb_Ba) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_18_TB_Ba=1/asin(Relative_Cover))

Norm_TB_18_RelCov_Forb_Ba_TF <- lm(data = subset(RelCov_Forb, year == 2018 & site== "TB"& aerial_basal=="Basal"), RelCov_18_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_Forb_Ba_TF) 
ols_test_normality(Norm_TB_18_RelCov_Forb_Ba_TF) #still not normal but better

#TB - Basal - Relative_Cover: 2019 
#non transformed data
Norm_TB_19_RelCov_Forb_Ba <- lm(data = subset(RelCov_Forb, year == 2019 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_Forb_Ba) 
ols_test_normality(Norm_TB_19_RelCov_Forb_Ba) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_19_TB_Ba=1/asin(Relative_Cover))

Norm_TB_19_RelCov_Forb_Ba_TF <- lm(data = subset(RelCov_Forb, year == 2019 & site== "TB"& aerial_basal=="Basal"), RelCov_19_TB_Ba  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_Forb_Ba_TF) 
ols_test_normality(Norm_TB_19_RelCov_Forb_Ba_TF) #still not normal but better


#TB - Basal - Relative_Cover: 2020 
#non transformed data
Norm_TB_20_RelCov_Forb_Ba <- lm(data = subset(RelCov_Forb, year == 2020 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_Forb_Ba) 
ols_test_normality(Norm_TB_20_RelCov_Forb_Ba) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_20_TB_Ba=1/asin(Relative_Cover))

Norm_TB_20_RelCov_Forb_Ba_TF <- lm(data = subset(RelCov_Forb, year == 2020 & site== "TB"& aerial_basal=="Basal"), RelCov_20_TB_Ba  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_Forb_Ba_TF) 
ols_test_normality(Norm_TB_20_RelCov_Forb_Ba_TF) #still not normal but better

#TB - Basal - Relative_Cover: 2021 
#non transformed data
Norm_TB_21_RelCov_Forb_Ba <- lm(data = subset(RelCov_Forb, year == 2021 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_Forb_Ba) 
ols_test_normality(Norm_TB_21_RelCov_Forb_Ba) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_21_TB_Ba=1/sqrt(Relative_Cover))

Norm_TB_21_RelCov_Forb_Ba_TF  <- lm(data = subset(RelCov_Forb, year == 2021 & site== "TB"& aerial_basal=="Basal"), RelCov_21_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_Forb_Ba_TF) 
ols_test_normality(Norm_TB_21_RelCov_Forb_Ba_TF) #normalish

#TB - Basal - Relative_Cover: 2022 
#non transformed data
Norm_TB_22_RelCov_Forb_Ba <- lm(data = subset(RelCov_Forb, year == 2022 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_Forb_Ba) 
ols_test_normality(Norm_TB_22_RelCov_Forb_Ba) #right skewed

#Transform Data
RelCov_Forb<-RelCov_Forb %>% 
  mutate(RelCov_22_TB_Ba=1/asin(Relative_Cover))

Norm_TB_22_RelCov_Forb_Ba_TF  <- lm(data = subset(RelCov_Forb, year == 2022 & site== "TB"& aerial_basal=="Basal"), RelCov_22_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_Forb_Ba_TF) 
ols_test_normality(Norm_TB_22_RelCov_Forb_Ba_TF) #still not normal but better

#### Stats: TB Forb ####

#TB 2018 - checking drought and grazing
TB_18_Forb_Ar <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2018 & site== "TB" & aerial_basal=="Aerial"), RelCov_18_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_Forb_Ar, type = 3) #ns

#TB 2019 - just drought
TB_19_Forb_Ar <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2019 & site== "TB" & aerial_basal=="Aerial"), RelCov_19_TB_AR ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_Forb_Ar, type = 3) #ns

#TB 2020 - droughtxgrazing
TB_20_Forb_Ar <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2020 & site== "TB" & aerial_basal=="Aerial"), RelCov_20_TB_AR ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_Forb_Ar, type = 3) #grazing (0.007)

#TB 2021- droughtxgrazing
TB_21_Forb_Ar <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2021 & site== "TB" & aerial_basal=="Aerial"), RelCov_21_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_Forb_Ar, type = 3) #drought (2.037x10-07), dxG (0.003)

#TB 2022- droughtxgrazing
TB_22_Forb_Ar <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2022 & site== "TB" & aerial_basal=="Aerial"), RelCov_22_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_Forb_Ar, type = 3) #grazing (0.008)

#Basal 
#TB 2018 - checking drought and grazing
TB_18_Forb_Ba <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2018 & site== "TB" & aerial_basal=="Basal"), RelCov_18_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_Forb_Ba, type = 3) #drought (0.04), Grazing (8.4x10-07)

#TB 2019 - just drought
TB_19_Forb_Ba <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2019 & site== "TB" & aerial_basal=="Basal"), RelCov_19_TB_Ba ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_Forb_Ba, type = 3) #ns

#TB 2020 - droughtxgrazing
TB_20_Forb_Ba <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2020 & site== "TB" & aerial_basal=="Basal"), RelCov_20_TB_Ba ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_Forb_Ba, type = 3) #grazing (4.05x10-09)

#TB 2021- droughtxgrazing
TB_21_Forb_Ba <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2021 & site== "TB" & aerial_basal=="Basal"), RelCov_21_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_Forb_Ba, type = 3) # drought (2.7x10-08), grazing(1.74x10-05), dxg (9.146x10-07)

#TB 2022- droughtxgrazing
TB_22_Forb_Ba <- lmerTest::lmer(data = subset(RelCov_Forb, year == 2022 & site== "TB" & aerial_basal=="Basal"), RelCov_22_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_Forb_Ba, type = 3)  #  grazing (1.423-06), DxG (0.02)


#### Normality: FK Woody ####

RelCov_Woody<-FG_RelCov %>% 
  filter(Functional_Group=="Woody") 

#FK - Aerial - Relative_Cover: 2018 
#non transformed data
Norm_FK_18_RelCov_Woody_Ar <- lm(data = subset(RelCov_Woody, year == 2018 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_Woody_Ar) 
ols_test_normality(Norm_FK_18_RelCov_Woody_Ar) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_18_FK_AR=1/asin(Relative_Cover))

Norm_FK_18_RelCov_Woody_Ar_TF <- lm(data = subset(RelCov_Woody, year == 2018 & site== "FK"& aerial_basal=="Aerial"), RelCov_18_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_Woody_Ar_TF) 
ols_test_normality(Norm_FK_18_RelCov_Woody_Ar_TF) #still not normal but better

#FK - Aerial - Relative_Cover: 2019 
#non transformed data
Norm_FK_19_RelCov_Woody_Ar <- lm(data = subset(RelCov_Woody, year == 2019 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_Woody_Ar) 
ols_test_normality(Norm_FK_19_RelCov_Woody_Ar) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_19_FK_AR=1/sqrt(Relative_Cover))

Norm_FK_19_RelCov_Woody_Ar_TF <- lm(data = subset(RelCov_Woody, year == 2019 & site== "FK"& aerial_basal=="Aerial"), RelCov_19_FK_AR  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_Woody_Ar_TF) 
ols_test_normality(Norm_FK_19_RelCov_Woody_Ar_TF) #still not normal but better


#FK - Aerial - Relative_Cover: 2020 
#non transformed data
Norm_FK_20_RelCov_Woody_Ar <- lm(data = subset(RelCov_Woody, year == 2020 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_Woody_Ar) 
ols_test_normality(Norm_FK_20_RelCov_Woody_Ar) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_20_FK_AR=1/sqrt(Relative_Cover))

Norm_FK_20_RelCov_Woody_Ar_TF <- lm(data = subset(RelCov_Woody, year == 2020 & site== "FK"& aerial_basal=="Aerial"), RelCov_20_FK_AR  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_Woody_Ar_TF ) 
ols_test_normality(Norm_FK_20_RelCov_Woody_Ar_TF ) #still not normal but better

#FK - Aerial - Relative_Cover: 2021 
#non transformed data
Norm_FK_21_RelCov_Woody_Ar <- lm(data = subset(RelCov_Woody, year == 2021 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_Woody_Ar) 
ols_test_normality(Norm_FK_21_RelCov_Woody_Ar) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_21_FK_AR=1/sqrt(Relative_Cover))

Norm_FK_21_RelCov_Woody_Ar_TF  <- lm(data = subset(RelCov_Woody, year == 2021 & site== "FK"& aerial_basal=="Aerial"), RelCov_21_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_Woody_Ar_TF) 
ols_test_normality(Norm_FK_21_RelCov_Woody_Ar_TF) #normal 

#FK - Aerial - Relative_Cover: 2022 
#non transformed data
Norm_FK_22_RelCov_Woody_Ar <- lm(data = subset(RelCov_Woody, year == 2022 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_Woody_Ar) 
ols_test_normality(Norm_FK_22_RelCov_Woody_Ar) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_22_FK_AR=1/sqrt(Relative_Cover))

Norm_FK_22_RelCov_Woody_Ar_TF  <- lm(data = subset(RelCov_Woody, year == 2022 & site== "FK"& aerial_basal=="Aerial"), RelCov_22_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_Woody_Ar_TF) 
ols_test_normality(Norm_FK_22_RelCov_Woody_Ar_TF) #normal

#FK - Basal - Relative_Cover: 2018 
#non transformed data
Norm_FK_18_RelCov_Woody_Ba <- lm(data = subset(RelCov_Woody, year == 2018 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_Woody_Ba) 
ols_test_normality(Norm_FK_18_RelCov_Woody_Ba) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_18_FK_Ba=1/asin(Relative_Cover))

Norm_FK_18_RelCov_Woody_Ba_TF <- lm(data = subset(RelCov_Woody, year == 2018 & site== "FK"& aerial_basal=="Basal"), RelCov_18_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_Woody_Ba_TF) 
ols_test_normality(Norm_FK_18_RelCov_Woody_Ba_TF) #normalish

#FK - Basal - Relative_Cover: 2019 
#non transformed data
Norm_FK_19_RelCov_Woody_Ba <- lm(data = subset(RelCov_Woody, year == 2019 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_Woody_Ba) 
ols_test_normality(Norm_FK_19_RelCov_Woody_Ba) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_19_FK_Ba=1/sqrt(Relative_Cover))

Norm_FK_19_RelCov_Woody_Ba_TF <- lm(data = subset(RelCov_Woody, year == 2019 & site== "FK"& aerial_basal=="Basal"), RelCov_19_FK_Ba  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_Woody_Ba_TF) 
ols_test_normality(Norm_FK_19_RelCov_Woody_Ba_TF) #normalish


#FK - Basal - Relative_Cover: 2020 
#non transformed data
Norm_FK_20_RelCov_Woody_Ba <- lm(data = subset(RelCov_Woody, year == 2020 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_Woody_Ba) 
ols_test_normality(Norm_FK_20_RelCov_Woody_Ba) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_20_FK_Ba=1/asin(Relative_Cover))

Norm_FK_20_RelCov_Woody_Ba_TF <- lm(data = subset(RelCov_Woody, year == 2020 & site== "FK"& aerial_basal=="Basal"), RelCov_20_FK_Ba  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_Woody_Ba_TF) 
ols_test_normality(Norm_FK_20_RelCov_Woody_Ba_TF) #still not normal but better

#FK - Basal - Relative_Cover: 2021 
#non transformed data
Norm_FK_21_RelCov_Woody_Ba <- lm(data = subset(RelCov_Woody, year == 2021 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_Woody_Ba) 
ols_test_normality(Norm_FK_21_RelCov_Woody_Ba) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_21_FK_Ba=sqrt(Relative_Cover))

Norm_FK_21_RelCov_Woody_Ba_TF  <- lm(data = subset(RelCov_Woody, year == 2021 & site== "FK"& aerial_basal=="Basal"), RelCov_21_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_Woody_Ba_TF) 
ols_test_normality(Norm_FK_21_RelCov_Woody_Ba_TF) #normal

#FK - Basal - Relative_Cover: 2022 
#non transformed data
Norm_FK_22_RelCov_Woody_Ba <- lm(data = subset(RelCov_Woody, year == 2022 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_Woody_Ba) 
ols_test_normality(Norm_FK_22_RelCov_Woody_Ba) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_22_FK_Ba=1/asin(Relative_Cover))

Norm_FK_22_RelCov_Woody_Ba_TF  <- lm(data = subset(RelCov_Woody, year == 2022 & site== "FK"& aerial_basal=="Basal"), RelCov_22_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_Woody_Ba_TF) 
ols_test_normality(Norm_FK_22_RelCov_Woody_Ba_TF) #normal

#### Stats: FK Woodys ####

#FK 2018 - checking drought and grazing
FK_18_Woody_Ar <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2018 & site== "FK" & aerial_basal=="Aerial"), RelCov_18_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Woody_Ar, type = 3) #ns

#FK 2019 - just drought
FK_19_Woody_Ar <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2019 & site== "FK" & aerial_basal=="Aerial"), RelCov_19_FK_AR ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_Woody_Ar, type = 3) #ns

#FK 2020 - droughtxgrazing
FK_20_Woody_Ar <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2020 & site== "FK" & aerial_basal=="Aerial"), RelCov_20_FK_AR ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_Woody_Ar, type = 3) #ns

#FK 2021- droughtxgrazing
FK_21_Woody_Ar <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2021 & site== "FK" & aerial_basal=="Aerial"), RelCov_21_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_Woody_Ar, type = 3) #ns

#FK 2022- droughtxgrazing
FK_22_Woody_Ar <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2022 & site== "FK" & aerial_basal=="Aerial"), RelCov_22_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_Woody_Ar, type = 3) #ns

#Basal 
#FK 2018 - checking drought and grazing
FK_18_Woody_Ba <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2018 & site== "FK" & aerial_basal=="Basal"), RelCov_18_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Woody_Ba, type = 3) #ns

#FK 2019 - just drought
FK_19_Woody_Ba <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2019 & site== "FK" & aerial_basal=="Basal"), RelCov_19_FK_Ba ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_Woody_Ba, type = 3) #ns

#FK 2020 - droughtxgrazing
FK_20_Woody_Ba <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2020 & site== "FK" & aerial_basal=="Basal"), RelCov_20_FK_Ba ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_Woody_Ba, type = 3) #ns

#FK 2021- droughtxgrazing
FK_21_Woody_Ba <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2021 & site== "FK" & aerial_basal=="Basal"), RelCov_21_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_Woody_Ba, type = 3) #ns

#FK 2022- droughtxgrazing
FK_22_Woody_Ba <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2022 & site== "FK" & aerial_basal=="Basal"), RelCov_22_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_Woody_Ba, type = 3)  #ns

#### Normality: TB Woody ####

#TB - Aerial - Relative_Cover: 2018 
#non transformed data
Norm_TB_18_RelCov_Woody_Ar <- lm(data = subset(RelCov_Woody, year == 2018 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_Woody_Ar) 
ols_test_normality(Norm_TB_18_RelCov_Woody_Ar) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_18_TB_AR=log(Relative_Cover))

Norm_TB_18_RelCov_Woody_Ar_TF <- lm(data = subset(RelCov_Woody, year == 2018 & site== "TB"& aerial_basal=="Aerial"), RelCov_18_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_Woody_Ar_TF) 
ols_test_normality(Norm_TB_18_RelCov_Woody_Ar_TF) #normal

#TB - Aerial - Relative_Cover: 2019 
#non transformed data
Norm_TB_19_RelCov_Woody_Ar <- lm(data = subset(RelCov_Woody, year == 2019 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_Woody_Ar) 
ols_test_normality(Norm_TB_19_RelCov_Woody_Ar) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_19_TB_AR=sqrt(Relative_Cover))

Norm_TB_19_RelCov_Woody_Ar_TF <- lm(data = subset(RelCov_Woody, year == 2019 & site== "TB"& aerial_basal=="Aerial"), RelCov_19_TB_AR  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_Woody_Ar_TF) 
ols_test_normality(Norm_TB_19_RelCov_Woody_Ar_TF) #normal


#TB - Aerial - Relative_Cover: 2020 
#non transformed data
Norm_TB_20_RelCov_Woody_Ar <- lm(data = subset(RelCov_Woody, year == 2020 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_Woody_Ar) 
ols_test_normality(Norm_TB_20_RelCov_Woody_Ar) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_20_TB_AR=log(Relative_Cover))

Norm_TB_20_RelCov_Woody_Ar_TF <- lm(data = subset(RelCov_Woody, year == 2020 & site== "TB"& aerial_basal=="Aerial"), RelCov_20_TB_AR  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_Woody_Ar_TF ) 
ols_test_normality(Norm_TB_20_RelCov_Woody_Ar_TF ) #normal

#TB - Aerial - Relative_Cover: 2021 
#non transformed data
Norm_TB_21_RelCov_Woody_Ar <- lm(data = subset(RelCov_Woody, year == 2021 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_Woody_Ar) 
ols_test_normality(Norm_TB_21_RelCov_Woody_Ar) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_21_TB_AR=1/sqrt(Relative_Cover))

Norm_TB_21_RelCov_Woody_Ar_TF  <- lm(data = subset(RelCov_Woody, year == 2021 & site== "TB"& aerial_basal=="Aerial"), RelCov_21_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_Woody_Ar_TF) 
ols_test_normality(Norm_TB_21_RelCov_Woody_Ar_TF) #normalish

#TB - Aerial - Relative_Cover: 2022 
#non transformed data
Norm_TB_22_RelCov_Woody_Ar <- lm(data = subset(RelCov_Woody, year == 2022 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_Woody_Ar) 
ols_test_normality(Norm_TB_22_RelCov_Woody_Ar) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_22_TB_AR=log10(Relative_Cover))

Norm_TB_22_RelCov_Woody_Ar_TF  <- lm(data = subset(RelCov_Woody, year == 2022 & site== "TB"& aerial_basal=="Aerial"), RelCov_22_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_Woody_Ar_TF) 
ols_test_normality(Norm_TB_22_RelCov_Woody_Ar_TF) #normal

#TB - Basal - Relative_Cover: 2018 
#non transformed data
Norm_TB_18_RelCov_Woody_Ba <- lm(data = subset(RelCov_Woody, year == 2018 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_Woody_Ba) 
ols_test_normality(Norm_TB_18_RelCov_Woody_Ba) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_18_TB_Ba=log(Relative_Cover))

Norm_TB_18_RelCov_Woody_Ba_TF <- lm(data = subset(RelCov_Woody, year == 2018 & site== "TB"& aerial_basal=="Basal"), RelCov_18_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_Woody_Ba_TF) 
ols_test_normality(Norm_TB_18_RelCov_Woody_Ba_TF) #normal

#TB - Basal - Relative_Cover: 2019 
#non transformed data
Norm_TB_19_RelCov_Woody_Ba <- lm(data = subset(RelCov_Woody, year == 2019 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_Woody_Ba) 
ols_test_normality(Norm_TB_19_RelCov_Woody_Ba) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_19_TB_Ba=log10(Relative_Cover))

Norm_TB_19_RelCov_Woody_Ba_TF <- lm(data = subset(RelCov_Woody, year == 2019 & site== "TB"& aerial_basal=="Basal"), RelCov_19_TB_Ba  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_Woody_Ba_TF) 
ols_test_normality(Norm_TB_19_RelCov_Woody_Ba_TF) #normalish


#TB - Basal - Relative_Cover: 2020 
#non transformed data
Norm_TB_20_RelCov_Woody_Ba <- lm(data = subset(RelCov_Woody, year == 2020 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_Woody_Ba) 
ols_test_normality(Norm_TB_20_RelCov_Woody_Ba) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_20_TB_Ba=log10(Relative_Cover))

Norm_TB_20_RelCov_Woody_Ba_TF <- lm(data = subset(RelCov_Woody, year == 2020 & site== "TB"& aerial_basal=="Basal"), RelCov_20_TB_Ba  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_Woody_Ba_TF) 
ols_test_normality(Norm_TB_20_RelCov_Woody_Ba_TF) #normal

#TB - Basal - Relative_Cover: 2021 
#non transformed data
Norm_TB_21_RelCov_Woody_Ba <- lm(data = subset(RelCov_Woody, year == 2021 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_Woody_Ba) 
ols_test_normality(Norm_TB_21_RelCov_Woody_Ba) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_21_TB_Ba=log(Relative_Cover))

Norm_TB_21_RelCov_Woody_Ba_TF  <- lm(data = subset(RelCov_Woody, year == 2021 & site== "TB"& aerial_basal=="Basal"), RelCov_21_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_Woody_Ba_TF) 
ols_test_normality(Norm_TB_21_RelCov_Woody_Ba_TF) #normal

#TB - Basal - Relative_Cover: 2022 
#non transformed data
Norm_TB_22_RelCov_Woody_Ba <- lm(data = subset(RelCov_Woody, year == 2022 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_Woody_Ba) 
ols_test_normality(Norm_TB_22_RelCov_Woody_Ba) #right skewed

#Transform Data
RelCov_Woody<-RelCov_Woody %>% 
  mutate(RelCov_22_TB_Ba=log(Relative_Cover))

Norm_TB_22_RelCov_Woody_Ba_TF  <- lm(data = subset(RelCov_Woody, year == 2022 & site== "TB"& aerial_basal=="Basal"), RelCov_22_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_Woody_Ba_TF) 
ols_test_normality(Norm_TB_22_RelCov_Woody_Ba_TF) #normal

#### Stats: TB Woodys ####

#TB 2018 - checking drought and grazing
TB_18_Woody_Ar <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2018 & site== "TB" & aerial_basal=="Aerial"), RelCov_18_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_Woody_Ar, type = 3) #grazing (0.005), DxG(0.018)

#TB 2019 - just drought
TB_19_Woody_Ar <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2019 & site== "TB" & aerial_basal=="Aerial"), RelCov_19_TB_AR ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_Woody_Ar, type = 3) #ns

#TB 2020 - droughtxgrazing
TB_20_Woody_Ar <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2020 & site== "TB" & aerial_basal=="Aerial"), RelCov_20_TB_AR ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_Woody_Ar, type = 3) #ns

#TB 2021- droughtxgrazing
TB_21_Woody_Ar <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2021 & site== "TB" & aerial_basal=="Aerial"), RelCov_21_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_Woody_Ar, type = 3) #drought (0.05), DxG(0.05)

#TB 2022- droughtxgrazing
TB_22_Woody_Ar <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2022 & site== "TB" & aerial_basal=="Aerial"), RelCov_22_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_Woody_Ar, type = 3) #ns

#Basal 
#TB 2018 - checking drought and grazing
TB_18_Woody_Ba <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2018 & site== "TB" & aerial_basal=="Basal"), RelCov_18_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_Woody_Ba, type = 3) #ns

#TB 2019 - just drought
TB_19_Woody_Ba <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2019 & site== "TB" & aerial_basal=="Basal"), RelCov_19_TB_Ba ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_Woody_Ba, type = 3) #ns

#TB 2020 - droughtxgrazing
TB_20_Woody_Ba <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2020 & site== "TB" & aerial_basal=="Basal"), RelCov_20_TB_Ba ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_Woody_Ba, type = 3) #ns

#TB 2021- droughtxgrazing
TB_21_Woody_Ba <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2021 & site== "TB" & aerial_basal=="Basal"), RelCov_21_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_Woody_Ba, type = 3) #ns

#TB 2022- droughtxgrazing
TB_22_Woody_Ba <- lmerTest::lmer(data = subset(RelCov_Woody, year == 2022 & site== "TB" & aerial_basal=="Basal"), RelCov_22_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_Woody_Ba, type = 3)  #ns

#### Normality: FK C4-Ps ####
RelCov_C4_P<-FG_RelCov %>% 
  filter(Functional_Group=="C4-P")

#FK - Aerial - Relative_Cover: 2018 
#non transformed data
Norm_FK_18_RelCov_C4_P_Ar <- lm(data = subset(RelCov_C4_P, year == 2018 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C4_P_Ar) 
ols_test_normality(Norm_FK_18_RelCov_C4_P_Ar) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_18_FK_AR=1/sqrt(Relative_Cover))

Norm_FK_18_RelCov_C4_P_Ar_TF <- lm(data = subset(RelCov_C4_P, year == 2018 & site== "FK"& aerial_basal=="Aerial"), RelCov_18_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C4_P_Ar_TF) 
ols_test_normality(Norm_FK_18_RelCov_C4_P_Ar_TF) #normalish

#FK - Aerial - Relative_Cover: 2019 
#non transformed data
Norm_FK_19_RelCov_C4_P_Ar <- lm(data = subset(RelCov_C4_P, year == 2019 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_C4_P_Ar) 
ols_test_normality(Norm_FK_19_RelCov_C4_P_Ar) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_19_FK_AR=log10(Relative_Cover))

Norm_FK_19_RelCov_C4_P_Ar_TF <- lm(data = subset(RelCov_C4_P, year == 2019 & site== "FK"& aerial_basal=="Aerial"), RelCov_19_FK_AR  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_C4_P_Ar_TF) 
ols_test_normality(Norm_FK_19_RelCov_C4_P_Ar_TF) #normal


#FK - Aerial - Relative_Cover: 2020 
#non transformed data
Norm_FK_20_RelCov_C4_P_Ar <- lm(data = subset(RelCov_C4_P, year == 2020 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_C4_P_Ar) 
ols_test_normality(Norm_FK_20_RelCov_C4_P_Ar) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_20_FK_AR=log10(Relative_Cover))

Norm_FK_20_RelCov_C4_P_Ar_TF <- lm(data = subset(RelCov_C4_P, year == 2020 & site== "FK"& aerial_basal=="Aerial"), RelCov_20_FK_AR  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_C4_P_Ar_TF ) 
ols_test_normality(Norm_FK_20_RelCov_C4_P_Ar_TF ) #normal

#FK - Aerial - Relative_Cover: 2021 
#non transformed data
Norm_FK_21_RelCov_C4_P_Ar <- lm(data = subset(RelCov_C4_P, year == 2021 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C4_P_Ar) 
ols_test_normality(Norm_FK_21_RelCov_C4_P_Ar) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_21_FK_AR=1/sqrt(Relative_Cover))

Norm_FK_21_RelCov_C4_P_Ar_TF  <- lm(data = subset(RelCov_C4_P, year == 2021 & site== "FK"& aerial_basal=="Aerial"), RelCov_21_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C4_P_Ar_TF) 
ols_test_normality(Norm_FK_21_RelCov_C4_P_Ar_TF) #normal 

#FK - Aerial - Relative_Cover: 2022 
#non transformed data
Norm_FK_22_RelCov_C4_P_Ar <- lm(data = subset(RelCov_C4_P, year == 2022 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C4_P_Ar) 
ols_test_normality(Norm_FK_22_RelCov_C4_P_Ar) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_22_FK_AR=1/sqrt(Relative_Cover))

Norm_FK_22_RelCov_C4_P_Ar_TF  <- lm(data = subset(RelCov_C4_P, year == 2022 & site== "FK"& aerial_basal=="Aerial"), RelCov_22_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C4_P_Ar_TF) 
ols_test_normality(Norm_FK_22_RelCov_C4_P_Ar_TF) #normal

#FK - Basal - Relative_Cover: 2018 
#non transformed data
Norm_FK_18_RelCov_C4_P_Ba <- lm(data = subset(RelCov_C4_P, year == 2018 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C4_P_Ba) 
ols_test_normality(Norm_FK_18_RelCov_C4_P_Ba) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_18_FK_Ba=1/asin(Relative_Cover))

Norm_FK_18_RelCov_C4_P_Ba_TF <- lm(data = subset(RelCov_C4_P, year == 2018 & site== "FK"& aerial_basal=="Basal"), RelCov_18_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C4_P_Ba_TF) 
ols_test_normality(Norm_FK_18_RelCov_C4_P_Ba_TF) #still not normal but better

#FK - Basal - Relative_Cover: 2019 
#non transformed data
Norm_FK_19_RelCov_C4_P_Ba <- lm(data = subset(RelCov_C4_P, year == 2019 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_C4_P_Ba) 
ols_test_normality(Norm_FK_19_RelCov_C4_P_Ba) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_19_FK_Ba=log(Relative_Cover))

Norm_FK_19_RelCov_C4_P_Ba_TF <- lm(data = subset(RelCov_C4_P, year == 2019 & site== "FK"& aerial_basal=="Basal"), RelCov_19_FK_Ba  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_C4_P_Ba_TF) 
ols_test_normality(Norm_FK_19_RelCov_C4_P_Ba_TF) #normal


#FK - Basal - Relative_Cover: 2020 
#non transformed data
Norm_FK_20_RelCov_C4_P_Ba <- lm(data = subset(RelCov_C4_P, year == 2020 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_C4_P_Ba) 
ols_test_normality(Norm_FK_20_RelCov_C4_P_Ba) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_20_FK_Ba=1/asin(Relative_Cover))

Norm_FK_20_RelCov_C4_P_Ba_TF <- lm(data = subset(RelCov_C4_P, year == 2020 & site== "FK"& aerial_basal=="Basal"), RelCov_20_FK_Ba  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_C4_P_Ba_TF) 
ols_test_normality(Norm_FK_20_RelCov_C4_P_Ba_TF) #still not normal but better

#FK - Basal - Relative_Cover: 2021 
#non transformed data
Norm_FK_21_RelCov_C4_P_Ba <- lm(data = subset(RelCov_C4_P, year == 2021 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C4_P_Ba) 
ols_test_normality(Norm_FK_21_RelCov_C4_P_Ba) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_21_FK_Ba=1/sqrt(Relative_Cover))

Norm_FK_21_RelCov_C4_P_Ba_TF  <- lm(data = subset(RelCov_C4_P, year == 2021 & site== "FK"& aerial_basal=="Basal"), RelCov_21_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C4_P_Ba_TF) 
ols_test_normality(Norm_FK_21_RelCov_C4_P_Ba_TF) #normalish

#FK - Basal - Relative_Cover: 2022 
#non transformed data
Norm_FK_22_RelCov_C4_P_Ba <- lm(data = subset(RelCov_C4_P, year == 2022 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C4_P_Ba) 
ols_test_normality(Norm_FK_22_RelCov_C4_P_Ba) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_22_FK_Ba=1/sqrt(Relative_Cover))

Norm_FK_22_RelCov_C4_P_Ba_TF  <- lm(data = subset(RelCov_C4_P, year == 2022 & site== "FK"& aerial_basal=="Basal"), RelCov_22_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C4_P_Ba_TF) 
ols_test_normality(Norm_FK_22_RelCov_C4_P_Ba_TF) #normal

#### Stats: FK C4_Ps ####

#FK 2018 - checking drought and grazing
FK_18_C4_P_Ar <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2018 & site== "FK" & aerial_basal=="Aerial"), RelCov_18_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_C4_P_Ar, type = 3) #ns

#FK 2019 - just drought
FK_19_C4_P_Ar <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2019 & site== "FK" & aerial_basal=="Aerial"), RelCov_19_FK_AR ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_C4_P_Ar, type = 3) #drought (0.006)

#FK 2020 - droughtxgrazing
FK_20_C4_P_Ar <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2020 & site== "FK" & aerial_basal=="Aerial"), RelCov_20_FK_AR ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_C4_P_Ar, type = 3) #ns

#FK 2021- droughtxgrazing
FK_21_C4_P_Ar <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2021 & site== "FK" & aerial_basal=="Aerial"), RelCov_21_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_C4_P_Ar, type = 3) #ns

#FK 2022- droughtxgrazing
FK_22_C4_P_Ar <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2022 & site== "FK" & aerial_basal=="Aerial"), RelCov_22_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_C4_P_Ar, type = 3) #ns

#Basal 
#FK 2018 - checking drought and grazing
FK_18_C4_P_Ba <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2018 & site== "FK" & aerial_basal=="Basal"), RelCov_18_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_C4_P_Ba, type = 3) #ns

#FK 2019 - just drought
FK_19_C4_P_Ba <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2019 & site== "FK" & aerial_basal=="Basal"), RelCov_19_FK_Ba ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_C4_P_Ba, type = 3) #drought (0.003)

#FK 2020 - droughtxgrazing
FK_20_C4_P_Ba <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2020 & site== "FK" & aerial_basal=="Basal"), RelCov_20_FK_Ba ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_C4_P_Ba, type = 3) #ns

#FK 2021- droughtxgrazing
FK_21_C4_P_Ba <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2021 & site== "FK" & aerial_basal=="Basal"), RelCov_21_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_C4_P_Ba, type = 3) #ns

#FK 2022- droughtxgrazing
FK_22_C4_P_Ba <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2022 & site== "FK" & aerial_basal=="Basal"), RelCov_22_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_C4_P_Ba, type = 3)  #ns

#### Normality: TB C4-Ps ####

#TB - Aerial - Relative_Cover: 2018 
#non transformed data
Norm_TB_18_RelCov_C4_P_Ar <- lm(data = subset(RelCov_C4_P, year == 2018 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C4_P_Ar) 
ols_test_normality(Norm_TB_18_RelCov_C4_P_Ar) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_18_TB_AR=1/log10(Relative_Cover))

Norm_TB_18_RelCov_C4_P_Ar_TF <- lm(data = subset(RelCov_C4_P, year == 2018 & site== "TB"& aerial_basal=="Aerial"), RelCov_18_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C4_P_Ar_TF) 
ols_test_normality(Norm_TB_18_RelCov_C4_P_Ar_TF) #normal

#TB - Aerial - Relative_Cover: 2019 
#non transformed data
Norm_TB_19_RelCov_C4_P_Ar <- lm(data = subset(RelCov_C4_P, year == 2019 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_C4_P_Ar) 
ols_test_normality(Norm_TB_19_RelCov_C4_P_Ar) #normalish


#TB - Aerial - Relative_Cover: 2020 
#non transformed data
Norm_TB_20_RelCov_C4_P_Ar <- lm(data = subset(RelCov_C4_P, year == 2020 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_C4_P_Ar) 
ols_test_normality(Norm_TB_20_RelCov_C4_P_Ar) #normalish

#TB - Aerial - Relative_Cover: 2021 
#non transformed data
Norm_TB_21_RelCov_C4_P_Ar <- lm(data = subset(RelCov_C4_P, year == 2021 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_C4_P_Ar) 
ols_test_normality(Norm_TB_21_RelCov_C4_P_Ar) #normal 

#TB - Aerial - Relative_Cover: 2022 
#non transformed data
Norm_TB_22_RelCov_C4_P_Ar <- lm(data = subset(RelCov_C4_P, year == 2022 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_C4_P_Ar) 
ols_test_normality(Norm_TB_22_RelCov_C4_P_Ar) #normal

#TB - Basal - Relative_Cover: 2018 
#non transformed data
Norm_TB_18_RelCov_C4_P_Ba <- lm(data = subset(RelCov_C4_P, year == 2018 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C4_P_Ba) 
ols_test_normality(Norm_TB_18_RelCov_C4_P_Ba) 

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_18_TB_Ba=1/log(Relative_Cover))

Norm_TB_18_RelCov_C4_P_Ba_TF <- lm(data = subset(RelCov_C4_P, year == 2018 & site== "TB"& aerial_basal=="Basal"), RelCov_18_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C4_P_Ba_TF) 
ols_test_normality(Norm_TB_18_RelCov_C4_P_Ba_TF) #normal

#TB - Basal - Relative_Cover: 2019 
#non transformed data
Norm_TB_19_RelCov_C4_P_Ba <- lm(data = subset(RelCov_C4_P, year == 2019 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_C4_P_Ba) 
ols_test_normality(Norm_TB_19_RelCov_C4_P_Ba) #normalish


#TB - Basal - Relative_Cover: 2020 
#non transformed data
Norm_TB_20_RelCov_C4_P_Ba <- lm(data = subset(RelCov_C4_P, year == 2020 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_C4_P_Ba) 
ols_test_normality(Norm_TB_20_RelCov_C4_P_Ba) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_20_TB_Ba=asin(Relative_Cover))

Norm_TB_20_RelCov_C4_P_Ba_TF <- lm(data = subset(RelCov_C4_P, year == 2020 & site== "TB"& aerial_basal=="Basal"), RelCov_20_TB_Ba  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_C4_P_Ba_TF) 
ols_test_normality(Norm_TB_20_RelCov_C4_P_Ba_TF) #still not normal but better

#TB - Basal - Relative_Cover: 2021 
#non transformed data
Norm_TB_21_RelCov_C4_P_Ba <- lm(data = subset(RelCov_C4_P, year == 2021 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_C4_P_Ba) 
ols_test_normality(Norm_TB_21_RelCov_C4_P_Ba) #right skewed

#Transform Data
RelCov_C4_P<-RelCov_C4_P %>% 
  mutate(RelCov_21_TB_Ba=sqrt(Relative_Cover))

Norm_TB_21_RelCov_C4_P_Ba_TF  <- lm(data = subset(RelCov_C4_P, year == 2021 & site== "TB"& aerial_basal=="Basal"), RelCov_21_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_C4_P_Ba_TF) 
ols_test_normality(Norm_TB_21_RelCov_C4_P_Ba_TF) #normal

#TB - Basal - Relative_Cover: 2022 
#non transformed data
Norm_TB_22_RelCov_C4_P_Ba <- lm(data = subset(RelCov_C4_P, year == 2022 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_C4_P_Ba) 
ols_test_normality(Norm_TB_22_RelCov_C4_P_Ba) #normal

#### Stats: TB C4_Ps ####

#TB 2018 - checking drought and grazing
TB_18_C4_P_Ar <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2018 & site== "TB" & aerial_basal=="Aerial"), RelCov_18_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_C4_P_Ar, type = 3) #Grazing(0.05)

#TB 2019 - just drought
TB_19_C4_P_Ar <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2019 & site== "TB" & aerial_basal=="Aerial"), Relative_Cover ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_C4_P_Ar, type = 3) #ns

#TB 2020 - droughtxgrazing
TB_20_C4_P_Ar <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2020 & site== "TB" & aerial_basal=="Aerial"), Relative_Cover ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_C4_P_Ar, type = 3) #ns

#TB 2021- droughtxgrazing
TB_21_C4_P_Ar <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2021 & site== "TB" & aerial_basal=="Aerial"), Relative_Cover ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_C4_P_Ar, type = 3) #ns

#TB 2022- droughtxgrazing
TB_22_C4_P_Ar <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2022 & site== "TB" & aerial_basal=="Aerial"), Relative_Cover ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_C4_P_Ar, type = 3) #grazing(0.02)

#Basal 
#TB 2018 - checking drought and grazing
TB_18_C4_P_Ba <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2018 & site== "TB" & aerial_basal=="Basal"), RelCov_18_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_C4_P_Ba, type = 3) #grazing (0.05)

#TB 2019 - just drought
TB_19_C4_P_Ba <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2019 & site== "TB" & aerial_basal=="Basal"), Relative_Cover ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_C4_P_Ba, type = 3) #ns

#TB 2020 - droughtxgrazing
TB_20_C4_P_Ba <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2020 & site== "TB" & aerial_basal=="Basal"), RelCov_20_TB_Ba ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_C4_P_Ba, type = 3) #ns

#TB 2021- droughtxgrazing
TB_21_C4_P_Ba <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2021 & site== "TB" & aerial_basal=="Basal"), RelCov_21_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_C4_P_Ba, type = 3) #ns

#TB 2022- droughtxgrazing
TB_22_C4_P_Ba <- lmerTest::lmer(data = subset(RelCov_C4_P, year == 2022 & site== "TB" & aerial_basal=="Basal"), Relative_Cover ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_C4_P_Ba, type = 3)  #grazing (0.04)

#### Normality: FK C3-Ps ####

RelCov_C3_P<-FG_RelCov %>% 
  filter(Functional_Group=="C3-P")

#FK - Aerial - Relative_Cover: 2018 
#non transformed data
Norm_FK_18_RelCov_C3_P_Ar <- lm(data = subset(RelCov_C3_P, year == 2018 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C3_P_Ar) 
ols_test_normality(Norm_FK_18_RelCov_C3_P_Ar) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_18_FK_AR=log(Relative_Cover))

Norm_FK_18_RelCov_C3_P_Ar_TF <- lm(data = subset(RelCov_C3_P, year == 2018 & site== "FK"& aerial_basal=="Aerial"), RelCov_18_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C3_P_Ar_TF) 
ols_test_normality(Norm_FK_18_RelCov_C3_P_Ar_TF) #still not normal but better

#FK - Aerial - Relative_Cover: 2019 
#non transformed data
Norm_FK_19_RelCov_C3_P_Ar <- lm(data = subset(RelCov_C3_P, year == 2019 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_C3_P_Ar) 
ols_test_normality(Norm_FK_19_RelCov_C3_P_Ar) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_19_FK_AR=log(Relative_Cover))

Norm_FK_19_RelCov_C3_P_Ar_TF <- lm(data = subset(RelCov_C3_P, year == 2019 & site== "FK"& aerial_basal=="Aerial"), RelCov_19_FK_AR  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_C3_P_Ar_TF) 
ols_test_normality(Norm_FK_19_RelCov_C3_P_Ar_TF) #still not normal but better


#FK - Aerial - Relative_Cover: 2020 
#non transformed data
Norm_FK_20_RelCov_C3_P_Ar <- lm(data = subset(RelCov_C3_P, year == 2020 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_C3_P_Ar) 
ols_test_normality(Norm_FK_20_RelCov_C3_P_Ar) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_20_FK_AR=log(Relative_Cover))

Norm_FK_20_RelCov_C3_P_Ar_TF <- lm(data = subset(RelCov_C3_P, year == 2020 & site== "FK"& aerial_basal=="Aerial"), RelCov_20_FK_AR  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_C3_P_Ar_TF ) 
ols_test_normality(Norm_FK_20_RelCov_C3_P_Ar_TF ) #still not normal but better

#FK - Aerial - Relative_Cover: 2021 
#non transformed data
Norm_FK_21_RelCov_C3_P_Ar <- lm(data = subset(RelCov_C3_P, year == 2021 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C3_P_Ar) 
ols_test_normality(Norm_FK_21_RelCov_C3_P_Ar) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_21_FK_AR=log(Relative_Cover))

Norm_FK_21_RelCov_C3_P_Ar_TF  <- lm(data = subset(RelCov_C3_P, year == 2021 & site== "FK"& aerial_basal=="Aerial"), RelCov_21_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C3_P_Ar_TF) 
ols_test_normality(Norm_FK_21_RelCov_C3_P_Ar_TF) #still not normal but better

#FK - Aerial - Relative_Cover: 2022 
#non transformed data
Norm_FK_22_RelCov_C3_P_Ar <- lm(data = subset(RelCov_C3_P, year == 2022 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C3_P_Ar) 
ols_test_normality(Norm_FK_22_RelCov_C3_P_Ar) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_22_FK_AR=log10(Relative_Cover))

Norm_FK_22_RelCov_C3_P_Ar_TF  <- lm(data = subset(RelCov_C3_P, year == 2022 & site== "FK"& aerial_basal=="Aerial"), RelCov_22_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C3_P_Ar_TF) 
ols_test_normality(Norm_FK_22_RelCov_C3_P_Ar_TF) #normalish 

#FK - Basal - Relative_Cover: 2018 
#non transformed data
Norm_FK_18_RelCov_C3_P_Ba <- lm(data = subset(RelCov_C3_P, year == 2018 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C3_P_Ba) 
ols_test_normality(Norm_FK_18_RelCov_C3_P_Ba) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_18_FK_Ba=log(Relative_Cover))

Norm_FK_18_RelCov_C3_P_Ba_TF <- lm(data = subset(RelCov_C3_P, year == 2018 & site== "FK"& aerial_basal=="Basal"), RelCov_18_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C3_P_Ba_TF) 
ols_test_normality(Norm_FK_18_RelCov_C3_P_Ba_TF) #still not normal but better

#FK - Basal - Relative_Cover: 2019 
#non transformed data
Norm_FK_19_RelCov_C3_P_Ba <- lm(data = subset(RelCov_C3_P, year == 2019 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_C3_P_Ba) 
ols_test_normality(Norm_FK_19_RelCov_C3_P_Ba) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_19_FK_Ba=log(Relative_Cover))

Norm_FK_19_RelCov_C3_P_Ba_TF <- lm(data = subset(RelCov_C3_P, year == 2019 & site== "FK"& aerial_basal=="Basal"), RelCov_19_FK_Ba  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_C3_P_Ba_TF) 
ols_test_normality(Norm_FK_19_RelCov_C3_P_Ba_TF) #still nto normal but better


#FK - Basal - Relative_Cover: 2020 
#non transformed data
Norm_FK_20_RelCov_C3_P_Ba <- lm(data = subset(RelCov_C3_P, year == 2020 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_C3_P_Ba) 
ols_test_normality(Norm_FK_20_RelCov_C3_P_Ba) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_20_FK_Ba=log10(Relative_Cover))

Norm_FK_20_RelCov_C3_P_Ba_TF <- lm(data = subset(RelCov_C3_P, year == 2020 & site== "FK"& aerial_basal=="Basal"), RelCov_20_FK_Ba  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_C3_P_Ba_TF) 
ols_test_normality(Norm_FK_20_RelCov_C3_P_Ba_TF) #still not normal but better

#FK - Basal - Relative_Cover: 2021 
#non transformed data
Norm_FK_21_RelCov_C3_P_Ba <- lm(data = subset(RelCov_C3_P, year == 2021 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C3_P_Ba) 
ols_test_normality(Norm_FK_21_RelCov_C3_P_Ba) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_21_FK_Ba=1/sqrt(Relative_Cover))

Norm_FK_21_RelCov_C3_P_Ba_TF  <- lm(data = subset(RelCov_C3_P, year == 2021 & site== "FK"& aerial_basal=="Basal"), RelCov_21_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C3_P_Ba_TF) 
ols_test_normality(Norm_FK_21_RelCov_C3_P_Ba_TF) #normalish

#FK - Basal - Relative_Cover: 2022 
#non transformed data
Norm_FK_22_RelCov_C3_P_Ba <- lm(data = subset(RelCov_C3_P, year == 2022 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C3_P_Ba) 
ols_test_normality(Norm_FK_22_RelCov_C3_P_Ba) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_22_FK_Ba=log(Relative_Cover))

Norm_FK_22_RelCov_C3_P_Ba_TF  <- lm(data = subset(RelCov_C3_P, year == 2022 & site== "FK"& aerial_basal=="Basal"), RelCov_22_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C3_P_Ba_TF) 
ols_test_normality(Norm_FK_22_RelCov_C3_P_Ba_TF) #still not normal but better

#### Stats: FK C3_Ps ####

#FK 2018 - checking drought and grazing
FK_18_C3_P_Ar <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2018 & site== "FK" & aerial_basal=="Aerial"), RelCov_18_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_C3_P_Ar, type = 3) #ns

#FK 2019 - just drought
FK_19_C3_P_Ar <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2019 & site== "FK" & aerial_basal=="Aerial"), RelCov_19_FK_AR ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_C3_P_Ar, type = 3) #ns

#FK 2020 - droughtxgrazing
FK_20_C3_P_Ar <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2020 & site== "FK" & aerial_basal=="Aerial"), RelCov_20_FK_AR ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_C3_P_Ar, type = 3) #ns

#FK 2021- droughtxgrazing
FK_21_C3_P_Ar <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2021 & site== "FK" & aerial_basal=="Aerial"), RelCov_21_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_C3_P_Ar, type = 3) #ns

#FK 2022- droughtxgrazing
FK_22_C3_P_Ar <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2022 & site== "FK" & aerial_basal=="Aerial"), RelCov_22_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_C3_P_Ar, type = 3) #ns

#Basal 
#FK 2018 - checking drought and grazing
FK_18_C3_P_Ba <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2018 & site== "FK" & aerial_basal=="Basal"), RelCov_18_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_C3_P_Ba, type = 3) #ns

#FK 2019 - just drought
FK_19_C3_P_Ba <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2019 & site== "FK" & aerial_basal=="Basal"), RelCov_19_FK_Ba ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_C3_P_Ba, type = 3) #ns

#FK 2020 - droughtxgrazing
FK_20_C3_P_Ba <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2020 & site== "FK" & aerial_basal=="Basal"), RelCov_20_FK_Ba ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_C3_P_Ba, type = 3) #ns

#FK 2021- droughtxgrazing
FK_21_C3_P_Ba <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2021 & site== "FK" & aerial_basal=="Basal"), RelCov_21_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_C3_P_Ba, type = 3) #ns

#FK 2022- droughtxgrazing
FK_22_C3_P_Ba <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2022 & site== "FK" & aerial_basal=="Basal"), RelCov_22_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_C3_P_Ba, type = 3)  #ns

#### Normality: TB C3-Ps ####

RelCov_C3_P<-FG_RelCov %>% 
  filter(Functional_Group=="C3-P")

#TB - Aerial - Relative_Cover: 2018 
#non transformed data
Norm_TB_18_RelCov_C3_P_Ar <- lm(data = subset(RelCov_C3_P, year == 2018 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C3_P_Ar) 
ols_test_normality(Norm_TB_18_RelCov_C3_P_Ar) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_18_TB_AR=log(Relative_Cover))

Norm_TB_18_RelCov_C3_P_Ar_TF <- lm(data = subset(RelCov_C3_P, year == 2018 & site== "TB"& aerial_basal=="Aerial"), RelCov_18_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C3_P_Ar_TF) 
ols_test_normality(Norm_TB_18_RelCov_C3_P_Ar_TF) #still not normal but better

#TB - Aerial - Relative_Cover: 2019 
#non transformed data
Norm_TB_19_RelCov_C3_P_Ar <- lm(data = subset(RelCov_C3_P, year == 2019 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_C3_P_Ar) 
ols_test_normality(Norm_TB_19_RelCov_C3_P_Ar) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_19_TB_AR=log(Relative_Cover))

Norm_TB_19_RelCov_C3_P_Ar_TF <- lm(data = subset(RelCov_C3_P, year == 2019 & site== "TB"& aerial_basal=="Aerial"), RelCov_19_TB_AR  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_C3_P_Ar_TF) 
ols_test_normality(Norm_TB_19_RelCov_C3_P_Ar_TF) #still not normal but better


#TB - Aerial - Relative_Cover: 2020 
#non transformed data
Norm_TB_20_RelCov_C3_P_Ar <- lm(data = subset(RelCov_C3_P, year == 2020 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_C3_P_Ar) 
ols_test_normality(Norm_TB_20_RelCov_C3_P_Ar) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_20_TB_AR=log(Relative_Cover))

Norm_TB_20_RelCov_C3_P_Ar_TF <- lm(data = subset(RelCov_C3_P, year == 2020 & site== "TB"& aerial_basal=="Aerial"), RelCov_20_TB_AR  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_C3_P_Ar_TF ) 
ols_test_normality(Norm_TB_20_RelCov_C3_P_Ar_TF ) #normal

#TB - Aerial - Relative_Cover: 2021 
#non transformed data
Norm_TB_21_RelCov_C3_P_Ar <- lm(data = subset(RelCov_C3_P, year == 2021 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_C3_P_Ar) 
ols_test_normality(Norm_TB_21_RelCov_C3_P_Ar) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_21_TB_AR=log(Relative_Cover))

Norm_TB_21_RelCov_C3_P_Ar_TF  <- lm(data = subset(RelCov_C3_P, year == 2021 & site== "TB"& aerial_basal=="Aerial"), RelCov_21_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_C3_P_Ar_TF) 
ols_test_normality(Norm_TB_21_RelCov_C3_P_Ar_TF) #normal

#TB - Aerial - Relative_Cover: 2022 
#non transformed data
Norm_TB_22_RelCov_C3_P_Ar <- lm(data = subset(RelCov_C3_P, year == 2022 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_C3_P_Ar) 
ols_test_normality(Norm_TB_22_RelCov_C3_P_Ar) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_22_TB_AR=log(Relative_Cover))

Norm_TB_22_RelCov_C3_P_Ar_TF  <- lm(data = subset(RelCov_C3_P, year == 2022 & site== "TB"& aerial_basal=="Aerial"), RelCov_22_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_C3_P_Ar_TF) 
ols_test_normality(Norm_TB_22_RelCov_C3_P_Ar_TF) #normalish 

#TB - Basal - Relative_Cover: 2018 
#non transformed data
Norm_TB_18_RelCov_C3_P_Ba <- lm(data = subset(RelCov_C3_P, year == 2018 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C3_P_Ba) 
ols_test_normality(Norm_TB_18_RelCov_C3_P_Ba) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_18_TB_Ba=log10(Relative_Cover))

Norm_TB_18_RelCov_C3_P_Ba_TF <- lm(data = subset(RelCov_C3_P, year == 2018 & site== "TB"& aerial_basal=="Basal"), RelCov_18_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C3_P_Ba_TF) 
ols_test_normality(Norm_TB_18_RelCov_C3_P_Ba_TF) #still not normal but better

#TB - Basal - Relative_Cover: 2019 
#non transformed data
Norm_TB_19_RelCov_C3_P_Ba <- lm(data = subset(RelCov_C3_P, year == 2019 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_C3_P_Ba) 
ols_test_normality(Norm_TB_19_RelCov_C3_P_Ba) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_19_TB_Ba=log10(Relative_Cover))

Norm_TB_19_RelCov_C3_P_Ba_TF <- lm(data = subset(RelCov_C3_P, year == 2019 & site== "TB"& aerial_basal=="Basal"), RelCov_19_TB_Ba  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_C3_P_Ba_TF) 
ols_test_normality(Norm_TB_19_RelCov_C3_P_Ba_TF) #still nto normal but better


#TB - Basal - Relative_Cover: 2020 
#non transformed data
Norm_TB_20_RelCov_C3_P_Ba <- lm(data = subset(RelCov_C3_P, year == 2020 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_C3_P_Ba) 
ols_test_normality(Norm_TB_20_RelCov_C3_P_Ba) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_20_TB_Ba=log10(Relative_Cover))

Norm_TB_20_RelCov_C3_P_Ba_TF <- lm(data = subset(RelCov_C3_P, year == 2020 & site== "TB"& aerial_basal=="Basal"), RelCov_20_TB_Ba  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_C3_P_Ba_TF) 
ols_test_normality(Norm_TB_20_RelCov_C3_P_Ba_TF) #still not normal but better

#TB - Basal - Relative_Cover: 2021 
#non transformed data
Norm_TB_21_RelCov_C3_P_Ba <- lm(data = subset(RelCov_C3_P, year == 2021 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_C3_P_Ba) 
ols_test_normality(Norm_TB_21_RelCov_C3_P_Ba) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_21_TB_Ba=log(Relative_Cover))

Norm_TB_21_RelCov_C3_P_Ba_TF  <- lm(data = subset(RelCov_C3_P, year == 2021 & site== "TB"& aerial_basal=="Basal"), RelCov_21_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_C3_P_Ba_TF) 
ols_test_normality(Norm_TB_21_RelCov_C3_P_Ba_TF) #normalish

#TB - Basal - Relative_Cover: 2022 
#non transformed data
Norm_TB_22_RelCov_C3_P_Ba <- lm(data = subset(RelCov_C3_P, year == 2022 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_C3_P_Ba) 
ols_test_normality(Norm_TB_22_RelCov_C3_P_Ba) #right skewed

#Transform Data
RelCov_C3_P<-RelCov_C3_P %>% 
  mutate(RelCov_22_TB_Ba=log10(Relative_Cover))

Norm_TB_22_RelCov_C3_P_Ba_TF  <- lm(data = subset(RelCov_C3_P, year == 2022 & site== "TB"& aerial_basal=="Basal"), RelCov_22_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_C3_P_Ba_TF) 
ols_test_normality(Norm_TB_22_RelCov_C3_P_Ba_TF) #still not normal but better

#### Stats: TB C3_Ps ####

#TB 2018 - checking drought and grazing
TB_18_C3_P_Ar <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2018 & site== "TB" & aerial_basal=="Aerial"), RelCov_18_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_C3_P_Ar, type = 3) #drought (0.012)

#TB 2019 - just drought
TB_19_C3_P_Ar <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2019 & site== "TB" & aerial_basal=="Aerial"), RelCov_19_TB_AR ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_C3_P_Ar, type = 3) #ns

#TB 2020 - droughtxgrazing
TB_20_C3_P_Ar <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2020 & site== "TB" & aerial_basal=="Aerial"), RelCov_20_TB_AR ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_C3_P_Ar, type = 3) #ns

#TB 2021- droughtxgrazing
TB_21_C3_P_Ar <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2021 & site== "TB" & aerial_basal=="Aerial"), RelCov_21_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_C3_P_Ar, type = 3) #ns

#TB 2022- droughtxgrazing
TB_22_C3_P_Ar <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2022 & site== "TB" & aerial_basal=="Aerial"), RelCov_22_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_C3_P_Ar, type = 3) #grazing (0.04)

#Basal 
#TB 2018 - checking drought and grazing
TB_18_C3_P_Ba <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2018 & site== "TB" & aerial_basal=="Basal"), RelCov_18_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_C3_P_Ba, type = 3) #drought (0.02), grazing (0.03)

#TB 2019 - just drought
TB_19_C3_P_Ba <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2019 & site== "TB" & aerial_basal=="Basal"), RelCov_19_TB_Ba ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_C3_P_Ba, type = 3) #ns

#TB 2020 - droughtxgrazing
TB_20_C3_P_Ba <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2020 & site== "TB" & aerial_basal=="Basal"), RelCov_20_TB_Ba ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_C3_P_Ba, type = 3) #ns

#TB 2021- droughtxgrazing
TB_21_C3_P_Ba <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2021 & site== "TB" & aerial_basal=="Basal"), RelCov_21_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_C3_P_Ba, type = 3) #ns

#TB 2022- droughtxgrazing
TB_22_C3_P_Ba <- lmerTest::lmer(data = subset(RelCov_C3_P, year == 2022 & site== "TB" & aerial_basal=="Basal"), RelCov_22_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_C3_P_Ba, type = 3)  #grazing (0.01)


#### Normality: FK C3-As ####

RelCov_C3_A<-FG_RelCov %>% 
  filter(Functional_Group=="C3-A")

#FK - Aerial - Relative_Cover: 2018 
#non transformed data
Norm_FK_18_RelCov_C3_A_Ar <- lm(data = subset(RelCov_C3_A, year == 2018 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C3_A_Ar) 
ols_test_normality(Norm_FK_18_RelCov_C3_A_Ar) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_18_FK_AR=1/log10(Relative_Cover))

Norm_FK_18_RelCov_C3_A_Ar_TF <- lm(data = subset(RelCov_C3_A, year == 2018 & site== "FK"& aerial_basal=="Aerial"), RelCov_18_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C3_A_Ar_TF) 
ols_test_normality(Norm_FK_18_RelCov_C3_A_Ar_TF) #still not normal but better

#FK - Aerial - Relative_Cover: 2019 
#non transformed data
Norm_FK_19_RelCov_C3_A_Ar <- lm(data = subset(RelCov_C3_A, year == 2019 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_C3_A_Ar) 
ols_test_normality(Norm_FK_19_RelCov_C3_A_Ar) #

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_19_FK_AR=asin(Relative_Cover))

Norm_FK_19_RelCov_C3_A_Ar_TF <- lm(data = subset(RelCov_C3_A, year == 2019 & site== "FK"& aerial_basal=="Aerial"), RelCov_19_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_19_RelCov_C3_A_Ar_TF) 
ols_test_normality(Norm_FK_19_RelCov_C3_A_Ar_TF) #still not normal but better


#FK - Aerial - Relative_Cover: 2020 
#non transformed data
Norm_FK_20_RelCov_C3_A_Ar <- lm(data = subset(RelCov_C3_A, year == 2020 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_C3_A_Ar) 
ols_test_normality(Norm_FK_20_RelCov_C3_A_Ar) #

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_20_FK_AR=asin(Relative_Cover))

Norm_FK_20_RelCov_C3_A_Ar_TF <- lm(data = subset(RelCov_C3_A, year == 2020 & site== "FK"& aerial_basal=="Aerial"), RelCov_20_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_20_RelCov_C3_A_Ar_TF) 
ols_test_normality(Norm_FK_20_RelCov_C3_A_Ar_TF) #still not normal but better

#FK - Aerial - Relative_Cover: 2021 
#non transformed data
Norm_FK_21_RelCov_C3_A_Ar <- lm(data = subset(RelCov_C3_A, year == 2021 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C3_A_Ar) 
ols_test_normality(Norm_FK_21_RelCov_C3_A_Ar) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_21_FK_AR=log10(Relative_Cover))

Norm_FK_21_RelCov_C3_A_Ar_TF  <- lm(data = subset(RelCov_C3_A, year == 2021 & site== "FK"& aerial_basal=="Aerial"), RelCov_21_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C3_A_Ar_TF) 
ols_test_normality(Norm_FK_21_RelCov_C3_A_Ar_TF) #not normal but better

#FK - Aerial - Relative_Cover: 2022 
#non transformed data
Norm_FK_22_RelCov_C3_A_Ar <- lm(data = subset(RelCov_C3_A, year == 2022 & site== "FK"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C3_A_Ar) 
ols_test_normality(Norm_FK_22_RelCov_C3_A_Ar) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_22_FK_AR=log10(Relative_Cover))

Norm_FK_22_RelCov_C3_A_Ar_TF  <- lm(data = subset(RelCov_C3_A, year == 2022 & site== "FK"& aerial_basal=="Aerial"), RelCov_22_FK_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C3_A_Ar_TF) 
ols_test_normality(Norm_FK_22_RelCov_C3_A_Ar_TF) #still not normal but better

#FK - Basal - Relative_Cover: 2018 
#non transformed data
Norm_FK_18_RelCov_C3_A_Ba <- lm(data = subset(RelCov_C3_A, year == 2018 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C3_A_Ba) 
ols_test_normality(Norm_FK_18_RelCov_C3_A_Ba) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_18_FK_Ba=1/log10(Relative_Cover))

Norm_FK_18_RelCov_C3_A_Ba_TF <- lm(data = subset(RelCov_C3_A, year == 2018 & site== "FK"& aerial_basal=="Basal"), RelCov_18_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_18_RelCov_C3_A_Ba_TF) 
ols_test_normality(Norm_FK_18_RelCov_C3_A_Ba_TF) #still not normal but better

#FK - Basal - Relative_Cover: 2019 
#non transformed data
Norm_FK_19_RelCov_C3_A_Ba <- lm(data = subset(RelCov_C3_A, year == 2019 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_C3_A_Ba) 
ols_test_normality(Norm_FK_19_RelCov_C3_A_Ba) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_19_FK_Ba=asin(Relative_Cover))

Norm_FK_19_RelCov_C3_A_Ba_TF <- lm(data = subset(RelCov_C3_A, year == 2019 & site== "FK"& aerial_basal=="Basal"), RelCov_19_FK_Ba  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_FK_19_RelCov_C3_A_Ba_TF) 
ols_test_normality(Norm_FK_19_RelCov_C3_A_Ba_TF) #not normal but better


#FK - Basal - Relative_Cover: 2020 
#non transformed data
Norm_FK_20_RelCov_C3_A_Ba <- lm(data = subset(RelCov_C3_A, year == 2020 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_C3_A_Ba) 
ols_test_normality(Norm_FK_20_RelCov_C3_A_Ba) #not transformed is best

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_20_FK_Ba=asin(Relative_Cover))

Norm_FK_20_RelCov_C3_A_Ba_TF <- lm(data = subset(RelCov_C3_A, year == 2020 & site== "FK"& aerial_basal=="Basal"), RelCov_20_FK_Ba  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_FK_20_RelCov_C3_A_Ba_TF) 
ols_test_normality(Norm_FK_20_RelCov_C3_A_Ba_TF) #not normal but better

#FK - Basal - Relative_Cover: 2021 
#non transformed data
Norm_FK_21_RelCov_C3_A_Ba <- lm(data = subset(RelCov_C3_A, year == 2021 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C3_A_Ba) 
ols_test_normality(Norm_FK_21_RelCov_C3_A_Ba) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_21_FK_Ba=1/sqrt(Relative_Cover))

Norm_FK_21_RelCov_C3_A_Ba_TF  <- lm(data = subset(RelCov_C3_A, year == 2021 & site== "FK"& aerial_basal=="Basal"), RelCov_21_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_21_RelCov_C3_A_Ba_TF) 
ols_test_normality(Norm_FK_21_RelCov_C3_A_Ba_TF) #not normal but better

#FK - Basal - Relative_Cover: 2022 
#non transformed data
Norm_FK_22_RelCov_C3_A_Ba <- lm(data = subset(RelCov_C3_A, year == 2022 & site== "FK"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C3_A_Ba) 
ols_test_normality(Norm_FK_22_RelCov_C3_A_Ba) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_22_FK_Ba=1/sqrt(Relative_Cover))

Norm_FK_22_RelCov_C3_A_Ba_TF  <- lm(data = subset(RelCov_C3_A, year == 2022 & site== "FK"& aerial_basal=="Basal"), RelCov_22_FK_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_FK_22_RelCov_C3_A_Ba_TF) 
ols_test_normality(Norm_FK_22_RelCov_C3_A_Ba_TF) #still not normal but better


#### Stats: FK C3_As ####

#FK 2018 - checking drought and grazing
FK_18_C3_A_Ar <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2018 & site== "FK" & aerial_basal=="Aerial"), RelCov_18_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_C3_A_Ar, type = 3) #ns

#FK 2019 - just drought
FK_19_C3_A_Ar <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2019 & site== "FK" & aerial_basal=="Aerial"), RelCov_19_FK_AR ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_C3_A_Ar, type = 3) #ns

#FK 2020 - droughtxgrazing
FK_20_C3_A_Ar <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2020 & site== "FK" & aerial_basal=="Aerial"), RelCov_20_FK_AR ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_C3_A_Ar, type = 3) #ns

#FK 2021- droughtxgrazing
FK_21_C3_A_Ar <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2021 & site== "FK" & aerial_basal=="Aerial"), RelCov_21_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_C3_A_Ar, type = 3) #ns

#FK 2022- droughtxgrazing
FK_22_C3_A_Ar <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2022 & site== "FK" & aerial_basal=="Aerial"), RelCov_22_FK_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_C3_A_Ar, type = 3) #ns

#Basal 
#FK 2018 - checking drought and grazing
FK_18_C3_A_Ba <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2018 & site== "FK" & aerial_basal=="Basal"), RelCov_18_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_C3_A_Ba, type = 3) #ns

#FK 2019 - just drought
FK_19_C3_A_Ba <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2019 & site== "FK" & aerial_basal=="Basal"), RelCov_19_FK_Ba ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_C3_A_Ba, type = 3) #ns

#FK 2020 - droughtxgrazing
FK_20_C3_A_Ba <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2020 & site== "FK" & aerial_basal=="Basal"), RelCov_20_FK_Ba ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(FK_20_C3_A_Ba, type = 3) #ns

#FK 2021- droughtxgrazing
FK_21_C3_A_Ba <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2021 & site== "FK" & aerial_basal=="Basal"), RelCov_21_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_C3_A_Ba, type = 3) #drought(0.003)

#FK 2022- droughtxgrazing
FK_22_C3_A_Ba <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2022 & site== "FK" & aerial_basal=="Basal"), RelCov_22_FK_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_C3_A_Ba, type = 3)  #drought(0.004)

#### Normality: TB C3-As ####

RelCov_C3_A<-FG_RelCov %>% 
  filter(Functional_Group=="C3-A")

#TB - Aerial - Relative_Cover: 2018 
#non transformed data
Norm_TB_18_RelCov_C3_A_Ar <- lm(data = subset(RelCov_C3_A, year == 2018 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C3_A_Ar) 
ols_test_normality(Norm_TB_18_RelCov_C3_A_Ar) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_18_TB_AR=1/asin(Relative_Cover))

Norm_TB_18_RelCov_C3_A_Ar_TF <- lm(data = subset(RelCov_C3_A, year == 2018 & site== "TB"& aerial_basal=="Aerial"), RelCov_18_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C3_A_Ar_TF) 
ols_test_normality(Norm_TB_18_RelCov_C3_A_Ar_TF) #normal

#TB - Aerial - Relative_Cover: 2019 
#non transformed data
Norm_TB_19_RelCov_C3_A_Ar <- lm(data = subset(RelCov_C3_A, year == 2019 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_C3_A_Ar) 
ols_test_normality(Norm_TB_19_RelCov_C3_A_Ar) #

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_19_TB_AR=1/sqrt(Relative_Cover))

Norm_TB_19_RelCov_C3_A_Ar_TF <- lm(data = subset(RelCov_C3_A, year == 2019 & site== "TB"& aerial_basal=="Aerial"), RelCov_19_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_19_RelCov_C3_A_Ar_TF) 
ols_test_normality(Norm_TB_19_RelCov_C3_A_Ar_TF) #still not normal but better


#TB - Aerial - Relative_Cover: 2020 
#non transformed data
Norm_TB_20_RelCov_C3_A_Ar <- lm(data = subset(RelCov_C3_A, year == 2020 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_C3_A_Ar) 
ols_test_normality(Norm_TB_20_RelCov_C3_A_Ar) #

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_20_TB_AR=1/asin(Relative_Cover))

Norm_TB_20_RelCov_C3_A_Ar_TF <- lm(data = subset(RelCov_C3_A, year == 2020 & site== "TB"& aerial_basal=="Aerial"), RelCov_20_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_20_RelCov_C3_A_Ar_TF) 
ols_test_normality(Norm_TB_20_RelCov_C3_A_Ar_TF) #normal

#TB - Aerial - Relative_Cover: 2021 
#non transformed data
Norm_TB_21_RelCov_C3_A_Ar <- lm(data = subset(RelCov_C3_A, year == 2021 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_C3_A_Ar) 
ols_test_normality(Norm_TB_21_RelCov_C3_A_Ar) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_21_TB_AR=1/sqrt(Relative_Cover))

Norm_TB_21_RelCov_C3_A_Ar_TF  <- lm(data = subset(RelCov_C3_A, year == 2021 & site== "TB"& aerial_basal=="Aerial"), RelCov_21_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_C3_A_Ar_TF) 
ols_test_normality(Norm_TB_21_RelCov_C3_A_Ar_TF) #not normal but better

#TB - Aerial - Relative_Cover: 2022 
#non transformed data
Norm_TB_22_RelCov_C3_A_Ar <- lm(data = subset(RelCov_C3_A, year == 2022 & site== "TB"& aerial_basal=="Aerial"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_C3_A_Ar) 
ols_test_normality(Norm_TB_22_RelCov_C3_A_Ar) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_22_TB_AR=1/asin(Relative_Cover))

Norm_TB_22_RelCov_C3_A_Ar_TF  <- lm(data = subset(RelCov_C3_A, year == 2022 & site== "TB"& aerial_basal=="Aerial"), RelCov_22_TB_AR  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_C3_A_Ar_TF) 
ols_test_normality(Norm_TB_22_RelCov_C3_A_Ar_TF) #still not normal but better

#TB - Basal - Relative_Cover: 2018 
#non transformed data
Norm_TB_18_RelCov_C3_A_Ba <- lm(data = subset(RelCov_C3_A, year == 2018 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C3_A_Ba) 
ols_test_normality(Norm_TB_18_RelCov_C3_A_Ba) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_18_TB_Ba=1/asin(Relative_Cover))

Norm_TB_18_RelCov_C3_A_Ba_TF <- lm(data = subset(RelCov_C3_A, year == 2018 & site== "TB"& aerial_basal=="Basal"), RelCov_18_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_18_RelCov_C3_A_Ba_TF) 
ols_test_normality(Norm_TB_18_RelCov_C3_A_Ba_TF) #still not normal but better

#TB - Basal - Relative_Cover: 2019 
#non transformed data
Norm_TB_19_RelCov_C3_A_Ba <- lm(data = subset(RelCov_C3_A, year == 2019 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_C3_A_Ba) 
ols_test_normality(Norm_TB_19_RelCov_C3_A_Ba) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_19_TB_Ba=1/asin(Relative_Cover))

Norm_TB_19_RelCov_C3_A_Ba_TF <- lm(data = subset(RelCov_C3_A, year == 2019 & site== "TB"& aerial_basal=="Basal"), RelCov_19_TB_Ba  ~ rainfall_reduction)
ols_plot_resid_hist(Norm_TB_19_RelCov_C3_A_Ba_TF) 
ols_test_normality(Norm_TB_19_RelCov_C3_A_Ba_TF) #not normal but better


#TB - Basal - Relative_Cover: 2020 
#non transformed data
Norm_TB_20_RelCov_C3_A_Ba <- lm(data = subset(RelCov_C3_A, year == 2020 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_C3_A_Ba) 
ols_test_normality(Norm_TB_20_RelCov_C3_A_Ba) #not transformed is best

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_20_TB_Ba=1/sqrt(Relative_Cover))

Norm_TB_20_RelCov_C3_A_Ba_TF <- lm(data = subset(RelCov_C3_A, year == 2020 & site== "TB"& aerial_basal=="Basal"), RelCov_20_TB_Ba  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_RelCov_C3_A_Ba_TF) 
ols_test_normality(Norm_TB_20_RelCov_C3_A_Ba_TF) #not normal but better

#TB - Basal - Relative_Cover: 2021 
#non transformed data
Norm_TB_21_RelCov_C3_A_Ba <- lm(data = subset(RelCov_C3_A, year == 2021 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_C3_A_Ba) 
ols_test_normality(Norm_TB_21_RelCov_C3_A_Ba) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_21_TB_Ba=1/asin(Relative_Cover))

Norm_TB_21_RelCov_C3_A_Ba_TF  <- lm(data = subset(RelCov_C3_A, year == 2021 & site== "TB"& aerial_basal=="Basal"), RelCov_21_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_21_RelCov_C3_A_Ba_TF) 
ols_test_normality(Norm_TB_21_RelCov_C3_A_Ba_TF) #normal

#TB - Basal - Relative_Cover: 2022 
#non transformed data
Norm_TB_22_RelCov_C3_A_Ba <- lm(data = subset(RelCov_C3_A, year == 2022 & site== "TB"& aerial_basal=="Basal"), Relative_Cover  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_C3_A_Ba) 
ols_test_normality(Norm_TB_22_RelCov_C3_A_Ba) #right skewed

#Transform Data
RelCov_C3_A<-RelCov_C3_A %>% 
  mutate(RelCov_22_TB_Ba=1/asin(Relative_Cover))

Norm_TB_22_RelCov_C3_A_Ba_TF  <- lm(data = subset(RelCov_C3_A, year == 2022 & site== "TB"& aerial_basal=="Basal"), RelCov_22_TB_Ba  ~ rainfall_reduction*grazing_treatment)
ols_plot_resid_hist(Norm_TB_22_RelCov_C3_A_Ba_TF) 
ols_test_normality(Norm_TB_22_RelCov_C3_A_Ba_TF) #normal


#### Stats: TB C3_As ####

#TB 2018 - checking drought and grazing
TB_18_C3_A_Ar <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2018 & site== "TB" & aerial_basal=="Aerial"), RelCov_18_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_C3_A_Ar, type = 3) #grazing (0.02)

#TB 2019 - just drought
TB_19_C3_A_Ar <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2019 & site== "TB" & aerial_basal=="Aerial"), RelCov_19_TB_AR ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_C3_A_Ar, type = 3) #ns

#TB 2020 - droughtxgrazing
TB_20_C3_A_Ar <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2020 & site== "TB" & aerial_basal=="Aerial"), RelCov_20_TB_AR ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_C3_A_Ar, type = 3) #ns

#TB 2021- droughtxgrazing
TB_21_C3_A_Ar <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2021 & site== "TB" & aerial_basal=="Aerial"), RelCov_21_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_C3_A_Ar, type = 3) #ns

#TB 2022- droughtxgrazing
TB_22_C3_A_Ar <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2022 & site== "TB" & aerial_basal=="Aerial"), RelCov_22_TB_AR ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_C3_A_Ar, type = 3) #drought (0.023), grazing (0.001)

#Basal 
#TB 2018 - checking drought and grazing
TB_18_C3_A_Ba <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2018 & site== "TB" & aerial_basal=="Basal"), RelCov_18_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_C3_A_Ba, type = 3) #grazing (2.63x-05)

#TB 2019 - just drought
TB_19_C3_A_Ba <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2019 & site== "TB" & aerial_basal=="Basal"), RelCov_19_TB_Ba ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_C3_A_Ba, type = 3) #ns

#TB 2020 - droughtxgrazing
TB_20_C3_A_Ba <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2020 & site== "TB" & aerial_basal=="Basal"), RelCov_20_TB_Ba ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_C3_A_Ba, type = 3) #ns

#TB 2021- droughtxgrazing
TB_21_C3_A_Ba <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2021 & site== "TB" & aerial_basal=="Basal"), RelCov_21_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_C3_A_Ba, type = 3) #grazing (0.03)

#TB 2022- droughtxgrazing
TB_22_C3_A_Ba <- lmerTest::lmer(data = subset(RelCov_C3_A, year == 2022 & site== "TB" & aerial_basal=="Basal"), RelCov_22_TB_Ba ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_C3_A_Ba, type = 3)  #drought (0.04), grazing (0.0004)


