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
  mutate(Genus_Species=ifelse(Genus_Species_3=="Antennaria.KW.pic.unknown","Antennaria.UNKWN1",
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
                       ifelse(Genus_Species_3=="DECA","DECA.UNKWN39",
                              Genus_Species_3))))))))))))))))))))))))))))))))))))))))))))
                                                                    
                                                                    
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
ols_test_normality(Norm_TB_19_Richness_Ar) #not normal

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
ols_test_normality(Norm_TB_19_Richness_Ba) #not normal


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
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment",drop = FALSE)+
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
  scale_color_manual(values=c("chocolate1","chocolate3","chocolate4"),labels = c("Destock", "Stable","Heavy"), breaks = c("destock","stable","heavy"),name="Grazing Treatment",drop = FALSE)+
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


#TB - Basal - Evar: 2020
#non transformed data
Norm_TB_20_Evar_Ba <- lm(data = subset(CommunityMetrics_Basal, year == 2020 & site== "TB"), Evar  ~ rainfall_reduction*livestock_util_2019)
ols_plot_resid_hist(Norm_TB_20_Evar_Ba) 
ols_test_normality(Norm_TB_20_Evar_Ba) #not normal

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


#### Stats: Fort Keogh Aerial + Basal - Evar ####

#FK 2018 - checking drought and grazing
FK_18_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "FK"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Evar_Aerial, type = 3) #NS

#FK 2019 - just drought
FK_19_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "FK"), Evar ~ rainfall_reduction + (1|block) + (1|block:slope))
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
TB_22_Evar_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2022 & site== "TB"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_22_Evar_Aerial, type = 3) #NS

#basal
#TB 2018 - checking drought and grazing
TB_18_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2018 & site== "TB"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_18_Evar_Basal, type = 3) #NS

#TB 2019 - just drought
TB_19_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2019 & site== "TB"), Evar ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(TB_19_Evar_Basal, type = 3) #NS

#TB 2020 - droughtxgrazing
TB_20_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2020 & site== "TB"), Evar ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
anova(TB_20_Evar_Basal, type = 3) #grazing (0.02589)

#TB 2021- droughtxgrazing
TB_21_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2021 & site== "TB"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(TB_21_Evar_Basal, type = 3) #drought (0.08525)

#TB 2022- droughtxgrazing
TB_22_Evar_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2022 & site== "TB"), Evar ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
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


#### Stats: Fort Keogh Aerial + Basal - Shannon's ####

#FK 2018 - checking drought and grazing
FK_18_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "FK"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_18_Shannon_Aerial, type = 3) #NS

#FK 2019 - just drought
FK_19_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2019 & site== "FK"), Shannon ~ rainfall_reduction + (1|block) + (1|block:slope))
anova(FK_19_Shannon_Aerial, type = 3) #drought (0.008147)

#FK 2020 - droughtxgrazing
FK_20_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2020 & site== "FK"), Shannon ~ rainfall_reduction*livestock_util_2019 + (1|block) + (1|block:slope))
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
FK_21_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2021 & site== "FK"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_21_Shannon_Basal, type = 3) #NS

#FK 2022- droughtxgrazing
FK_22_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2022 & site== "FK"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
anova(FK_22_Shannon_Basal, type = 3) #grazing (0.008034), interaction (0.054988)

#### Stats: Thunder  Basin Aerial + Basal - Shannon's####

#TB 2018 - checking drought and grazing
TB_18_Shannon_Aerial <- lmerTest::lmer(data = subset(CommunityMetrics_Aerial, year == 2018 & site== "TB"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
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
TB_22_Shannon_Basal <- lmerTest::lmer(data = subset(CommunityMetrics_Basal, year == 2022 & site== "TB"), Shannon ~ rainfall_reduction*grazing_treatment + (1|block) + (1|block:slope))
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

