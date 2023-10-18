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
library(olsrr)
library(vegan)


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
  dplyr::select(Species_Code,Site,Block,Grazing_Treatment,Total.Area,NOTES) %>% 
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

#Read in Plot Data
plot_layoutK<-read.csv("DxG_Plant_Traits/GMDR_site_plot_metadata.csv") %>% 
  dplyr::select(site,block,paddock,plot,slope,rainfall_reduction,drought,grazing_category,grazing_treatment,livestock_util_2019,livestock_util_2020,livestock_util_2021)
plot_layoutK$plot<-as.factor(plot_layoutK$plot)

#Read in Species Comp Data
#All data have previously been cleaned and saved in GMDR_SpeciesComp_Cleaning.R
Species_Comp_RelCov_Clean<-read.csv("Species_Comp_RelCov_Clean.csv")
Species_Comp_RelCov_Clean$plot<-as.factor(Species_Comp_RelCov_Clean$plot)

#### Clean up trait data ####


##make dataframes match up

#remove all NAs from height column to remove any plants not collected/measured but to avoid removing plants where percent green was not collected  by accident
Traits_Clean <- Traits [complete.cases(Traits[ , 6]),] %>% 
  filter(comments_lab!="not BRTE - did not measure, remove from data") %>% 
  filter(comments_lab!="maybe KOMA?") %>% 
  filter(comments_lab!="add 0.0012 to total biomass (wet)") %>% 
  mutate(wet_leaf_weight_g=as.numeric(ifelse(wet_leaf_weight_g=="<0.0001","0.00001",wet_leaf_weight_g)))

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
  dplyr::select(-genus_species,-Genus_Species_2) %>% 
  filter(SLA==ifelse(SLA>447,NA,SLA)) %>% 
  filter(Total.Area==ifelse(Total.Area>7.8,NA,Total.Area))

#outlier test for SLA
summary(Traits_Clean_2$SLA)
boxplot(Traits_Clean_2$SLA,
        ylab = "SLA"
)
boxplot.stats(Traits_Clean_2$SLA)$out

#Rosner Test for Outliers
test_SLA <- rosnerTest(Traits_Clean_2$SLA,
                       k = 75
)
test_SLA

#outlier test for leaf area
summary(Traits_Clean_2$Total.Area)
boxplot(Traits_Clean_2$Total.Area,
        ylab = "Leaf Area"
)
boxplot.stats(Traits_Clean_2$Total.Area)$out

#Rosner Test for Outliers
test_Total.Area <- rosnerTest(Traits_Clean_2$Total.Area,
                       k = 47
)
test_Total.Area



#### Calculate CWM ####

#Average traits to get one trait number for each block x species
AverageTraits<-Traits_Clean_2%>% 
  group_by(Site,Genus_Species_Correct,species_code,Season,DxG_block) %>% 
  summarise(
    Avg_height_cm=mean(height_cm,na.rm=T),
    Avg_leaf_thickness=mean(leaf_thickness_.mm.,na.rm=T),
    Avg_LDMC=mean(LDMC,na.rm=T),
    Avg_Area=mean(Total.Area,na.rm=T),
    Avg_SLA=mean(SLA,na.rm=T)
  ) %>% 
  ungroup() 

#Find community weighted mean values
CWM_Collected_Data<- Species_Comp_RelCov_Clean %>% 
  left_join(plot_layoutK) %>% 
  rename(Site=site) %>%
  rename(Genus_Species_Correct=Genus_Species) %>% 
  rename(DxG_block=block) %>% 
  filter(!is.na(Relative_Cover)) %>% 
  filter(Relative_Cover!=0) %>% 
  left_join(AverageTraits) %>%
  group_by(year,Site,plot,DxG_block,paddock,slope,rainfall_reduction,drought) %>% 
  #calculate CWM using tidyr function, removing NAs for now until more data are collected
  summarise(
    Height_CWM=weighted.mean(Avg_height_cm,Relative_Cover,na.rm = T),
    #Biomass_CWM=weighted.mean(Avg_biomass_mg,Relative_Cover,na.rm=T),
    LeafThickness_CWM=weighted.mean(Avg_leaf_thickness,Relative_Cover,na.rm=T),
    LDMC_CWM=weighted.mean(Avg_LDMC,Relative_Cover,na.rm=T),
    Area_CWM=weighted.mean(Avg_Area,na.rm=T),
    SLA_CWM=weighted.mean(Avg_SLA,na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  rename(block=DxG_block)



#### Normality: CWM Height FK ####

#FK - Height CWM - 2019
CWM_Height_Norm_19_FK <- lm(data = subset(CWM_Collected_Data, year == 2019 & Site== "FK"), 1/log(Height_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Height_Norm_19_FK) 
ols_test_normality(CWM_Height_Norm_19_FK) #normal

#FK - Height CWM -2020
CWM_Height_Norm_20_FK <- lm(data = subset(CWM_Collected_Data, year == 2020 & Site== "FK"), 1/log(Height_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Height_Norm_20_FK) 
ols_test_normality(CWM_Height_Norm_20_FK) #not ideal but fine

#FK - Height CWM -2021
CWM_Height_Norm_21_FK <- lm(data = subset(CWM_Collected_Data, year == 2021 & Site== "FK"), log(Height_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Height_Norm_21_FK) 
ols_test_normality(CWM_Height_Norm_21_FK) #normal

#FK - Height CWM -2022
CWM_Height_Norm_22_FK <- lm(data = subset(CWM_Collected_Data, year == 2022 & Site== "FK"), log(Height_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Height_Norm_22_FK) 
ols_test_normality(CWM_Height_Norm_22_FK) #not ideal but fine

#FK - Height CWM -2023
CWM_Height_Norm_23_FK <- lm(data = subset(CWM_Collected_Data, year == 2023 & Site== "FK"), 1/log(Height_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Height_Norm_23_FK) 
ols_test_normality(CWM_Height_Norm_23_FK) #normal

### Stats: CWM Height FK #### 

#CWM of height for FK 2019 - LMER
FK_Height_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), 1/log(Height_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Height_2019_LMER_slope, type = 3) #ns

#CWM of height for FK 2020 - LMER
FK_Height_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), 1/log(Height_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Height_2020_LMER_slope, type = 3) #ns

#CWM of height for FK 2021 - LMER
FK_Height_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), log(Height_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Height_2021_LMER_slope, type = 3) #ns

#CWM of height for FK 2022 - LMER
FK_Height_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), log(Height_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Height_2022_LMER_slope, type = 3) #ns

#CWM of height for FK 2023 - LMER
FK_Height_2023_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2023&Site=="FK"), 1/log(Height_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Height_2023_LMER_slope, type = 3) #ns

# adjust pvalues for height FK
p.adjust(0.7448, method = "BH", n=5)
p.adjust(0.5778, method = "BH", n=5)
p.adjust(0.03485, method = "BH", n=5)
p.adjust(0.24, method = "BH", n=5)
p.adjust(0.0739, method = "BH", n=5)

#### Normality: CWM Height TB ####
#TB - Height CWM - 2019
CWM_Height_Norm_19_TB <- lm(data = subset(CWM_Collected_Data, year == 2019 & Site== "TB"), 1/log(Height_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Height_Norm_19_TB) 
ols_test_normality(CWM_Height_Norm_19_TB) #normal

#TB - Height CWM -2020
CWM_Height_Norm_20_TB <- lm(data = subset(CWM_Collected_Data, year == 2020 & Site== "TB"), 1/log(Height_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Height_Norm_20_TB) 
ols_test_normality(CWM_Height_Norm_20_TB) #normal

#TB - Height CWM -2021
CWM_Height_Norm_21_TB <- lm(data = subset(CWM_Collected_Data, year == 2021 & Site== "TB"), log(Height_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Height_Norm_21_TB) 
ols_test_normality(CWM_Height_Norm_21_TB) #normal

#TB - Height CWM -2022
CWM_Height_Norm_22_TB <- lm(data = subset(CWM_Collected_Data, year == 2022 & Site== "TB"), 1/log(Height_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Height_Norm_22_TB) 
ols_test_normality(CWM_Height_Norm_22_TB) #not ideal but fine

#TB - Height CWM -2023
CWM_Height_Norm_23_TB <- lm(data = subset(CWM_Collected_Data, year == 2023 & Site== "TB"), 1/log(Height_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Height_Norm_23_TB) 
ols_test_normality(CWM_Height_Norm_23_TB) #normal

### Stats: CWM Height TB #### 

#CWM of height for TB 2019 - LMER
TB_Height_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), 1/log(Height_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Height_2019_LMER_slope, type = 3) #ns

#CWM of height for TB 2020 - LMER
TB_Height_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), 1/log(Height_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Height_2020_LMER_slope, type = 3) #ns

#CWM of height for TB 2021 - LMER
TB_Height_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), log(Height_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Height_2021_LMER_slope, type = 3) #ns

#CWM of height for TB 2022 - LMER
TB_Height_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), 1/log(Height_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Height_2022_LMER_slope, type = 3) #ns

#CWM of height for TB 2023 - LMER
TB_Height_2023_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2023&Site=="TB"), 1/log(Height_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Height_2023_LMER_slope, type = 3) #ns

# adjust pvalues for height TB
p.adjust(0.8736, method = "BH", n=5)
p.adjust(0.3806, method = "BH", n=5)
p.adjust(0.4228, method = "BH", n=5)
p.adjust(0.2576, method = "BH", n=5)
p.adjust(0.1584, method = "BH", n=5)

#### Normality: CWM LeafThickness FK ####

#FK - LeafThickness CWM - 2019
CWM_LeafThickness_Norm_19_FK <- lm(data = subset(CWM_Collected_Data, year == 2019 & Site== "FK"), 1/(LeafThickness_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LeafThickness_Norm_19_FK) 
ols_test_normality(CWM_LeafThickness_Norm_19_FK) #normalish

#FK - LeafThickness CWM -2020
CWM_LeafThickness_Norm_20_FK <- lm(data = subset(CWM_Collected_Data, year == 2020 & Site== "FK"), (LeafThickness_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LeafThickness_Norm_20_FK) 
ols_test_normality(CWM_LeafThickness_Norm_20_FK) #normal

#FK - LeafThickness CWM -2021
CWM_LeafThickness_Norm_21_FK <- lm(data = subset(CWM_Collected_Data, year == 2021 & Site== "FK"), (LeafThickness_CWM)*1/3  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LeafThickness_Norm_21_FK) 
ols_test_normality(CWM_LeafThickness_Norm_21_FK) #not normal but fine

#FK - LeafThickness CWM -2022
CWM_LeafThickness_Norm_22_FK <- lm(data = subset(CWM_Collected_Data, year == 2022 & Site== "FK"), (LeafThickness_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LeafThickness_Norm_22_FK) 
ols_test_normality(CWM_LeafThickness_Norm_22_FK) #not ideal but fine

#FK - LeafThickness CWM -2023
CWM_LeafThickness_Norm_23_FK <- lm(data = subset(CWM_Collected_Data, year == 2023 & Site== "FK"), (LeafThickness_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LeafThickness_Norm_23_FK) 
ols_test_normality(CWM_LeafThickness_Norm_23_FK) #normal

### Stats: CWM LeafThickness FK #### 

#CWM of LeafThickness for FK 2019 - LMER
FK_LeafThickness_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), 1/(LeafThickness_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LeafThickness_2019_LMER_slope, type = 3) #ns

#CWM of LeafThickness for FK 2020 - LMER
FK_LeafThickness_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), (LeafThickness_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LeafThickness_2020_LMER_slope, type = 3) #ns

#CWM of LeafThickness for FK 2021 - LMER
FK_LeafThickness_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), (LeafThickness_CWM)*1/3 ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LeafThickness_2021_LMER_slope, type = 3) #ns

#CWM of LeafThickness for FK 2022 - LMER
FK_LeafThickness_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), (LeafThickness_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LeafThickness_2022_LMER_slope, type = 3) #ns

#CWM of LeafThickness for FK 2023 - LMER
FK_LeafThickness_2023_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2023&Site=="FK"), (LeafThickness_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LeafThickness_2023_LMER_slope, type = 3) #ns

# adjust pvalues for LeafThickness FK
p.adjust(0.8168, method = "BH", n=5)
p.adjust(0.7607, method = "BH", n=5)
p.adjust(0.1264, method = "BH", n=5)
p.adjust(0.1743, method = "BH", n=5)
p.adjust(0.5461, method = "BH", n=5)

#### Normality: CWM Leaf Thickness TB ####

#TB - LeafThickness CWM - 2019
CWM_LeafThickness_Norm_19_TB <- lm(data = subset(CWM_Collected_Data, year == 2019 & Site== "TB"), (LeafThickness_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LeafThickness_Norm_19_TB) 
ols_test_normality(CWM_LeafThickness_Norm_19_TB) #normal

#TB - LeafThickness CWM -2020
CWM_LeafThickness_Norm_20_TB <- lm(data = subset(CWM_Collected_Data, year == 2020 & Site== "TB"), log(LeafThickness_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LeafThickness_Norm_20_TB) 
ols_test_normality(CWM_LeafThickness_Norm_20_TB) #normal

#TB - LeafThickness CWM -2021
CWM_LeafThickness_Norm_21_TB <- lm(data = subset(CWM_Collected_Data, year == 2021 & Site== "TB"), (LeafThickness_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LeafThickness_Norm_21_TB) 
ols_test_normality(CWM_LeafThickness_Norm_21_TB) #normal

#TB - LeafThickness CWM -2022
CWM_LeafThickness_Norm_22_TB <- lm(data = subset(CWM_Collected_Data, year == 2022 & Site== "TB"), (LeafThickness_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LeafThickness_Norm_22_TB) 
ols_test_normality(CWM_LeafThickness_Norm_22_TB) #not ideal but fine

#TB - LeafThickness CWM -2023
CWM_LeafThickness_Norm_23_TB <- lm(data = subset(CWM_Collected_Data, year == 2023 & Site== "TB"), (LeafThickness_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LeafThickness_Norm_23_TB) 
ols_test_normality(CWM_LeafThickness_Norm_23_TB) #normal

### Stats: CWM LeafThickness TB #### 

#CWM of LeafThickness for TB 2019 - LMER
TB_LeafThickness_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), (LeafThickness_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LeafThickness_2019_LMER_slope, type = 3) #ns

#CWM of LeafThickness for TB 2020 - LMER
TB_LeafThickness_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), log(LeafThickness_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LeafThickness_2020_LMER_slope, type = 3) #ns

#CWM of LeafThickness for TB 2021 - LMER
TB_LeafThickness_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), (LeafThickness_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LeafThickness_2021_LMER_slope, type = 3) #ns

#CWM of LeafThickness for TB 2022 - LMER
TB_LeafThickness_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), (LeafThickness_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LeafThickness_2022_LMER_slope, type = 3) #ns

#CWM of LeafThickness for TB 2023 - LMER
TB_LeafThickness_2023_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2023&Site=="TB"), (LeafThickness_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LeafThickness_2023_LMER_slope, type = 3) #ns

# adjust pvalues for LeafThickness TB
p.adjust(0.8958, method = "BH", n=5)
p.adjust(0.7826, method = "BH", n=5)
p.adjust(0.9232, method = "BH", n=5)
p.adjust(0.4546, method = "BH", n=5)
p.adjust(0.5632, method = "BH", n=5)



#### Normality: CWM LDMC FK ####

#FK - LDMC CWM - 2019
CWM_LDMC_Norm_19_FK <- lm(data = subset(CWM_Collected_Data, year == 2019 & Site== "FK"), log(LDMC_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LDMC_Norm_19_FK) 
ols_test_normality(CWM_LDMC_Norm_19_FK) #not normal but okay

#FK - LDMC CWM -2020
CWM_LDMC_Norm_20_FK <- lm(data = subset(CWM_Collected_Data, year == 2020 & Site== "FK"), log(LDMC_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LDMC_Norm_20_FK) 
ols_test_normality(CWM_LDMC_Norm_20_FK) #normal

#FK - LDMC CWM -2021
CWM_LDMC_Norm_21_FK <- lm(data = subset(CWM_Collected_Data, year == 2021 & Site== "FK"), log(LDMC_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LDMC_Norm_21_FK) 
ols_test_normality(CWM_LDMC_Norm_21_FK) #normal

#FK - LDMC CWM -2022
CWM_LDMC_Norm_22_FK <- lm(data = subset(CWM_Collected_Data, year == 2022 & Site== "FK"), 1/(LDMC_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LDMC_Norm_22_FK) 
ols_test_normality(CWM_LDMC_Norm_22_FK) #not normal but okay

#FK - LDMC CWM -2023
CWM_LDMC_Norm_23_FK <- lm(data = subset(CWM_Collected_Data, year == 2023 & Site== "FK"), log(LDMC_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LDMC_Norm_23_FK) 
ols_test_normality(CWM_LDMC_Norm_23_FK) #normal

#### Stats: CWM LDMC FK #### 

#CWM of LDMC for FK 2019 - LMER
FK_LDMC_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), log(LDMC_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LDMC_2019_LMER_slope, type = 3) #ns

#CWM of LDMC for FK 2020 - LMER
FK_LDMC_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), log(LDMC_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LDMC_2020_LMER_slope, type = 3) #ns

#CWM of LDMC for FK 2021 - LMER
FK_LDMC_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), log(LDMC_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LDMC_2021_LMER_slope, type = 3) #ns

#CWM of LDMC for FK 2022 - LMER
FK_LDMC_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), 1/(LDMC_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LDMC_2022_LMER_slope, type = 3) #ns

#CWM of LDMC for FK 2023 - LMER
FK_LDMC_2023_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2023&Site=="FK"), log(LDMC_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_LDMC_2023_LMER_slope, type = 3) #ns

# adjust pvalues for LDMC FK
p.adjust(0.9316, method = "BH", n=5)
p.adjust(0.9619, method = "BH", n=5)
p.adjust(0.7951, method = "BH", n=5)
p.adjust(0.2426, method = "BH", n=5)
p.adjust(0.1384, method = "BH", n=5)

#### Normality: CWM LDMC TB ####

#TB - LDMC CWM - 2019
CWM_LDMC_Norm_19_TB <- lm(data = subset(CWM_Collected_Data, year == 2019 & Site== "TB"), 1/sqrt(LDMC_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LDMC_Norm_19_TB) 
ols_test_normality(CWM_LDMC_Norm_19_TB) #not normal but okay

#TB - LDMC CWM -2020
CWM_LDMC_Norm_20_TB <- lm(data = subset(CWM_Collected_Data, year == 2020 & Site== "TB"), log(LDMC_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LDMC_Norm_20_TB) 
ols_test_normality(CWM_LDMC_Norm_20_TB) #not normal but alright

#TB - LDMC CWM -2021
CWM_LDMC_Norm_21_TB <- lm(data = subset(CWM_Collected_Data, year == 2021 & Site== "TB"), 1/(LDMC_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LDMC_Norm_21_TB) 
ols_test_normality(CWM_LDMC_Norm_21_TB) #not normal but okay

#TB - LDMC CWM -2022
CWM_LDMC_Norm_22_TB <- lm(data = subset(CWM_Collected_Data, year == 2022 & Site== "TB"), 1/(LDMC_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LDMC_Norm_22_TB) 
ols_test_normality(CWM_LDMC_Norm_22_TB) #not ideal but fine

#TB - LDMC CWM -2023
CWM_LDMC_Norm_23_TB <- lm(data = subset(CWM_Collected_Data, year == 2023 & Site== "TB"), 1/(LDMC_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_LDMC_Norm_23_TB) 
ols_test_normality(CWM_LDMC_Norm_23_TB) #not ideal but fine

#### Stats: CWM LDMC TB #### 

#CWM of LDMC for TB 2019 - LMER
TB_LDMC_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), 1/sqrt(LDMC_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LDMC_2019_LMER_slope, type = 3) #ns

#CWM of LDMC for TB 2020 - LMER
TB_LDMC_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), log(LDMC_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LDMC_2020_LMER_slope, type = 3) #ns

#CWM of LDMC for TB 2021 - LMER
TB_LDMC_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), 1/(LDMC_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LDMC_2021_LMER_slope, type = 3) #ns

#CWM of LDMC for TB 2022 - LMER
TB_LDMC_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), 1/(LDMC_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LDMC_2022_LMER_slope, type = 3) #ns

#CWM of LDMC for TB 2023 - LMER
TB_LDMC_2023_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2023&Site=="TB"), 1/(LDMC_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_LDMC_2023_LMER_slope, type = 3) #ns

# adjust pvalues for LDMC TB
p.adjust(0.7177, method = "BH", n=5)
p.adjust(0.5127, method = "BH", n=5)
p.adjust(0.4164, method = "BH", n=5)
p.adjust(0.5166, method = "BH", n=5)
p.adjust(0.577, method = "BH", n=5)



#### Normality: CWM SLA FK ####

#FK - SLA CWM - 2019
CWM_SLA_Norm_19_FK <- lm(data = subset(CWM_Collected_Data, year == 2019 & Site== "FK"), log(SLA_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_SLA_Norm_19_FK) 
ols_test_normality(CWM_SLA_Norm_19_FK) #normal

#FK - SLA CWM -2020
CWM_SLA_Norm_20_FK <- lm(data = subset(CWM_Collected_Data, year == 2020 & Site== "FK"), (SLA_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_SLA_Norm_20_FK) 
ols_test_normality(CWM_SLA_Norm_20_FK) #normalish

#FK - SLA CWM -2021
CWM_SLA_Norm_21_FK <- lm(data = subset(CWM_Collected_Data, year == 2021 & Site== "FK"), log(SLA_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_SLA_Norm_21_FK) 
ols_test_normality(CWM_SLA_Norm_21_FK) #normal

#FK - SLA CWM -2022
CWM_SLA_Norm_22_FK <- lm(data = subset(CWM_Collected_Data, year == 2022 & Site== "FK"), log(SLA_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_SLA_Norm_22_FK) 
ols_test_normality(CWM_SLA_Norm_22_FK) #normalish

#FK - SLA CWM -2023
CWM_SLA_Norm_23_FK <- lm(data = subset(CWM_Collected_Data, year == 2023 & Site== "FK"), log(SLA_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_SLA_Norm_23_FK) 
ols_test_normality(CWM_SLA_Norm_23_FK) #not normal

#### Stats: CWM SLA FK #### 

#CWM of SLA for FK 2019 - LMER
FK_SLA_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), log(SLA_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_SLA_2019_LMER_slope, type = 3) #ns

#CWM of SLA for FK 2020 - LMER
FK_SLA_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), (SLA_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_SLA_2020_LMER_slope, type = 3) #ns

#CWM of SLA for FK 2021 - LMER
FK_SLA_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), log(SLA_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_SLA_2021_LMER_slope, type = 3) #ns

#CWM of SLA for FK 2022 - LMER
FK_SLA_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), log(SLA_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_SLA_2022_LMER_slope, type = 3) #ns

#CWM of SLA for FK 2023 - LMER
FK_SLA_2023_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2023&Site=="FK"), log(SLA_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_SLA_2023_LMER_slope, type = 3) #ns

# adjust pvalues for SLA FK
p.adjust(0.18, method = "BH", n=5)
p.adjust(0.06, method = "BH", n=5)
p.adjust(0.96, method = "BH", n=5)
p.adjust(0.53, method = "BH", n=5)
p.adjust(0.54, method = "BH", n=5)

#### Normality: CWM SLA TB ####

#TB - SLA CWM - 2019
CWM_SLA_Norm_19_TB <- lm(data = subset(CWM_Collected_Data, year == 2019 & Site== "TB"), log(SLA_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_SLA_Norm_19_TB) 
ols_test_normality(CWM_SLA_Norm_19_TB) #normalish

#TB - SLA CWM -2020
CWM_SLA_Norm_20_TB <- lm(data = subset(CWM_Collected_Data, year == 2020 & Site== "TB"), log(SLA_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_SLA_Norm_20_TB) 
ols_test_normality(CWM_SLA_Norm_20_TB) #normal

#TB - SLA CWM -2021
CWM_SLA_Norm_21_TB <- lm(data = subset(CWM_Collected_Data, year == 2021 & Site== "TB"), sqrt(SLA_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_SLA_Norm_21_TB) 
ols_test_normality(CWM_SLA_Norm_21_TB) #normal

#TB - SLA CWM -2022
CWM_SLA_Norm_22_TB <- lm(data = subset(CWM_Collected_Data, year == 2022 & Site== "TB"), log(SLA_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_SLA_Norm_22_TB) 
ols_test_normality(CWM_SLA_Norm_22_TB) #normal

#TB - SLA CWM -2023
CWM_SLA_Norm_23_TB <- lm(data = subset(CWM_Collected_Data, year == 2023 & Site== "TB"), sqrt(SLA_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_SLA_Norm_23_TB) 
ols_test_normality(CWM_SLA_Norm_23_TB) #not normal but okay

### Stats: CWM SLA TB #### 

#CWM of SLA for TB 2019 - LMER
TB_SLA_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), log(SLA_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_SLA_2019_LMER_slope, type = 3) #ns

#CWM of SLA for TB 2020 - LMER
TB_SLA_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), log(SLA_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_SLA_2020_LMER_slope, type = 3) #ns

#CWM of SLA for TB 2021 - LMER
TB_SLA_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), sqrt(SLA_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_SLA_2021_LMER_slope, type = 3) #ns

#CWM of SLA for TB 2022 - LMER
TB_SLA_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), log(SLA_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_SLA_2022_LMER_slope, type = 3) #ns

#CWM of SLA for TB 2023 - LMER
TB_SLA_2023_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2023&Site=="TB"), sqrt(SLA_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_SLA_2023_LMER_slope, type = 3) #ns

# adjust pvalues for SLA TB
p.adjust(0.58, method = "BH", n=5)
p.adjust(0.69, method = "BH", n=5)
p.adjust(0.96, method = "BH", n=5)
p.adjust(0.82, method = "BH", n=5)
p.adjust(0.11, method = "BH", n=5)


#### Normality: CWM Area FK ####

#FK - Area CWM - 2019
CWM_Area_Norm_19_FK <- lm(data = subset(CWM_Collected_Data, year == 2019 & Site== "FK"), (Area_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Area_Norm_19_FK) 
ols_test_normality(CWM_Area_Norm_19_FK) #normal

#FK - Area CWM -2020
CWM_Area_Norm_20_FK <- lm(data = subset(CWM_Collected_Data, year == 2020 & Site== "FK"), (Area_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Area_Norm_20_FK) 
ols_test_normality(CWM_Area_Norm_20_FK) #normal

#FK - Area CWM -2021
CWM_Area_Norm_21_FK <- lm(data = subset(CWM_Collected_Data, year == 2021 & Site== "FK"), (Area_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Area_Norm_21_FK) 
ols_test_normality(CWM_Area_Norm_21_FK) #normal

#FK - Area CWM -2022
CWM_Area_Norm_22_FK <- lm(data = subset(CWM_Collected_Data, year == 2022 & Site== "FK"), (Area_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Area_Norm_22_FK) 
ols_test_normality(CWM_Area_Norm_22_FK) #normal

#FK - Area CWM -2023
CWM_Area_Norm_23_FK <- lm(data = subset(CWM_Collected_Data, year == 2023 & Site== "FK"), log(Area_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Area_Norm_23_FK) 
ols_test_normality(CWM_Area_Norm_23_FK) #normal

#### Stats: CWM Area FK #### 

#CWM of Area for FK 2019 - LMER
FK_Area_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="FK"), (Area_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Area_2019_LMER_slope, type = 3) #ns

#CWM of Area for FK 2020 - LMER
FK_Area_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="FK"), (Area_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Area_2020_LMER_slope, type = 3) #ns

#CWM of Area for FK 2021 - LMER
FK_Area_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="FK"), (Area_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Area_2021_LMER_slope, type = 3) #ns

#CWM of Area for FK 2022 - LMER
FK_Area_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="FK"), (Area_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Area_2022_LMER_slope, type = 3) #ns

#CWM of Area for FK 2023 - LMER
FK_Area_2023_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2023&Site=="FK"), log(Area_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FK_Area_2023_LMER_slope, type = 3) #ns

# adjust pvalues for Area FK
p.adjust(0.1958, method = "BH", n=5)
p.adjust(0.0017, method = "BH", n=5)
p.adjust(0.1084, method = "BH", n=5)
p.adjust(0.609, method = "BH", n=5)
p.adjust(0.6982, method = "BH", n=5)

#### Normality: CWM Area TB ####

#TB - Area CWM - 2019
CWM_Area_Norm_19_TB <- lm(data = subset(CWM_Collected_Data, year == 2019 & Site== "TB"), (Area_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Area_Norm_19_TB) 
ols_test_normality(CWM_Area_Norm_19_TB) #normal

#TB - Area CWM -2020
CWM_Area_Norm_20_TB <- lm(data = subset(CWM_Collected_Data, year == 2020 & Site== "TB"), log(Area_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Area_Norm_20_TB) 
ols_test_normality(CWM_Area_Norm_20_TB) #normal

#TB - Area CWM -2021
CWM_Area_Norm_21_TB <- lm(data = subset(CWM_Collected_Data, year == 2021 & Site== "TB"), log(Area_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Area_Norm_21_TB) 
ols_test_normality(CWM_Area_Norm_21_TB) #normal

#TB - Area CWM -2022
CWM_Area_Norm_22_TB <- lm(data = subset(CWM_Collected_Data, year == 2022 & Site== "TB"), log(Area_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Area_Norm_22_TB) 
ols_test_normality(CWM_Area_Norm_22_TB) #not ideal but fine

#TB - Area CWM -2023
CWM_Area_Norm_23_TB <- lm(data = subset(CWM_Collected_Data, year == 2023 & Site== "TB"), log(Area_CWM)  ~ rainfall_reduction)
ols_plot_resid_hist(CWM_Area_Norm_23_TB) 
ols_test_normality(CWM_Area_Norm_23_TB) #normal

### Stats: CWM Area TB #### 

#CWM of Area for TB 2019 - LMER
TB_Area_2019_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2019&Site=="TB"), (Area_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Area_2019_LMER_slope, type = 3) #ns

#CWM of Area for TB 2020 - LMER
TB_Area_2020_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2020&Site=="TB"), log(Area_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Area_2020_LMER_slope, type = 3) #ns

#CWM of Area for TB 2021 - LMER
TB_Area_2021_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2021&Site=="TB"), log(Area_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Area_2021_LMER_slope, type = 3) #ns

#CWM of Area for TB 2022 - LMER
TB_Area_2022_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2022&Site=="TB"), log(Area_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Area_2022_LMER_slope, type = 3) #ns

#CWM of Area for TB 2023 - LMER
TB_Area_2023_LMER_slope <- lmerTest::lmer(data = subset(CWM_Collected_Data,year==2023&Site=="TB"), log(Area_CWM) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(TB_Area_2023_LMER_slope, type = 3) #ns

# adjust pvalues for Area TB
p.adjust(0.3642, method = "BH", n=5)
p.adjust(0.5677, method = "BH", n=5)
p.adjust(0.2897, method = "BH", n=5)
p.adjust(0.5407, method = "BH", n=5)
p.adjust(0.7767, method = "BH", n=5)



### CWM Multivariate Space ####

#Create seperate dataframes for each site and year for PCAs
CWM_Collected_Data_FK_19<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2019)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Height_CWM,LeafThickness_CWM,LDMC_CWM,SLA_CWM,Area_CWM)

CWM_Collected_Data_FK_20<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2020)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Height_CWM,LeafThickness_CWM,LDMC_CWM,SLA_CWM,Area_CWM)

CWM_Collected_Data_FK_21<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2021)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Height_CWM,LeafThickness_CWM,LDMC_CWM,SLA_CWM,Area_CWM)

CWM_Collected_Data_FK_22<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2022)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Height_CWM,LeafThickness_CWM,LDMC_CWM,SLA_CWM,Area_CWM)

CWM_Collected_Data_FK_23<-CWM_Collected_Data %>% 
  filter(Site=="FK" & year==2023)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Height_CWM,LeafThickness_CWM,LDMC_CWM,SLA_CWM,Area_CWM)

#TB

CWM_Collected_Data_TB_19<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2019) %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Height_CWM,LeafThickness_CWM,LDMC_CWM,SLA_CWM,Area_CWM)

CWM_Collected_Data_TB_20<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2020) %>%  
  na.omit(LDMC_CWM)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Height_CWM,LeafThickness_CWM,LDMC_CWM,SLA_CWM,Area_CWM)

CWM_Collected_Data_TB_21<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2021) %>%
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Height_CWM,LeafThickness_CWM,LDMC_CWM,SLA_CWM,Area_CWM)

CWM_Collected_Data_TB_22<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2022)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Height_CWM,LeafThickness_CWM,LDMC_CWM,SLA_CWM,Area_CWM)

CWM_Collected_Data_TB_23<-CWM_Collected_Data %>% 
  filter(Site=="TB" & year==2023)%>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat,Height_CWM,LeafThickness_CWM,LDMC_CWM,SLA_CWM,Area_CWM)

#make two seperate dataframes for each year and site to have treatment data and trait data seperate
#adding 1 to all leaf thickness measures since they are negative numbers

## FK ##
#2019
CWM_FK_19_Trait<-CWM_Collected_Data_FK_19 %>% 
  dplyr::select(-year,-Site,-plot,-slope,-block,-paddock,-rainfall_reduction,-drought,-Rainfall_reduction_cat) 

CWM_FK_19_Treatment<-CWM_Collected_Data_FK_19 %>% 
  dplyr::select(year,Site,plot,block,paddock,slope,rainfall_reduction,drought,Rainfall_reduction_cat)

#2020
CWM_FK_20_Trait<-CWM_Collected_Data_FK_20 %>% 
  dplyr::select(-year,-Site,-plot,-block,-slope,-paddock,-rainfall_reduction,-drought,-Rainfall_reduction_cat)

CWM_FK_20_Treatment<-CWM_Collected_Data_FK_20 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat)

#2021
CWM_FK_21_Trait<-CWM_Collected_Data_FK_21 %>% 
  dplyr::select(-year,-Site,-plot,-block,-slope,-paddock,-rainfall_reduction,-drought,-Rainfall_reduction_cat)

CWM_FK_21_Treatment<-CWM_Collected_Data_FK_21 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat)

#2022
CWM_FK_22_Trait<-CWM_Collected_Data_FK_22 %>% 
  dplyr::select(-year,-Site,-plot,-slope,-block,-paddock,-rainfall_reduction,-drought,-Rainfall_reduction_cat)

CWM_FK_22_Treatment<-CWM_Collected_Data_FK_22 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat)

#2023
CWM_FK_23_Trait<-CWM_Collected_Data_FK_23 %>% 
  dplyr::select(-year,-Site,-plot,-slope,-block,-paddock,-rainfall_reduction,-drought,-Rainfall_reduction_cat)

CWM_FK_23_Treatment<-CWM_Collected_Data_FK_23 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat)

## TB ##

#2019
CWM_TB_19_Trait<-CWM_Collected_Data_TB_19 %>% 
  dplyr::select(-year,-Site,-plot,-block,-paddock,-slope,-rainfall_reduction,-drought,-Rainfall_reduction_cat)

CWM_TB_19_Treatment<-CWM_Collected_Data_TB_19 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat)

#2020
CWM_TB_20_Trait<-CWM_Collected_Data_TB_20 %>% 
  dplyr::select(-year,-Site,-plot,-block,-paddock,-slope,-rainfall_reduction,-drought,-Rainfall_reduction_cat)

CWM_TB_20_Treatment<-CWM_Collected_Data_TB_20 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat)

#2021
CWM_TB_21_Trait<-CWM_Collected_Data_TB_21 %>% 
  dplyr::select(-year,-Site,-plot,-block,-paddock,-slope,-rainfall_reduction,-drought,-Rainfall_reduction_cat)

CWM_TB_21_Treatment<-CWM_Collected_Data_TB_21 %>% 
  dplyr::select(year,Site,plot,block,slope,paddock,rainfall_reduction,drought,Rainfall_reduction_cat)

#2022
CWM_TB_22_Trait<-CWM_Collected_Data_TB_22 %>% 
  dplyr::select(-year,-Site,-plot,-block,-paddock,-slope,-rainfall_reduction,-drought,-Rainfall_reduction_cat)

CWM_TB_22_Treatment<-CWM_Collected_Data_TB_22 %>% 
  dplyr::select(year,Site,plot,slope,block,paddock,rainfall_reduction,drought,Rainfall_reduction_cat)

#2023
CWM_TB_23_Trait<-CWM_Collected_Data_TB_23 %>% 
  dplyr::select(-year,-Site,-plot,-block,-paddock,-slope,-rainfall_reduction,-drought,-Rainfall_reduction_cat)

CWM_TB_23_Treatment<-CWM_Collected_Data_TB_23 %>% 
  dplyr::select(year,Site,plot,slope,block,paddock,rainfall_reduction,drought,Rainfall_reduction_cat)

# run PERMANOVA using adonis using trait dataframe as data to run adonis on and treatment dataframe as variables
#### Permanova FK ####
## FK ##
#FK 2019
PERMANOVA_FK_19 <-adonis2(CWM_FK_19_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_FK_19_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_19) #N.S

#FK 2020
PERMANOVA_FK_20 <-adonis2(CWM_FK_20_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_FK_20_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_20) 

#FK 2021
PERMANOVA_FK_21 <-adonis2(CWM_FK_21_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_FK_21_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_21) 

#FK 2022
PERMANOVA_FK_22 <-adonis2(CWM_FK_22_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_FK_22_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_22) 

#FK 2023
PERMANOVA_FK_23 <-adonis2(CWM_FK_23_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_FK_23_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_FK_23) 

# adjust pvalues for perMANOVA FK
p.adjust(0.4056, method = "BH", n=5)
p.adjust(0.3147, method = "BH", n=5)
p.adjust(0.9331, method = "BH", n=5)
p.adjust(0.9191, method = "BH", n=5)
p.adjust(0.7073, method = "BH", n=5)

#### PERMANOVA TB ####
##TB##
#TB 2019
PERMANOVA_TB_19 <-adonis2(CWM_TB_19_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_TB_19_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_19)  #NS

#TB 2020
PERMANOVA_TB_20 <-adonis2(CWM_TB_20_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_TB_20_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_20) #NS

#TB 2021
PERMANOVA_TB_21 <-adonis2(CWM_TB_21_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_TB_21_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_21) #NS

#TB 2022
PERMANOVA_TB_22 <-adonis2(CWM_TB_22_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_TB_22_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_22) 

#TB 2023
PERMANOVA_TB_23 <-adonis2(CWM_TB_23_Trait~Rainfall_reduction_cat + (1|block/slope), data = CWM_TB_23_Treatment, 
                          permutations = 1000, method = 'bray') 
print(PERMANOVA_TB_23) 

# adjust pvalues for perMANOVA FK
p.adjust(0.97, method = "BH", n=5)
p.adjust(0.987, method = "BH", n=5)
p.adjust(0.994, method = "BH", n=5)
p.adjust(0.965, method = "BH", n=5)
p.adjust(0.8012, method = "BH", n=5)



#### Calculate Multivariate FDis: FK ####

#create dataframe from the raw trait data where we subset FK data and then average across blocks, paddocks. then add species numbers 1-33 to assign to each species for future identification and analysis 
Avg_Traits_FK<-Traits_Clean_2 %>%
  filter(Site=="FK") %>% 
  group_by(Site,Genus_Species_Correct) %>% 
  summarise(
    Avg_height_cm=mean(height_cm,na.rm=T),
    Avg_leaf_thickness=mean(leaf_thickness_.mm.,na.rm=T),
    Avg_LDMC=mean(LDMC,na.rm=T),
    Avg_Area=mean(Total.Area, na.rm=T),
    Avg_SLA=mean(SLA,na.rm=T)
  ) 

Avg_Traits_FK_All<-Avg_Traits_FK%>% 
  filter(Genus_Species_Correct!="Pediomelum.esculentum") %>% 
  mutate(Sp_Num=c(1:30)) %>% 
  ungroup() 

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Data<-Avg_Traits_FK_All  %>% 
  dplyr::select(-Genus_Species_Correct,-Sp_Num,-Site) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data) <- c(1:30)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames<-Avg_Traits_FK_All %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK<- Species_Comp_RelCov_Clean %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  dplyr::select(-X) %>% 
  filter(aerial_basal=="Aerial") %>% 
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
  mutate(ID_Num=c(1:324)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity <- dbFD(Avg_Traits_FK_Data, Species_Comp_FK_Wide_Data,corr = "none")
summary(FK_FunctionalDiversity)

#### Calculate Multivariate FDis: TB ####
##create dataframe from the raw trait data where wesubset TB data and then average across blocks, paddocks. then add species numbers 1-33 to assign to each species for future identification and analysis 
Avg_Traits_TB<-Traits_Clean_2 %>%
  filter(Site=="TB") %>% 
  group_by(Site,Genus_Species_Correct) %>% 
  summarise(
    Avg_height_cm=mean(height_cm,na.rm=T),
    Avg_leaf_thickness=mean(leaf_thickness_.mm.,na.rm=T),
    Avg_LDMC=mean(LDMC,na.rm=T),
    Avg_Area=mean(Total.Area, na.rm=T),
    Avg_SLA=mean(SLA,narm=T)
  ) %>% 
  ungroup() 

Avg_Traits_TB_All<-Avg_Traits_TB %>% 
  filter(Genus_Species_Correct %in% c("Alyssum.desertorum", "Androsace.occidentalis","Aristida.purpurea","Artemisia.dracunculus","Artemisia.frigida","Bouteloua.gracilis","Bromus.arvensis","Bromus.tectorum","Carex.duriuscula","Carex.filifolia","Conyza.canadensis","Hedeoma.hispida","Hesperostipa.comata","Koeleria.macrantha","Lepidium.densiflorum","Lithospermum.incisum","Logfia.arvensis","Pascopyrum.smithii","Plantago.patagonica","Poa.secunda","Sphaeralcea.coccinea","Sporobolus.cryptandrus","Taraxacum.officinale","Tragopogon.dubius","Vulpia.octoflora")) %>% 
  mutate(Sp_Num=c(1:17))

#Create a matrix with just average trait data removing all identifiers
Avg_Traits_TB_Data<-Avg_Traits_TB_All %>% 
  dplyr::select(-Genus_Species_Correct,-Sp_Num,-Site) %>% 
  as.matrix()

#make row names 1-44 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data) <- c(1:17)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames<-Avg_Traits_TB_All %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num)

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB<- Species_Comp_RelCov_Clean %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  dplyr::select(-X) %>% 
  filter(aerial_basal=="Aerial") %>% 
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
  mutate(ID_Num=c(1:323)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity <- dbFD(Avg_Traits_TB_Data, Species_Comp_TB_Wide_Data,corr = "none")

#### FDis Data Merging ####

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
  mutate(FDis_All=FDis)


#### Normality: Multivariate FDis FK ####

#FK - FDis - 2019
FDis_Multi_Norm_19_FK <- lm(data = subset(Functional_Diversity, year == 2019 & site== "FK"), (FDis_All)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Multi_Norm_19_FK) 
ols_test_normality(FDis_Multi_Norm_19_FK) #normalish

#FK - FDis - 2020
FDis_Multi_Norm_20_FK <- lm(data = subset(Functional_Diversity, year == 2020 & site== "FK"), FDis_All  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Multi_Norm_20_FK) 
ols_test_normality(FDis_Multi_Norm_20_FK) #normalish

#FK - FDis - 2021
FDis_Multi_Norm_21_FK <- lm(data = subset(Functional_Diversity, year == 2021 & site== "FK"), FDis_All  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Multi_Norm_21_FK) 
ols_test_normality(FDis_Multi_Norm_21_FK) #normalish

#FK - FDis - 2022
FDis_Multi_Norm_22_FK <- lm(data = subset(Functional_Diversity, year == 2022 & site== "FK"), FDis_All  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Multi_Norm_22_FK) 
ols_test_normality(FDis_Multi_Norm_22_FK) #normal

#FK - FDis - 2023
FDis_Multi_Norm_23_FK <- lm(data = subset(Functional_Diversity, year == 2023 & site== "FK"),FDis_All  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Multi_Norm_23_FK) 
ols_test_normality(FDis_Multi_Norm_23_FK) #normal

#### Stats: Multivariate FDis FK ####

#FDis for Fort Keogh 2019 - LMER
FDis_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&site=="FK"), FDis_All ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 - LMER
FDis_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&site=="FK"), FDis_All ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&site=="FK"), FDis_All ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&site=="FK"), FDis_All ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK22_LMER, type = 3) #NS

#FDis for Fort Keogh 2023 - LMER
FDis_FK23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2023&site=="FK"), FDis_All ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK23_LMER, type = 3) #NS

# adjust pvalues for Area TB
p.adjust(0.098, method = "BH", n=5)
p.adjust(0.267, method = "BH", n=5)
p.adjust(0.6599, method = "BH", n=5)
p.adjust(0.539, method = "BH", n=5)
p.adjust(0.7278, method = "BH", n=5)



#### Normality: Multivariate FDis TB ####

#TB - FDis - 2019
FDis_Multi_Norm_19_TB <- lm(data = subset(Functional_Diversity, year == 2019 & site== "TB"), log(FDis_All)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Multi_Norm_19_TB) 
ols_test_normality(FDis_Multi_Norm_19_TB) #normal

#TB - FDis - 2020
FDis_Multi_Norm_20_TB <- lm(data = subset(Functional_Diversity, year == 2020 & site== "TB"), log(FDis_All)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Multi_Norm_20_TB) 
ols_test_normality(FDis_Multi_Norm_20_TB) #normal

#TB - FDis - 2021
FDis_Multi_Norm_21_TB <- lm(data = subset(Functional_Diversity, year == 2021 & site== "TB"), FDis_All  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Multi_Norm_21_TB) 
ols_test_normality(FDis_Multi_Norm_21_TB) #normal

#TB - FDis - 2022
FDis_Multi_Norm_22_TB <- lm(data = subset(Functional_Diversity, year == 2022 & site== "TB"), FDis_All  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Multi_Norm_22_TB) 
ols_test_normality(FDis_Multi_Norm_22_TB) #normal

#TB - FDis - 2023
FDis_Multi_Norm_23_TB <- lm(data = subset(Functional_Diversity, year == 2023 & site== "TB"),(FDis_All)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Multi_Norm_23_TB) 
ols_test_normality(FDis_Multi_Norm_23_TB) #normal

#### Stats: Multivariate FDis TB ####

#FDis for TB 2019 - LMER
FDis_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2019&site=="TB"), log(FDis_All) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB19_LMER, type = 3) #NS

#FDis for TB 2020 - LMER
FDis_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2020&site=="TB"), log(FDis_All) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB20_LMER, type = 3)  #NS

#FDis for TB 2021 - LMER
FDis_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2021&site=="TB"), FDis_All ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_TB21_LMER, type = 3) #NS

#FDis for TB 2022 - LMER
FDis_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2022&site=="TB"), FDis_All ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB22_LMER, type = 3) #NS

#FDis for TB 2023 - LMER
FDis_TB23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity,year==2023&site=="TB"), FDis_All ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB23_LMER, type = 3) #NS

# adjust pvalues for Area TB
p.adjust(0.08426, method = "BH", n=5)
p.adjust(0.5519, method = "BH", n=5)
p.adjust(0.2323, method = "BH", n=5)
p.adjust(0.005333, method = "BH", n=5)
p.adjust(0.07632, method = "BH", n=5)


#### Calculate Height FDis: FK ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Height<-Avg_Traits_FK%>% 
  filter(Genus_Species_Correct!="Pediomelum.esculentum") %>% 
  mutate(Sp_Num=c(1:32)) %>% 
  ungroup() 

Avg_Traits_FK_Data_Height<-Avg_Traits_FK_Height %>% 
  dplyr::select(Avg_height_cm) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_Height) <- c(1:3)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_Height<-Avg_Traits_FK_Height %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num) 

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_Height<- Species_Comp_RelCov_Clean %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  filter(aerial_basal=="Aerial") %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
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
  mutate(ID_Num=c(1:324)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_Height <- dbFD(Avg_Traits_FK_Data_Height, Species_Comp_FK_Wide_Data_Height,corr = "none")
summary(FK_FunctionalDiversity_Height)

#### Calculate Height FDis: TB ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_TB_Height<-Avg_Traits_TB%>% 
  filter(!Genus_Species_Correct %in% c("Erigeron.pumilus","Nothocalais.cuspidata","Oenothera.suffrtescuns","Erigeron.canus")) %>% 
  mutate(Sp_Num=c(1:40)) %>% 
  ungroup() 

Avg_Traits_TB_Data_Height<-Avg_Traits_TB_Height %>% 
  dplyr::select(Avg_height_cm) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_Height) <- c(1:40)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_Height<-Avg_Traits_TB_Height %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num) 

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_TB_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_Height<- Species_Comp_RelCov_Clean %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  filter(aerial_basal=="Aerial") %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
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
  mutate(ID_Num=c(1:323)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_Height <- dbFD(Avg_Traits_TB_Data_Height, Species_Comp_TB_Wide_Data_Height,corr = "none")

#### Merge FK and TB Height ####
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
  mutate(FDis_Height=FDis)



#### Normality: Height FDis FK ####

#FK - FDis - 2019
FDis_Height_Norm_19_FK <- lm(data = subset(Functional_Diversity_Height, year == 2019 & site== "FK"), FDis_Height  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Height_Norm_19_FK) 
ols_test_normality(FDis_Height_Norm_19_FK) #normal

#FK - FDis - 2020
FDis_Height_Norm_20_FK <- lm(data = subset(Functional_Diversity_Height, year == 2020 & site== "FK"), (FDis_Height)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Height_Norm_20_FK) 
ols_test_normality(FDis_Height_Norm_20_FK) #normal

#FK - FDis - 2021
FDis_Height_Norm_21_FK <- lm(data = subset(Functional_Diversity_Height, year == 2021 & site== "FK"), FDis_Height  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Height_Norm_21_FK) 
ols_test_normality(FDis_Height_Norm_21_FK) #normal

#FK - FDis - 2022
FDis_Height_Norm_22_FK <- lm(data = subset(Functional_Diversity_Height, year == 2022 & site== "FK"), FDis_Height  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Height_Norm_22_FK) 
ols_test_normality(FDis_Height_Norm_22_FK) #normal

#FK - FDis - 2023
FDis_Height_Norm_23_FK <- lm(data = subset(Functional_Diversity_Height, year == 2023 & site== "FK"),FDis_Height  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Height_Norm_23_FK) 
ols_test_normality(FDis_Height_Norm_23_FK) #normal

#### Stats: Height FDis FK ####

#FDis for Fort Keogh 2019 - LMER
FDis_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2019&site=="FK"), FDis_Height ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 - LMER
FDis_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2020&site=="FK"), FDis_Height ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2021&site=="FK"), FDis_Height ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2022&site=="FK"), FDis_Height ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK22_LMER, type = 3) #NS

#FDis for Fort Keogh 2023 - LMER
FDis_FK23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2023&site=="FK"), FDis_Height ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK23_LMER, type = 3) #NS

# adjust pvalues for Area TB
p.adjust(0.8277, method = "BH", n=5)
p.adjust(0.1418, method = "BH", n=5)
p.adjust(0.4283, method = "BH", n=5)
p.adjust(0.1461, method = "BH", n=5)
p.adjust(0.1895, method = "BH", n=5)

#### Normality: Height FDis TB ####

#TB - FDis - 2019
FDis_Height_Norm_19_TB <- lm(data = subset(Functional_Diversity_Height, year == 2019 & site== "TB"), FDis_Height  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Height_Norm_19_TB) 
ols_test_normality(FDis_Height_Norm_19_TB) #normal

#TB - FDis - 2020
FDis_Height_Norm_20_TB <- lm(data = subset(Functional_Diversity_Height, year == 2020 & site== "TB"), FDis_Height  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Height_Norm_20_TB) 
ols_test_normality(FDis_Height_Norm_20_TB) #normal

#TB - FDis - 2021
FDis_Height_Norm_21_TB <- lm(data = subset(Functional_Diversity_Height, year == 2021 & site== "TB"), FDis_Height  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Height_Norm_21_TB) 
ols_test_normality(FDis_Height_Norm_21_TB) #normal

#TB - FDis - 2022
FDis_Height_Norm_22_TB <- lm(data = subset(Functional_Diversity_Height, year == 2022 & site== "TB"), FDis_Height  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Height_Norm_22_TB) 
ols_test_normality(FDis_Height_Norm_22_TB) #normal

#TB - FDis - 2023
FDis_Height_Norm_23_TB <- lm(data = subset(Functional_Diversity_Height, year == 2023 & site== "TB"),FDis_Height  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Height_Norm_23_TB) 
ols_test_normality(FDis_Height_Norm_23_TB) #normalish

#### Stats: Height FDis TB ####

#FDis for Fort Keogh 2019 - LMER
FDis_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2019&site=="TB"), FDis_Height ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 - LMER
FDis_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2020&site=="TB"), FDis_Height ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2021&site=="TB"), FDis_Height ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_TB21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2022&site=="TB"), FDis_Height ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB22_LMER, type = 3) #NS

#FDis for Fort Keogh 2023 - LMER
FDis_TB23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Height,year==2023&site=="TB"), FDis_Height ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB23_LMER, type = 3) #NS

# adjust pvalues for Area TB
p.adjust(0.4036, method = "BH", n=5)
p.adjust(0.6143, method = "BH", n=5)
p.adjust(0.2578, method = "BH", n=5)
p.adjust(0.7359, method = "BH", n=5)
p.adjust(0.5551, method = "BH", n=5)



#### Calculate Leaf_Thickness FDis: FK ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Leaf_Thickness<-Avg_Traits_FK%>% 
  filter(!Genus_Species_Correct %in% c("Pediomelum.esculentum","Linum.rigidum")) %>% 
  mutate(Sp_Num=c(1:31)) %>% 
  ungroup() 

Avg_Traits_FK_Data_Leaf_Thickness<-Avg_Traits_FK_Leaf_Thickness %>% 
  dplyr::select(Avg_leaf_thickness) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_Leaf_Thickness) <- c(1:31)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_Leaf_Thickness<-Avg_Traits_FK_Leaf_Thickness %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num) 

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_Leaf_Thickness<- Species_Comp_RelCov_Clean %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  filter(aerial_basal=="Aerial") %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
  left_join(Avg_Traits_FK_SpNames_Leaf_Thickness) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_Leaf_Thickness<-Species_Comp_FK_Leaf_Thickness %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_Leaf_Thickness<-Species_Comp_FK_Wide_Leaf_Thickness %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_Leaf_Thickness<-Species_Comp_FK_Wide_Leaf_Thickness %>% 
  mutate(ID_Num=c(1:324)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_Leaf_Thickness <- dbFD(Avg_Traits_FK_Data_Leaf_Thickness, Species_Comp_FK_Wide_Data_Leaf_Thickness,corr = "none")
summary(FK_FunctionalDiversity_Leaf_Thickness)

#### Calculate Leaf_Thickness FDis: TB ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_TB_Leaf_Thickness<-Avg_Traits_TB%>% 
  filter(!Genus_Species_Correct %in% c("Erigeron.pumilus","Nothocalais.cuspidata","Oenothera.suffrtescuns","Erigeron.canus","Elymus.elymoides")) %>% 
  mutate(Sp_Num=c(1:39)) %>% 
  ungroup() 

Avg_Traits_TB_Data_Leaf_Thickness<-Avg_Traits_TB_Leaf_Thickness %>% 
  dplyr::select(Avg_leaf_thickness) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_Leaf_Thickness) <- c(1:39)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_Leaf_Thickness<-Avg_Traits_TB_Leaf_Thickness %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num) 

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_TB_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_Leaf_Thickness<- Species_Comp_RelCov_Clean %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  filter(aerial_basal=="Aerial") %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
  left_join(Avg_Traits_TB_SpNames_Leaf_Thickness) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_Leaf_Thickness<-Species_Comp_TB_Leaf_Thickness %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_Leaf_Thickness<-Species_Comp_TB_Wide_Leaf_Thickness %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_Leaf_Thickness<-Species_Comp_TB_Wide_Leaf_Thickness %>% 
  mutate(ID_Num=c(1:323)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_Leaf_Thickness <- dbFD(Avg_Traits_TB_Data_Leaf_Thickness, Species_Comp_TB_Wide_Data_Leaf_Thickness,corr = "none")

#### Merge FK and TB Leaf_Thickness ####
#merge FK and TB functional diversity matrices back into dataframes and join environmental data 
Functional_Diversity_FK_Leaf_Thickness<-as.data.frame(FK_FunctionalDiversity_Leaf_Thickness) %>% 
  cbind(Species_Comp_FK_Wide_PlotData_Leaf_Thickness)

Functional_Diversity_TB_Leaf_Thickness<-as.data.frame(TB_FunctionalDiversity_Leaf_Thickness) %>% 
  cbind(Species_Comp_TB_Wide_PlotData_Leaf_Thickness)

Functional_Diversity_Leaf_Thickness<-Functional_Diversity_FK_Leaf_Thickness %>% 
  rbind(Functional_Diversity_TB_Leaf_Thickness) %>% 
  separate(ID,c("year","site","plot"), sep = "_") %>% 
  dplyr::select(-ID_Num) %>% 
  left_join(plot_layoutK) %>% 
  mutate(Rainfall_reduction_cat=as.factor(rainfall_reduction)) %>% 
  mutate(FDis_Leaf_Thickness=FDis)



#### Normality: Leaf_Thickness FDis FK ####

#FK - FDis - 2019
FDis_Leaf_Thickness_Norm_19_FK <- lm(data = subset(Functional_Diversity_Leaf_Thickness, year == 2019 & site== "FK"), FDis_Leaf_Thickness  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Leaf_Thickness_Norm_19_FK) 
ols_test_normality(FDis_Leaf_Thickness_Norm_19_FK) #normal

#FK - FDis - 2020
FDis_Leaf_Thickness_Norm_20_FK <- lm(data = subset(Functional_Diversity_Leaf_Thickness, year == 2020 & site== "FK"), (FDis_Leaf_Thickness)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Leaf_Thickness_Norm_20_FK) 
ols_test_normality(FDis_Leaf_Thickness_Norm_20_FK) #normal

#FK - FDis - 2021
FDis_Leaf_Thickness_Norm_21_FK <- lm(data = subset(Functional_Diversity_Leaf_Thickness, year == 2021 & site== "FK"), exp(FDis_Leaf_Thickness)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Leaf_Thickness_Norm_21_FK) 
ols_test_normality(FDis_Leaf_Thickness_Norm_21_FK) #not normal but okay

#FK - FDis - 2022
FDis_Leaf_Thickness_Norm_22_FK <- lm(data = subset(Functional_Diversity_Leaf_Thickness, year == 2022 & site== "FK"), FDis_Leaf_Thickness  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Leaf_Thickness_Norm_22_FK) 
ols_test_normality(FDis_Leaf_Thickness_Norm_22_FK) #normal

#FK - FDis - 2023
FDis_Leaf_Thickness_Norm_23_FK <- lm(data = subset(Functional_Diversity_Leaf_Thickness, year == 2023 & site== "FK"),FDis_Leaf_Thickness  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Leaf_Thickness_Norm_23_FK) 
ols_test_normality(FDis_Leaf_Thickness_Norm_23_FK) #normal

#### Stats: Leaf_Thickness FDis FK ####

#FDis for Fort Keogh 2019 - LMER
FDis_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Leaf_Thickness,year==2019&site=="FK"), FDis_Leaf_Thickness ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 - LMER
FDis_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Leaf_Thickness,year==2020&site=="FK"), FDis_Leaf_Thickness ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Leaf_Thickness,year==2021&site=="FK"), exp(FDis_Leaf_Thickness) ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Leaf_Thickness,year==2022&site=="FK"), FDis_Leaf_Thickness ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK22_LMER, type = 3) #NS

#FDis for Fort Keogh 2023 - LMER
FDis_FK23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Leaf_Thickness,year==2023&site=="FK"), FDis_Leaf_Thickness ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK23_LMER, type = 3) #NS

# adjust pvalues for Area TB
p.adjust(0.3337, method = "BH", n=5)
p.adjust(0.6804, method = "BH", n=5)
p.adjust(0.5967, method = "BH", n=5)
p.adjust(0.4663, method = "BH", n=5)
p.adjust(0.5493, method = "BH", n=5)

#### Normality: Leaf_Thickness FDis TB ####

#TB - FDis - 2019
FDis_Leaf_Thickness_Norm_19_TB <- lm(data = subset(Functional_Diversity_Leaf_Thickness, year == 2019 & site== "TB"), FDis_Leaf_Thickness  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Leaf_Thickness_Norm_19_TB) 
ols_test_normality(FDis_Leaf_Thickness_Norm_19_TB) #normal

#TB - FDis - 2020
FDis_Leaf_Thickness_Norm_20_TB <- lm(data = subset(Functional_Diversity_Leaf_Thickness, year == 2020 & site== "TB"), log(FDis_Leaf_Thickness)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Leaf_Thickness_Norm_20_TB) 
ols_test_normality(FDis_Leaf_Thickness_Norm_20_TB) #normal

#TB - FDis - 2021
FDis_Leaf_Thickness_Norm_21_TB <- lm(data = subset(Functional_Diversity_Leaf_Thickness, year == 2021 & site== "TB"), FDis_Leaf_Thickness  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Leaf_Thickness_Norm_21_TB) 
ols_test_normality(FDis_Leaf_Thickness_Norm_21_TB) #normal

#TB - FDis - 2022
FDis_Leaf_Thickness_Norm_22_TB <- lm(data = subset(Functional_Diversity_Leaf_Thickness, year == 2022 & site== "TB"), FDis_Leaf_Thickness  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Leaf_Thickness_Norm_22_TB) 
ols_test_normality(FDis_Leaf_Thickness_Norm_22_TB) #normal

#TB - FDis - 2023
FDis_Leaf_Thickness_Norm_23_TB <- lm(data = subset(Functional_Diversity_Leaf_Thickness, year == 2023 & site== "TB"),FDis_Leaf_Thickness  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Leaf_Thickness_Norm_23_TB) 
ols_test_normality(FDis_Leaf_Thickness_Norm_23_TB) #normalish

#### Stats: Leaf_Thickness FDis TB ####

#FDis for Fort Keogh 2019 - LMER
FDis_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Leaf_Thickness,year==2019&site=="TB"), FDis_Leaf_Thickness ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 - LMER
FDis_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Leaf_Thickness,year==2020&site=="TB"), log(FDis_Leaf_Thickness) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Leaf_Thickness,year==2021&site=="TB"), FDis_Leaf_Thickness ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_TB21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Leaf_Thickness,year==2022&site=="TB"), FDis_Leaf_Thickness ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB22_LMER, type = 3) #NS

#FDis for Fort Keogh 2023 - LMER
FDis_TB23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Leaf_Thickness,year==2023&site=="TB"), FDis_Leaf_Thickness ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB23_LMER, type = 3) #NS

# adjust pvalues for Area TB
p.adjust(0.8614, method = "BH", n=5)
p.adjust(0.9702, method = "BH", n=5)
p.adjust(0.3435, method = "BH", n=5)
p.adjust(0.9627, method = "BH", n=5)
p.adjust(0.5307, method = "BH", n=5)


#### Calculate LDMC FDis: FK ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_LDMC<-Avg_Traits_FK%>% 
  filter(!Genus_Species_Correct %in% c("Pediomelum.esculentum","Linum.rigidum","Hedeoma.hispida")) %>% 
  mutate(Sp_Num=c(1:30)) %>% 
  ungroup() 

Avg_Traits_FK_Data_LDMC<-Avg_Traits_FK_LDMC %>% 
  dplyr::select(Avg_LDMC) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_LDMC) <- c(1:30)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_LDMC<-Avg_Traits_FK_LDMC %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num) 

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_LDMC<- Species_Comp_RelCov_Clean %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  filter(aerial_basal=="Aerial") %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
  left_join(Avg_Traits_FK_SpNames_LDMC) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_LDMC<-Species_Comp_FK_LDMC %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_LDMC<-Species_Comp_FK_Wide_LDMC %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_LDMC<-Species_Comp_FK_Wide_LDMC %>% 
  mutate(ID_Num=c(1:324)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_LDMC <- dbFD(Avg_Traits_FK_Data_LDMC, Species_Comp_FK_Wide_Data_LDMC,corr = "none")

#### Calculate LDMC FDis: TB ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_TB_LDMC<-Avg_Traits_TB%>% 
  filter(!Genus_Species_Correct %in% c("Erigeron.pumilus","Nothocalais.cuspidata","Oenothera.suffrtescuns","Erigeron.canus","Elymus.elymoides","Hedeoma.hispida")) %>% 
  mutate(Sp_Num=c(1:38)) %>% 
  ungroup() 

Avg_Traits_TB_Data_LDMC<-Avg_Traits_TB_LDMC %>% 
  dplyr::select(Avg_LDMC) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_LDMC) <- c(1:38)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_LDMC<-Avg_Traits_TB_LDMC %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num) 

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_TB_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_LDMC<- Species_Comp_RelCov_Clean %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  filter(aerial_basal=="Aerial") %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
  left_join(Avg_Traits_TB_SpNames_LDMC) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_LDMC<-Species_Comp_TB_LDMC %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_LDMC<-Species_Comp_TB_Wide_LDMC %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_LDMC<-Species_Comp_TB_Wide_LDMC %>% 
  mutate(ID_Num=c(1:323)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_LDMC <- dbFD(Avg_Traits_TB_Data_LDMC, Species_Comp_TB_Wide_Data_LDMC,corr = "none")

#### Merge FK and TB LDMC ####
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
  mutate(FDis_LDMC=FDis)



#### Normality: LDMC FDis FK ####

#FK - FDis - 2019
FDis_LDMC_Norm_19_FK <- lm(data = subset(Functional_Diversity_LDMC, year == 2019 & site== "FK"), log(FDis_LDMC)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_LDMC_Norm_19_FK) 
ols_test_normality(FDis_LDMC_Norm_19_FK) #normal

#FK - FDis - 2020
FDis_LDMC_Norm_20_FK <- lm(data = subset(Functional_Diversity_LDMC, year == 2020 & site== "FK"), (FDis_LDMC)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_LDMC_Norm_20_FK) 
ols_test_normality(FDis_LDMC_Norm_20_FK) #normal

#FK - FDis - 2021
FDis_LDMC_Norm_21_FK <- lm(data = subset(Functional_Diversity_LDMC, year == 2021 & site== "FK"), log(FDis_LDMC)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_LDMC_Norm_21_FK) 
ols_test_normality(FDis_LDMC_Norm_21_FK) #normal

#FK - FDis - 2022
FDis_LDMC_Norm_22_FK <- lm(data = subset(Functional_Diversity_LDMC, year == 2022 & site== "FK"), FDis_LDMC  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_LDMC_Norm_22_FK) 
ols_test_normality(FDis_LDMC_Norm_22_FK) #normal

#FK - FDis - 2023
FDis_LDMC_Norm_23_FK <- lm(data = subset(Functional_Diversity_LDMC, year == 2023 & site== "FK"),FDis_LDMC  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_LDMC_Norm_23_FK) 
ols_test_normality(FDis_LDMC_Norm_23_FK) #normal

#### Stats: LDMC FDis FK ####

#FDis for Fort Keogh 2019 - LMER
FDis_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2019&site=="FK"), log(FDis_LDMC) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 - LMER
FDis_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2020&site=="FK"), FDis_LDMC ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2021&site=="FK"), log(FDis_LDMC) ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2022&site=="FK"), FDis_LDMC ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK22_LMER, type = 3) #NS

#FDis for Fort Keogh 2023 - LMER
FDis_FK23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2023&site=="FK"), FDis_LDMC ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK23_LMER, type = 3) #NS

# adjust pvalues for Area TB
p.adjust(0.2265, method = "BH", n=5)
p.adjust(0.6835, method = "BH", n=5)
p.adjust(0.2905, method = "BH", n=5)
p.adjust(0.06682, method = "BH", n=5)
p.adjust(0.6484, method = "BH", n=5)

#### Normality: LDMC FDis TB ####

#TB - FDis - 2019
FDis_LDMC_Norm_19_TB <- lm(data = subset(Functional_Diversity_LDMC, year == 2019 & site== "TB"), log(FDis_LDMC)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_LDMC_Norm_19_TB) 
ols_test_normality(FDis_LDMC_Norm_19_TB) #not normal but okay

#TB - FDis - 2020
FDis_LDMC_Norm_20_TB <- lm(data = subset(Functional_Diversity_LDMC, year == 2020 & site== "TB"), log(FDis_LDMC)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_LDMC_Norm_20_TB) 
ols_test_normality(FDis_LDMC_Norm_20_TB) #normal

#TB - FDis - 2021
FDis_LDMC_Norm_21_TB <- lm(data = subset(Functional_Diversity_LDMC, year == 2021 & site== "TB"), log(FDis_LDMC)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_LDMC_Norm_21_TB) 
ols_test_normality(FDis_LDMC_Norm_21_TB) #not normal but okay

#TB - FDis - 2022
FDis_LDMC_Norm_22_TB <- lm(data = subset(Functional_Diversity_LDMC, year == 2022 & site== "TB"), log(FDis_LDMC)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_LDMC_Norm_22_TB) 
ols_test_normality(FDis_LDMC_Norm_22_TB) #okay

#TB - FDis - 2023
FDis_LDMC_Norm_23_TB <- lm(data = subset(Functional_Diversity_LDMC, year == 2023 & site== "TB"), log(FDis_LDMC)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_LDMC_Norm_23_TB) 
ols_test_normality(FDis_LDMC_Norm_23_TB) #not normal but okay

#### Stats: LDMC FDis TB ####

#FDis for Fort Keogh 2019 - LMER
FDis_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2019&site=="TB"), log(FDis_LDMC) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 - LMER
FDis_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2020&site=="TB"), log(FDis_LDMC) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2021&site=="TB"), log(FDis_LDMC) ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_TB21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2022&site=="TB"), log(FDis_LDMC) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB22_LMER, type = 3) #NS

#FDis for Fort Keogh 2023 - LMER
FDis_TB23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_LDMC,year==2023&site=="TB"), log(FDis_LDMC) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB23_LMER, type = 3) #NS

# adjust pvalues for Area TB
p.adjust(0.8238, method = "BH", n=5)
p.adjust(0.9681, method = "BH", n=5)
p.adjust(0.1296, method = "BH", n=5)
p.adjust(0.5506, method = "BH", n=5)
p.adjust(0.5571, method = "BH", n=5)


#### Calculate SLA FDis: FK ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_SLA<-Avg_Traits_FK%>% 
  filter(!Genus_Species_Correct %in% c("Pediomelum.esculentum","Linum.rigidum","Hedeoma.hispida")) %>% 
  mutate(Sp_Num=c(1:30)) %>%
  ungroup() 

Avg_Traits_FK_Data_SLA<-Avg_Traits_FK_SLA %>% 
  dplyr::select(Avg_SLA) %>%  
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_SLA) <- c(1:30)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_SLA<-Avg_Traits_FK_SLA %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num) 

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_SLA<- Species_Comp_RelCov_Clean %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  filter(aerial_basal=="Aerial") %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
  left_join(Avg_Traits_FK_SpNames_SLA) %>%
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_SLA<-Species_Comp_FK_SLA %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_SLA<-Species_Comp_FK_Wide_SLA %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_SLA<-Species_Comp_FK_Wide_SLA %>% 
  mutate(ID_Num=c(1:324)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_SLA <- dbFD(Avg_Traits_FK_Data_SLA, Species_Comp_FK_Wide_Data_SLA,corr = "none")

#### Calculate SLA FDis: TB ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_TB_SLA<-Avg_Traits_TB%>% 
  na.omit(Avg_SLA) %>%
  filter(!Genus_Species_Correct %in% c("Erigeron.canus","Nothocalais.cuspidata", "Oenothera.suffrtescuns")) %>% 
  mutate(Sp_Num=c(1:34)) %>% 
  ungroup() 

Avg_Traits_TB_Data_SLA<-Avg_Traits_TB_SLA %>% 
  dplyr::select(Avg_SLA) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_SLA) <- c(1:34)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_SLA<-Avg_Traits_TB_SLA %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num) 

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_TB_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_SLA<- Species_Comp_RelCov_Clean %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  filter(aerial_basal=="Aerial") %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
  left_join(Avg_Traits_TB_SpNames_SLA) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_SLA<-Species_Comp_TB_SLA %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_SLA<-Species_Comp_TB_Wide_SLA %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_SLA<-Species_Comp_TB_Wide_SLA %>% 
  mutate(ID_Num=c(1:323)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_SLA <- dbFD(Avg_Traits_TB_Data_SLA, Species_Comp_TB_Wide_Data_SLA,corr = "none")

#### Merge FK and TB SLA ####
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
  mutate(FDis_SLA=FDis)



#### Normality: SLA FDis FK ####

#FK - FDis - 2019
FDis_SLA_Norm_19_FK <- lm(data = subset(Functional_Diversity_SLA, year == 2019 & site== "FK"), 1/sqrt(FDis_SLA)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_SLA_Norm_19_FK) 
ols_test_normality(FDis_SLA_Norm_19_FK) #normal

#FK - FDis - 2020
FDis_SLA_Norm_20_FK <- lm(data = subset(Functional_Diversity_SLA, year == 2020 & site== "FK"), (FDis_SLA)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_SLA_Norm_20_FK) 
ols_test_normality(FDis_SLA_Norm_20_FK) #normalish

#FK - FDis - 2021
FDis_SLA_Norm_21_FK <- lm(data = subset(Functional_Diversity_SLA, year == 2021 & site== "FK"), (FDis_SLA)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_SLA_Norm_21_FK) 
ols_test_normality(FDis_SLA_Norm_21_FK) #normalish

#FK - FDis - 2022
FDis_SLA_Norm_22_FK <- lm(data = subset(Functional_Diversity_SLA, year == 2022 & site== "FK"), sqrt(FDis_SLA)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_SLA_Norm_22_FK) 
ols_test_normality(FDis_SLA_Norm_22_FK) #normal

#FK - FDis - 2023
FDis_SLA_Norm_23_FK <- lm(data = subset(Functional_Diversity_SLA, year == 2023 & site== "FK"),1/sqrt(FDis_SLA)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_SLA_Norm_23_FK) 
ols_test_normality(FDis_SLA_Norm_23_FK) #normalish 

#### Stats: SLA FDis FK ####

#FDis for Fort Keogh 2019 - LMER
FDis_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2019&site=="FK"), 1/sqrt(FDis_SLA) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 - LMER
FDis_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2020&site=="FK"), (FDis_SLA) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2021&site=="FK"), (FDis_SLA) ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2022&site=="FK"), log(FDis_SLA) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK22_LMER, type = 3) #NS

#FDis for Fort Keogh 2023 - LMER
FDis_FK23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2023&site=="FK"), 1/sqrt(FDis_SLA)~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK23_LMER, type = 3) #NS

# adjust pvalues for SLA TB
p.adjust(0.035, method = "BH", n=5)
p.adjust(0.333, method = "BH", n=5)
p.adjust(0.778, method = "BH", n=5)
p.adjust(0.2713, method = "BH", n=5)
p.adjust(0.0061, method = "BH", n=5)

#### Normality: SLA FDis TB ####

#TB - FDis - 2019
FDis_SLA_Norm_19_TB <- lm(data = subset(Functional_Diversity_SLA, year == 2019 & site== "TB"), log(FDis_SLA)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_SLA_Norm_19_TB) 
ols_test_normality(FDis_SLA_Norm_19_TB) #normal

#TB - FDis - 2020
FDis_SLA_Norm_20_TB <- lm(data = subset(Functional_Diversity_SLA, year == 2020 & site== "TB"), log(FDis_SLA)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_SLA_Norm_20_TB) 
ols_test_normality(FDis_SLA_Norm_20_TB) #normal

#TB - FDis - 2021
FDis_SLA_Norm_21_TB <- lm(data = subset(Functional_Diversity_SLA, year == 2021 & site== "TB"), sqrt(FDis_SLA)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_SLA_Norm_21_TB) 
ols_test_normality(FDis_SLA_Norm_21_TB) #normal

#TB - FDis - 2022
FDis_SLA_Norm_22_TB <- lm(data = subset(Functional_Diversity_SLA, year == 2022 & site== "TB"),(FDis_SLA)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_SLA_Norm_22_TB) 
ols_test_normality(FDis_SLA_Norm_22_TB) #normal

#TB - FDis - 2023
FDis_SLA_Norm_23_TB <- lm(data = subset(Functional_Diversity_SLA, year == 2023 & site== "TB"), log(FDis_SLA)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_SLA_Norm_23_TB) 
ols_test_normality(FDis_SLA_Norm_23_TB) #normal

#### Stats: SLA FDis TB ####

#FDis for Fort Keogh 2019 - LMER
FDis_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2019&site=="TB"), log(FDis_SLA) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 - LMER
FDis_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2020&site=="TB"), log(FDis_SLA) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2021&site=="TB"), sqrt(FDis_SLA) ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_TB21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2022&site=="TB"), (FDis_SLA) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB22_LMER, type = 3) #NS

#FDis for Fort Keogh 2023 - LMER
FDis_TB23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_SLA,year==2023&site=="TB"),log(FDis_SLA) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB23_LMER, type = 3) #NS

# adjust pvalues for SLA TB
p.adjust(0.559, method = "BH", n=5)
p.adjust(0.182, method = "BH", n=5)
p.adjust(0.1607, method = "BH", n=5)
p.adjust(0.09415, method = "BH", n=5)
p.adjust(0.8016, method = "BH", n=5)

#### Calculate Area FDis: FK ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_FK_Area<-Avg_Traits_FK%>% 
  filter(!Genus_Species_Correct %in% c("Pediomelum.esculentum","Linum.rigidum")) %>% 
  mutate(Sp_Num=c(1:30)) %>% 
  ungroup() 

Avg_Traits_FK_Data_Area<-Avg_Traits_FK_Area %>% 
  dplyr::select(Avg_Area) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_FK_Data_Area) <- c(1:30)

#make a dataframe with the species name and identification number 
Avg_Traits_FK_SpNames_Area<-Avg_Traits_FK_Area %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num) 

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only FK. Left join the Avg_Traits_FK_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_FK_Area<- Species_Comp_RelCov_Clean %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="FK") %>%
  filter(aerial_basal=="Aerial") %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
  left_join(Avg_Traits_FK_SpNames_Area) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_FK_Wide_Area<-Species_Comp_FK_Area %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_FK_Wide_Data_Area<-Species_Comp_FK_Wide_Area %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_FK_Wide_PlotData_Area<-Species_Comp_FK_Wide_Area %>% 
  mutate(ID_Num=c(1:324)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
FK_FunctionalDiversity_Area <- dbFD(Avg_Traits_FK_Data_Area, Species_Comp_FK_Wide_Data_Area,corr = "none")

#### Calculate Area FDis: TB ####

#Create a matrix with just average trait data removing all idetifiers
Avg_Traits_TB_Area<-Avg_Traits_TB%>% 
  filter(!Genus_Species_Correct %in% c("Aristida.purpurea","Artemisia.frigida","Artemisia.tridentata","Bouteloua.gracilis","Erigeron.pumilus","Nothocalais.cuspidata","Oenothera.suffrtescuns","Erigeron.canus","Elymus.elymoides","Gutierrezia.sarothrae")) %>% 
  mutate(Sp_Num=c(1:33)) %>% 
  ungroup() 

Avg_Traits_TB_Data_Area<-Avg_Traits_TB_Area %>% 
  dplyr::select(Avg_Area) %>% 
  as.matrix()

#make row names 1-33 to match the sp_num for future identification 
rownames(Avg_Traits_TB_Data_Area) <- c(1:33)

#make a dataframe with the species name and identification number 
Avg_Traits_TB_SpNames_Area<-Avg_Traits_TB_Area %>% 
  dplyr::select(Genus_Species_Correct,Sp_Num) 

#Create a new dataframe using species comp data and remove anything that has a relative cover of 0 then filter by site to include only TB. Left join the Avg_Traits_TB_SpNames so that species numbers and names match up between future matrices. create a new ID column for year, site, and plot together for future identification and stats
Species_Comp_TB_Area<- Species_Comp_RelCov_Clean %>% 
  filter(Relative_Cover!=0) %>% 
  filter(site=="TB") %>%
  filter(aerial_basal=="Aerial") %>% 
  rename(Genus_Species_Correct=Genus_Species) %>% 
  left_join(Avg_Traits_TB_SpNames_Area) %>% 
  na.omit(Sp_Num) %>% 
  mutate(ID=paste(year,site,plot,sep="_"))

#put dataframe into wide format with sp_num as columns and ID as first row, filling data with relative cover
Species_Comp_TB_Wide_Area<-Species_Comp_TB_Area %>% 
  dplyr::select(Sp_Num,Relative_Cover,ID) %>% 
  spread(key=Sp_Num,value=Relative_Cover,fill=0)

#Make a matrix with JUST the species comp data, no identifiers
Species_Comp_TB_Wide_Data_Area<-Species_Comp_TB_Wide_Area %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

#make a dataframe where ID is assigned a number 1-270 to match the ID row names from above dataframe
Species_Comp_TB_Wide_PlotData_Area<-Species_Comp_TB_Wide_Area %>% 
  mutate(ID_Num=c(1:323)) %>% 
  dplyr::select(ID,ID_Num) 

#run dbFD to recieve Frichness,Fdiversity, etc. for each plot and trait. Currently no correction, but can be sqrt, cailliez, or lingoes
TB_FunctionalDiversity_Area <- dbFD(Avg_Traits_TB_Data_Area, Species_Comp_TB_Wide_Data_Area,corr = "none")

#### Merge FK and TB Area ####
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
  mutate(FDis_Area=FDis)



#### Normality: Area FDis FK ####

#FK - FDis - 2019
FDis_Area_Norm_19_FK <- lm(data = subset(Functional_Diversity_Area, year == 2019 & site== "FK"), (FDis_Area)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Area_Norm_19_FK) 
ols_test_normality(FDis_Area_Norm_19_FK) #normal

#FK - FDis - 2020
FDis_Area_Norm_20_FK <- lm(data = subset(Functional_Diversity_Area, year == 2020 & site== "FK"), (FDis_Area)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Area_Norm_20_FK) 
ols_test_normality(FDis_Area_Norm_20_FK) #normal

#FK - FDis - 2021
FDis_Area_Norm_21_FK <- lm(data = subset(Functional_Diversity_Area, year == 2021 & site== "FK"), log(FDis_Area)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Area_Norm_21_FK) 
ols_test_normality(FDis_Area_Norm_21_FK) #normal

#FK - FDis - 2022
FDis_Area_Norm_22_FK <- lm(data = subset(Functional_Diversity_Area, year == 2022 & site== "FK"), log(FDis_Area)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Area_Norm_22_FK) 
ols_test_normality(FDis_Area_Norm_22_FK) #normal

#FK - FDis - 2023
FDis_Area_Norm_23_FK <- lm(data = subset(Functional_Diversity_Area, year == 2023 & site== "FK"),FDis_Area  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Area_Norm_23_FK) 
ols_test_normality(FDis_Area_Norm_23_FK) #normal

#### Stats: Area FDis FK ####

#FDis for Fort Keogh 2019 - LMER
FDis_FK19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2019&site=="FK"), (FDis_Area) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 - LMER
FDis_FK20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2020&site=="FK"), FDis_Area ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_FK21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2021&site=="FK"), log(FDis_Area) ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_FK21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_FK22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2022&site=="FK"), FDis_Area ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK22_LMER, type = 3) #NS

#FDis for Fort Keogh 2023 - LMER
FDis_FK23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2023&site=="FK"), FDis_Area ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_FK23_LMER, type = 3) #NS

# adjust pvalues for Area TB
p.adjust(0.3998, method = "BH", n=5)
p.adjust(0.6613, method = "BH", n=5)
p.adjust(0.1325, method = "BH", n=5)
p.adjust(0.7777, method = "BH", n=5)
p.adjust(0.05299, method = "BH", n=5)

#### Normality: Area FDis TB ####

#TB - FDis - 2019
FDis_Area_Norm_19_TB <- lm(data = subset(Functional_Diversity_Area, year == 2019 & site== "TB"), (FDis_Area)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Area_Norm_19_TB) 
ols_test_normality(FDis_Area_Norm_19_TB) #normal

#TB - FDis - 2020
FDis_Area_Norm_20_TB <- lm(data = subset(Functional_Diversity_Area, year == 2020 & site== "TB"), log(FDis_Area)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Area_Norm_20_TB) 
ols_test_normality(FDis_Area_Norm_20_TB) #normal

#TB - FDis - 2021
FDis_Area_Norm_21_TB <- lm(data = subset(Functional_Diversity_Area, year == 2021 & site== "TB"), (FDis_Area)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Area_Norm_21_TB) 
ols_test_normality(FDis_Area_Norm_21_TB) #normal

#TB - FDis - 2022
FDis_Area_Norm_22_TB <- lm(data = subset(Functional_Diversity_Area, year == 2022 & site== "TB"),(FDis_Area)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Area_Norm_22_TB) 
ols_test_normality(FDis_Area_Norm_22_TB) #okay

#TB - FDis - 2023
FDis_Area_Norm_23_TB <- lm(data = subset(Functional_Diversity_Area, year == 2023 & site== "TB"), (FDis_Area)  ~ rainfall_reduction)
ols_plot_resid_hist(FDis_Area_Norm_23_TB) 
ols_test_normality(FDis_Area_Norm_23_TB) #normal

#### Stats: Area FDis TB ####

#FDis for Fort Keogh 2019 - LMER
FDis_TB19_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2019&site=="TB"), (FDis_Area) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB19_LMER, type = 3) #NS

#FDis for Fort Keogh 2020 - LMER
FDis_TB20_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2020&site=="TB"), log(FDis_Area) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB20_LMER, type = 3)  #NS

#FDis for Fort Keogh 2021 - LMER
FDis_TB21_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2021&site=="TB"), (FDis_Area) ~ Rainfall_reduction_cat+ (1|block) + (1|block:slope))
anova(FDis_TB21_LMER, type = 3) #NS

#FDis for Fort Keogh 2022 - LMER
FDis_TB22_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2022&site=="TB"), (FDis_Area) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB22_LMER, type = 3) #NS

#FDis for Fort Keogh 2023 - LMER
FDis_TB23_LMER <- lmerTest::lmer(data = subset(Functional_Diversity_Area,year==2023&site=="TB"),(FDis_Area) ~ Rainfall_reduction_cat + (1|block) + (1|block:slope))
anova(FDis_TB23_LMER, type = 3) #NS

# adjust pvalues for Area TB
p.adjust(0.02851, method = "BH", n=5)
p.adjust(0.308, method = "BH", n=5)
p.adjust(0.1803, method = "BH", n=5)
p.adjust(0.2, method = "BH", n=5)
p.adjust(0.2501, method = "BH", n=5)
