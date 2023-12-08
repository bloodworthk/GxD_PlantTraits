##########################################################################################################
#Project: Plant Species Composition & Plant Traits in MGP with Drought x Grazing 
##########################################################################################################

#### Load Libraries ####
library(tidyverse) 
library(PerformanceAnalytics)
library(factoextra)
library(ggfortify)
library(ggplot2)
#install.packages("soiltexture")
library(soiltexture)

#### Set Working Directory ####
#Bloodworth - Mac
setwd("~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data")


#### Read in Data ####
#Read in Plot Data
plot_layoutK<-read.csv("DxG_Plant_Traits/GMDR_site_plot_metadata.csv") %>% 
  dplyr::select(site,block,paddock,plot,slope,rainfall_reduction,drought,grazing_category,grazing_treatment,livestock_util_2019,livestock_util_2020,livestock_util_2021)
plot_layoutK$plot<-as.factor(plot_layoutK$plot)

Soil_Texture<-read.csv("DxG_Plant_Traits/GMDR_SoilTexture.csv") %>% 
  rename(site="Site.Name") %>% 
  rename(plot="PlotID")
Soil_Texture$plot<-as.factor(Soil_Texture$plot)

#Merge together slope and soil texture data
Plot_Texture_Slope<-plot_layoutK %>% 
  left_join(Soil_Texture) %>% 
  select(site,block,paddock,plot,slope,X..Sand,X..Clay,X..Silt)

#Merge together slope and soil texture data
Plot_Texture_Slope_FK<-Plot_Texture_Slope %>% 
  filter(site=="FK")

#all correlations
chart.Correlation(Plot_Texture_Slope_FK[5:8],pch="41", cex = 4, method="spearman", histogram = TRUE)


#Merge together slope and soil texture data
Plot_Texture_Slope_TB<-Plot_Texture_Slope %>% 
  filter(site=="TB")

#all correlations
chart.Correlation(Plot_Texture_Slope_TB[5:8],pch="41", cex = 4, method="spearman", histogram = TRUE)


#by block
Plot_Texture_Slope_FK<-Plot_Texture_Slope %>% 
  filter(site=="FK" & block==3)

#all correlations
chart.Correlation(Plot_Texture_Slope_FK[5:8],pch="41", cex = 4, method="spearman", histogram = TRUE)


#Merge together slope and soil texture data
Plot_Texture_Slope_TB<-Plot_Texture_Slope %>% 
  filter(site=="TB"& block==3)

#all correlations
chart.Correlation(Plot_Texture_Slope_TB[5:8],pch="41", cex = 4, method="spearman", histogram = TRUE)


#### PCA for FK ####
PCA_Texture_Slope_FK<-plot_layoutK %>% 
  left_join(Soil_Texture) %>% 
  filter(site=="FK") %>% 
  select(site,block,paddock,plot,rainfall_reduction,slope,X..Sand,X..Clay,X..Silt) %>% 
  na.omit()
PCA_Texture_Slope_FK$block<-as.factor(PCA_Texture_Slope_FK$block)
PCA_Texture_Slope_FK$slope<-as.factor(PCA_Texture_Slope_FK$slope)

PCA_FK<-prcomp(PCA_Texture_Slope_FK[,7:9],scale=TRUE)
PCA_FK
summary(PCA_FK)

axes_FK <- predict(PCA_FK, newdata = PCA_Texture_Slope_FK)
head(axes_FK)

#put PCA axes with site and plot #   
PCA_FK_meta<-cbind(PCA_Texture_Slope_FK,axes_FK)%>%
  dplyr::select(plot,block,paddock,rainfall_reduction,slope,PC1,PC2)
PCA_FK_meta$block<-as.factor(PCA_FK_meta$block)
PCA_FK_meta$slope<-as.factor(PCA_FK_meta$slope)

#find contributions of CW traits to PCA axes #
var_FK <- get_pca_var(PCA_FK)
var_FK
head(var_FK$contrib)

#FK
autoplot(PCA_FK, data=PCA_Texture_Slope_FK,colour="slope",shape="block", scale=0, loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=F)


#### PCA for TB ####
PCA_Texture_Slope_TB<-plot_layoutK %>% 
  left_join(Soil_Texture) %>% 
  filter(site=="TB") %>% 
  select(site,block,paddock,plot,rainfall_reduction,slope,X..Sand,X..Clay,X..Silt) %>% 
  na.omit()
PCA_Texture_Slope_TB$block<-as.factor(PCA_Texture_Slope_TB$block)
PCA_Texture_Slope_TB$slope<-as.factor(PCA_Texture_Slope_TB$slope)

PCA_TB<-prcomp(PCA_Texture_Slope_TB[,7:9],scale=TRUE)
PCA_TB
summary(PCA_TB)

axes_TB <- predict(PCA_TB, newdata = PCA_Texture_Slope_TB)
head(axes_TB)

#put PCA axes with site and plot #   
PCA_TB_meta<-cbind(PCA_Texture_Slope_TB,axes_TB)%>%
  dplyr::select(plot,block,paddock,rainfall_reduction,slope,PC1,PC2)
PCA_TB_meta$block<-as.factor(PCA_TB_meta$block)
PCA_TB_meta$slope<-as.factor(PCA_TB_meta$slope)

#find contributions of CW traits to PCA axes #
var_TB <- get_pca_var(PCA_TB)
var_TB
head(var_TB$contrib)

#FK
autoplot(PCA_TB, data=PCA_Texture_Slope_TB,colour="slope",shape="block", scale=0, loadings=TRUE, loadings.colour="black", size=3, loadings.label=TRUE, loadings.label.colour="black", loadings.label.size=6, frame=F)

#### Soil Texture Triangle ####

Plot_Texture_Slope_FK<-PCA_Texture_Slope_FK %>% 
  rename(CLAY="X..Clay") %>% 
  rename(SAND="X..Sand") %>% 
  rename(SILT="X..Silt")
Plot_Texture_Slope_FK$plot=as.numeric(Plot_Texture_Slope_FK$plot)
Plot_Texture_Slope_FK$slope=as.numeric(Plot_Texture_Slope_FK$slope)


par(mar = rep(0.3, 4))
spectrumBins <- 6
mySpectrum <- viridisLite::plasma(spectrumBins)

# Cut our reflectance data into categories
binnedReflectance <- cut(Plot_Texture_Slope_FK$slope, spectrumBins)

# Assign each data point a colour from the spectrum
pointCol <- mySpectrum[binnedReflectance]

# Initialize the plot
TernaryPlot(alab = "% CLAY",
            blab = "% SILT",
            clab = "% SAND"
)

# Plot the points
TernaryPoints(Plot_Texture_Slope_FK[, c("CLAY", "SILT", "SAND")],
              cex = 2, # Point size
              col = pointCol,  # Point colour
              pch = 16        # Plotting symbol (16 = filled circle)
)


PlotTools::SpectrumLegend(
  "topleft",
  cex = 1, # Font size
  palette = mySpectrum,
  legend = paste(
    seq(from = max(Plot_Texture_Slope_FK$slope), to = min(Plot_Texture_Slope_FK$slope),
        length.out = 6),
    "%"
  ),
  bty = "n", # No framing box
  xpd = NA, # Don't clip at margins
  # title.font = 2, # Bold. Supported from R 3.6 onwards
  title = "Slope")

Plot_Texture_Slope_TB<-PCA_Texture_Slope_TB %>% 
  rename(CLAY="X..Clay") %>% 
  rename(SAND="X..Sand") %>% 
  rename(SILT="X..Silt")
Plot_Texture_Slope_TB$plot=as.numeric(Plot_Texture_Slope_TB$plot)
Plot_Texture_Slope_TB$slope=as.numeric(Plot_Texture_Slope_TB$slope)


par(mar = rep(0.3, 4))
spectrumBins <- 6
mySpectrum <- viridisLite::plasma(spectrumBins)

# Cut our reflectance data into categories
binnedReflectance <- cut(Plot_Texture_Slope_TB$slope, spectrumBins)

# Assign each data point a colour from the spectrum
pointCol <- mySpectrum[binnedReflectance]

# Initialize the plot
TernaryPlot(alab = "% CLAY",
            blab = "% SILT",
            clab = "% SAND"
)

# Plot the points
TernaryPoints(Plot_Texture_Slope_TB[, c("CLAY", "SILT", "SAND")],
              cex = 2, # Point size
              col = pointCol,  # Point colour
              pch = 16        # Plotting symbol (16 = filled circle)
)


PlotTools::SpectrumLegend(
  "topleft",
  cex = 1, # Font size
  palette = mySpectrum,
  legend = paste(
    seq(from = max(Plot_Texture_Slope_TB$slope), to = min(Plot_Texture_Slope_TB$slope),
        length.out = 6),
    "%"
  ),
  bty = "n", # No framing box
  xpd = NA, # Don't clip at margins
  # title.font = 2, # Bold. Supported from R 3.6 onwards
  title = "Slope")

