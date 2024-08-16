##################################################### Fungi 
##################################################### SEM  
#################################################### Rebecca Oester 
#################################################### April, 2024 


rm(list=ls())
#### 0. Packages ####
library(rstan)
library(brms)
library(bayesplot)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(outliers)
library(stringr)
library(patchwork)
library(multifunc)

#### 1. Preparations #####
### scaling (z-score)
scale2 <- function(x, na.rm = TRUE) ((x - mean(x, na.rm = na.rm)) / sd(x, na.rm)) ### na.rm = TRUE leaves NAs in the scaling and just considers values

### prior setting 
## vaguely informative prior with normal distribution around 0
prior1 <- c(prior(normal(0, 1), class = Intercept),
            prior(normal(0, 1), class = "b"))
prior2 <- c(prior(normal(0, 1), class = Intercept), ## this had the lower loo so we are going with this one
            prior(normal(0, 0.5), class = "b"))

### set working directory
setwd("")

### load data
dat.ergx <- read.csv("dat.ergx.csv", sep=";")


##### 2.1 transform and calculate three main ecosystem functions ####
###### 2.1.1 log transform k (decomposition rate) ####
dat.ergx$logk <- log10(dat.ergx$k_corr1)

###### 2.1.2 log transform fungal biomass ####
dat.ergx$logFungal_Biom_Leaf_mgg_noRR_1 <- log10(dat.ergx$Fungal_Biom_Leaf_mgg_noRR_1)


##### 2.2 Data prep ####

###### 2.2.1 Alder ####
dat.ergxxAlder<- subset(dat.ergx, LeafLeafLeaf=="Alder")
CN_Alder_m <-  dat.ergxxAlder %>% 
  dplyr::select(ID,Stream, Region, LeafLeaf, Vegetation,Mesh,logFungal_Biom_Leaf_mgg_noRR_1,N_Loss, logk) %>% ## subset only the variables of interest
  dplyr::mutate_at(vars(logFungal_Biom_Leaf_mgg_noRR_1,N_Loss, logk), scale2) # scale numberic variables to 0 
CN_Alder_m_big <-  CN_Alder_m[complete.cases(CN_Alder_m), ] # 246

## reorder the levels so that we have the "less" complex treatment first as reference
# Vegetation
CN_Alder_m_big$Vegetation<- as.factor(CN_Alder_m_big$Vegetation)
levels(CN_Alder_m_big$Vegetation)
CN_Alder_m_big$Vegetation<- factor(CN_Alder_m_big$Vegetation, levels=c("non-forested", "forested"))

## Mesh
CN_Alder_m_big$Mesh<- as.factor(CN_Alder_m_big$Mesh)
levels(CN_Alder_m_big$Mesh)
CN_Alder_m_big$Mesh<- factor(CN_Alder_m_big$Mesh, levels=c("Fine", "Coarse"))

## Mix
CN_Alder_m_big$LeafLeaf<- as.factor(CN_Alder_m_big$LeafLeaf)
levels(CN_Alder_m_big$LeafLeaf)
CN_Alder_m_big$LeafLeaf<- factor(CN_Alder_m_big$LeafLeaf, levels=c("AlderMono", "AlderMix"))


###### 2.2.2 Ash ####
dat.ergxxAsh<- subset(dat.ergx, LeafLeafLeaf=="Ash")
CN_Ash_m <-  dat.ergxxAsh %>% 
  dplyr::select(ID,Stream, Region, LeafLeaf, Vegetation,Mesh,logFungal_Biom_Leaf_mgg_noRR_1,N_Loss, logk) %>% ## subset only the variables of interest
  dplyr::mutate_at(vars(logFungal_Biom_Leaf_mgg_noRR_1,N_Loss, logk), scale2) # scale numberic variables to 0 
CN_Ash_m_big <-  CN_Ash_m[complete.cases(CN_Ash_m), ] # 241

## reorder the levels so that we have the "less" complex treatment first as reference
# Vegetation
CN_Ash_m_big$Vegetation<- as.factor(CN_Ash_m_big$Vegetation)
levels(CN_Ash_m_big$Vegetation)
CN_Ash_m_big$Vegetation<- factor(CN_Ash_m_big$Vegetation, levels=c("non-forested", "forested"))

## Mesh
CN_Ash_m_big$Mesh<- as.factor(CN_Ash_m_big$Mesh)
levels(CN_Ash_m_big$Mesh)
CN_Ash_m_big$Mesh<- factor(CN_Ash_m_big$Mesh, levels=c("Fine", "Coarse"))

## Mix
CN_Ash_m_big$LeafLeaf<- as.factor(CN_Ash_m_big$LeafLeaf)
levels(CN_Ash_m_big$LeafLeaf)
CN_Ash_m_big$LeafLeaf<- factor(CN_Ash_m_big$LeafLeaf, levels=c("AshMono", "AshMix"))



###### 2.2.3 Inga ####
dat.ergxxInga<- subset(dat.ergx, LeafLeafLeaf=="Inga")
CN_Inga_m <-  dat.ergxxInga %>% 
  dplyr::select(ID,Stream, Region, LeafLeaf, Vegetation,Mesh,logFungal_Biom_Leaf_mgg_noRR_1,N_Loss, logk) %>% ## subset only the variables of interest
  dplyr::mutate_at(vars(logFungal_Biom_Leaf_mgg_noRR_1,N_Loss, logk), scale2) # scale numberic variables to 0 
CN_Inga_m_big <-  CN_Inga_m[complete.cases(CN_Inga_m), ] # 255


## reorder the levels so that we have the "less" complex treatment first as reference
# Vegetation
CN_Inga_m_big$Vegetation<- as.factor(CN_Inga_m_big$Vegetation)
levels(CN_Inga_m_big$Vegetation)
CN_Inga_m_big$Vegetation<- factor(CN_Inga_m_big$Vegetation, levels=c("non-forested", "forested"))

## Mesh
CN_Inga_m_big$Mesh<- as.factor(CN_Inga_m_big$Mesh)
levels(CN_Inga_m_big$Mesh)
CN_Inga_m_big$Mesh<- factor(CN_Inga_m_big$Mesh, levels=c("Fine", "Coarse"))

## Mix
CN_Inga_m_big$LeafLeaf<- as.factor(CN_Inga_m_big$LeafLeaf)
levels(CN_Inga_m_big$LeafLeaf)
CN_Inga_m_big$LeafLeaf<- factor(CN_Inga_m_big$LeafLeaf, levels=c("IngaMono", "IngaMix"))



###### 2.2.4 Miconia ####
dat.ergxxMiconia<- subset(dat.ergx, LeafLeafLeaf=="Miconia")
CN_Miconia_m <-  dat.ergxxMiconia %>% 
  dplyr::select(ID,Stream, Region, LeafLeaf, Vegetation,Mesh,logFungal_Biom_Leaf_mgg_noRR_1,N_Loss, logk) %>% ## subset only the variables of interest
  dplyr::mutate_at(vars(logFungal_Biom_Leaf_mgg_noRR_1,N_Loss, logk), scale2) # scale numberic variables to 0 
CN_Miconia_m_big <-  CN_Miconia_m[complete.cases(CN_Miconia_m), ] # 251


## reorder the levels so that we have the "less" complex treatment first as reference
# Vegetation
CN_Miconia_m_big$Vegetation<- as.factor(CN_Miconia_m_big$Vegetation)
levels(CN_Miconia_m_big$Vegetation)
CN_Miconia_m_big$Vegetation<- factor(CN_Miconia_m_big$Vegetation, levels=c("non-forested", "forested"))

## Mesh
CN_Miconia_m_big$Mesh<- as.factor(CN_Miconia_m_big$Mesh)
levels(CN_Miconia_m_big$Mesh)
CN_Miconia_m_big$Mesh<- factor(CN_Miconia_m_big$Mesh, levels=c("Fine", "Coarse"))

## Mix
CN_Miconia_m_big$LeafLeaf<- as.factor(CN_Miconia_m_big$LeafLeaf)
levels(CN_Miconia_m_big$LeafLeaf)
CN_Miconia_m_big$LeafLeaf<- factor(CN_Miconia_m_big$LeafLeaf, levels=c("MiconiaMono", "MiconiaMix"))



### 3. one big SEM ####
# Prepare SEM
# specify models
meshmod5 <- brmsformula(logk ~ Mesh+LeafLeaf+Vegetation+logFungal_Biom_Leaf_mgg_noRR_1+N_Loss+(1|Region/Stream))
meshmod6 <- brmsformula(N_Loss ~  Mesh+LeafLeaf+Vegetation+logFungal_Biom_Leaf_mgg_noRR_1 +(1|Region/Stream))
meshmod7 <- brmsformula(logFungal_Biom_Leaf_mgg_noRR_1 ~ Mesh+LeafLeaf+Vegetation+(1|Region/Stream))

##### 3.1 Alder ####
big_Alder_Stan_mesh_scaled_p2 <- brm(
    meshmod5+
    meshmod6+
    meshmod7,
  chains=4, cores=4,iter=10000,
  family=c(gaussian,
           gaussian,
           gaussian),
  prior= prior2, ## let's try with prior2 
  inits=0,data = CN_Alder_m_big,
  save_all_pars=T,control = list(adapt_delta = 0.99,max_treedepth=20))

big_Alder_Stan_mesh_scaled_p2$prior
summary(big_Alder_Stan_mesh_scaled_p2)
pp_check(big_Alder_Stan_mesh_scaled_p2, resp = 'DeltalogCNabs') ### looks ok
pp_check(big_Alder_Stan_mesh_scaled_p2, resp = 'logFungalBiomLeafmggnoRR1') ### looks ok
pp_check(big_Alder_Stan_mesh_scaled_p2, resp = 'logk') ### looks ok


##### 3.2 Ash ####
big_Ash_Stan_mesh_scaled_p2 <- brm(
  meshmod5+
    meshmod6+
    meshmod7,
  chains=4, cores=4,iter=10000,
  family=c(gaussian,
           gaussian,
           gaussian),
  prior= prior2,
  inits=0,data = CN_Ash_m_big,
  save_all_pars=T,control = list(adapt_delta = 0.99,max_treedepth=20))

big_Ash_Stan_mesh_scaled_p2$prior
summary(big_Ash_Stan_mesh_scaled_p2)

pp_check(big_Ash_Stan_mesh_scaled_p2, resp = 'DeltalogCNabs') ### looks ok
pp_check(big_Ash_Stan_mesh_scaled_p2, resp = 'logFungalBiomLeafmggnoRR1') ### looks ok
pp_check(big_Ash_Stan_mesh_scaled_p2, resp = 'logk') ### looks ok


##### 3.1 Inga ####
big_Inga_Stan_mesh_scaled_p2 <- brm(
  meshmod5+
    meshmod6+
    meshmod7,
  chains=4, cores=4,iter=10000,
  family=c(gaussian,
           gaussian,
           gaussian),
  prior= prior2,
  inits=0,data = CN_Inga_m_big,
  save_all_pars=T,control = list(adapt_delta = 0.99,max_treedepth=20))

big_Inga_Stan_mesh_scaled_p2$prior
summary(big_Inga_Stan_mesh_scaled_p2)

pp_check(big_Inga_Stan_mesh_scaled_p2, resp = 'DeltalogCNabs') ### looks ok
pp_check(big_Inga_Stan_mesh_scaled_p2, resp = 'logFungalBiomLeafmggnoRR1') ### looks ok
pp_check(big_Inga_Stan_mesh_scaled_p2, resp = 'logk') ### looks ok

##### 3.1 Miconia ####
big_Miconia_Stan_mesh_scaled_p2 <- brm(
  meshmod5+
    meshmod6+
    meshmod7,
  chains=4, cores=4,iter=10000,
  family=c(gaussian,
           gaussian,
           gaussian),
  prior= prior2,
  inits=0,data = CN_Miconia_m_big,
  save_all_pars=T,control = list(adapt_delta = 0.99,max_treedepth=20))

big_Miconia_Stan_mesh_scaled_p2$prior
summary(big_Miconia_Stan_mesh_scaled_p2)

pp_check(big_Miconia_Stan_mesh_scaled_p2, resp = 'DeltalogCNabs') ### looks ok
pp_check(big_Miconia_Stan_mesh_scaled_p2, resp = 'logFungalBiomLeafmggnoRR1') ### looks ok
pp_check(big_Miconia_Stan_mesh_scaled_p2, resp = 'logk') ### looks ok