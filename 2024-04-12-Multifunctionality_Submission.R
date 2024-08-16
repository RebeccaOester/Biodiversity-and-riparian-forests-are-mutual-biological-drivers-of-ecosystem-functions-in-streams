# Assessing Multifunctionality with the multifunc package
# Rebecca Oester
# April 2024

rm(list=ls())

##### 0. Packages ####
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(plyr)
library(vegan)
library(reshape2)
library(lme4)
library(sjPlot)
library(RColorBrewer)
library(nlme)
library(rstan)
library(brms)
library(bayesplot)
library(multifunc)
library(ggpubr)
library(plotly)


#### 1. Load data #####
## set working directory
setwd("")


## load data 
dat.ergx <- read.csv("dat.ergx.csv", sep=";")


#### 2. Select functions for multifunctionality calculation####
vars <- c("N_Loss", "logFungal_Biom_Leaf_mgg_noRR_1", "logk")
## Nitrogen Loss, secondary fungal procution (biomass), decomposition rate k


#### 3. get Multifunctionality of these 3 functions ####
#### scale only within leaf group to get scaled meanFunction
### first calculate meanFunction for each group

## decomposition rate (k)
dat.ergx$logk <- log10(dat.ergx$k_corr1)

## fungal secondary production (biomass)
dat.ergx$logFungal_Biom_Leaf_mgg_noRR_1 <- log10(dat.ergx$Fungal_Biom_Leaf_mgg_noRR_1)


## Alder
dat.ergxxAlder<- subset(dat.ergx, LeafLeafLeaf=="Alder")
dat.ergxxAlder1<-cbind(dat.ergxxAlder, getStdAndMeanFunctions(dat.ergxxAlder, vars, standardizeZScore)) # score and standarzise functions

# quick double check if z-score worked ==> correlation should be 1
ggplot(dat.ergxxAlder1)+
  geom_point(aes(x= logk.std, y= logk))
ggplot(dat.ergxxAlder1)+
  geom_point(aes(x= logFungal_Biom_Leaf_mgg_noRR_1.std, y= logFungal_Biom_Leaf_mgg_noRR_1))
ggplot(dat.ergxxAlder1)+
  geom_point(aes(x= N_Loss.std, y= N_Loss))

## Ash
dat.ergxxAsh<- subset(dat.ergx, LeafLeafLeaf=="Ash")
dat.ergxxAsh1<-cbind(dat.ergxxAsh, getStdAndMeanFunctions(dat.ergxxAsh, vars, standardizeZScore))

## Inga
dat.ergxxInga<- subset(dat.ergx, LeafLeafLeaf=="Inga")
dat.ergxxInga1<-cbind(dat.ergxxInga, getStdAndMeanFunctions(dat.ergxxInga, vars, standardizeZScore))

## Miconia
dat.ergxxMiconia<- subset(dat.ergx, LeafLeafLeaf=="Miconia")
dat.ergxxMiconia1<-cbind(dat.ergxxMiconia, getStdAndMeanFunctions(dat.ergxxMiconia, vars, standardizeZScore))



### then rbind it to dataframe 
dat.merged <- rbind(dat.ergxxAlder1,dat.ergxxAsh1,dat.ergxxInga1,dat.ergxxMiconia1)

## newer order
dat.merged$Group<- factor(dat.merged$Group, levels=c("Mononon-forestedFine",
                                                     "Mixnon-forestedFine",
                                                     "Mononon-forestedCoarse",
                                                     "Mixnon-forestedCoarse",
                                                     "MonoforestedFine",
                                                     "MixforestedFine",
                                                     "MonoforestedCoarse",
                                                     "MixforestedCoarse" ))

### histograms to check if for all four leaves the distribution is between 0 and 1 and more or less normal
ggplot(dat.merged)+facet_wrap(~LeafLeafLeaf)+
  geom_histogram(aes(logk.std))

ggplot(dat.merged)+facet_wrap(~LeafLeafLeaf)+
  geom_histogram(aes(logFungal_Biom_Leaf_mgg_noRR_1.std))

ggplot(dat.merged)+facet_wrap(~LeafLeafLeaf)+
  geom_histogram(aes(N_Loss.std))




### preliminary plot
cols <- rev(c("#01665e", "#35978f", "#80cdc1", "#c7eae5",
          "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3"))

ggplot(data=dat.merged)+theme_bw()+
  geom_boxplot(aes(x= LeafLeafLeaf, y= meanFunction, fill=Group))+
  scale_fill_manual(values = (cols))

dat.merged <- dat.merged %>%
  mutate(Treatment = recode(Group, 
                            MixforestedCoarse = 'mix    forested micro+macro', 
                            'Mixnon-forestedCoarse' = 'mix    non-forested micro+macro', 
                            MixforestedFine =  'mix    forested micro' , 
                            'Mixnon-forestedFine' = 'mix    non-forested micro',
                            MonoforestedCoarse = 'mono forested micro+macro',
                            'Mononon-forestedCoarse' = 'mono non-forested micro+macro',
                            MonoforestedFine =  'mono forested micro' ,
                            'Mononon-forestedFine' = 'mono non-forested micro'))




## check quickly for correlations between functions
## supplementary figure S3 

a<-ggplot(dat.merged)+facet_wrap(~LeafLeafLeaf)+theme_bw()+
  geom_point(aes(x= logk.std, y= logFungal_Biom_Leaf_mgg_noRR_1.std, color=Treatment))+
  scale_color_manual(values = (cols))+
  geom_smooth(aes(x= logk.std, y= logFungal_Biom_Leaf_mgg_noRR_1.std), method="lm", se=F, color="black")+
  ylab("scaled secondary fungal production")+
  xlab("scaled decomposition rate")
a


b<-ggplot(dat.merged)+facet_wrap(~LeafLeafLeaf)+theme_bw()+
  geom_point(aes(x= N_Loss.std, y= logFungal_Biom_Leaf_mgg_noRR_1.std, color=Treatment))+
  scale_color_manual(values = (cols))+
  ylab("scaled secondary fungal production")+
  geom_smooth(aes(x= N_Loss.std, y= logFungal_Biom_Leaf_mgg_noRR_1.std, linetype=LeafLeafLeaf), method="lm", se=F, color="black")+
  scale_linetype_manual(values=c("dashed", "dashed", "dashed", "solid"))+
  xlab("scaled N loss")
b

c<-ggplot(dat.merged)+facet_wrap(~LeafLeafLeaf)+theme_bw()+
  geom_point(aes(x= N_Loss.std, y= logk.std, color=Treatment))+
  scale_color_manual(values = (cols))+
  geom_smooth(aes(x= N_Loss.std, y= logk.std, linetype=LeafLeafLeaf), method="lm", se=F, color="black")+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid"))+
  ylab("scaleddecomposition rate")+
  xlab("scaled N loss")
c

# Extract the legend. Returns a gtable
leg <- get_legend(c)

# Convert to a ggplot and print
leg<- as_ggplot(leg)
leg

## supplementary figure
ggarrange(a,b,c, legend="none", label="AUTO")

#### 4. Plot Multifunctionality ####
## Figure 3
all<-ggplot(data=dat.merged)+theme_bw()+
  geom_boxplot(aes(x= LeafLeafLeaf, y= meanFunction, fill=Treatment), outlier.colour = "grey")+
  scale_fill_manual(values = (cols))+
  ylab("scaled mean multifunctionality")+
  xlab("")+
  theme(text = element_text(size = 16)) +  
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank())+
  scale_y_continuous(breaks=c(0.25, 0.50,0.75), limits=c(0.25,0.875))+
  ##Alder
  # annotate("segment", x = 0.5, xend = 1.4, y = 0.86, yend = 0.86, colour = "black") +
  # annotate("text", x = 1, y = 0.865, label = "*", size=8)+
  annotate("segment", x = 0.5, xend = 1.4, y = 0.84, yend = 0.84, colour = "black", linetype="dashed") +
  annotate("text", x = 1, y = 0.845, label = "*", size=8)+
  annotate("segment", x = 0.5, xend = 1.4, y = 0.82, yend = 0.82, colour = "black", linetype="dotted") +
  annotate("text", x = 1, y = 0.825, label = "*", size=8)+
  ## Ash
  # annotate("segment", x = 1.5, xend = 2.4, y = 0.86, yend = 0.86, colour = "black") +
  # annotate("text", x = 2, y = 0.865, label = "*", size=8)+
  annotate("segment", x = 1.5, xend = 2.4, y = 0.84, yend = 0.84, colour ="black",linetype="dashed") +
  annotate("text", x = 2, y = 0.845, label = "*", size=8)+
  annotate("segment", x = 1.5, xend = 2.4, y = 0.82, yend = 0.82, colour = "black", linetype="dotted") +
  annotate("text", x = 2, y = 0.825, label = "*", size=8)+
  ## Inga
  # annotate("segment", x = 2.5, xend = 3.4, y = 0.86, yend = 0.86, colour = "black") +
  # annotate("text", x = 3, y = 0.865, label = "*", size=8)+
  annotate("segment", x = 2.5, xend = 3.4, y = 0.84, yend = 0.84, colour = "black",linetype="dashed") +
  annotate("text", x = 3, y = 0.845, label = "*", size=8)+
  # annotate("segment", x = 2.5, xend = 3.4, y = 0.82, yend = 0.82 colour = "black", linetype="dotted") +
  # annotate("text", x = 3, y = 0.825, label = "*", size=8)+
  ## Miconia
  # annotate("segment", x = 3.5, xend = 4.4, y = 0.86, yend = 0.86, colour = "black") +
  # annotate("text", x = 4, y = 0.865, label = "*", size=8)+
  annotate("segment", x = 3.5, xend = 4.4, y = 0.84, yend = 0.84, colour ="black", linetype="dashed") +
  annotate("text", x = 4, y = 0.845, label = "*", size=8)+
  annotate("segment", x = 3.5, xend = 4.4, y = 0.82, yend = 0.82, colour = "black",linetype="dotted") +
  annotate("text", x = 4, y = 0.825, label = "*", size=8)+
  theme(legend.position = c(0.3, 0.1),legend.background = element_blank())+
  guides(fill=guide_legend(ncol=2))
  

all

#### 5. Plot also other functions ####
## Delta CN
cn<-ggplot(data=dat.merged)+theme_bw()+
  geom_boxplot(aes(x= LeafLeafLeaf, y= N_Loss.std, fill=Treatment), outlier.colour = "grey")+
  scale_fill_manual(values = (cols))+
  ylab("scaled N loss")+
  xlab("")+
  theme(text = element_text(size = 16)) +  
  theme(text = element_text(size = 16)) +  
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  scale_y_continuous(breaks=c(0,0.25, 0.50,0.75,1), limits=c(0,1.1))+
  ##Alder
  # annotate("segment", x = 0.5, xend = 1.4, y = 1.09, yend = 1.09, colour = "black") +
  # annotate("text", x = 1, y = 1.095, label = "*", size=8)+
  annotate("segment", x = 0.5, xend = 1.4, y = 1.04, yend = 1.04, colour = "black", linetype="dashed") +
  annotate("text", x = 1, y = 1.045, label = "*", size=8)+
  annotate("segment", x = 0.5, xend = 1.4, y = 1.002, yend = 1.002, colour = "black", linetype="dotted") +
  annotate("text", x = 1, y = 1.0025, label = "*", size=8)+
  ## Ash
  # annotate("segment", x = 1.5, xend = 2.4, y = 1.09, yend = 1.09, colour = "black") +
  # annotate("text", x = 2, y = 1.095, label = "*", size=8)+
  annotate("segment", x = 1.5, xend = 2.4, y = 1.04, yend = 1.04, colour ="black",linetype="dashed") +
  annotate("text", x = 2, y = 1.045, label = "*", size=8)+
  annotate("segment", x = 1.5, xend = 2.4, y = 1.002, yend = 1.002, colour = "black", linetype="dotted") +
  annotate("text", x = 2, y = 1.0025, label = "*", size=8)+
  # ## Inga
  annotate("segment", x = 2.5, xend = 3.4, y = 1.09, yend = 1.09, colour = "black") +
  annotate("text", x = 3, y = 1.095, label = "*", size=8)+
  # annotate("segment", x = 2.5, xend = 3.4, y = 1.04, yend = 1.04, colour = "black",linetype="dashed") +
  # annotate("text", x = 3, y = 1.045, label = "*", size=8)
  # annotate("segment", x = 2.5, xend = 3.4, y = 1.002 yend = 1.002 colour = "black", linetype="dotted") +
  # annotate("text", x = 3, y = 1.0025 label = "*", size=8)
  ## Miconia
  # annotate("segment", x = 3.5, xend = 4.4, y = 1.09, yend = 1.09, colour = "black") +
  annotate("text", x = 4, y = 1.095, label = "*", size=8)+
  annotate("segment", x = 3.5, xend = 4.4, y = 1.04, yend = 1.04, colour ="black", linetype="dashed") +
  annotate("text", x = 4, y = 1.045, label = "*", size=8)+
  annotate("segment", x = 3.5, xend = 4.4, y = 1.002, yend = 1.002, colour = "black",linetype="dotted") +
  annotate("text", x = 4, y = 1.0025, label = "*", size=8)
cn

## logk
k<-ggplot(data=dat.merged)+theme_bw()+
  geom_boxplot(aes(x= LeafLeafLeaf, y= logk.std, fill=Treatment), outlier.colour = "grey")+
  scale_fill_manual(values = (cols))+
  ylab("scaled k")+
  xlab("")+
  theme(text = element_text(size = 16)) +  
  scale_y_continuous(breaks=c(0,0.25, 0.50,0.75,1), limits=c(0,1.1))+
  ##Alder
  annotate("segment", x = 0.5, xend = 1.4, y = 1.09, yend = 1.09, colour = "red") +
  annotate("text", x = 1, y = 1.095, label = "*", size=8)+
  annotate("segment", x = 0.5, xend = 1.4, y = 1.04, yend = 1.04, colour = "black", linetype="dashed") +
  annotate("text", x = 1, y = 1.045, label = "*", size=8)+
  annotate("segment", x = 0.5, xend = 1.4, y = 1.002, yend = 1.002, colour = "black", linetype="dotted") +
  annotate("text", x = 1, y = 1.0025, label = "*", size=8)+
  ## Ash
  # annotate("segment", x = 1.5, xend = 2.4, y = 1.09, yend = 1.09, colour = "black") +
  # annotate("text", x = 2, y = 1.095, label = "*", size=8)+
  annotate("segment", x = 1.5, xend = 2.4, y = 1.04, yend = 1.04, colour ="black",linetype="dashed") +
  annotate("text", x = 2, y = 1.045, label = "*", size=8)+
  annotate("segment", x = 1.5, xend = 2.4, y = 1.002, yend = 1.002, colour = "black", linetype="dotted") +
  annotate("text", x = 2, y = 1.0025, label = "*", size=8)+
  ## Inga
  # annotate("segment", x = 2.5, xend = 3.4, y = 1.09, yend = 1.09, colour = "red") +
  # annotate("text", x = 3, y = 1.095, label = "*", size=8)+
  annotate("segment", x = 2.5, xend = 3.4, y = 1.04, yend = 1.04, colour = "black",linetype="dashed") +
  annotate("text", x = 3, y = 1.045, label = "*", size=8)+
  # annotate("segment", x = 2.5, xend = 3.4, y = 1.002 yend = 1.002 colour = "black", linetype="dotted") +
  # annotate("text", x = 3, y = 1.0025 label = "*", size=8)
## Miconia
# annotate("segment", x = 3.5, xend = 4.4, y = 1.09, yend = 1.09, colour = "black") +
# annotate("text", x = 4, y = 1.095, label = "*", size=8)+
annotate("segment", x = 3.5, xend = 4.4, y = 1.04, yend = 1.04, colour ="black", linetype="dashed") +
annotate("text", x = 4, y = 1.045, label = "*", size=8)+
annotate("segment", x = 3.5, xend = 4.4, y = 1.002, yend = 1.002, colour = "black",linetype="dotted") +
annotate("text", x = 4, y = 1.0025, label = "*", size=8)
k

## log biomass
b<-ggplot(data=dat.merged)+theme_bw()+
  geom_boxplot(aes(x= LeafLeafLeaf, y= logFungal_Biom_Leaf_mgg_noRR_1.std, fill=Treatment), outlier.colour = "grey")+
  scale_fill_manual(values = (cols))+
  ylab("scaled fungal secondary production")+
  xlab("")+
  theme(text = element_text(size = 16))+
  scale_y_continuous(breaks=c(0,0.25, 0.50,0.75,1), limits=c(0,1.1))+
  ##Alder
  # annotate("segment", x = 0.5, xend = 1.4, y = 1.09, yend = 1.09, colour = "black") +
  # annotate("text", x = 1, y = 1.095, label = "*", size=8)+
  # annotate("segment", x = 0.5, xend = 1.4, y = 1.04, yend = 1.04, colour = "black", linetype="dashed") +
  # annotate("text", x = 1, y = 1.045, label = "*", size=8)+
  annotate("segment", x = 0.5, xend = 1.4, y = 1.002, yend = 1.002, colour = "black", linetype="dotted") +
  annotate("text", x = 1, y = 1.0025, label = "*", size=8)+
  # ## Ash
  # # annotate("segment", x = 1.5, xend = 2.4, y = 1.09, yend = 1.09, colour = "black") +
  # # annotate("text", x = 2, y = 1.095, label = "*", size=8)+
  # annotate("segment", x = 1.5, xend = 2.4, y = 1.04 yend = 1.04 colour ="black",linetype="dashed") +
  # annotate("text", x = 2, y = 1.045 label = "*", size=8)+
  annotate("segment", x = 1.5, xend = 2.4, y = 1.002, yend = 1.002, colour = "black", linetype="dotted") +
  annotate("text", x = 2, y = 1.0025, label = "*", size=8)+
  # ## Inga
  annotate("segment", x = 2.5, xend = 3.4, y = 1.09, yend = 1.09, colour = "red") +
  annotate("text", x = 3, y = 1.095, label = "*", size=8)+
  # annotate("segment", x = 2.5, xend = 3.4, y = 1.04 yend = 1.04 colour = "black",linetype="dashed") +
  # annotate("text", x = 3, y = 1.045 label = "*", size=8)+
  # # annotate("segment", x = 2.5, xend = 3.4, y = 1.002 yend = 1.002 colour = "black", linetype="dotted") +
  # # annotate("text", x = 3, y = 1.0025 label = "*", size=8)+
  # ## Miconia
  annotate("segment", x = 3.5, xend = 4.4, y = 1.09, yend = 1.09, colour = "black") +
  annotate("text", x = 4, y = 1.095, label = "*", size=8)+
  annotate("segment", x = 3.5, xend = 4.4, y = 1.04, yend = 1.04, colour ="black", linetype="dashed") +
  annotate("text", x = 4, y = 1.045, label = "*", size=8)
  # annotate("segment", x = 3.5, xend = 4.4, y = 1.002 yend = 1.002 colour = "black",linetype="dotted") +
  # annotate("text", x = 4, y = 1.0025 label = "*", size=8)
  # 

b


####  7. Models Treatment effects ####
###### 7.1 preparation steps ####
## reorder the levels so that we have the "less" complex treatment first as reference

# Vegetation
dat.merged$Vegetation<- as.factor(dat.merged$Vegetation)
levels(dat.merged$Vegetation)
dat.merged$Vegetation<- factor(dat.merged$Vegetation, levels=c("non-forested", "forested"))

## Mesh
dat.merged$Mesh<- as.factor(dat.merged$Mesh)
levels(dat.merged$Mesh)
dat.merged$Mesh<- factor(dat.merged$Mesh, levels=c("Fine", "Coarse"))

## Mix
dat.merged$MonoMix<- as.factor(dat.merged$MonoMix)
levels(dat.merged$MonoMix)
dat.merged$MonoMix<- factor(dat.merged$MonoMix, levels=c("Mono", "Mix"))


###### 7.2  prior setting ####
prior_intercept <- prior(normal(0.5, 1), class="Intercept")
prior_slope <- prior(normal(0.5, 1), class="b")

prior1 <- c(prior_intercept, prior_slope)

prior2 <- c(prior(normal(0.5,1), class = "Intercept"))


###### 7.3 Multifuncitonality ~ Treatment ####
## Alder
alder_brm<- brm(meanFunction ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Alder"),
              prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(alder_brm)
summary(alder_brm) 
pp_check(alder_brm) ## fits well
plot(alder_brm) ## all converged fine

### Ash
ash_brm<- brm(meanFunction ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Ash"),
              prior = prior1, control = list(adapt_delta = 0.95))
summary(ash_brm) 
pp_check(ash_brm) ## fits well
plot(ash_brm) ## all converged fine

### Inga
inga_brm<- brm(meanFunction ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Inga"),
              prior = prior1, control = list(adapt_delta = 0.95))
summary(inga_brm)
pp_check(inga_brm) ## fits ok
plot(inga_brm) ## all converged fine

#### Miconia
miconia_brm<- brm(meanFunction ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Miconia"),
              prior = prior1, control = list(adapt_delta = 0.95))
summary(miconia_brm) 
pp_check(miconia_brm) ## fits well
plot(miconia_brm) ## all converged fine

###### 7.4 C:N ~ Treatment ####
## Alder
alder_brm_cn<- brm(N_Loss.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Alder"),
                prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(alder_brm_cn)
summary(alder_brm_cn)
pp_check(alder_brm_cn) ## fits well
plot(alder_brm_cn) ## all converged fine

## Ash
ash_brm_cn<- brm(N_Loss.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Ash"),
                   control = list(adapt_delta = 0.95)) ## prior was removed because of bad fit
prior_summary(ash_brm_cn)
summary(ash_brm_cn)
pp_check(ash_brm_cn) ## fits well
plot(ash_brm_cn) ## all converged fine

## Inga
inga_brm_cn<- brm(N_Loss.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Inga"),
                 prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(inga_brm_cn)
summary(inga_brm_cn)
pp_check(inga_brm_cn) ## fits well
plot(inga_brm_cn) ## all converged fine

## Miconia
miconia_brm_cn<- brm(N_Loss.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Miconia"),
                  prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(miconia_brm_cn)
summary(miconia_brm_cn)
pp_check(miconia_brm_cn) ## fits well
plot(miconia_brm_cn) ## all converged fine

###### 7.5 k ~ Treatment ######
## Alder
alder_brm_k<- brm(logk.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Alder"),
                   prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(alder_brm_k)
summary(alder_brm_k)
pp_check(alder_brm_k) ## fits well
plot(alder_brm_k) ## all converged fine

## Ash
ash_brm_k<- brm(logk.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Ash"),
                 prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(ash_brm_k)
summary(ash_brm_k) 
pp_check(ash_brm_k) ## fits well
plot(ash_brm_k) ## all converged fine


## Inga (flat priors as others did not fit at all)
## this is really a bad fit
# inga_brm_k<- brm(logk.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Inga"),
#                   prior = prior1, control = list(adapt_delta = 0.95))
# 
# ## let's check what happens if we use flat priors
# inga_brm_k<- brm(logk.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Inga"),
#                  control = list(adapt_delta = 0.95))


## let's increase delta to 0.99 (slowing down the sampling process)
inga_brm_k<- brm(logk.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Inga"),
                 control = list(adapt_delta = 0.99)) ## this seems best

prior_summary(inga_brm_k)
summary(inga_brm_k) ## Mesh, Veg and Mix all significant
pp_check(inga_brm_k) ## fits well
plot(inga_brm_k) ## all converged fine

## Miconia
miconia_brm_k<- brm(logk.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Miconia"),
                     prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(miconia_brm_k)
summary(miconia_brm_k)
pp_check(miconia_brm_k) ## fits well
plot(miconia_brm_k) ## all converged fine

###### 7.6 biomass ~Treatment ####
## Alder
alder_brm_b<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Alder"),
                  prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(alder_brm_b)
summary(alder_brm_b)
pp_check(alder_brm_b) ## fits well
plot(alder_brm_b) ## all converged fine

## Ash
ash_brm_b<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Ash"),
                prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(ash_brm_b)
summary(ash_brm_b)
pp_check(ash_brm_b) ## fits well
plot(ash_brm_b) ## all converged fine

## Inga
inga_brm_b<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Inga"),
                 prior=prior2, control = list(adapt_delta = 0.99))
prior_summary(inga_brm_b)
summary(inga_brm_b)
pp_check(inga_brm_b) ## fits well
plot(inga_brm_b) ## all converged fine

## Miconia
miconia_brm_b<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~Mesh+Vegetation+ MonoMix+(1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Miconia"),
                    prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(miconia_brm_b)
summary(miconia_brm_b)
pp_check(miconia_brm_b) ## fits well
plot(miconia_brm_b) ## all converged fine

#### 8. Models relationships between functions ####

###### 8.1 fungal biomass ~ k #######

## Alder
alder_bk<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~ logk.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Alder"),
                  prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(alder_bk)
summary(alder_bk)
pp_check(alder_bk) ## fits well
plot(alder_bk) ## all converged fine

## Ash
Ash_bk<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~ logk.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Ash"),
               prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(Ash_bk)
summary(Ash_bk)
pp_check(Ash_bk) ## fits well
plot(Ash_bk) ## all converged fine

## Inga
Inga_bk<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~ logk.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Inga"),
             control = list(adapt_delta = 0.99))  # no prior here
prior_summary(Inga_bk)
summary(Inga_bk)
pp_check(Inga_bk) ## fits well
plot(Inga_bk) ## all converged fine

## Miconia
Miconia_bk<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~ logk.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Miconia"),
             prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(Miconia_bk)
summary(Miconia_bk)
pp_check(Miconia_bk) ## fits well
plot(Miconia_bk) ## all converged fine

###### 8.2 fungal biomass ~ C:N ####

## Alder
alder_bcn<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~ N_Loss.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Alder"),
               prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(alder_bcn)
summary(alder_bcn)
pp_check(alder_bcn) ## fits well
plot(alder_bcn) ## all converged fine

## Ash
Ash_bcn<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~ N_Loss.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Ash"),
                prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(Ash_bcn)
summary(Ash_bcn)
pp_check(Ash_bcn) ## fits well
plot(Ash_bcn) ## all converged fine

## Inga
Inga_bcn<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~ N_Loss.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Inga"),
                control = list(adapt_delta = 0.99)) ## flat priors here
prior_summary(Inga_bcn)
summary(Inga_bcn)
pp_check(Inga_bcn) ## fits well
plot(Inga_bcn) ## all converged fine

## Miconia
Miconia_bcn<- brm(logFungal_Biom_Leaf_mgg_noRR_1.std ~ N_Loss.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Miconia"),
                prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(Miconia_bcn)
summary(Miconia_bcn)
pp_check(Miconia_bcn) ## fits well
plot(Miconia_bcn) ## all converged fine

###### 8.3 k ~ C:N ####

## Alder
Alder_kcn<- brm(logk.std ~ N_Loss.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Alder"),
              prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(Alder_kcn)
summary(Alder_kcn)
pp_check(Alder_kcn) ## fits well
plot(Alder_kcn) ## all converged fine

## Ash
Ash_kcn<- brm(logk.std ~ N_Loss.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Ash"),
              control = list(adapt_delta = 0.99)) ## flat prior here
prior_summary(Ash_kcn)
summary(Ash_kcn)
pp_check(Ash_kcn) ## fits well
plot(Ash_kcn) ## all converged fine

## Inga
Inga_kcn<- brm(logk.std ~ N_Loss.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Inga"),
              prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(Inga_kcn)
summary(Inga_kcn)
pp_check(Inga_kcn) ## fits well
plot(Inga_kcn) ## all converged fine

## Miconia
Miconia_kcn<- brm(logk.std ~ N_Loss.std+ (1|Region/Stream), data=subset(dat.merged, LeafLeafLeaf=="Miconia"),
              prior = prior1, control = list(adapt_delta = 0.95))
prior_summary(Miconia_kcn)
summary(Miconia_kcn)
pp_check(Miconia_kcn) ## fits well
plot(Miconia_kcn) ## all converged fine
