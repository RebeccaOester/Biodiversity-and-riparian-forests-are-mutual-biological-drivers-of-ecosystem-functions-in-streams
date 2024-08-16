##################################################### Fungi ############################
##################################################### SEM Model estimates ################################
#################################################### Rebecca Oester #################################
#################################################### April, 2024 ################################

### Data available on Dryad-Link:
rm(list=ls())

#### 1. Packages ####
library(ggplot2)
library(plyr)
library(stringr)
library(patchwork)
library(readxl)
library(grid) 
library(RColorBrewer)
library(dplyr)
library(ggpubr)
library(gridExtra)


#### 2. Data #####

## set working directory
setwd("")


# Data
dat <-  read.csv("SEM.csv", sep=";")

# rename standard error
dat$se<- dat$Est.Error

# get rid of intercepts (4 leaves times 12 links = 48 rows)
dat_SEM <- dat[!grepl("Intercept", dat[["model"]]),] 

# define colors
col<- rev(c("#B2182B" ,"#D6604D" ,"#FDDBC7" ,"#D1E5F0","#4393C3","#2166AC"))
grast <- rasterGrob(col, width=unit(1,"npc"), height = unit(1,"npc"), 
                    interpolate = TRUE) 

## find min and max of estimates
min(dat_SEM$Estimate) ## -0.42
max(dat_SEM$Estimate) ## 0.96


min(dat_SEM$l95) ## -0.73
max(dat_SEM$u05) ## 1.26


## find the mean of each relationship
dat_SEM<- dat_SEM %>% group_by (modelabc) %>%
  mutate(Meanabc = mean(Estimate))

#### 3.  Overview Plot ####
ggplot(dat_SEM, aes(x=species, y=Estimate)) + 
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.1) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(16, 17, 18, 19))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  geom_abline(slope=0, intercept=0)+
  #geom_abline(slope=0, intercept=dat_SEM$Meanabc)+
  #ylim(c(-0.4, 0.4))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")
  

#### 4. Separate panel plot ####
a <- subset(dat_SEM, modelabc=="A")
pa<- ggplot(a, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=a$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pa


b <- subset(dat_SEM, modelabc=="B")
pb<- ggplot(b, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=b$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pb


c <- subset(dat_SEM, modelabc=="C")
pc<- ggplot(c, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=c$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pc



d <- subset(dat_SEM, modelabc=="D")
pd<- ggplot(d, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=d$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pd


e <- subset(dat_SEM, modelabc=="E")
pe<- ggplot(e, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=e$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pe






f <- subset(dat_SEM, modelabc=="F")
pf<- ggplot(f, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=f$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pf


g <- subset(dat_SEM, modelabc=="G")
pg<- ggplot(g, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=g$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pg


h <- subset(dat_SEM, modelabc=="H")
ph<- ggplot(h, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=h$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ph



i <- subset(dat_SEM, modelabc=="I")
pi<- ggplot(i, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=i$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pi


j <- subset(dat_SEM, modelabc=="J")
pj<- ggplot(j, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=j$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pj


k <- subset(dat_SEM, modelabc=="K")
pk<- ggplot(k, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=k$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pk

l <- subset(dat_SEM, modelabc=="L")
pl<- ggplot(l, aes(x=species, y=Estimate)) +
  theme_bw()+
  annotation_custom(grast, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_abline(slope=0, intercept=0, color="white")+
  geom_abline(slope=0, intercept=l$Meanabc, color="black", linetype="solid")+
  geom_errorbar(aes(ymin=l95, ymax=u05), width=.4, linetype="solid") +
  #geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se), width=.2) +
  geom_point(aes(x= species, y= Estimate, shape=species,), color = 'black', size = 3.5) +
  geom_point(aes(x= species, y= Estimate, shape=species, col=species), size=3)+
  scale_shape_manual(values=c(15, 16, 17, 18))+
  scale_color_manual(values=c("black", "black", "black", "black"))+
  facet_wrap(~modelabc, ncol=3)+
  ylim(c(-1.26, 1.26))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill="white"))+
  ylab("Model estimate")+
  xlab("")+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pl



#### 5. Final Plot ####
require(grid)   # for the textGrob() function

figure<-(grid.arrange(patchworkGrob(
pa + rremove("ylab") + rremove("xlab") + rremove("x.text")+
pb + rremove("ylab") + rremove("xlab")+ rremove("xy.text")+
pc + rremove("ylab") + rremove("xlab")+ rremove("xy.text")+
pd + rremove("ylab") + rremove("xlab")+ rremove("x.text")+
pe + rremove("ylab") + rremove("xlab")+ rremove("xy.text")+
pf + rremove("ylab") + rremove("xlab")+ rremove("xy.text")+
pg + rremove("ylab") + rremove("xlab")+ rremove("x.text")+
ph + rremove("ylab") + rremove("xlab")+ rremove("xy.text")+
pi + rremove("ylab") + rremove("xlab")+ rremove("xy.text")+
pj + rremove("ylab") + rremove("xlab")+
pk + rremove("ylab") + rremove("xlab")+ rremove("y.text")+
pl + rremove("ylab") + rremove("xlab")+ rremove("y.text")+
plot_layout(ncol = 3, nrow=4)), left = "Model Estimate"))

figure

