READ ME

Biodiversity and riparian vegetation type are mutual biological drivers of ecosystem functioning in temperate and tropical streams

Rebecca Oester, Paula M. de Omena, Larissa Corteletti, Marcelo S. Moretti, Florian Altermatt, Andreas Bruder

rebecca.oester@eawag.ch


This READ ME file contains information on the r-scripts and variables in the datasets used for the analyses. 


Datafiles:

1) dat.ergx
Main dataset with information on each leaf litter bag with the following variables:

Sample_ID: Unique identifier for each treatment (leaf litter in each bag); NA for Brazil because of naming differences
Site: Site identifier (Ending F indicating forested and ending A indicating non-forested)
Country: Study country (either Switzerland or Brazil)
Region: Region identifier for Switzerland and Brazil
Stream: Stream identifier
Vegetation: Riparian vegetation type (either forested or non-forested)
Mesh: Mesh size of leaf litter bag (either fine or coarse mesh)
LeafLeafLeaf: Leaf species (either Alder, Ash, Inga or Miconia)
MonoMix: Indication of whether species was in a single species leaf litter bag (mono) or mixed (mix)
LeafLeaf: String combination of leaf speices and mix
EA_N_Percent_corr: % Nitrogen in the leaf litter at the end of the experiment measured with an elemental analyzer
EA_C_Percent_corr: % Carbon in the leaf litter at the end of the experiment measured with an elemental analyzer
molar_CN: molar ratio between Nitrogen and Carbon
log_molar_CN: logged value of molar_CN
N_Loss: % Nitrogen lost based on calculations in Handa et al (2014)
C_Loss: % Carbon lost based on calculations in Handa et al (2014)
Fungal_Biom_Leaf_mgg_noRR_1: Fungal Biomass in mg / g leaf as proxy for fungal secondary production
k_corr1: the decay rate k based on the decay function incorporating initial and final mass and time progressen expressed as Temperature degree days. 
Delta15_N_Air: the stable isotope signature of N
Delta13_C_VPDB: the stable isotope signature of C
Delta_logCN_abs: the difference in C:N ratio from initial and final leaves
ID: string combination of sample id and leafleaf
Group: treatment combination (string combination of MonoMix, Vegetation and Mesh)


2) SEM
Dataset used to visualize and summarise effect sizes from the SEM (summary outputs from SEM.r)

model: Description of individual links of the SEM variables (Intercepts, NLoss ~ Mesh, etc.)
Estimate: Effect size of the links
Est.Error: estimated error
l95: lower 95% interval
u05: upper 95% interval
Rhat: R hat (indication of fit)
Bulk_ESS: 
Tail_ESS:
species: Leaf litter species (Alder, Ash, Inga, Miconia)
modelabc: Letters A-L indicating corresponding links in Figure 4
type: description of link type



R-scripts:
Run in R (version 4.1.2) on Linux. 


1) Multifunctionality

requires dat.ergx.csv

With getStdAndMeanFunctions(), the three functions (decomposition rate (k), N loss, and fungal secondary production) get scaled and averaged to calculate an overall multifunctionaltiy score. 

This score and the individual functions get analyzed and plotted depending on the leaf litter species and treatment effect separately using the brms() function. 


2) SEM

requires dat.ergx.csv

After data preparation, for each leaf litter separately, a big SEM with links specified in the manuscript is run and analyzed. 


3) Slopes

requires SEM.csv


From the SEM analyzes, the summary outputs are aggregated and vizalised to produce Figure 4 in the manuscript.




