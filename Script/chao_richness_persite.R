# Using Chao1 to compare interaction richness between treatments

#1. Load Packages ----
library(readxl)
library(grDevices)
library(dplyr)
library(fossil)
library(bipartite)
library(vegan)
library(reshape2)
library(nat)
library(rgl)

#2. Upload dataset ----
allvisits <- read_excel("~/path/allvisits.xlsx")
names(allvisits)

#3. Subset data by TREATMENT ----
invaded_data <- allvisits %>% filter(type == "Invaded")
pristine_data <- allvisits %>% filter(type == "Pristine")
cleared_data <- allvisits %>% filter(type == "Cleared")

#4. Create matrices: Freq. of interaction_codes for different treatments, by site. ---- 
#(When using dcast, watch what is being used to fill matrix - the freq or ratio?)

# Matrix of interaction_codes by site, for all treatments
allvisits_dcast<-dcast(allvisits,site~interaction_code,fun.aggregate = sum, value.var='freq') # Total No. of interaction_codes across all treatments = 1614
# Matrices of interaction_codes by site, for each treatment
invaded_dcast <-dcast(invaded_data,site~interaction_code,fun.aggregate = sum, value.var='freq') #No. of interaction_codes for invaded treatment = 646
pristine_dcast <-dcast(pristine_data,site~interaction_code,fun.aggregate = sum, value.var='freq') #No. of interaction_codes for pristine treatment = 584
cleared_dcast <-dcast(cleared_data,site~interaction_code,fun.aggregate = sum, value.var='freq') #No. of interaction_codes for cleared treatment = 672

#4. Chao1 estimates to compare campling completeness of interactions between treatments ----
View(estimateR(invaded_dcast[2:646])) # Gives Chao1 per site
estimateR(colSums(invaded_dcast[2:646])) # Chao1 for invaded sites = Pools sites
estimateR(colSums(cleared_dcast[2:672])) # Chao1 for cleared sites = Pools sites
estimateR(colSums(pristine_dcast[2:584])) # Chao1 for pristine sites = Pools sites
?estimateR
#5. Interaction accumulation curves for different treatments ----
plot(specaccum(allvisits_dcast[2:1629],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation across all sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(specaccum(invaded_dcast[2:646],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation at Invaded sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(specaccum(cleared_dcast[2:672],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation at Cleared sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(specaccum(pristine_dcast[2:584],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation at Pristine sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")



#b&c: FOR CHAO STATS USED IN MS ----
#b: Interaction richness ----
#3b. Subset data by SITE ----
site1_data <- allvisits %>% filter(site == "Boesmansrivier 1")
site2_data <- allvisits %>% filter(site == "Boesmansrivier 2")
site3_data <- allvisits %>% filter(site == "Awilla")
site4_data <- allvisits %>% filter(site == "Grootbos 1")
site5_data <- allvisits %>% filter(site == "Blue Horizon")
site6_data <- allvisits %>% filter(site == "Groeneweide")
site7_data <- allvisits %>% filter(site == "Grootbos 2")
site8_data <- allvisits %>% filter(site == "Helderfontein 2")
site9_data <- allvisits %>% filter(site == "Helderfontein 4")
site10_data <- allvisits %>% filter(site == "Byeneskrans 2")
site11_data <- allvisits %>% filter(site == "Flippie")
site12_data <- allvisits %>% filter(site == "Byeneskrans 1")
site13_data <- allvisits %>% filter(site == "Grootbos 4")
site14_data <- allvisits %>% filter(site == "Helderfontein 3")
site15_data <- allvisits %>% filter(site == "Lomond")
site16_data <- allvisits %>% filter(site == "Helderfontein Pri")
site17_data <- allvisits %>% filter(site == "Helderfontein 1")
site18_data <- allvisits %>% filter(site == "Helderfontein 5")
site19_data <- allvisits %>% filter(site == "Elim 1")
site20_data <- allvisits %>% filter(site == "Elim Barney")
site21_data <- allvisits %>% filter(site == "Elim 2")
View(allvisits)

#4b. Create matrices: Freq. of interaction_codes for different sites. ---- 
#(When using dcast, watch what is being used to fill matrix - the freq or ratio?)

# Matrix of interaction_codes by site, for all treatments
allvisits_dcast<-dcast(allvisits,site~interaction_code,fun.aggregate = sum, value.var='freq') # Total No. of interaction_codes across all treatments = 1629
# Matrices of interaction_codes for each site, scroll along heading to find out total no. interations
# Chao fundtion in the next step also provides the observed total
site1_dcast <-dcast(site1_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site1 = 111
site2_dcast <-dcast(site2_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site2 = 103
site3_dcast <-dcast(site3_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site3 = 103
site4_dcast <-dcast(site4_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site4 = 98
site5_dcast <-dcast(site5_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site5 = 143
site6_dcast <-dcast(site6_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site6 = 103
site7_dcast <-dcast(site7_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site7 = 71
site8_dcast <-dcast(site8_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site8 = 85
site9_dcast <-dcast(site9_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site9 = 104
site10_dcast <-dcast(site10_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site10 = 98
site11_dcast <-dcast(site11_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site11 = 90
site12_dcast <-dcast(site12_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site12 = 123
site13_dcast <-dcast(site13_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site13 = 91
site14_dcast <-dcast(site14_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site14 = 97
site15_dcast <-dcast(site15_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site15 = 95
site16_dcast <-dcast(site16_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site16 = 106
site17_dcast <-dcast(site17_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site17 = 124
site18_dcast <-dcast(site18_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site18 = 100
site19_dcast <-dcast(site19_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site19 = 77
site20_dcast <-dcast(site20_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site20 = 104
site21_dcast <-dcast(site21_data,site~interaction_code,fun.aggregate = sum, value.var='freq') # Interaction.divfor site21 = 86
View(site21_dcast) 

#4b. Chao1 estimates to compare sampling completeness of interactions for each site ----
View(estimateR(allvisits_dcast[2:1614])) # Gives Chao1 per site (not useful as I don't know which site is which here)
View(estimateR(site1_dcast[2:112])) # By individual site dcast
View(estimateR(site2_dcast[2:104]))
View(estimateR(site3_dcast[2:104]))
View(estimateR(site4_dcast[2:99]))
View(estimateR(site5_dcast[2:144]))
View(estimateR(site6_dcast[2:104]))
View(estimateR(site7_dcast[2:72]))
View(estimateR(site8_dcast[2:86]))
View(estimateR(site9_dcast[2:104]))
View(estimateR(site10_dcast[2:97]))
View(estimateR(site11_dcast[2:91]))
View(estimateR(site12_dcast[2:124]))
View(estimateR(site13_dcast[2:92]))
View(estimateR(site14_dcast[2:98]))
View(estimateR(site15_dcast[2:96]))
View(estimateR(site16_dcast[2:107]))
View(estimateR(site17_dcast[2:125]))
View(estimateR(site18_dcast[2:101]))
View(estimateR(site19_dcast[2:78]))
View(estimateR(site20_dcast[2:105]))
View(estimateR(site21_dcast[2:87]))
View(site10_dcast)
?estimateR
#5. Interaction accumulation curves for different sites ----
plot(specaccum(allvisits_dcast[2:1615],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation across all sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(specaccum(invaded_dcast[2:646],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation at Invaded sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(specaccum(cleared_dcast[2:672],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation at Cleared sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(specaccum(pristine_dcast[2:584],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation at Pristine sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")



#c: Plant and Insect richness ----
#4c. Create matrices: Freq. of plants & insects for different sites. ---- 
#(When using dcast, watch what is being used to fill matrix - the freq or ratio?)
View(site1_data)
# Matrix of interaction_codes by site, for all treatments
allvisits_dcast<-dcast(allvisits,site~interaction_code,fun.aggregate = sum, value.var='freq') # Total No. of interaction_codes across all treatments = 1629
# Matrices of interaction_codes for each site, scroll to end of rows/columns to find out total no. interations
# Chao function in the next step also provides the observed total
#Change order of function (final_id~plant) to look at chao for insects or plants
site1_dcast2 <-dcast(site1_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 24, Insects = 78
site2_dcast2 <- dcast(site2_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 20, Insects = 76
site3_dcast2 <-dcast(site3_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 19, Insects = 68
site4_dcast2 <-dcast(site4_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 26, Insects = 61
site5_dcast2 <-dcast(site5_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 32, Insects = 92
site6_dcast2 <-dcast(site6_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 22, Insects = 65
site7_dcast2 <-dcast(site7_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 23, Insects = 44
site8_dcast2 <-dcast(site8_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 15, Insects = 50
site9_dcast2 <-dcast(site9_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 13, Insects = 61
site10_dcast2 <-dcast(site10_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 20, Insects = 64
site11_dcast2 <-dcast(site11_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 20, Insects = 60
site12_dcast2 <-dcast(site12_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 24, Insects = 64
site13_dcast2 <-dcast(site13_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 19, Insects = 62
site14_dcast2 <-dcast(site14_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 21, Insects = 64
site15_dcast2 <-dcast(site15_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 15, Insects = 61
site16_dcast2 <-dcast(site16_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 20, Insects = 79
site17_dcast2 <-dcast(site17_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 27, Insects = 75
site18_dcast2 <-dcast(site18_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 20, Insects = 67
site19_dcast2 <-dcast(site19_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 15, Insects = 58
site20_dcast2 <-dcast(site20_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 22, Insects = 69
site21_dcast2 <-dcast(site21_data,final_id~plant,fun.aggregate = sum, value.var='freq') # Plants = 20, Insects = 42
View(site9_dcast2) 

#4b. Chao1 estimates to compare sampling completeness of interactions for each site ----

#With values for INSECTS
estimateR(colSums(site1_dcast2[2:79]))
estimateR(colSums(site2_dcast2[2:77]))
estimateR(colSums(site3_dcast2[2:69]))
estimateR(colSums(site4_dcast2[2:62]))
estimateR(colSums(site5_dcast2[2:93]))
estimateR(colSums(site6_dcast2[2:66]))
estimateR(colSums(site7_dcast2[2:45]))
estimateR(colSums(site8_dcast2[2:51]))
estimateR(colSums(site9_dcast2[2:62]))
estimateR(colSums(site10_dcast2[2:65]))
estimateR(colSums(site11_dcast2[2:61]))
estimateR(colSums(site12_dcast2[2:65]))
estimateR(colSums(site13_dcast2[2:63]))
estimateR(colSums(site14_dcast2[2:65]))
estimateR(colSums(site15_dcast2[2:62]))
estimateR(colSums(site16_dcast2[2:80]))
estimateR(colSums(site17_dcast2[2:76]))
estimateR(colSums(site18_dcast2[2:68]))
estimateR(colSums(site19_dcast2[2:59]))
estimateR(colSums(site20_dcast2[2:70]))
estimateR(colSums(site21_dcast2[2:43]))

#With values for PLANTS
estimateR(colSums(site1_dcast2[2:25]))
estimateR(colSums(site2_dcast2[2:21]))
estimateR(colSums(site3_dcast2[2:20]))
estimateR(colSums(site4_dcast2[2:27]))
estimateR(colSums(site5_dcast2[2:33]))
estimateR(colSums(site6_dcast2[2:23]))
estimateR(colSums(site7_dcast2[2:24]))
estimateR(colSums(site8_dcast2[2:16]))
estimateR(colSums(site9_dcast2[2:14]))
estimateR(colSums(site10_dcast2[2:21]))
estimateR(colSums(site11_dcast2[2:21]))
estimateR(colSums(site12_dcast2[2:25]))
estimateR(colSums(site13_dcast2[2:20]))
estimateR(colSums(site14_dcast2[2:22]))
estimateR(colSums(site15_dcast2[2:16]))
estimateR(colSums(site16_dcast2[2:21]))
estimateR(colSums(site17_dcast2[2:28]))
estimateR(colSums(site18_dcast2[2:21]))
estimateR(colSums(site19_dcast2[2:16]))
estimateR(colSums(site20_dcast2[2:23]))
estimateR(colSums(site21_dcast2[2:21]))

#5. Interaction accumulation curves for different sites ----
plot(specaccum(allvisits_dcast[2:1615],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation across all sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(specaccum(invaded_dcast[2:646],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation at Invaded sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(specaccum(cleared_dcast[2:672],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation at Cleared sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(specaccum(pristine_dcast[2:584],"rarefaction"),xlab="No. sites", ylab="Cumulative interaction richness",main="Interaction accumulation at Pristine sites",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")


