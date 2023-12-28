# Installing Packages ----
library(readxl) # to load data
library(reshape2) # to create species accumulation curves
library(vegan)
library(bipartite)
library(econullnetr)
library(ggplot2) # to create boxplots
library(RColorBrewer) # to access colour palettes used in plots
library(ggrepel)
library(openxlsx)
library(MASS)
library(glmmTMB)
library(DHARMa)
library(multcomp)
library(plyr) # for analysis of residual dispersion
library(cowplot)
citation("glmmTMB")

## Importing focals insect visitation data ----
# Enter your own file paths
allfocals <- read_excel("~/path/focals_allvisits.xlsx", na="NA")
focalsorders <- read_excel("~/path/focalsorders.xlsx")
focalsordertotals <- read_excel("~/path/focals_insectordertotals.xlsx")
omoniliferum <- read_excel("~/path/omoniliferum.xlsx", na="NA")
esesamoides <- read_excel("~/path/esesamoides.xlsx", na="NA")
cedulis <- read_excel("~/path/cedulis.xlsx", na="NA")
seedcounts <- read_excel("~/path/focals_seedcounts.xlsx", na="NA")

View(allfocals) # complete visitation data for all three focal species
View(focalsorders)
View(focalsordertotals)# total visitation frequency by different insect orders

## Checking data
hist(allfocals$sitefreq)
hist(allfocals$sitevisitrate)
# To check overdispersion for each species:
ddply(allfocals, 'Plant' , summarise,
      total_visits=sum(sitefreq),
      var_visits=var(sitefreq),
      mean_visits=mean(sitefreq),
      var_by_mean=var_visits/mean_visits, drop=TRUE,parallel=TRUE) 
# For all three species, 'var_by_mean' is well over 1, especially C.edulis - big overdispersion

## CHAO estimates for insect species richness ----
# Creating matrices of insect visits for focal species and site types:
# Create matrix: Freq. of insect species visits for each focal species. (NB When using dcast, watch what is being used to fill matrix - the freq or ratio)
focalsdcast <-dcast(allfocals,Plant~Insect,fun.aggregate = sum, value.var='Freq') 
View(focalsdcast)
# Create matrix: Freq. of insect species visits for invaded & pristine sites
focalsdcast_treatments <-dcast(allfocals, Type~Insect,fun.aggregate = sum, value.var='Freq')
View(focalsdcast_treatments)
# Create 3 matrices: Freq. of insect species visits for each focal species, seperately
omdcast <-dcast(omoniliferum, treatment~insect,fun.aggregate = sum, value.var='freq')
esdcast <-dcast(esesamoides, treatment~insect,fun.aggregate = sum, value.var='freq')
cedcast <-dcast(cedulis, treatment~insect,fun.aggregate = sum, value.var='freq')

View(estimateR(focalsdcast_treatments[2:221])) # Gives Chao1 per treatment
estimateR(colSums(focalsdcast_treatments[2:221])) # Pools treatments
View(estimateR(focalsdcast[2:221])) # Gives Chao1 per plant
estimateR(colSums(focalsdcast[2:221])) # Pools plants (same as for focalsdcast_treatments)
View(estimateR(omdcast[2:116])) # Gives Chao1 per treatment for OM
View(estimateR(esdcast[2:73])) # Gives Chao1 per treatment for ES
View(estimateR(cedcast[2:94])) # Gives Chao1 per treatment for CE

## Focal species insect visitor species accumulation curves 
plot(specaccum(focalsdcast[2:221],"rarefaction"),xlab="No. sites", ylab="Cumulative species richness",main="Insect species accumulation",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
rarefy(focalsdcast[2:221], 220, se = FALSE, MARGIN = 1)
rrarefy(focalsdcast[2:221], 220)
drarefy(focalsdcast[2:221], 220)
rarecurve(focalsdcast[2:221], step = 1, 220, xlab = "Sample Size", ylab = "No. Species",
          label = TRUE,ces=0.6, main="Insect species accumulation curves per site")
rareslope(focalsdcast[2:221], 220)

# Summary stats workings ----
## For insect visits to focal species
View(focalsorders)
View(allfocals)
mean(subset(focalsorders, type =='Pristine')$dip.rate) 
sd(subset(focalsorders, type =='Pristine')$dip.rate)
mean(subset(allfocals, Type == 'pristine')$sitevisitrate)
View(omoniliferum)
mean(subset(omoniliferum, treatment == 'Invaded')$site.insectdiv)
sd(subset(omoniliferum, treatment == 'Invaded')$site.insectdiv)
mean(subset(esesamoides, treatment == 'Pristine')$site.visitrate)
sd(subset(esesamoides, treatment == 'Pristine')$site.visitrate)
mean(subset(cedulis, treatment == 'Pristine')$site.visitrate)
sd(subset(cedulis, treatment == 'Pristine')$site.visitrate)
## For seed counts of focal species 
seedcounts <- read_excel("~/MByRes Research/r/rdata/Focals/focals_seedcounts.xlsx", na="NA")
View(seedcounts)
sd(subset(seedcounts1, species == 'C.edulis')$seed.count[site.type =='Invaded'])
length(allfocals[Type=='invaded'])
sd(subset(allfocals, Type == 'pristine')$site.insectdiv)

## Mean & sd visitation rate: per insect order, per focal species ----
# Could repeat for the diptera & hymenoptera subset by importing the .diptera / .hymenoptera files for om, ce and es
omcol<-read_excel("~/path/om.coleoptera.xlsx")
escol<-read_excel("~/path/es.coleoptera.xlsx")
cecol<-read_excel("~/path/ce.coleoptera.xlsx")
mean(subset(omcol, treatment == 'Pristine')$site.visitrate)
sd(subset(omcol, treatment == 'Invaded')$site.visitrate)


# PLOTS: To create figure 4 in manuscript ----

#Figure 4a: Visitation per floral unit
figure3a<-ggplot(data=focalsordertotals, aes(order,rate, fill = Treatment)) +
  geom_boxplot()+xlab("Flower visitor taxa")+ylab("Visitation per floral unit") +
  facet_grid(rows=vars(plant),scale = "free") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=11)) +
  scale_fill_manual(values = c("#6BAED6", "#A1D99B")) + guides(fill = "none")
  # + geom_jitter(width=0.2) 
#Figure 4b: Insect species richness per focal species
figure3b<- ggplot(data=focalsordertotals, aes(order,richness, fill = Treatment))+ 
    geom_boxplot()+xlab("Flower visitor taxa")+ylab("Insect species richness") +
    facet_grid(rows=vars(plant),scale = "free") + guides(fill = "none") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=11)) +
  scale_fill_manual(values = c("#6BAED6", "#A1D99B")) 
    # + geom_jitter(width=0.2) 
#Figure 4c: Seed abundance per focal species
figure3c<-ggplot(data=seedcounts, aes(Treatment,seed.count, fill = Treatment))+
      geom_boxplot()+xlab("Treatment")+ylab("Seed Abundance")+
      facet_grid(rows=vars(species), scales='free') + guides() +
      theme(axis.text=element_text(size=10), axis.title=element_text(size=11), legend.title = element_text(face = "bold")) +
      scale_fill_manual(values = c("#6BAED6", "#A1D99B")) 
    # + geom_jitter(width=0.2)

display.brewer.pal(n = 8, name = "Greens")
brewer.pal(n = 6, name = "Greens")

# To plot them all together: (a) seeds, (b) visitation, (c) species richness
plot_grid(figure3a, figure3b,figure3c, nrow = 1, labels = c("a)", "b)", "c)"),
              rel_widths = c(1, 1, 1))


# GLMs: Insect visitation per flower, by treatment & plant ----

# Preparing factors for post-hocs of glms
allfocals$Type <- as.factor(allfocals$Type)
allfocals$Plant <-as.factor(allfocals$Plant)
allfocals$Flowerno <-as.factor(allfocals$Flowerno)
allfocals$interaction <- with(allfocals, interaction(Type, Plant))
allfocals$interaction2 <- with(allfocals, interaction(Type, Flowerno))
allfocals$interaction3 <- with(allfocals, interaction(Flowerno, Plant))

# Testing the right model to use for visitrate~treatment - glm.nb comes out top
par(mfrow=c(1,1))
hist(allfocals$sitevisitrate)
mod1<-glm.nb(data=allfocals, sitevisitrate~Type+Plant+Type:Plant)
mod2<-glm(data=allfocals, sitevisitrate~Type+Plant+Type:Plant, family=gaussian)
mod3<-glm(data=allfocals, sitevisitrate~Type+Plant+Type:Plant, family=gaussian(link='log'))
mod4<-glm(data=allfocals, sitevisitrate~Type+Plant+Type:Plant, family=gaussian(link='inverse'))
mod5<-glm(data=allfocals, sitevisitrate~Type+Plant+Type:Plant, family=poisson)
mod6<-glmmTMB(data=allfocals, sitevisitrate~ Type + (1|Plant))
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod7)
AIC_mods[order(AIC_mods$AIC),] # mod 1 glm.nb is best

# Running the models
summary(mod1)
mod1_sim <- simulateResiduals(mod1, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)#Significant KS deviation...

# POST-HOC TESTS
anova(lm(mod1)) # For overall effects - WORKS!
summary(glht(mod1, linfct = mcp(Type = "Tukey"), data=allfocals), interaction_average=TRUE) # To test single variables
summary(glht(mod1, linfct = mcp(Plant = "Tukey"), data=allfocals))
mod1.2 <- glm.nb(sitevisitrate ~ interaction - 1, data = allfocals) # To test interaction
summary(glht(mod1.2, linfct = mcp(interaction = "Tukey")))
#Expected results, but still looks very over-dispersed...
# Preparing factors for post-hocs of glms (for order specific data, in 'focalsorders')
focalsorders$type <- as.factor(focalsorders$type)
focalsorders$plant <-as.factor(focalsorders$plant)
focalsorders$interaction <- with(focalsorders, interaction(type, plant))

## COLEOPTERA visitation ----
ggplot(data=focalsorders,aes(type,col.rate,fill=type)) + geom_boxplot() +
  facet_grid(rows=vars(plant),scale = "free")
hist(col.rate, data=focalsorders)
# Models
mod1<-glm.nb(col.rate~plant+type+type:plant, data=focalsorders)
mod2<-glm(col.rate~type+plant+type:plant, data=focalsorders, family=gaussian)
mod3<-glm(col.rate~plant+type+type:plant, data=focalsorders, family=gaussian(link='log'))
mod4<-glm(col.rate~plant+type+type:plant, data=focalsorders, family=gaussian(link='inverse'))
mod5<-glm(col.rate~plant+type+type:plant, data=focalsorders, family=poisson)
mod6<-glmmTMB(col.rate~type + (1|plant) + (1|flowerno),data=focalsorders)
mod7<-glmmTMB(col.rate~type + (1|plant),data=focalsorders)
AIC_mods <- AIC(mod1,mod2,mod3,mod4,mod5, mod6, mod7)
AIC_mods[order(AIC_mods$AIC),] # Mod2 (gaussian) on top, despite skew
# Running and checking model
summary(mod2)
imod6_sim <- simulateResiduals(mod2, n = 1000)#Plotting residuals to look at goodness of fit
plot(imod6_sim)
plot(mod2)
# POST-HOC
anova(lm(mod2)) # For overall effects
summary(glht(mod2, linfct = mcp(type = "Tukey"), data=focalsorders)) # To test single variables
summary(glht(mod2, linfct = mcp(plant = "Tukey"), data=focalsorders))
mod1.2 <- glm(col.rate ~ interaction - 1, data =focalsorders, family=gaussian) # To test interaction
summary(glht(mod1.2, linfct = mcp(interaction = "Tukey")))

## DIPTERA visitation ----
ggplot(data=focalsorders,aes(type,dip.rate,fill=type)) + geom_boxplot() +
  facet_grid(rows=vars(plant),scale = "free")
hist(dip.rate, data=focalsorders)
# Models
mod1<-glm.nb(dip.rate~type+plant+type:plant, data=focalsorders)
mod2<-glm(dip.rate~type+plant+type:plant, data=focalsorders, family=gaussian)
mod3<-glm(dip.rate~type+plant+type:plant, data=focalsorders, family=gaussian(link='log'))
mod4<-glm(dip.rate~type+plant+type:plant, data=focalsorders, family=gaussian(link='inverse'))
mod5<-glm(dip.rate~type+plant+type:plant, data=focalsorders, family=poisson)
mod6<-glmmTMB(dip.rate~type + (1|plant),data=focalsorders)
AIC_mods <- AIC(mod1,mod2,mod3,mod4,mod5, mod6, mod7)
AIC_mods[order(AIC_mods$AIC),] # Mod6 (glmm) on top, normal dist
# Running and checking model
summary(mod6)
imod6_sim <- simulateResiduals(mod6, n = 1000)#Plotting residuals to look at goodness of fit
plot(imod6_sim)
plot(mod6)
# POST-HOC
anova(lm(mod6)) # For overall effects
summary(glht(mod2, linfct = mcp(type = "Tukey"), data=focalsorders)) # To test single variables
summary(glht(mod2, linfct = mcp(plant = "Tukey"), data=focalsorders))
mod1.2 <- glm(dip.rate ~ interaction - 1, data =focalsorders, family=gaussian) # To test interaction
summary(glht(mod1.2, linfct = mcp(interaction = "Tukey")))

## HYMENOPTERA visitation ----
ggplot(data=focalsorders,aes(type,hym.rate,fill=type)) + geom_boxplot() +
  facet_grid(rows=vars(plant),scale = "free")
hist(hym.rate, data=focalsorders)
# Models
mod1<-glm.nb(hym.rate~type+plant+type:plant, data=focalsorders)
mod2<-glm(hym.rate~type+plant+type:plant, data=focalsorders, family=gaussian)
mod3<-glm(hym.rate~type+plant+type:plant, data=focalsorders, family=gaussian(link='log'))
mod4<-glm(hym.rate~type+plant+type:plant, data=focalsorders, family=gaussian(link='inverse'))
mod5<-glm(hym.rate~type+plant+type:plant, data=focalsorders, family=poisson)
mod6<-glmmTMB(hym.rate~type + (1|plant),data=focalsorders)
AIC_mods <- AIC(mod1,mod2,mod3,mod4,mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod2 (gaussian) on top, equal to 3 and 4
# Running and checking model
summary(mod2)
imod6_sim <- simulateResiduals(mod2, n = 1000)#Plotting residuals to look at goodness of fit
plot(imod6_sim)
plot(mod2)
# POST-HOC
anova(lm(mod2)) # For overall effects
summary(glht(mod2, linfct = mcp(type = "Tukey"), data=focalsorders)) # To test single variables
summary(glht(mod2, linfct = mcp(plant = "Tukey"), data=focalsorders))
mod1.2 <- glm(hym.rate ~ interaction - 1, data =focalsorders, family=gaussian) # To test interaction
summary(glht(mod1.2, linfct = mcp(interaction = "Tukey")))

# GLMs: Insect species richness - Testing effect of treatment, plant species & flower no. ----
## Species richness: all insect orders ----
# Testing models goodness of fit
hist(allfocals$site.insectdiv)
imod1<-glm.nb(site.insectdiv~Type+Plant+Type:Plant, data=allfocals)
imod2<-glm(site.insectdiv~Type+Plant+Type:Plant,data=allfocals, family=gaussian)
imod2<-glm(site.insectdiv~Type*Plant*Flowerno,data=allfocals, family=gaussian)
imod3<-glm(site.insectdiv~Type+Plant+Type:Plant,data=allfocals, family=gaussian(link='log'))
imod4<-glm(site.insectdiv~Type+Plant+Type:Plant,data=allfocals, family=gaussian(link='inverse'))
imod5<-glm(site.insectdiv~Type+Plant+Type:Plant,data=allfocals, family=poisson)
imod6<-glmmTMB(site.insectdiv~Type + (1|Plant) + (1|No. Flowers),data=allfocals)
imod7<-glmmTMB(site.insectdiv~Type + (1|Plant),data=allfocals)
AIC_mods <- AIC(imod1,imod2,imod3,imod4,imod5, imod7)
AIC_mods[order(AIC_mods$AIC),]
View(allfocals)
# Running and checking model
summary(imod2)
imod6_sim <- simulateResiduals(imod2, n = 1000)#Plotting residuals to look at goodness of fit
plot(imod6_sim)#KS deviation
plot(imod2)
# POST-HOC
anova(lm(imod2)) # For overall effects
summary(glht(imod2, linfct = mcp(Type = "Tukey"), data=allfocals),interaction.evenness=TRUE) # To test single variables
summary(glht(imod2, linfct = mcp(Plant = "Tukey"), data=allfocals))
mod1.2 <- glm(site.insectdiv ~ interaction - 1, data = allfocals, family=gaussian) # To test interaction
summary(glht(mod1.2, linfct = mcp(interaction = "Tukey"),interaction.evenness=TRUE))

# To plot them all together (INSECTDIV in different treatments)...
ggplot(data=allfocals, aes(Type,site.insectdiv, fill = Type))+
  geom_boxplot()+xlab("Treatment")+ylab("Insect species richness") +
  facet_grid(rows=vars(Plant),scale = "free") +
  geom_jitter(width=0.2) +
  scale_fill_brewer(palette = "Paired")
ggplot(data=focalsorders2, aes(order,richness, fill = type))+
  geom_boxplot()+xlab("")+ylab("") +
  facet_grid(rows=vars(plant),scale = "free") +
  scale_fill_brewer(palette = "Paired")

## COLEOPTERA richness ----
ggplot(data=focalsorders,aes(type,col.rich,fill=type)) + geom_boxplot() +
  facet_grid(rows=vars(plant),scale = "free") + geom_jitter(width=0.2)
hist(focalsorders$col.rich)
# Models
mod1<-glm.nb(col.rich~type+plant+type:plant, data=focalsorders)
mod2<-glm(col.rich~type+plant+type:plant, data=focalsorders, family=gaussian)
mod3<-glm(col.rich~type+plant+type:plant, data=focalsorders, family=gaussian(link='log'))
mod4<-glm(col.rich~type+plant+type:plant, data=focalsorders, family=gaussian(link='inverse'))
mod5<-glm(col.rich~type+plant+type:plant, data=focalsorders, family=poisson)
mod6<-glmmTMB(col.rich~type + (1|plant),data=focalsorders)
mod7<-glmmTMB(col.rich~type + (1|plant) + (1|flowerno),data=focalsorders)
AIC_mods <- AIC(mod1,mod2,mod3,mod4,mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod2 (gaussian) on top
# Running and checking model
summary(mod2)
imod6_sim <- simulateResiduals(mod2, n = 1000)#Plotting residuals to look at goodness of fit
plot(imod6_sim)# No probs
plot(mod2)
# POST-HOC
anova(lm(mod2)) # For overall effects
anova(lm(mod5)) # For overall effects
summary(glht(mod2, linfct = mcp(type = "Tukey"), data=focalsorders)) # To test single variables
summary(glht(mod2, linfct = mcp(plant = "Tukey"), data=focalsorders))
mod1.2 <- glm(col.rich ~ interaction - 1, data =focalsorders, family=gaussian) # To test interaction
summary(glht(mod1.2, linfct = mcp(interaction = "Tukey")))

## DIPTERA richness ----
ggplot(data=focalsorders,aes(type,dip.rich,fill=type)) + geom_boxplot() +
  facet_grid(rows=vars(plant),scale = "free")
hist(focalsorders$dip.rich)
# Models
mod1<-glm.nb(dip.rich~type+plant+type:plant, data=focalsorders)
mod2<-glm(dip.rich~type+plant+type:plant, data=focalsorders, family=gaussian)
mod3<-glm(dip.rich~type+plant+type:plant, data=focalsorders, family=gaussian(link='log'))
mod4<-glm(dip.rich~type+plant+type:plant, data=focalsorders, family=gaussian(link='inverse'))
mod5<-glm(dip.rich~type+plant+type:plant, data=focalsorders, family=poisson)
mod6<-glmmTMB(dip.rich~type + (1|plant),data=focalsorders)
mod7<-glmmTMB(dip.rich~type + (1|plant) + (1|flowerno),data=focalsorders)
AIC_mods <- AIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7)
AIC_mods[order(AIC_mods$AIC),] # Mod1 (glm.nb) on top, skewed data (followed by poisson)
# Running and checking model
summary(mod1)
imod6_sim <- simulateResiduals(mod1, n = 1000)#Plotting residuals to look at goodness of fit
plot(imod6_sim)# No probs
plot(mod1)
# POST-HOC
anova(lm(mod1)) # For overall effects (Works!)
summary(glht(mod2, linfct = mcp(type = "Tukey"), data=focalsorders)) # To test single variables
summary(glht(mod2, linfct = mcp(plant = "Tukey"), data=focalsorders))
mod1.2 <- glm.nb(dip.rich ~ interaction - 1, data =focalsorders) # To test interaction
summary(glht(mod1.2, linfct = mcp(interaction = "Tukey")))

## HYMENOPTERA richness ----
ggplot(data=focalsorders,aes(type,hym.rich,fill=type)) + geom_boxplot() +
  facet_grid(rows=vars(plant),scale = "free")
hist(focalsorders$hym.rich)
# Models
mod1<-glm.nb(hym.rich~type+plant+type:plant, data=focalsorders)
mod2<-glm(hym.rich~type+plant+type:plant, data=focalsorders, family=gaussian)
mod3<-glm(hym.rich~type+plant+type:plant, data=focalsorders, family=gaussian(link='log'))
mod4<-glm(hym.rich~type+plant+type:plant, data=focalsorders, family=gaussian(link='inverse'))
mod5<-glm(hym.rich~type+plant+type:plant, data=focalsorders, family=poisson)
mod6<-glmmTMB(hym.rich~type + (1|plant),data=focalsorders)
mod7<-glmmTMB(hym.rich~type + (1|plant) + (1|flowerno),data=focalsorders)
AIC_mods <- AIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7)
AIC_mods[order(AIC_mods$AIC),] # Mod5 (poisson) on top, but not very skewed data (followed by nb)
# Running and checking model
summary(mod5)
imod6_sim <- simulateResiduals(mod1, n = 1000)#Plotting residuals to look at goodness of fit
plot(imod6_sim)# No probs
plot(mod5)
# POST-HOC
anova(lm(mod5)) # For overall effects (Works!)
anova(mod1)
summary(glht(mod5, linfct = mcp(type = "Tukey"), data=focalsorders)) # To test single variables
summary(glht(mod5, linfct = mcp(plant = "Tukey"), data=focalsorders))
mod1.2 <- glm(hym.rich ~ interaction - 1, data =focalsorders, family=poisson) # To test interaction
summary(glht(mod1.2, linfct = mcp(interaction = "Tukey")))

