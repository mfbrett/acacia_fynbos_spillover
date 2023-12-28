  library(glmmTMB)
  library(DHARMa)
  library(readxl)
  library(ggplot2)
  library(MASS)
  library(multcomp)
  library(cowplot)
  library(tidyverse)
  library(hrbrthemes)
  library(patchwork)
  library(GGally)
  library(viridis)

# Importing & preparing data (enter your file path) ----
allvisitsresults <- read_excel("~/path/allvisitsresults.xlsx", 
                                  col_types = c("numeric", "text", "text", 
                                                           "text", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "text", "text", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric","numeric","numeric","numeric", "numeric", 
                                                           "numeric","numeric","numeric"))
allvisitsresults2 <- read_excel("~/path/allvisitsresults2.xlsx")
View(allvisitsresults)
names(allvisitsresults)
names(allvisitsresults2)
par(mfrow=c(1,1))

# Data Cleaning (To treat variables as factors in glmm & post-hoc models)
allvisitsresults$Treatment <- as.factor(allvisitsresults$Treatment)
allvisitsresults$veg.type <- as.factor(allvisitsresults$veg.type)
allvisitsresults$flowerno <- as.numeric(allvisitsresults$flowerno)
allvisitsresults$flowersperhectare <- as.numeric(allvisitsresults$flowersperhectare)
allvisitsresults$interaction1 <- with(allvisitsresults, interaction(Treatment, veg.type))
allvisitsresults$interaction2 <- with(allvisitsresults, interaction(Treatment, flowerno))
allvisitsresults$interaction3 <- with(allvisitsresults, interaction(veg.type, flowerno))
allvisitsresults$interaction2a <- with(allvisitsresults, interaction(Treatment,flowersperhectare))
allvisitsresults$interaction3a <- with(allvisitsresults, interaction(veg.type, flowersperhectare))

# Testing for differences in vegetation age and elevation among veg types ----
# This will determine whether we need to include veg type as a random factor in our glmm analysis
anova(lm(elevation~veg.type))
anova(lm(veg.age~veg.type))
anova(lm(obsinsectdiv~Treatment))
summary(lm(obsinsectdiv~Treatment))
Treatmentordered<- factor(Treatment, levels=c("P","C","I"), ordered = TRUE)

# VISITATION RATE (total visits per flower for each site) ----
hist(allvisitsresults$visitrate)
mean(allvisitsresults$visitrate)
shapiro.test(visitrate) #Non-normal
ggplot(data=Invadedsiteresults, aes(flowerno,visitrate)) + geom_point()
ggplot(data=allvisitsresults, aes(flowerno,visitrate)) +
  geom_point(aes(color=Treatment)) + 
  xlab("Site flower no.")+ 
  ylab("Insect visitation per flower") + 
  facet_grid(rows=vars(Treatment),scale = "free") + 
  scale_fill_brewer(palette = "Paired") + geom_smooth(method='lm', se=TRUE, colour='grey') # Different behaviour at invaded?

# TRUE VISITATION RATE (total visits per flower for each hectare site, visits/number of flowers per hectare) ----
hist(allvisitsresults$visitratehectare)
mean(allvisitsresults$visitratehectare)
shapiro.test(allvisitsresults$visitratehectare) #Significant = Non-normal data distribution
ggplot(data=Invadedsiteresults, aes(flowerno,visitrate)) + geom_point()
ggplot(data=allvisitsresults, aes(flowersperhectare,visitratehectare)) +
  geom_point(aes(color=Treatment)) + 
  xlab("Site flower no.")+ 
  ylab("Insect visitation per flower") + 
  facet_grid(rows=vars(Treatment),scale = "free") + 
  scale_fill_brewer(palette = "Paired") + geom_smooth(method='lm', se=TRUE, colour='grey') # Different behaviour at invaded?

#### **Plot code for creating manuscript figure 3** ####
par(mfrow=c(2,2))
#Figure 3a
plot2a<-ggplot(data=allvisitsresults, aes(Treatment,links.per.species,fill=Treatment)) +
  geom_boxplot() + 
  xlab("")+ 
  ylab("Mean links per species") + 
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) + guides(fill = "none")
#Figure 3b
plot2b<-ggplot(data=allvisitsresults, aes(Treatment,interaction.evenness,fill=Treatment)) +
  geom_boxplot() + 
  xlab("")+ 
  ylab("Network interaction eveness") + 
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) + guides(fill = "none")
#Figure 3c
plot2c<-ggplot(data=allvisitsresults2, aes(Order,visitratehectare,fill=Treatment)) +
  geom_boxplot() + 
  xlab("") +
  ylab("Mean insect visits per floral unit") + guides(fill = "none") +
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) 
#Figure 3d
plot2d<-ggplot(data=allvisitsresults2, aes(Order,richness,fill=Treatment)) +
  geom_boxplot() + 
  xlab("") +
  ylab("Insect visitor species richness") + #guides(fill = "none") +
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11), legend.position = c(0.8,.77),legend.title = element_text(face = "bold")) +
  scale_fill_brewer(palette = 4,labels=c("Flower-removed","Invaded","Pristine")) 
facet_grid(rows=vars(order),scale = "free")

plot_grid(plot2a, plot2b, plot2c, plot2d, nrow = 2, labels = c("a)", "b)", "c)", "d)"),
          rel_heights = c(1, 1))

ggplot(data=allvisitsresults, aes(Treatment,visitrate, fill = Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Visitation rate (visits.flower.site)")+
  scale_fill_brewer(palette = "Paired",labels=c("Flower-removed","Invaded","Pristine")) 
facet_grid(rows=vars(veg.type),scale='free')


# Running glm(m)s on visit.freq, including flowerno. ----
mod1<-glm.nb(visitation.freq~Treatment*veg.type*flowerno, data=allvisitsresults)
mod2<-glm(visitation.freq~Treatment*veg.type*flowerno, data=allvisitsresults, family=gaussian)
mod3<-glm(visitation.freq~Treatment*veg.type*flowerno, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(visitation.freq~Treatment*veg.type*flowerno, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(visitation.freq~Treatment*veg.type*flowerno, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(visitation.freq~Treatment + (1|veg.type) + (1|flowerno), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod2 best (gaussian)
summary(mod2)
mod6_sim <- simulateResiduals(mod6, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod6_sim)#Significant outlier deviation...
# Post-hocs
anova(lm(mod3)) # For overall results
summary(glht(mod3, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod3, linfct = mcp(veg.type = "Tukey"))) # Between veg types
summary(glht(mod3, linfct = mcp(flowerno = "Tukey"))) # Between veg types
mod5.1 <- glm(visitrate ~ interaction - 1, data = allvisitsresults, family=gaussian, link='log')
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type


# Running glm(m)s on visitrate ----
plot(visitrate~Treatment, data=allvisitsresults)
mod1<-glm.nb(visitrate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(visitrate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(visitrate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(visitrate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(visitrate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(visitrate~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best
summary(mod6)
mod6_sim <- simulateResiduals(mod6, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod6_sim)#Significant outlier deviation...
# POST-HOC TESTS - can't do on GLMM...
anova(lm(mod6))
summary(glht(mod6, linfct = mcp(Treatment = "Tukey"), data=allvisitsresults)) # Between Treatments
summary(glht(mod6, linfct = mcp(veg.type = "Tukey"), data=allvisitsresults)) # Between veg types
mod5.1 <- glm(visitrate ~ interaction - 1, data = allvisitsresults, family=poisson)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type

# Plotting visitrate ~ Treatment (optional facets of veg types)
ggplot(data=allvisitsresults, aes(Treatment,visitrate, fill = Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Visitation rate (visits.flower.site)")+
  scale_fill_brewer(palette = "Paired",labels=c("Flower-removed","Invaded","Pristine")) 
facet_grid(rows=vars(veg.type),scale='free')
# Plotting visitrate ~ veg type
ggplot(data=allvisitsresults, aes(veg.type,visitrate, fill = veg.type))+
  geom_boxplot()+xlab("Treatment")+ylab("Visitation rate (visits per flower) per site")+
  scale_fill_brewer(palette = "Paired",labels=c("AS","EF","OM"))

# Running glm(m)s on insect species richness - accounting for random effects: veg type----
hist(obsinsectdiv)
shapiro.test(obsinsectdiv) # Normally distributed
mod1<-glm.nb(obsinsectdiv~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(obsinsectdiv~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(obsinsectdiv~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(obsinsectdiv~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(obsinsectdiv~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glm(obsinsectdiv~Treatment*veg.type, data=allvisitsresults, family=poisson)
mod7<-glmmTMB(obsinsectdiv~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
AIC_mods[order(AIC_mods$AIC),] # Mod5 best
summary(mod5)
mod5_sim <- simulateResiduals(mod7, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod5_sim)#Significant outlier deviation...

# POST-HOC TESTS
anova(lm(mod5))
summary(glht(mod5, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod5, linfct = mcp(veg.type = "Tukey"))) # Between veg types
mod5.1 <- glm(obsinsectdiv ~ interaction - 1, data = allvisitsresults, family=poisson)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type

# Plotting
ggplot(data=allvisitsresults, aes(Treatment,obsinsectdiv, fill=Treatment)) +
  geom_boxplot() +
  xlab("Treatment") + ylab("Insect species richness") +
  scale_fill_brewer(palette="Paired") + geom_jitter(width=0.2)
facet_grid(rows=vars(veg.type),scale='free')
ggplot(data=allvisitsresults, aes(veg.type,obsinsectdiv, fill=veg.type)) +
  geom_boxplot() +
  xlab("Vegetation type") + ylab("Insect species richness") +
  scale_fill_brewer(palette=1) + geom_jitter(width = 0.2)

plot(obsinsectdiv~veg.type, data=allvisitsresults)
mod1<-glm.nb(obsinsectdiv~Treatment*veg.type, data=allvisitsresults)
mod2<-glm(obsinsectdiv~Treatment*veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(obsinsectdiv~Treatment*veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(obsinsectdiv~Treatment*veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(obsinsectdiv~veg.type+Treatment, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(obsinsectdiv~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod5 best
summary(mod5)
mod5_sim <- simulateResiduals(mod5, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod5_sim)# All good
# POST-HOC TESTS
anova(lm(mod5))
summary(glht(mod5, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod5, linfct = mcp(veg.type = "Tukey"))) # Between veg types
mod5.1 <- glm(obsinsectdiv ~ interaction - 1, data = allvisitsresults, family=poisson)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type
mod1<-(glm(obsinsectdiv~Treatment+veg.type+obsfloraldiv,data=allvisitsresults))# sig affected by veg type
mod2<-(glm(obsinsectdiv~Treatment+floraldiv+veg.type+Treatment:floraldiv))
anova(lm(obsinsectdiv~veg.type, data=allvisitsresults))
anova(lm(obsinsectdiv~floraldiv))
anova(lm(obsinsectdiv~Treatment))
TukeyHSD(aov(lm(obsinsectdiv~Treatment, data=allvisitsresults)))
TukeyHSD(aov(glm(obsinsectdiv~veg.type, data=allvisitsresults)))

#Insect species richness metrics for different vegetation types
summary(obsinsectdiv[veg.type=='AS'])
summary(obsinsectdiv[veg.type=='EF'])
summary(obsinsectdiv[veg.type=='OS'])
summary(glm(obsinsectdiv~veg.type, data=allvisitsresults))

## Coleoptera visitation frequency ----
View(allvisitsresults)
hist(col.rate, data=allvisitsresults)
plot(col.rate~Treatment, data=allvisitsresults)
plot(col.rate~veg.type, data=allvisitsresults)
mod1<-glm.nb(col.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(col.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(col.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(col.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(col.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(col.rate~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod5 best - yes, makes sense to be poisson... but no sig.results!
summary(mod6)
mod5_sim <- simulateResiduals(mod5, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod5_sim)#Significant outlier deviation...

# POST-HOC TESTS
anova(lm(mod6))
summary(glht(mod6, linfct = mcp(Treatment = "Tukey", interaction_average=TRUE))) # Between Treatments
summary(glht(mod2, linfct = mcp(veg.type = "Tukey"), interaction_average=TRUE)) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm(col.rate ~ interaction - 1, data = allvisitsresults, family=poisson)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"),interaction_average=FALSE)) # Between Treatment:veg.type

## Coleoptera species richness ----
plot(coleoptera.rich~Treatment, outline=FALSE, data=allvisitsresults)
plot(coleoptera.rich~veg.type, outline=FALSE, data=allvisitsresults)
hist(coleoptera.rich, data=allvisitsresults)
mod1<-glm.nb(coleoptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(coleoptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(coleoptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(coleoptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(coleoptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(coleoptera.rich~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod1 best - NB - does make sense
summary(mod1)
mod1_sim <- simulateResiduals(mod1, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)#Some problems...
# POST-HOC TESTS
anova(lm(mod1))
summary(glht(mod1, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod1, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm.nb(coleoptera.rich ~ interaction - 1, data = allvisitsresults)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type


## Hymenoptera visitation frequency ----
plot(hym.rate~Treatment, outline=FALSE, data=allvisitsresults)
plot(hym.rate~veg.type, outline=FALSE, data=allvisitsresults)
hist(hym.rate, data=allvisitsresults)
mod1<-glm.nb(hym.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(hym.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(hym.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(hym.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(hym.rate~Treatment*veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(hym.rate~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best
summary(mod6) # No sig results?
plot(glm(hym.rate~Treatment*veg.type, data=allvisitsresults))
mod1_sim <- simulateResiduals(mod1, n = 1000)
plot(mod1_sim)#No problems!...
# POST-HOC TESTS
anova(lm(mod6))
aov(summary(mod5, data=allvisitsresults)) # For overall results
summary(glht(mod5, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod5, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm(hym.rate ~ interaction - 1, data = allvisitsresults, family=poisson)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type

## Hymenoptera species richness ----
plot(hymenoptera.rich~Treatment, outline=FALSE, data=allvisitsresults)
plot(hymenoptera.rich~veg.type, outline=FALSE, data=allvisitsresults)
hist(hymenoptera.rich, data=allvisitsresults)
mod1<-glm.nb(hymenoptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(hymenoptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(hymenoptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(hymenoptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(hymenoptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(hymenoptera.rich~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod5 best - makes sense with skewed dist.
summary(mod5)
mod1_sim <- simulateResiduals(mod5, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)
# POST-HOC TESTS
anova(lm(mod5))
summary(glht(mod5, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod5, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm(hymenoptera.rich ~ interaction - 1, data = allvisitsresults, family=poisson)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type


## Diptera visitation frequency ----
hist(dip.rate)
plot(dip.rate~Treatment, outline=FALSE, data=allvisitsresults)
plot(dip.rate~veg.type, outline=FALSE, data=allvisitsresults)
mod1<-glm.nb(dip.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(dip.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(dip.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(dip.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(dip.rate~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(dip.rate~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod2 best - makes sense, normal dist.
summary(mod2)
mod1_sim <- simulateResiduals(mod2, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)
# POST-HOC TESTS
anova(lm(mod2))
aov(summary(mod2, data=allvisitsresults)) # For overall results
summary(glht(mod2, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod2, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm(dip.rate ~ interaction - 1, data = allvisitsresults, family=gaussian)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type

## Diptera species richness ----
hist(diptera.rich)
plot(diptera.rich~Treatment, outline=FALSE, data=allvisitsresults)
plot(diptera.rich~veg.type, outline=FALSE, data=allvisitsresults)
mod1<-glm.nb(diptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(diptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(diptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(diptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(diptera.rich~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(diptera.rich~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best (glmm) - makes sense, normal dist.
summary(mod6) 
mod1_sim <- simulateResiduals(mod6, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)
# POST-HOC TESTS - CAN'T DO FOR GLMM
anova(lm(mod6)) # For overall results
summary(glht(mod6, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod6, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glmmTMB(dip.rate ~ interaction - 1, data = allvisitsresults)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type

## LINKS PER SPECIES ----
hist(links.per.species)
boxplot(links.per.species~Treatment, data=allvisitsresults, outline=TRUE)
boxplot(links.per.species~veg.type, data=allvisitsresults, outline=TRUE)
mean(subset(allvisitsresults, Treatment == 'Invaded')$links.per.species)
sd(subset(allvisitsresults, Treatment == 'Pristine')$links.per.species) # to get standard dev
# standard dev /(sqrt(7) # To calculate Standard error (se/sqrt number of rows)
summary(links.per.species[Treatment=='Invaded'])

mod1<-glm.nb(links.per.species~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(links.per.species~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(links.per.species~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(links.per.species~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(links.per.species~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(links.per.species~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best, but chosing Model2.
summary(mod6)
mod1_sim <- simulateResiduals(mod2, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)# No problems
# POST-HOC TESTS - No sig results! :(
anova(lm(mod2)) # For overall results
summary(glht(mod2, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod2, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm(links.per.species ~ interaction - 1, data = allvisitsresults, family=gaussian)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type
# Plotting
ggplot(data=allvisitsresults, aes(Treatment,links.per.species, fill=Treatment)) +
  geom_boxplot() +
  xlab("Treatment") + ylab("Links per species") +
  scale_fill_brewer(palette="Paired") 
  facet_grid(rows=vars(veg.type),scale = "free") +
  geom_jitter(width=0.2)
ggplot(data=allvisitsresults, aes(veg.type,links.per.species, fill=veg.type)) +
  geom_boxplot() +
  xlab("Vegetation type") + ylab("Links per species") +
  scale_fill_brewer(palette="Paired")
?networklevel

## Links per species - Insect Generality ----
ggplot(data=allvisitsresults, aes(Treatment,lps.insects, fill = Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Links per species - Insects")+
  scale_fill_brewer(palette = "Paired") 

mod1<-glm.nb(lps.insects~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(lps.insects~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(lps.insects~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(lps.insects~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(lps.insects~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(lps.insects~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod2 best
summary(mod2) 
mod1_sim <- simulateResiduals(mod2, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)# No problems.
# POST-HOC TESTS  - Some funny interaction going on.
anova(lm(mod2))
summary(glht(mod2,linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod2, linfct = mcp(veg.type = "Tukey"))) # Between veg types
?glht
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm(lps.insects ~ interaction - 1, data = allvisitsresults, family=gaussian)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type
summary(mod5.1)

mean(subset(allvisitsresults, Treatment =='Cleared')$coleoptera.rich)
mean(subset(allvisitsresults, Treatment =='Cleared')$lps.insects)
sd(subset(allvisitsresults, Treatment =='Invaded')$lps.insects)

sd(subset(focalsorders, type =='Pristine')$dip.rate)


## Links per species - Plant Vulnerability ----

ggplot(data=allvisitsresults, aes(Treatment,lps.plants, fill = Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Plant generality")+
  scale_fill_brewer(palette = "Paired",labels=c("Cleared","Invaded","Pristine"))
mod1<-glm.nb(lps.plants~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(lps.plants~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(lps.plants~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(lps.plants~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(lps.plants~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(lps.plants~Treatment * veg.type, data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best
summary(mod2) 
mod1_sim <- simulateResiduals(mod3, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)# No problems.
# POST-HOC TESTS  - Everything too close to 1??
anova(lm(mod2))
summary(glht(mod2, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod2, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm(lps.plants ~ interaction - 1, data = allvisitsresults, family=poisson)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type


## Links per species - Insects ----
hist(lps.insect)
lps.i.t1 <- glmmTMB(lps.insect ~ Treatment + (1|veg.type), 
                    data=allvisitsresults, 
                    family='gaussian')
summary(lps.i.t1)
lps.i.t1_sim <- simulateResiduals(lps.i.t1, n = 1000)#Plotting residuals to look at goodness of fit
plot(lps.i.t1_sim)#No significant problems detected
ggplot(data=NULL, aes(Treatment,lps.insect, fill=Treatment)) +
  geom_boxplot() +
  xlab("Treatment") + ylab("Links per insect species") +
  scale_fill_brewer(palette="Paired")
# Links per species - Plants *TO REVISE*
hist(allvisitsresults, plant.lps)
lps.p.t1 <- glmmTMB(plant.lps ~ Treatment + (1|veg.type), 
                    data=allvisitsresults, 
                    family='gaussian')
summary(lps.p.t1)
lps.p.t1_sim <- simulateResiduals(lps.p.t1, n = 1000)#Plotting residuals to look at goodness of fit
plot(lps.p.t1_sim)#No significant problems detected
ggplot(data=NULL, aes(Treatment,lps.plant, fill=Treatment)) +
  geom_boxplot() +
  xlab("Treatment") + ylab("Links per insect species") +
  scale_fill_brewer(palette="Paired")


# Links per species - Per species, between treatments ----
lps_species <- read_excel("~/path/lps_species_wide.xlsx")
lps_species_long <- read_excel("~/path/lps_species.xlsx")
lps_col <- lps_species_long %>% filter(order == "Coleoptera")
lps_dip <- lps_species_long %>% filter(order == "Diptera")
lps_hym <- lps_species_long %>% filter(order == "Hymenoptera")
View(lps_species_long)
lps_species_long <- lps_species_long %>% mutate(mean_lps_group = case_when(mean_lps < 0 ~ 'under_0', mean_lps >= 0 & mean_lps <= 1 ~ '0-1', mean_lps > 1 ~ 'over_1', TRUE ~ NA_character_ ))
View(lps_species_long)

#Analysis
hist(lps_species_long$mean_lps)
# glmm to look at how lps changes with treatment, taking into account order...
mod1<-glm.nb(mean_lps~treatment+order+treatment:order, data=lps_species_long)
mod2<-glm(mean_lps~treatment+order+treatment:order, data=lps_species_long, family=gaussian)
mod3<-glm(mean_lps~treatment+order+treatment:order, data=lps_species_long, family=gaussian(link='log'))
mod4<-glm(mean_lps~treatment+order+treatment:order, data=lps_species_long, family=gaussian(link='inverse'))
mod5<-glm(mean_lps~treatment+order+treatment:order, data=lps_species_long, family=poisson)
mod6<-glmmTMB(mean_lps~treatment + (1|order), data=lps_species_long)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] 
summary(mod1)
anova(lm(mod1))
summary(glht(mod1, linfct = mcp(treatment = "Tukey"))) # Between Treatments
summary(glht(mod1, linfct = mcp(veg.type = "Tukey"))) # Between veg types
#Plotting
# Bar chart
ggplot(data=lps_species_long, aes(order, mean_lps, fill=treatment)) +
  geom_boxplot() +
  xlab("Treatment") + ylab("Links per insect species") +
  scale_fill_brewer(palette="Paired")


## Floral species richness and abundance ----
hist(obsfloraldiv) # Normal distribution
sd(subset(allvisitsresults, Treatment=='Invaded')$flowerno)
plot(obsfloraldiv~Treatment, data=allvisitsresults)
plot(obsfloraldiv~veg.type, data=allvisitsresults)
mod1<-glm.nb(obsfloraldiv~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(obsfloraldiv~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(obsfloraldiv~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(obsfloraldiv~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(obsfloraldiv~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(obsfloraldiv~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod5 best (poisson), despite normal dist.
summary(mod5) 
mod1_sim <- simulateResiduals(mod5, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)
# POST-HOC TESTS
anova(lm(mod5))
summary(glht(mod5, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod5, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm(obsfloraldiv ~ interaction - 1, data = allvisitsresults, family=poisson)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type

# FLORAL ABUNDANCE
hist(flowerno) # Normal
plot(flowerno~Treatment, data=allvisitsresults)
plot(flowerno~veg.type, data=allvisitsresults)
# Models
mod1<-glm.nb(flowerno~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(flowerno~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(flowerno~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(flowerno~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(flowerno~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(flowerno~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best
summary(mod6) 
mod1_sim <- simulateResiduals(mod2, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)
# POST-HOC TESTS
anova(lm(mod6)) # For overall results
summary(glht(mod2, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod2, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm(flowerno ~ interaction - 1, data = allvisitsresults, family=gaussian)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type

# INTERACTION EVENNESS ----
hist(interaction.evenness) # Skewed distribution
plot(interaction.evenness~Treatment, data=allvisitsresults)
plot(interaction.evenness~veg.type, data=allvisitsresults)
mod1<-glm.nb(interaction.evenness~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults)
mod2<-glm(interaction.evenness~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(interaction.evenness~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(interaction.evenness~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(interaction.evenness~Treatment+veg.type+Treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(interaction.evenness~Treatment + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best
summary(mod6) 
mod1_sim <- simulateResiduals(mod6, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)# No problems.
# POST-HOC TESTS
anova(lm(mod6)) # For overall results
summary(glht(mod6, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod6, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm(flowerno ~ interaction - 1, data = allvisitsresults, family=gaussian)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type
?ggplot
# Plotting
ggplot(data=allvisitsresults, aes(Treatment,interaction.evenness, fill=Treatment)) +
  geom_boxplot() +
  xlab("Treatment") + ylab("Interaction evenness") +
  scale_fill_brewer(palette="Paired") + geom_jitter(width=0.2)


# Shannon diversity ----
# Shannon diversity is related to interaction evenness and links per species
hist(shannon.diversity)
shapiro.test(shannon.diversity) #Normally distributed data
plot(glm(shannon.diversity~Treatment+veg.type+obsfloraldiv))# Normally distributed residuals
summary(glm(shannon.diversity~Treatment+veg.type))
anova(lm(shannon.diversity~site))
anova(lm(shannon.diversity~Treatment))
TukeyHSD(aov(lm(shannon.diversity~Treatment))) 
anova(lm(shannon.diversity~veg.type))
TukeyHSD(aov(lm(shannon.diversity~as.factor(floraldiv))))# How to I plot this/ test two continuous variables?
ggplot(data=allvisitsresults, aes(Treatment,shannon.diversity, fill=Treatment)) +
  geom_boxplot() +
  xlab("Treatment") + ylab("Shannon Diversity") +
  scale_fill_brewer(palette="Paired")

# Post-hoc tests for plant data
hist(robust.plant)
shapiro.test(robust.plant) # Non normal
kruskal.test(robust.plant~Treatment) # Non significant
kruskal.test(robust.plant~site)
kruskal.test(robust.plant~veg.type)
kruskal.test(robust.plant~floraldiv)

# Post-hoc tests for insect data
hist(robust.insect)
shapiro.test(robust.insect) # Normal
anova(lm(robust.insect~Treatment))
TukeyHSD(aov(robust.insect~Treatment)) #Non significant
anova(lm(robust.insect~veg.type))
anova(lm(robust.insect~floraldiv))


## Analysis of data from just 'invaded' sites ----
#Import data (enter your own file path)
Invadedsiteresults <- read_excel("~/path/Invadedsiteresults.xlsx", col_types = c("numeric", "text", "text", 
                                                           "text", "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric","numeric","numeric", 
                                                            "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                           "text", "text", "numeric"))
# Comparing fynbos flower denstiy to Acacia flower density at invaded sites
ggplot(data=Invadedsiteresults, aes(acaciaperhectare,flowersperhectare))+geom_point()
t.test(acaciaperhectare~flowersperhectare,data=Invadedsiteresults,)
?t.test
# Total visits per flower and flower abundance seem effected by acacia abundance
mod1<-glm.nb(visitrate~flowerno+veg.type+flowerno:veg.type, data=allvisitsresults)
mod2<-glm(visitrate~flowerno+veg.type+flowerno:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(visitrate~flowerno+veg.type+flowerno:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(visitrate~flowerno+veg.type+flowerno:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(visitrate~flowerno+veg.type+flowerno:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(visitrate~flowerno + (1|veg.type), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod4 best
summary(mod6) 
mod1_sim <- simulateResiduals(mod6, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)

# POST-HOC TESTS
aov(summary(mod6, data=allvisitsresults)) # For overall results
summary(glht(mod6, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod6, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm(flowerno ~ interaction - 1, data = allvisitsresults, family=gaussian)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type
?ggplot

# Plotting
ggplot(data=allvisitsresults, aes(Treatment,interaction.evenness, fill=Treatment)) +
  geom_boxplot() +
  xlab("Treatment") + ylab("Interaction evenness") +
  scale_fill_brewer(palette="Paired")

