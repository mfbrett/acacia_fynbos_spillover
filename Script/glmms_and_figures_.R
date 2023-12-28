# Main Analysis Code, now with most up to date allvisitsresults_new
#1. Load packages ----
library(glmmTMB)
library(DHARMa)
library(readxl)
library(ggplot2)
library(MASS)
library(multcomp)
library(cowplot)
library(lme4)
library(ggdist) # for stat_halfeye, useful plot feature for looking at data distribution
library(broom) # to extract coefficients from plots

#2. Importing & preparing data (enter your file path) ----
allvisitsresults <- read_excel("~/path/allvisitsresults.xlsx")
View(allvisitsresults)
allvisitsresults2 <- read_excel("~/path/allvisitsresults2.xlsx")
par(mfrow=c(1,1))
# ordering levels for 'treatment', as it makes sense in GLMs to compare cleared/invaded sites to Distant sites:
allvisitsresults$treatment<- factor(allvisitsresults$treatment)
allvisitsresults$treatment <- relevel(allvisitsresults$treatment, "Distant")
levels(allvisitsresults$treatment)

# Data Cleaning (To treat variables as factors in glmm & post-hoc models) 
allvisitsresults$treatment <- as.factor(allvisitsresults$treatment)
allvisitsresults$veg.type <- as.factor(allvisitsresults$veg.type)
allvisitsresults$flowerno <- as.numeric(allvisitsresults$flowerno)
allvisitsresults$insectdiv <- as.numeric(allvisitsresults$insectdiv)
allvisitsresults$flowersperhectare <- as.numeric(allvisitsresults$flowersperhectare)
allvisitsresults$interaction1 <- with(allvisitsresults, interaction(treatment, veg.type))
allvisitsresults$interaction2 <- with(allvisitsresults, interaction(treatment, flowerno))
allvisitsresults$interaction3 <- with(allvisitsresults, interaction(veg.type, flowerno))
allvisitsresults$interaction2a <- with(allvisitsresults, interaction(treatment,flowersperhectare))
allvisitsresults$interaction3a <- with(allvisitsresults, interaction(veg.type, flowersperhectare))

#3. Calculating means and SE of metrics for each treatment ----
# To generate means: 
      # If you want means for all columns, use aggregate(. ~ treatment,data = allvisitsresults, FUN = function(x) if(is.numeric(x)) mean(x) else x[1])

mean_data <- aggregate(cbind(insectdiv, floraldiv, interaction_sampling_percent, coleoptera.rich, visitrate, hymenoptera.rich, diptera.rich, col.rate, hym.rate, dip.rate, interaction.evenness, weighted.nestedness, links.per.species, lps.insects, lps.plants, nestedness) ~ treatment, data = allvisitsresults, FUN = function(x) if(is.numeric(x)) mean(x) else x[1])
mean_data
# To generate standard error:
stderr_data <- aggregate(cbind(insectdiv, floraldiv, interaction_sampling_percent, coleoptera.rich, visitrate, hymenoptera.rich, diptera.rich, col.rate, hym.rate, dip.rate, interaction.evenness, weighted.nestedness, links.per.species, lps.insects, lps.plants, nestedness) ~ treatment, data=allvisitsresults, function(x) sqrt(var(x)/length(x)))
stderr_data
# To print means and SEs together in table:
data.frame(treatment = mean_data$treatment, mean = mean_data, stderr = stderr_data)

#4. **Plot code for creating manuscript figure 3** ####
par(mfrow=c(2,2))
#Figure 3a
plot2a<-ggplot(data=allvisitsresults, aes(treatment,links.per.species,fill=treatment)) +
  geom_boxplot() + 
  xlab("")+ 
  ylab("Mean links per species") + 
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) + guides(fill = "none")
plot2a
#Figure 3b
plot2b<-ggplot(data=allvisitsresults, aes(treatment,interaction.evenness,fill=treatment)) +
  geom_boxplot() + 
  xlab("")+ 
  ylab("Network interaction eveness") + 
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) + guides(fill = "none")
plot2b

#Figure 3c
plot2c<-ggplot(data=allvisitsresults2, aes(Order,visitratehectare,fill=Treatment)) +
  geom_boxplot() + 
  xlab("") +
  ylab("Mean insect visits per floral unit") + guides(fill = "none") +
  coord_cartesian(ylim = c(0, 0.00053)) +
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) 
plot2c
#Figure 3d
plot2d<-ggplot(data=allvisitsresults2, aes(Order,richness,fill=Treatment)) +
  geom_boxplot() + 
  xlab("") +
  ylab("Insect visitor species richness") + #guides(fill = "none") +
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11), legend.position = c(0.8,.77),legend.title = element_text(face = "bold")) +
  scale_fill_brewer(palette = 4,labels=c("Flower-removed","Invaded","Distant")) 
plot2d
facet_grid(rows=vars(order),scale = "free")

plot_grid(plot2a, plot2b, plot2c, plot2d, nrow = 2, labels = c("a)", "b)", "c)", "d)"),
          rel_heights = c(1, 1))

# For reviewers: ANOVA and plotting interaction sampling effectiveness against metrics ----

# ANOVA to compare sampling completeness between treatments:
anova(lm(interaction_sampling_percent~treatment, data=allvisitsresults))

#Plots:
ggplot(data=allvisitsresults, aes(treatment,interaction_sampling_percent, fill=treatment)) +
  geom_boxplot() + xlab("Links per species")+ ylab("Interaction Sampling Completeness") + 
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) + guides(fill = "none")

lps_ic_plot<-ggplot(data=allvisitsresults, aes(links.per.species,interaction_sampling_percent, col=treatment)) +
   geom_point() + xlab("Links per species")+ ylab("Interaction Sampling Completeness") + 
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) + guides(fill = "none") +
    annotate("text",x=1.1,y=58,label=(paste0("slope==",coef(lm(allvisitsresults$interaction_sampling_percent~allvisitsresults$links.per.species))[2])),parse=TRUE) # annotate line allows display of the slope of the line
lps_ic_plot
anova(lm(formula = interaction_sampling_percent~links.per.species, data = allvisitsresults))
summary(lm(formula = interaction_sampling_percent~links.per.species, data = allvisitsresults))

ggplot(data=allvisitsresults, aes(interaction.evenness,interaction_sampling_percent, col=treatment)) +
  geom_point() + xlab("Interaction Evenness")+ ylab("Interaction Sampling Completeness") + 
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) + guides(fill = "none")

ggplot(data=allvisitsresults, aes(visitratehectare,interaction_sampling_percent, col=treatment)) +
  geom_point() + xlab("Visitation rate per hectare")+ ylab("Interaction Sampling Completeness") + 
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) + guides(fill = "none")

ggplot(data=allvisitsresults, aes(nestedness,interaction_sampling_percent, col=treatment)) +
  geom_point() + xlab("Nestedness")+ ylab("Interaction Sampling Completeness") + 
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) + guides(fill = "none")

ggplot(data=allvisitsresults, aes(wegihted.nestedness,interaction_sampling_percent, col=treatment)) +
  geom_point() + xlab("Nestedness")+ ylab("Interaction Sampling Completeness") + 
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) + guides(fill = "none")

ggplot(data=allvisitsresults, aes(insectdiv,interaction_sampling_percent, col=treatment)) +
  geom_point() + xlab("Visitation rate per hectare")+ ylab("Interaction Sampling Completeness") + 
  theme(axis.text=element_text(size=11), axis.title=element_text(size=11)) +
  scale_fill_brewer(palette = 4) + guides(fill = "none")

View(allvisitsresults)
#. Testing for differences in vegetation age and elevation amongst veg types ----
# This will determine whether we need to include veg type as a random factor in our glmm analysis
boxplot(elevation~veg.type, data=allvisitsresults)
anova(lm(elevation~veg.type, data=allvisitsresults))
summary(lm(elevation~veg.type, data=allvisitsresults))

boxplot(veg.age~veg.type, data=allvisitsresults)
anova(lm(veg.age~veg.type,  data=allvisitsresults))
summary(lm(veg.age~veg.type,  data=allvisitsresults))

boxplot(insectdiv~veg.type, data=allvisitsresults)
boxplot(insectdiv~treatment, data=allvisitsresults)
anova(lm(insectdiv~treatment,  data=allvisitsresults))
summary(lm(insectdiv~treatment,  data=allvisitsresults))

#. VISITATION RATE  PER FLOWER(total visits per flower for each site) ----
hist(allvisitsresults$visitrate) # poisson distribution
mean(allvisitsresults$visitrate)
shapiro.test(visitrate) #Non-normal

#. VISITATION RATE PER HECTARE (total visits per flower for each hectare site, visits/number of flowers per hectare) ----
hist(allvisitsresults$visitratehectare) # poisson distribution
mean(allvisitsresults$visitratehectare)
shapiro.test(allvisitsresults$visitratehectare) #Significant = Non-normal data distribution
ggplot(data=Invadedsiteresults, aes(flowerno,visitrate)) + geom_point()
ggplot(data=allvisitsresults, aes(flowersperhectare,visitratehectare)) +
  geom_point(aes(color=treatment)) + 
  xlab("Site flower freq.")+ 
  ylab("Insect visitation per flower") + 
  facet_grid(rows=vars(treatment),scale = "free") + 
  scale_fill_brewer(palette = "Paired") + geom_smooth(method='lm', se=TRUE, colour='grey') # Different behaviour at invaded?

# Running glm(m)s on visit.freq, including flowerno. ----
par(mfrow=c(1,1))
boxplot(visitation.freq~treatment, data=allvisitsresults)
moNot d1<-glm.nb(visitation.freq~treatment*veg.type*flowerno, data=allvisitsresults)
mod2<-glm(visitation.freq~treatment*veg.type*flowerno, data=allvisitsresults, family=gaussian)
mod3<-glm(visitation.freq~treatment*veg.type*flowerno, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(visitation.freq~treatment*veg.type*flowerno, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(visitation.freq~treatment*veg.type*flowerno, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(visitation.freq~treatment + (1|veg.type) + (1|flowerno), data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best (glmm)
summary(mod6) # **Results used in final table**
mod6_sim <- simulateResiduals(mod6, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod6_sim)#No Significant outlier deviation - good

# Post-hocs
anova(lm(mod6)) # For overall results
summary(glht(mod6, linfct = mcp(treatment = "Tukey"))) # Between Treatments - invaded is different from P & C.
summary(glht(mod6, linfct = mcp(veg.type = "Tukey"))) # Between veg types
summary(glht(mod6, linfct = mcp(flowerno = "Tukey"))) # Between veg types
mod5.1 <- glm(visitrate ~ interaction - 1, data = allvisitsresults, family=gaussian, link='log')
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type



#5. Running glm(m)s on visitrate ---- Very overdispersed, hard to get the right model
#### 5a visualising data: ----
histogram(allvisitsresults$visitrate) # NB distribution?
boxplot(visitrate~treatment, data=allvisitsresults) # invaded sites have a higher visitrate.
boxplot(visitrate~veg.type, data=allvisitsresults) # no apparent diff between veg.types
# stat_halfeye good plot type for looking at distributions:
ggplot(allvisitsresults, aes(x = treatment, y = visitrate, colour = treatment))+
  ggdist::stat_halfeye(point_interval = mean_qi) 
# Plotting visitrate ~ treatment (optional facets of veg types)
ggplot(data=allvisitsresults, aes(treatment,visitrate, fill = treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Visitation rate (visits.flower.site)")+
  scale_fill_brewer(palette = "Paired",labels=c("Distant","Flower-removed","Invaded")) 
facet_grid(rows=vars(veg.type),scale='free')
# Plotting visitrate ~ veg type
ggplot(data=allvisitsresults, aes(veg.type,visitrate, fill = veg.type))+
  geom_boxplot()+xlab("Treatment")+ylab("Visitation rate (visits per flower) per site")+
  scale_fill_brewer(palette = "Paired",labels=c("AS","EF","OS"))
#### 5b running the models: ----
mod1<-glm.nb(visitrate~treatment+veg.type+treatment:veg.type, data=allvisitsresults) # negative binomial family
mod2a<-glm(visitrate~treatment+veg.type, data=allvisitsresults, family=gaussian) # Gaussian, without interaction
mod2<-glm(visitrate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(visitrate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(visitrate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(visitrate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(visitrate~treatment + (1|veg.type), data=allvisitsresults)
mod7<-glmmTMB(visitrate~treatment + (1|veg.type), family=negative.binomial, data=allvisitsresults) # not quite right
mod8<-glm(visitrate~treatment+veg.type, data=allvisitsresults, family=gaussian)
AIC_mods <- AIC(mod1, mod2, mod2a, mod3, mod4, mod5, mod6, mod7, mod8)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best
summary(mod6)
mod6_sim <- simulateResiduals(mod6, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod6_sim)#Significant outlier deviation...but least bad of all models
# POST-HOC TESTS - can't do on GLMM...
anova(lm(mod6))
# Below don't work for glmm...
summary(glht(mod6, linfct = mcp(treatment = "Tukey"), data=allvisitsresults)) # Between Treatments
summary(glht(mod6, linfct = mcp(veg.type = "Tukey"), data=allvisitsresults)) # Between veg types
mod5.1 <- glm(visitrate ~ interaction - 1, data = allvisitsresults, family=poisson)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type


#6. Running glm(m)s on insect species richness - accounting for random effects: veg type----
#### 6a visualising data: ----
boxplot(insectdiv~treatment, data=allvisitsresults)
boxplot(insectdiv~veg.type, data=allvisitsresults) # veg type affects insect diversity - greater at AS sites
hist(allvisitsresults$insectdiv)
shapiro.test(allvisitsresults$insectdiv) # Normally distributed
# stat_halfeye good plot type for looking at distributions between groups:
ggplot(allvisitsresults, aes(x = treatment, y = insectdiv, colour = treatment))+
  ggdist::stat_halfeye(point_interval = mean_qi) 

#### 6b running the models: ----
mod1<-glm.nb(insectdiv~treatment+veg.type+treatment:veg.type, data=allvisitsresults)
mod2<-glm(insectdiv~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian) # most likely
mod3<-glm(insectdiv~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(insectdiv~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(insectdiv~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glm(insectdiv~treatment*veg.type, data=allvisitsresults, family=poisson)
mod7<-glmmTMB(insectdiv~treatment + (1|veg.type), data=allvisitsresults)
mod8<-glm(insectdiv~treatment+veg.type, data=allvisitsresults)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)
AIC_mods[order(AIC_mods$AIC),] # Mod8 best, mod2 most suitable... but mod 8 best fit
summary(mod8)
summary(residuals(mod8))
mod8_sim <- simulateResiduals(mod8, n = 1000) #Plotting residuals to look at goodness of fit
plot(mod8_sim)#No Significant outliers - good 

# POST-HOC TESTS
anova(lm(mod8))
summary(glht(mod8, linfct = mcp(treatment = "Tukey"))) # Between Treatments
summary(glht(mod8, linfct = mcp(veg.type = "Tukey"))) # Between veg types
mod5.1 <- glm(obsinsectdiv ~ interaction - 1, data =allvisitsresults, family=poisson)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type

# Plotting
ggplot(data=allvisitsresults, aes(treatment,insectdiv, fill=treatment)) +
  geom_boxplot() +
  xlab("Treatment") + ylab("Insect species richness") +
  scale_fill_brewer(palette="Paired") + geom_jitter(width=0.2)
facet_grid(rows=vars(veg.type),scale='free')
ggplot(data=allvisitsresults, aes(veg.type,insectdiv, fill=veg.type)) +
  geom_boxplot() +
  xlab("Vegetation type") + ylab("Insect species richness") +
  scale_fill_brewer(palette=1) + geom_jitter(width = 0.2)

# More models on insect spp. richness...
boxplot(insectdiv~veg.type, data=allvisitsresults)
mod1<-glm.nb(insectdiv~treatment*veg.type, data=allvisitsresults)
mod2<-glm(insectdiv~treatment*veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(insectdiv~treatment*veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(insectdiv~treatment*veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(insectdiv~veg.type+treatment, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(insectdiv~treatment + (1|veg.type), data=allvisitsresults)
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
summary(insectdiv[veg.type=='AS'], data=allvisitsresults)
summary(insectdiv[veg.type=='EF'], data=allvisitsresults)
summary(insectdiv[veg.type=='OS'], data=allvisitsresults)
summary(glm(insectdiv~veg.type, data=allvisitsresults))



#7. Coleoptera visitation rate----
#### 7a Visualising data -----
hist(allvisitsresults$col.rate) # poisson / nb distribution
boxplot(col.rate~treatment, data=allvisitsresults) # higher col.rate at invaded sites
boxplot(col.rate~veg.type, data=allvisitsresults) # no diff between veg types
shapiro.test(allvisitsresults$insectdiv) # Normally distributed
# stat_halfeye good plot type for looking at distributions between groups:
ggplot(allvisitsresults, aes(x = treatment, y = col.rate, colour = treatment))+
  ggdist::stat_halfeye(point_interval = mean_qi) 
#### 7b running the models: ----
mod1<-glm.nb(col.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults)
mod1a<-glm.nb(col.rate~treatment+veg.type, data=allvisitsresults)
mod2<-glm(col.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(col.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(col.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(col.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=poisson)
mod5a<-glm(col.rate~treatment+veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(col.rate~treatment + (1|veg.type), data=allvisitsresults)
mod7<-glm(col.rate~treatment+veg.type, data=allvisitsresults, family=gaussian)
AIC_mods <- AIC(mod1, mod1a, mod2, mod3, mod4, mod5, mod5a, mod6, mod7)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best
summary(mod6)
summary(residuals(mod6)) # residuals under 2, good
mod6_sim <- simulateResiduals(mod6)#Plotting residuals to look at goodness of fit
plot(mod6_sim)#Significant outlier deviation...but best of the models

# POST-HOC TESTS # can't do with glmm
anova(lm(mod6))
summary(glht(mod6, linfct = mcp(treatment = "Tukey", interaction_average=TRUE))) # Between Treatments
summary(glht(mod6, linfct = mcp(veg.type = "Tukey"), interaction_average=TRUE)) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(treatment, veg.type))
mod5.1 <- glm(col.rate ~ interaction - 1, data = allvisitsresults, family=poisson)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"),interaction_average=FALSE)) # Between Treatment:veg.type



##8. Coleoptera species richness ----
#### 8a visualising data: ----
boxplot(coleoptera.rich~treatment, outline=FALSE, data=allvisitsresults)
boxplot(coleoptera.rich~veg.type, outline=FALSE, data=allvisitsresults) # col.richness higher at AS sites
hist(allvisitsresults$coleoptera.rich) # normally distributed
# stat_halfeye good plot type for looking at distributions between groups:
ggplot(allvisitsresults, aes(x = treatment, y = coleoptera.rich, colour = treatment))+
  ggdist::stat_halfeye(point_interval = mean_qi) # variance looks equal across groups
#### 8b running models: ----
mod1<-glm.nb(coleoptera.rich~treatment+veg.type+treatment:veg.type, data=allvisitsresults)
mod2<-glm(coleoptera.rich~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(coleoptera.rich~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(coleoptera.rich~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(coleoptera.rich~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(coleoptera.rich~treatment + (1|veg.type), data=allvisitsresults)
mod7<-glm(coleoptera.rich~treatment+veg.type, data=allvisitsresults, family=gaussian)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
AIC_mods[order(AIC_mods$AIC),] # Mod7 - makes sense, gaussian
summary(mod7)
summary(residuals(mod7))
mod6_sim <- simulateResiduals(mod7, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod6_sim)#Some problems...
?glmmTMB
# POST-HOC TESTS
anova(lm(mod7))
summary(glht(mod1, linfct = mcp(Treatment = "Tukey"))) # Between Treatments
summary(glht(mod1, linfct = mcp(veg.type = "Tukey"))) # Between veg types
allvisitsresults$interaction <- with(allvisitsresults, interaction(Treatment, veg.type))
mod5.1 <- glm.nb(coleoptera.rich ~ interaction - 1, data = allvisitsresults)
summary(glht(mod5.1, linfct = mcp(interaction = "Tukey"))) # Between Treatment:veg.type


## Hymenoptera visitation frequency ----
boxplot(hym.rate~treatment, outline=FALSE, data=allvisitsresults)
boxplot(hym.rate~veg.type, outline=FALSE, data=allvisitsresults)
hist(hym.rate, data=allvisitsresults)
mod1<-glm.nb(hym.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults)
mod2<-glm(hym.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(hym.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(hym.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(hym.rate~treatment*veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(hym.rate~treatment + (1|veg.type), data=allvisitsresults)
mod7<-glm(hym.rate~treatment+veg.type, data=allvisitsresults, family=gaussian)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best
summary(mod6) # No sig results?
plot(glm(hym.rate~Treatment*veg.type, data=allvisitsresults))
mod6_sim <- simulateResiduals(mod6, n = 1000)
plot(mod6_sim)#No problems!...
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


## Apis mellifera visitation frequency ----
boxplot(am.rate~treatment, outline=FALSE, data=allvisitsresults)
boxplot(am.rate~veg.type, outline=FALSE, data=allvisitsresults)
hist(am.rate, data=allvisitsresults)
mod1<-glm.nb(am.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults)
mod2<-glm(am.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(am.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(am.rate~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(am.rate~treatment*veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(am.rate~treatment + (1|veg.type), data=allvisitsresults)
mod7<-glm(am.rate~treatment+veg.type, data=allvisitsresults, family=gaussian)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best
summary(mod6) # No sig results?
plot(glm(am.rate~treatment*veg.type, data=allvisitsresults))
mod6_sim <- simulateResiduals(mod6, n = 1000)
plot(mod6_sim)#No problems!...
# POST-HOC TESTS
anova(lm(mod7))
aov(summary(mod7, data=allvisitsresults)) # For overall results
summary(glht(mod6, linfct = mcp(treatment = "Tukey"))) # Between Treatments
summary(glht(mod6, linfct = mcp(veg.type = "Tukey"))) # Between veg types


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
sd(subset(allvisitsresults, Treatment == 'Distant')$links.per.species) # to get standard dev
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

sd(subset(focalsorders, type =='Distant')$dip.rate)


## Links per species - Plant Vulnerability ----

ggplot(data=allvisitsresults, aes(Treatment,lps.plants, fill = Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Plant generality")+
  scale_fill_brewer(palette = "Paired",labels=c("Cleared","Invaded","Distant"))
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

# WEIGHTED NESTEDNESS ----
View(allvisitsresults)
hist(weighted.nestedness, data=allvisitsresults) # Skewed distribution
plot(weighted.nestedness~treatment, data=allvisitsresults)
boxplot(weighted.nestedness~veg.type, data=allvisitsresults)
mod1<-glm.nb(weighted.nestedness~treatment+veg.type+treatment:veg.type, data=allvisitsresults)
mod2<-glm(weighted.nestedness~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian)
mod3<-glm(weighted.nestedness~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='log'))
mod4<-glm(weighted.nestedness~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=gaussian(link='inverse'))
mod5<-glm(weighted.nestedness~treatment+veg.type+treatment:veg.type, data=allvisitsresults, family=poisson)
mod6<-glmmTMB(interaction.evenness~treatment + (1|veg.type), data=allvisitsresults)
mod7<-glm(interaction.evenness~treatment+veg.type, data=allvisitsresults, family=gaussian)
AIC_mods <- AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
AIC_mods[order(AIC_mods$AIC),] # Mod6 best
summary(mod7) 
mod1_sim <- simulateResiduals(mod6, n = 1000)#Plotting residuals to look at goodness of fit
plot(mod1_sim)# No problems.
# POST-HOC TESTS
anova(lm(mod7)) # For overall results
summary(glht(mod6, linfct = mcp(treatment = "Tukey"))) # Between Treatments
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

