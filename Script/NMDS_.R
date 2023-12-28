#Load Packages
library(readxl)
library(grDevices)
library(dplyr)
library(fossil)
library(bipartite)
library(vegan)
library(vegan3d)
library(scatterplot3d)
library(reshape2)
library(devtools)
library(nat)
library(rgl)

#Upload dataset ----
allvisits <- read_excel("~/path/allvisits.xlsx")
names(allvisits)

##Apis mellifera freq. per site ----
# Filter rows with 'Apis mellifera'
apis_data <- allvisits[allvisits$final_id == 'Apis mellifera', ]
# Use aggregate to sum the frequencies for each site 
result <- aggregate(freq ~ site, data = apis_data, sum)
# Print the result
print(result)

## NMDS of insect visitors ----

#1. Prepping data - making matrices ----
#Insect Matrix
names(allvisits)
speciesdcast <-dcast(allvisits, site~final_id, fun.aggregate = sum, value.var = 'freq') # watch what is being used to fill matrix - freq or ratio?

speciesdcast2 <-dcast(allvisits,treatment~final_id,fun.aggregate = sum) # By type not site
write.xlsx(speciesdcast, file="insectvisitswide.xlsx",)
speciesdcast <- read_excel("~/path/insectvisitswide.xlsx")
View(speciesdcast2)

## Plant matrix
plantdcast <-dcast(allvisits,site~plant,fun.aggregate = sum) # watch what is being used to fill matrix - freq or ratio?
plantdcast2 <-dcast(allvisits,treatment~plant,fun.aggregate = sum) # By type not site

## 2. Insect NMDS test ----
View(speciesdcast)
speciesMDS <- metaMDS(speciesdcast[2:511], k=2, trymax= 100, distance = "bray") # metaMDS test
metaMDS(speciesdcast)

# Plotting species assemblages for 21 sites, 3 treatments - by species
par(mfrow=c(2,1))
ordiplot3d(speciesMDS, type='p', display=c('sites'),scaling = "symmetric") # Good plotting!

sp <- ordiplot3d(speciesMDS, display=c('sites'),angle=35, col="steelblue",scaling = "symmetric",type="p",main="Insect assemblage across 21 sites")
points(sp, "points", pch=16, col="steelblue", cex = 1)
text(sp, "points", col="darkred", pos=3, cex=0.8)
plot(speciesMDS,type='t',display=c('site'),title(main='metaMDS(speciesMDS)')) # Good plotting!
??ordiplot3d()

speciestreatment <- c(rep("Cleared", 7), rep("Invaded", 7), rep("Pristine", 7)) # defining treatments... only in right order because of ordering of excel file
speciessitescores <- data.frame(speciestreatment, scores(speciesMDS, display = "sites")) # Extracting scores per site
View(speciessitescores) 
ggplot(speciessitescores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(colour = speciestreatment, shape = speciestreatment), alpha = .9) + 
  theme_bw() + ggtitle("site NMDS scores for Insect Assemblage") + 
  scale_colour_manual(values = c("#fc8d59", "#91bfdb","blue"))

speciesscores <- data.frame(scores(speciesMDS, display = "species"))
write.xlsx(speciesscores, file="allspeciesscores.xlsx")
orderstreatment2 <-c(rep("Arachnidae", 7), rep("Aves", 7), rep("Blattoidea", 7),rep("Coleoptera", 7),rep("Collembola", 7),rep("Dermaptera", 7),rep("Diptera", 7),rep("Hemiptera", 7),rep("Hymenoptera", 7),rep("Lepidoptera", 7),rep("Orthoptera", 7),rep("Thysanoptera", 7))
View(speciesscores)
ggplot(speciesscores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(pch = 16, cex = 3,colour = ordertreatment2, shape = ordertreatment2) +
  theme_bw() + ggtitle("metaMDS scores for plant visitor species")

# Plotting species assemblages for 21 sites, 3 treatments - BY ORDER
#ordersdcast <-dcast(allvisits,site~order,fun.aggregate = sum) # watch what is being used to fill matrix - freq or ratio?
#write.xlsx(ordersdcast, file="ordervisitswide.xlsx",)
ordersdcast <- read_excel("~/path/ordervisitswide.xlsx")
View(ordersdcast)
ordersMDS <- metaMDS(ordersdcast[4:16], k=3, trymax= 100, distance = "bray")
ord <- ordiplot3d(ordersMDS, display=c('sites'),angle=25, col="black",scaling = "symmetric",type="p",main="Insect order assemblage across 21 sites")
ord <- ordiplot3d(ordersMDS, display=c('sites'),angle=80, col="black",scaling = "symmetric",type="p",main="Insect order assemblage across 21 sites")
points(ord, "points", pch=16, col="steelblue", cex = 1)
text(ord, "points", col="darkred", pos=3, cex=0.6)

ordertreatment <- c(rep("Cleared", 7), rep("Invaded", 7), rep("Pristine", 7)) # defining treatments... only in right order because of ordering of excel file
ordersitescores <- data.frame(ordertreatment, scores(ordersMDS, display = "sites")) # Extracting scores per site
View(ordersitescores) 
ggplot(ordersitescores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(colour = ordertreatment, shape = ordertreatment), alpha = .9) + 
  theme_bw() + ggtitle("site metaMDS scores (insect orders)") + 
  scale_colour_manual(values = c("#fc8d59", "#91bfdb","blue"))

orderscores <- data.frame(scores(ordersMDS, display = "species"))
View(orderscores)
ggplot(orderscores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(pch = 16, cex = 3, colour = "steelblue") + 
  geom_label_repel(aes(label = substr(rownames(orderscores), 1, 4)), size = 3) +
  theme_bw() + ggtitle("metaMDS scores for plant visitor orders")

##ADONIS##
# default test by terms - no sig dif between treatments or veg types, yes to diversity - makes sense?
View(ordersdcast)
View(insectvisitswide)

adonis2(insectvisitswide[4:662] ~ treatment+vegtype, data = insectvisitswide)
adonis2(ordersdcast[4:16] ~ treatment*vegtype, data = ordersdcast) # Significant!!
?adonis2
anova(lm(Coleoptera~treatment+veg.type, data=ordersdcast))
ggplot(data=ordersdcast, aes(treatment,Coleoptera, fill = treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Coleoptera freq.")+
  scale_fill_brewer(palette = "Paired",labels=c("Cleared","Pristine","Invaded"))

boxplot(Diptera~treatment, data=ordersdcast)


allvisits_orders <- read_excel("~/path/allvisits.orders.xlsx")
allvisits_orders %>%
  gather("Type", "Value",-treatment) %>%
  ggplot(aes(treatment, Value, fill = Type,)) +
  geom_bar(position =position_fill(),stat = 'identity') + 
  theme_classic() +
  ylab("% of total insect assemblage")

##ADONIS EXAMPLE
data(dune)
data(dune.env)
## default test by terms
adonis2(dune ~ Management*A1, data = dune.env)
## overall tests
adonis2(dune ~ Management*A1, data = dune.env, by = NULL)

### Example of use with strata, for nested (e.g., block) designs.
dat <- expand.grid(rep=gl(2,1), NO3=factor(c(0,10)),field=gl(3,1) )
dat
Agropyron <- with(dat, as.numeric(field) + as.numeric(NO3)+2) +rnorm(12)/2
Schizachyrium <- with(dat, as.numeric(field) - as.numeric(NO3)+2) +rnorm(12)/2
total <- Agropyron + Schizachyrium
dotplot(total ~ NO3, dat, jitter.x=TRUE, groups=field,
        type=c('p','a'), xlab="NO3", auto.key=list(columns=3, lines=TRUE) )

Y <- data.frame(Agropyron, Schizachyrium)
mod <- metaMDS(Y, trace = FALSE)
plot(mod)
### Ellipsoid hulls show treatment
with(dat, ordiellipse(mod, field, kind = "ehull", label = TRUE))
### Spider shows fields
with(dat, ordispider(mod, field, lty=3, col="red"))

### Incorrect (no strata)
perm <- how(nperm = 199)
adonis2 (Y ~ NO3, data = dat, permutations = perm)

## Correct with strata
setBlocks(perm) <- with(dat, field)
adonis2(Y ~ NO3, data = dat, permutations = perm)


