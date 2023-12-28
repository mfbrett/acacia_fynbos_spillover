library(vegan)
library(readxl)
library(ggplot2)

## Import data

site_info <- read_excel("~/path/site_info_ms.xlsx", 
                        col_types = c("numeric", "text", "text", 
                                      "text", "numeric", "numeric", "numeric", 
                                      "text", "numeric", "numeric", "numeric"))
View(site_info)

## Perform ANOVA & post-hoc tukey test
summary(aov(isolation.metric~treatment, data = site_info))
TukeyHSD(aov(isolation.metric~treatment, data = site_info))

##Plot results
ggplot(data=site_info, aes(treatment,isolation.metric, fill = treatment)) +
  geom_boxplot()+xlab("")+ylab("Isolation Metric") + theme_classic()
