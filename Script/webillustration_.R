install.packages("bipartite")
install.packages("devtools")
install.packages("gdtools")
install.packages("bipartiteD3")
install.packages("grDevices")
library(bipartite)
library(bipartiteD3)
library(devtools)
library(gdtools)
library(readxl)
library(grDevices)

## Importing and Setting up matrix of plant-insect visitation data ('allvisitsweb') ----
# Enter your own file path
allvisits <- read_excel("~/path/allvisits.xlsx")
View(al1visits) 
names(allvisits)
insect<-allvisits$insect
allvisits <-as.data.frame(allvisits)
allvisitsweb <- frame2webs(allvisits,varnames=c("Plant","Insect","Site","Freq"),type.out = "list",emptylist=FALSE)
View(allvisitsweb)
names(allvisitsweb)
class(allvisitsweb)
plotweb(as.data.frame(allvisitsweb[[3]]), method = "normal", empty = TRUE, labsize = 0.4, ybig =1, y.width.low = 0.1, y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", col.interaction="grey10", col.high = "grey10", col.low="grey10", bor.col.interaction ="NA", bor.col.high="black", bor.col.low="black", high.lablength = NULL, low.lablength = NULL, sequence=NULL, low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=-0.01, adj.low=1, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE,high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")
?plotweb
?frame2webs

## CODE FOR ILLUSTRATIONS ---- 
#Subsetting web plots by site, adding insect order colours

# Figure S2 (a) - Site name: 'GROOTBOS 2'
gb2<-allvisits[which(allvisits$Site == 'Grootbos 2'),c("Site","Insect","Order","Plant","Freq")]
gb2web <- frame2webs(gb2,varnames=c("Plant","Insect","Order","Freq"),type.out = "list",emptylist=FALSE)
plotweb(as.data.frame(gb2web), method = "normal", empty = TRUE, labsize = 0.4, ybig =1, y.width.low = 0.1, y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", col.interaction="grey10", col.high = "grey10", col.low="grey10", bor.col.interaction ="NA", bor.col.high="NA", bor.col.low="black", high.lablength = NULL, low.lablength = NULL, sequence=NULL, low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=-0.01, adj.low=1, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE,high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")
# Count the number of species at the two levels based on your interaction matrix (called obs.web here)
n.consumers <- 50
n.resources <- 23
# Create a matrix of the same size as the interaction one and fill with the colours you want.
arachnidae<-rep(c("darkred"),length=3)
coleoptera<-rep(c("darkred"), length=18) # Edit lengths to match order of species in the data, total=n.consumers
diptera<-rep(c("orange"), length=9) 
hemiptera<-rep(c("yellow"), length=1) 
hymenoptera<-rep(c("darkgreen"), length=15) 
lepidoptera<-rep(c("skyblue"), length=2)
thysanoptera<-rep(c("blue3"), length=1)
ordercolour <-c(arachnidae,coleoptera,diptera,hemiptera,hymenoptera,lepidoptera,thysanoptera)
select.vector <- matrix(rep(ordercolour, length = n.consumers), ncol = n.consumers, nrow = n.resources, byrow=TRUE)
# Convert matrix to a vector
select.vector <- as.vector(t(select.vector))
# Create the plot
plotweb(as.data.frame(gb2web), method = "normal", empty = TRUE, labsize = 0.6, ybig =1, y.width.low = 0.1, y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", col.interaction = adjustcolor('grey80', alpha.f = 0.9), col.high =select.vector, col.low="grey10", bor.col.interaction ="NA", bor.col.high="NA", bor.col.low="black", high.lablength = NULL, low.lablength = NULL, sequence=NULL, low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=-0.01, adj.low=1, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE,high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")

#Figure S2 (b) - Site name: 'GROOTBOS 2'
hf2<-allvisits[which(allvisits$Site == 'Helderfontein 2'),c("Site","Insect","Order","Plant","Freq")]
hf2web <- frame2webs(hf2,varnames=c("Plant","Insect","Order","Freq"),type.out = "list",emptylist=FALSE)
plotweb(as.data.frame(hf2web), method = "normal", empty = TRUE, labsize = 0.4, ybig =1, y.width.low = 0.1, y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", col.interaction="grey10", col.high = "grey10", col.low="grey10", bor.col.interaction ="NA", bor.col.high="NA", bor.col.low="black", high.lablength = NULL, low.lablength = NULL, sequence=NULL, low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=-0.01, adj.low=1, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE,high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")
# Count the number of species at the two levels based on your interaction matrix (called obs.web here)
n.consumers <- 56
n.resources <- 15
# Create a matrix of the same size as the interaction one and fill with the colours you want.
arachnidae<-rep(c("hotpink"),length=5)
coleoptera<-rep(c("darkred"), length=19) # Edit lengths to match order of species in the data, total=n.consumers
diptera<-rep(c("orange"), length=9) 
hemiptera<-rep(c("yellow"), length=3) 
hymenoptera<-rep(c("darkgreen"), length=14) 
lepidoptera<-rep(c("skyblue"), length=2)
orthoptera<-rep(c("turquoise3"), length=1)
thysanoptera<-rep(c("blue3"), length=1)
ordercolour <-c(arachnidae,coleoptera,diptera,hemiptera,hymenoptera,lepidoptera,orthoptera,thysanoptera)
select.vector <- matrix(rep(ordercolour, length = n.consumers), ncol = n.consumers, nrow = n.resources, byrow=TRUE)
# Convert matrix to a vector
select.vector <- as.vector(t(select.vector))
# Create the plot
plotweb(as.data.frame(hf2web), method = "normal", empty = TRUE, labsize = 0.6, ybig =1, y.width.low = 0.1, y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", col.interaction = adjustcolor('grey80', alpha.f = 0.9), col.high =select.vector, col.low="grey10", bor.col.interaction ="NA", bor.col.high="NA", bor.col.low="black", high.lablength = NULL, low.lablength = NULL, sequence=NULL, low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=-0.01, adj.low=1, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE,high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")


#Figure S2 (c) - Site name: 'GROOTBOS 4'
gb4<-allvisits[which(allvisits$Site == 'Grootbos 4'),c("Site","Insect","Order","Plant","Freq")]
gb4web <- frame2webs(gb4,varnames=c("Plant","Insect","Order","Freq"),type.out = "list",emptylist=FALSE)
plotweb(as.data.frame(gb4web), method = "normal", empty = TRUE, labsize = 0.4, ybig =1, y.width.low = 0.1, y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", col.interaction="grey10", col.high = "grey10", col.low="grey10", bor.col.interaction ="NA", bor.col.high="NA", bor.col.low="black", high.lablength = NULL, low.lablength = NULL, sequence=NULL, low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=-0.01, adj.low=1, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE,high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")
# Count the number of species at the two levels based on your interaction matrix (called obs.web here)
n.consumers <- 67
n.resources <- 20
# Create a matrix of the same size as the interaction one and fill with the colours you want.
arachnidae<-rep(c("hotpink"),length=5)
coleoptera<-rep(c("darkred"), length=23) # Edit lengths to match order of species in the data, total=n.consumers
diptera<-rep(c("orange"), length=16) 
hemiptera<-rep(c("yellow"), length=5) 
hymenoptera<-rep(c("darkgreen"), length=16) 
lepidoptera<-rep(c("skyblue"), length=1)
thysanoptera<-rep(c("blue3"), length=1)
ordercolour <-c(arachnidae,coleoptera,diptera,hemiptera,hymenoptera,lepidoptera,thysanoptera)
select.vector <- matrix(rep(ordercolour, length = n.consumers), ncol = n.consumers, nrow = n.resources, byrow=TRUE)
# Convert matrix to a vector
select.vector <- as.vector(t(select.vector))
# Create the plot
plotweb(as.data.frame(gb4web), method = "normal", empty = TRUE, labsize = 0.6, ybig =1, y.width.low = 0.1, y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", col.interaction = adjustcolor('grey80', alpha.f = 0.9), col.high =select.vector, col.low="grey10", bor.col.interaction ="NA", bor.col.high="NA", bor.col.low="black", high.lablength = NULL, low.lablength = NULL, sequence=NULL, low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=-0.01, adj.low=1, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE,high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")

