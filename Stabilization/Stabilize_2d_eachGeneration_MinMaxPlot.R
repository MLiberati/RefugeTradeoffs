library(dplyr)
library(doBy)
library(ggplot2)
library(cowplot)

remove(list=ls())

targetGen <- 770

# CFA <- "Fort"
# CFA <- "Mascoma"
# CFA <- "Mill"
# CFA <- "Muddy"
# CFA <- "Salmon"
CFA <- "Sprague"

date <- "2017.10.18"
gens <- c(seq(from=0,to=900,by=10))
# gens <- c(seq(from=0,to=1000,by=1))


setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))


for(i in 1:length(gens)){
  GENS <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  GENS$Generation <- rep(gens[i])
  assign(paste("GENS",gens[i],sep=''), GENS)
}

df <- rbind(GENS0,GENS10,GENS20,GENS30,GENS40,GENS50,
            GENS60,GENS70,GENS80,GENS90,GENS100,
            GENS110,GENS120,GENS130,GENS140,GENS150,
            GENS160,GENS170,GENS180,GENS190,GENS200,
            GENS210,GENS220,GENS230,GENS240,GENS250,
            GENS260,GENS270,GENS280,GENS290,GENS300,
            GENS310,GENS320,GENS330,GENS340,GENS350,
            GENS360,GENS370,GENS380,GENS390,GENS400,
            GENS410,GENS420,GENS430,GENS440,GENS450,
            GENS460,GENS470,GENS480,GENS490,GENS500,
            GENS510,GENS520,GENS530,GENS540,GENS550,
            GENS560,GENS570,GENS580,GENS590,GENS600,
            GENS610,GENS620,GENS630,GENS640,GENS650,
            GENS660,GENS670,GENS680,GENS690,GENS700,
            GENS710,GENS720,GENS730,GENS740,GENS750,
            GENS760,GENS770,GENS780,GENS790,GENS800,
            GENS810,GENS820,GENS830,GENS840,GENS850,
            GENS860,GENS870,GENS880,GENS890,GENS900)
            # ,
            # GENS910,GENS920,GENS930,GENS940,GENS950,
            # GENS960,GENS970,GENS980,GENS990,GENS1000)

#######################################################################################################
#######################################################################################################

# summary statistics by generation
dfSummary <- summaryBy(Area + Cost + Forest + Devel + Charact + Connect ~ Generation, data = df, 
                       FUN = c(function(x) { c(min = min(x), max = max(x), r = max(x)-min(x)) } ))
solnSet <- summaryBy(Area ~ Generation, data = df, 
                     FUN = c(function(x) {l = length(x)} ))
names(solnSet)[names(solnSet)=="Area.FUN1"] <- "Soln.Set"
sims <- cbind(solnSet,dfSummary[,-1])
sims


#### SIX OBJECTIVE PLOTS W/ A LINE FOR THE OBJECTIVE'S SPREAD (max-min)
#######################################################################################################
#######################################################################################################

# objectives <- c("Area","Forest","Cost","Devel","Charact","Connect")

pArea <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Area.max)) +
  geom_point(aes(y=sims$Area.max), size=3, shape=16, color="gray20") +  
  geom_path() +
  geom_point(aes(y=sims$Area.min), size=3, shape=17, color="gray20") +  
  geom_path(aes(y=sims$Area.min)) + 
  scale_x_discrete("Generations", limits=sims$Generation, labels=as.character(sims$Generation)) +
  ylab("Area (ha)") +
  theme(text=element_text(family="serif", size=20)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_vline(aes(xintercept=targetGen), linetype="longdash", lwd=1)

pCost <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Cost.max)) +
  geom_point(size=3, shape=16, color="gray20") +  
  geom_path() +
  geom_point(aes(y=sims$Cost.min), size=3, shape=17, color="gray20") +  
  geom_path(aes(y=sims$Cost.min)) + 
  scale_x_discrete("Generations", limits=sims$Generation, labels=as.character(sims$Generation)) +
  ylab("Cost ($US million)") +
  theme(text=element_text(family="serif", size=20)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_vline(aes(xintercept=targetGen), linetype="longdash", lwd=1)

pForest <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Forest.max)) +
  geom_point(size=3, shape=16, color="gray20") +  
  geom_path() +
  geom_point(aes(y=sims$Forest.min), size=3, shape=17, color="gray20") +  
  geom_path(aes(y=sims$Forest.min)) + 
  scale_x_discrete("Generations", limits=sims$Generation, labels=as.character(sims$Generation)) +
  ylab("Non-forest (ha)") +
  theme(text=element_text(family="serif", size=20)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_vline(aes(xintercept=targetGen), linetype="longdash", lwd=1)

pDevel <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Devel.max)) +
  geom_point(size=3, shape=16, color="gray20") +  
  geom_path() +
  geom_point(aes(y=sims$Devel.min), size=3, shape=17, color="gray20") +  
  geom_path(aes(y=sims$Devel.min)) + 
  scale_x_discrete("Generations", limits=sims$Generation, labels=as.character(sims$Generation)) +
  ylab("Development risk deviation") +
  theme(text=element_text(family="serif", size=20)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_vline(aes(xintercept=targetGen), linetype="longdash", lwd=1)

pCharact <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Charact.max)) +
  geom_point(size=3, shape=16, color="gray20") +  
  geom_path() +
  geom_point(aes(y=sims$Charact.min), size=3, shape=17, color="gray20") +  
  geom_path(aes(y=sims$Charact.min)) + 
  scale_x_discrete("Generations", limits=sims$Generation, labels=as.character(sims$Generation)) +
  ylab("Town character conflict") +
  theme(text=element_text(family="serif", size=20)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_vline(aes(xintercept=targetGen), linetype="longdash", lwd=1)

pConnect <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Connect.max)) +
  geom_point(size=3, shape=16, color="gray20") +  
  geom_path() +
  geom_point(aes(y=sims$Connect.min), size=3, shape=17, color="gray20") +  
  geom_path(aes(y=sims$Connect.min)) + 
  scale_x_discrete("Generations", limits=sims$Generation, labels=as.character(sims$Generation)) +
  ylab("Fragmentation distance (km)") +
  theme(text=element_text(family="serif", size=20)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_vline(aes(xintercept=targetGen), linetype="longdash", lwd=1)

# folder with figures
setwd("P:\\GeneticAlgorithm\\Figures")

# make jpg
grid <- plot_grid(pArea,pCost,pCharact,pDevel,pConnect,pForest,
                  align="hv", nrow=1, ncol=6)
save_plot("test.jpg", grid, ncol=6, nrow=1)

# reset folder to data location
setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))

