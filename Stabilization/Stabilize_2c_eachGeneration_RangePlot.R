# Visualize changes for the objectives's range (min-max) across generations from 'NSGAII_forallCFAs' results
# Liberati et al. (2019)
# Title: "Addressing ecological, economic, and social tradeoffs of refuge expansion in a constrained landscape"
# Journal: Landscape Ecology
# DOI: 10.1007/s10980-019-00798-8

library(dplyr)
library(doBy)
library(ggplot2)
library(cowplot)

remove(list=ls())

targetGen <- 1000

# CFA <- "Fort"
CFA <- "Mascoma"
# CFA <- "Mill"
# CFA <- "Muddy"
# CFA <- "Salmon"
# CFA <- "Sprague"

date <- "2017.10.18"
gens <- c(seq(from=0,to=1000,by=10))


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
            GENS460, GENS470,GENS480,GENS490,GENS500,
            GENS510,GENS520,GENS530,GENS540,GENS550,
            GENS560,GENS570,GENS580,GENS590,GENS600,
            GENS610,GENS620,GENS630,GENS640,GENS650,
            GENS660,GENS670,GENS680,GENS690,GENS700,
            GENS710,GENS720,GENS730,GENS740,GENS750,
            GENS760,GENS770,GENS780,GENS790,GENS800,
            GENS810,GENS820,GENS830,GENS840,GENS850,
            GENS860,GENS870,GENS880,GENS890,GENS900,
            GENS910,GENS920,GENS930,GENS940,GENS950,
            GENS960,GENS970,GENS980,GENS990,GENS1000)

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

pArea <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Area.r, label=sims$Soln.Set)) +
  geom_point(aes(y=sims$Area.r), size=3, shape=15, color="gray20") +  
  geom_path() +
  # geom_text(aes(label=paste("n=",sims$Soln.Set)),vjust=-1,hjust=1) +
  scale_x_continuous("Generations", breaks=c(0, sims$Generation), 
                     labels=as.character(c(0,sims$Generation)), limits=c(-2,max(sims$Generation))) +
  scale_y_continuous(limits=c(min(sims$Area.r),max(sims$Area.r)+15)) +
  ylab("Area spread (max-min)") +
  theme(text=element_text(family="serif", size=20)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_vline(aes(xintercept=targetGen), linetype="longdash", lwd=1)

pCost <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Cost.r, label=sims$Soln.Set)) +
  geom_point(aes(y=sims$Cost.r), size=3, shape=15, color="gray20") +  
  geom_path() +
  # geom_text(aes(label=paste("n=",sims$Soln.Set)),vjust=-1,hjust=1) +
  scale_x_continuous("Generations", breaks=c(0, sims$Generation), 
                     labels=as.character(c(0,sims$Generation)), limits=c(-2,max(sims$Generation))) +
  scale_y_continuous(limits=c(min(sims$Cost.r),max(sims$Cost.r)+1)) +
  ylab("Cost spread (max-min)") +
  theme(text=element_text(family="serif", size=20)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_vline(aes(xintercept=targetGen), linetype="longdash", lwd=1) 

pForest <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Forest.r, label=sims$Soln.Set)) +
  geom_point(aes(y=sims$Forest.r), size=3, shape=15, color="gray20") +  
  geom_path() +
  # geom_text(aes(label=paste("n=",sims$Soln.Set)),vjust=-1,hjust=1) +
  scale_x_continuous("Generations", breaks=c(0, sims$Generation), 
                     labels=as.character(c(0,sims$Generation)), limits=c(-2,max(sims$Generation))) +
  scale_y_continuous(limits=c(min(sims$Forest.r),max(sims$Forest.r)+10)) +
  ylab("Non-forest spread (max-min)") +
  theme(text=element_text(family="serif", size=20)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_vline(aes(xintercept=targetGen), linetype="longdash", lwd=1) 

pDevel <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Devel.r, label=sims$Soln.Set)) +
  geom_point(aes(y=sims$Devel.r), size=3, shape=15, color="gray20") +  
  geom_path() +
  # geom_text(aes(label=paste("n=",sims$Soln.Set)),vjust=-1,hjust=1) +
  scale_x_continuous("Generations", breaks=c(0, sims$Generation), 
                     labels=as.character(c(0,sims$Generation)), limits=c(-2,max(sims$Generation))) +
  scale_y_continuous(limits=c(min(sims$Devel.r),max(sims$Devel.r)+0.002)) +
  ylab("Development risk deviation\nspread (max-min)") +
  theme(text=element_text(family="serif", size=20)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_vline(aes(xintercept=targetGen), linetype="longdash", lwd=1) 

pCharact <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Charact.r, label=sims$Soln.Set)) +
  geom_point(aes(y=sims$Charact.r), size=3, shape=15, color="gray20") +  
  geom_path() +
  # geom_text(aes(label=paste("n=",sims$Soln.Set)),vjust=-1,hjust=1) +
  scale_x_continuous("Generations", breaks=c(0, sims$Generation), 
                     labels=as.character(c(0,sims$Generation)), limits=c(-2,max(sims$Generation))) +
  scale_y_continuous(limits=c(min(sims$Charact.r),max(sims$Charact.r)+1)) +
  ylab("Town character conflict\nspread (max-min)") +
  theme(text=element_text(family="serif", size=20)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_vline(aes(xintercept=targetGen), linetype="longdash", lwd=1) 

pConnect <- ggplot(sims, aes_string(x=sims$Generation, y=sims$Connect.r, label=sims$Soln.Set)) +
  geom_point(aes(y=sims$Connect.r), size=3, shape=15, color="gray20") +  
  geom_path() +
  # geom_text(aes(label=paste("n=",sims$Soln.Set)),vjust=-1,hjust=1) +
  scale_x_continuous("Generations", breaks=c(0, sims$Generation), 
                     labels=as.character(c(0,sims$Generation)), limits=c(-2,max(sims$Generation))) +
  scale_y_continuous(limits=c(min(sims$Connect.r),max(sims$Connect.r)+0.1)) +
  ylab("Fragementation distance\nspread (max-min)") +
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
