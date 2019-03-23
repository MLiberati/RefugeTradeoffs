# Summary information about algorithm results from 'NSGAII_forallCFAs'
# Liberati et al. (2019)
# Title: "Addressing ecological, economic, and social tradeoffs of refuge expansion in a constrained landscape"
# Journal: Landscape Ecology
# DOI: 10.1007/s10980-019-00798-8

library(dplyr)
library(doBy)
library(ggplot2)
library(cowplot)
library(tibble)
library(scales)

remove(list=ls())

# CFA <- "Fort"
# CFA <- "Mascoma"
# CFA <- "Mill"
CFA <- "Muddy"
# CFA <- "Salmon"
# CFA <- "Sprague"

date <- "2017.10.18"
gens <- c(seq(from=0,to=770,by=10))


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
dupsBetweenGroups <- function (df, idcol) {
  # df: the data frame
  # idcol: the column which identifies the group each row belongs to
  
  # Get the data columns to use for finding matches
  datacols <- setdiff(names(df), idcol)
  
  # Sort by idcol, then datacols. Save order so we can undo the sorting later.
  sortorder <- do.call(order, df)
  df <- df[sortorder,]
  
  # Find duplicates within each id group (first copy not marked)
  dupWithin <- duplicated(df)
  
  # With duplicates within each group filtered out, find duplicates between groups. 
  # Need to scan up and down with duplicated() because first copy is not marked.
  dupBetween = rep(NA, nrow(df))
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
  
  # ============= Replace NA's with previous non-NA value ==============
  # This is why we sorted earlier - it was necessary to do this part efficiently
  
  # Get indexes of non-NA's
  goodIdx <- !is.na(dupBetween)
  
  # These are the non-NA values from x only
  # Add a leading NA for later use when we index into this vector
  goodVals <- c(NA, dupBetween[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  # The original vector, now with gaps filled
  dupBetween <- goodVals[fillIdx]
  
  # Undo the original sort
  dupBetween[sortorder] <- dupBetween
  
  # Return the vector of which entries are duplicated across groups
  return(dupBetween)
}
#######################################################################################################
#######################################################################################################

# Find the rows which have duplicates in a different generations
dupRows <- dupsBetweenGroups(df, "Generation")
summary(dupRows)

# summary statistics by generation
dfSummary <- summaryBy(Area + Cost + Forest + Devel + Charact + Connect ~ Generation, data = df, 
          FUN = c(function(x) { c(min = min(x), max = max(x), r = max(x)-min(x)) }))
solnSet <- summaryBy(Area ~ Generation, data = df, 
                     FUN = c(function(x) {l = length(x)} ))
names(solnSet)[names(solnSet)=="Area.FUN1"] <- "Soln.Set"
sims <- cbind(solnSet,dfSummary[,-1])
sims

filename <- "P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\Generation_Summaries_placeholder.csv"
write.csv(sims, file=filename,row.names=FALSE)

# dfFlip <- as.data.frame(t(sims))

#######################################################################################################
#######################################################################################################

# Evalute improvements to objectives between each generation
variables <- c("Area.max","Cost.min","Forest.min", "Devel.min","Charact.min","Connect.min")

improve <- matrix(nrow=length(sims$Area.max),ncol=length(variables)*2+1)

# Area.max
for(i in 1:(length(sims$Area.max)-1)){
  difference <- sims$Area.max[i+1] - sims$Area.max[i]
  improve[i+1,2] <- signif(difference, digits=3)
}
standard <- rescale(improve[,2],to=c(0,1))
improve[,3] <- signif(standard, digits=3)

# Cost.min
for(i in 1:(length(sims$Cost.min)-1)){
  difference <- sims$Cost.min[i] - sims$Cost.min[i+1]
  improve[i+1,4] <- signif(difference, digits=3)
}
standard <- rescale(improve[,4],to=c(0,1))
improve[,5] <- signif(standard, digits=3)

# Forest.min
for(i in 1:(length(sims$Forest.min)-1)){
  difference <- sims$Forest.min[i] - sims$Forest.min[i+1]
  improve[i+1,6] <- signif(difference, digits=3)
}
standard <- rescale(improve[,6],to=c(0,1))
improve[,7] <- signif(standard, digits=3)

# Devel.min
for(i in 1:(length(sims$Devel.min)-1)){
  difference <- sims$Devel.min[i] - sims$Devel.min[i+1]
  improve[i+1,8] <- signif(difference, digits=3)
}
standard <- rescale(improve[,8],to=c(0,1))
improve[,9] <- signif(standard, digits=3)

# Charact.min
for(i in 1:(length(sims$Charact.min)-1)){
  difference <- sims$Charact.min[i] - sims$Charact.min[i+1]
  improve[i+1,10] <- signif(difference, digits=3)
}
standard <- rescale(improve[,10],to=c(0,1))
improve[,11] <- signif(standard, digits=3)

# Connect.min
for(i in 1:(length(sims$Connect.min)-1)){
  difference <- sims$Connect.min[i] - sims$Connect.min[i+1]
  improve[i+1,12] <- signif(difference, digits=3)
}
standard <- rescale(improve[,12],to=c(0,1))
improve[,13] <- signif(standard, digits=3)

colnames(improve) <- c("Generations",
                    "Area.inc.raw","Area.scaled",
                    "Cost.dec.raw","Cost.scaled",
                    "Forest.dec.raw","Forest.scaled",
                    "Devel.dec.raw","Devel.scaled",
                    "Charact.dec.raw","Charact.scaled",
                    "Connect.dec.raw","Connect.scaled")

improve[,1] <- sims$Generation

filename <- "P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\Generation_Changes_placeholder.csv"
write.csv(improve, file=filename,row.names=FALSE)
