library(dplyr)
library(doBy)
library(ggplot2)
library(cowplot)

remove(list=ls())

date <- "2017.10.18"
# date <- "2017.12.03"

CFA <- "Fort"
# CFA <- "Mascoma"
# CFA <- "Mill"
# CFA <- "Muddy"
# CFA <- "Salmon"
# CFA <- "Sprague"

gens <- c(seq(from=0,to=1000,by=1))
# gens <- c(seq(from=896,to=896,by=1))

setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_OriginalFiles", sep=''))


for(i in 1:length(gens)){
  
  GENS <- read.csv(paste(CFA,"_",gens[i],"gens_",date,".csv",sep=''), header=FALSE)
  
  df <- data.frame(matrix(nrow=nrow(GENS), ncol=7))
  
  for(j in 1:nrow(GENS)){
    
    # unconcatinate objective values for each solution (row)
    commas <- unlist(strsplit(as.character(GENS[j,2]), split="[,]"))
    obj2to5 <- as.numeric(unlist(strsplit(as.character(GENS[j,2]), split="[,]"))[2:5])
    obj1 <- as.numeric(unlist(strsplit(commas[1], split="[[]"))[2])
    obj6 <- as.numeric(unlist(strsplit(commas[6], split="[]]"))[1])
    
    # asign objective values to data frame
    df[j,2] <- obj1
    df[j,7] <- obj6
    df[j,3:6] <- obj2to5
    
    # add solution number
    df[j,1] <- j
    
    names(df) <- c("Solution","Area","Forest","Cost","Devel","Charact","Connect")
  }
  
  filename = paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR\\",
                   CFA,"_",gens[i],"gens_forR_",date,".csv",sep='')
  write.csv(df, filename, row.names=FALSE)
  
}
