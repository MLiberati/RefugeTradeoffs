# Visualize changes in frontier across generations from 'NSGAII_forallCFAs' results
# Liberati et al. (2019)
# Title: "Addressing ecological, economic, and social tradeoffs of refuge expansion in a constrained landscape"
# Journal: Landscape Ecology
# DOI: 10.1007/s10980-019-00798-8

remove(list=ls())

## R Libraries ##
library(plotly)
library(ggplot2)
library(Rmisc)
library(cowplot)
library(scales)

# CFA <- "Fort"
# CFA <- "Mill"
CFA <- "Mascoma"
# CFA <- "Muddy"
# CFA <- "Salmon"
# CFA <- "Sprague"

date <- "2017.10.18"
gens <- c(seq(from=0,to=1000,by=10))


setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))

#####
### 2D Plots of Objectives ##
objectives <- c("Area","Forest","Cost","Devel","Charact","Connect")
Y_labels <- c("Area (ha)", "Non-forest (ha)", "Cost (mill $US)", "Development risk deviation",
              "Town character conflict", "Fragmentation distance (km)")
X_labels <- c("Area (ha)", "Non-forest (ha)", "Cost (mill US$)", "Development risk deviation",
              "Town character conflict", "Fragmentation distance (km)")


###################################################
###### FUNCTION TO GENERATE FRONTIER POINTS #######
###################################################

frontPnts <- function(binsize, xVar, yVar, Yminmax, minX, maxX){

  binwidth <- (maxX-minX)/binsize
  
  front <- as.data.frame(matrix(nrow=binsize,ncol=2))
  names(front) <- c("x","y")
  
  #establish the cutoffs
  cutoffs<-seq(from=minX,to=maxX,by=(binwidth)) ######### XAXIS*2
  
  #identify min/max solutions within each bin
  for(i in 1:binsize){
    low <- cutoffs[i]
    upper <- cutoffs[i+1]
    binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) ######### XAXIS*2
    y <- Yminmax(binbest[,yVar]) ######### min or max Y variable, YAXIS
    x <- min(binbest[,xVar][binbest[,yVar]%in%y])
    front[i,1:2]<-c(x,y) ######### XAXIS, YAXIS
  }

  return(front)
}



###################################################
############## GGPLOT TEMPLATES ###################
###################################################

mytheme_both <- theme(text=element_text(family="serif", size=20), 
                      legend.position="none", 
                      legend.background = element_rect(fill = "grey95"))

mytheme_noX <- theme(text=element_text(family="serif", size=20), 
                     legend.position="none", 
                     legend.background = element_rect(fill = "grey95"),
                     axis.title.x = element_blank(), 
                     axis.text.x=element_blank())

mytheme_noY <- theme(text=element_text(family="serif", size=20), 
                     legend.position="none", 
                     legend.background = element_rect(fill = "grey95"),
                     axis.title.y = element_blank(), 
                     axis.text.y=element_blank())

mytheme_noXY <- theme(text=element_text(family="serif", size=20), 
                      legend.position="none", 
                      legend.background = element_rect(fill = "grey95"),
                      axis.title.x = element_blank(), 
                      axis.text.x=element_blank(),
                      axis.title.y = element_blank(), 
                      axis.text.y=element_blank())

mypoints <- geom_point(size=1, shape=20)

myscale <- scale_fill_gradient(low='white', high='red', space="Lab", guide="colourbar")

myplot <- theme_classic(base_family = "serif")


size1 <- 4; size2 <- 5; size3 <- 3

shape1 <- 0; shape2 <- 1; shape3 <- 2

stroke <- 4
color <- "black"

linecol <- "grey60"
linesz <- 2

setscale <- read.csv(paste(CFA,"_",gens[length(gens)],"gens_forR_",date,".csv",sep=''))
minX <- 0
maxArea <- round(max(setscale$Area),-4)
maxCost <- round(max(setscale$Cost),1)
maxForest <- round(max(setscale$Forest),-2)
maxCharact <- round(max(setscale$Charact),-3)
maxConnect <- round(max(setscale$Connect),-1)
minConnect <- round(min(setscale$Connect))
maxDevel <- round(max(setscale$Devel),2)
minDevel <- round(min(setscale$Devel),1)

# ------------------ CHARACT -------------------------------- #

setscale <- read.csv(paste(CFA,"_",gens[length(gens)],"gens_forR_",date,".csv",sep=''))
plot(x=setscale$Charact, y=setscale$Area, type="n", xlab="Town character deviation", ylab="Area")

for(i in 1:25){
  # data location
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Charact", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxCharact)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Charact)),max(fitness$Charact),len=100) 
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="yellow",lwd=1)}
for(i in 26:50){
  # data location
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Charact", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxCharact)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Charact)),max(fitness$Charact),len=100) 
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="red",lwd=1)}
for(i in 51:75){
  # data location
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Charact", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxCharact)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Charact)),max(fitness$Charact),len=100) 
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="green",lwd=1)}
for(i in 76:length(gens)){
  # data location
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Charact", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxCharact)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Charact)),max(fitness$Charact),len=100) 
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="blue",lwd=1)}

# ------------------- CONNECT ------------------------------- #

setscale <- read.csv(paste(CFA,"_",gens[length(gens)],"gens_forR_",date,".csv",sep=''))
plot(x=setscale$Connect, y=setscale$Area, type="n", xlab="Fragmentation distance", ylab="Area")

for(i in 1:25){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  
  front <- frontPnts(binsize = 30, xVar = "Connect", yVar = "Area", Yminmax = max,
                     minX=minConnect, maxX=maxConnect)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Connect)),max(fitness$Connect),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="yellow",lwd=1)}
for(i in 26:50){
  # data location
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Connect", yVar = "Area", Yminmax = max,
                     minX=minConnect, maxX=maxConnect)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Connect)),max(fitness$Connect),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="red",lwd=1)}
for(i in 51:75){
  # data location
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Connect", yVar = "Area", Yminmax = max,
                     minX=minConnect, maxX=maxConnect)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Connect)),max(fitness$Connect),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="green",lwd=1)}
for(i in 76:length(gens)){
  # data location
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Connect", yVar = "Area", Yminmax = max,
                     minX=minConnect, maxX=maxConnect)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Connect)),max(fitness$Connect),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="blue",lwd=1)}

# ----------------- DEVEL --------------------------------- #

setscale <- read.csv(paste(CFA,"_",gens[length(gens)],"gens_forR_",date,".csv",sep=''))
plot(x=setscale$Devel, y=setscale$Area, type="n", xlab="Development risk deviation", ylab="Area")

for(i in 1:25){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Devel", yVar = "Area", Yminmax = max,
                     minX=0.15, maxX=maxDevel)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Devel)),max(fitness$Devel),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="yellow",lwd=1)}
for(i in 26:50){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Devel", yVar = "Area", Yminmax = max,
                     minX=minDevel, maxX=maxDevel)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Devel)),max(fitness$Devel),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="red",lwd=1)}
for(i in 51:75){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Devel", yVar = "Area", Yminmax = max,
                     minX=minDevel, maxX=maxDevel)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Devel)),max(fitness$Devel),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="green",lwd=1)}
for(i in 76:length(gens)){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Devel", yVar = "Area", Yminmax = max,
                     minX=minDevel, maxX=maxDevel)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Devel)),max(fitness$Devel),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="blue",lwd=1)}

# ----------------- FOREST --------------------------------- #

setscale <- read.csv(paste(CFA,"_",gens[length(gens)],"gens_forR_",date,".csv",sep=''))
plot(x=setscale$Forest, y=setscale$Area, type="n", xlab="Non-forest habitat", ylab="Area")

for(i in 1:25){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Forest", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxForest)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Forest)),max(fitness$Forest),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="yellow",lwd=1)}
for(i in 26:50){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Forest", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxForest)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Forest)),max(fitness$Forest),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="red",lwd=1)}
for(i in 51:75){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Forest", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxForest)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Forest)),max(fitness$Forest),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="green",lwd=1)}
for(i in 76:length(gens)){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Forest", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxForest)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Forest)),max(fitness$Forest),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="blue",lwd=1)}

# ------------------ COST -------------------------------- #

setscale <- read.csv(paste(CFA,"_",gens[length(gens)],"gens_forR_",date,".csv",sep=''))
plot(x=setscale$Cost, y=setscale$Area, type="n", xlab="Cost ($mill US)", ylab="Area")

for(i in 1:25){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Cost", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxCost)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Cost)),max(fitness$Cost),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="yellow",lwd=1)}
for(i in 26:50){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Cost", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxCost)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Cost)),max(fitness$Cost),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="red",lwd=1)}
for(i in 51:60){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Cost", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxCost)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Cost)),max(fitness$Cost),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="green",lwd=1)}
for(i in 61:length(gens)){
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  front <- frontPnts(binsize = 30, xVar = "Cost", yVar = "Area", Yminmax = max,
                     minX=minX, maxX=maxCost)
  is.na(front) <- sapply(front, is.infinite)
  fit <- loess(y~x, data=front, span=0.5)
  newx <- seq(min(min(fitness$Cost)),max(fitness$Cost),len=100)
  smooth <- predict(fit,newdata=data.frame(x=newx))
  lines(newx,smooth,col="blue",lwd=1)}


