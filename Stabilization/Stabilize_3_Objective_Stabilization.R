# Visualize and identify stabilization generation for objectives from 'NSGAII_forallCFAs' results
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

setwd("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns")

# CFA <- "Mascoma"
# CFA <- "Mill"
# CFA <- "Muddy"
# CFA <- "Sprague"
CFA <- "Fort"
# load(paste(CFA,"_ThresholdData_2017.10.18.RData",sep=''))

# NOTE - separate code for Salmon River because of gap in frontier
# NOTE - separate code for Fort River

date <- "2017.10.18"
# date <- "2017.12.03"
# gens <- c(seq(from=0,to=1000,by=1))
gens <- c(seq(from=0,to=1000,by=1))

setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))

objectives <- c("Area","Forest","Cost","Devel","Charact","Connect")
# Y_labels <- c("Area (ha)", "Non-forest (ha)", "Cost (mill $US)", "Development risk deviation",
#               "Town character conflict", "Network distance (km)")
# X_labels <- c("Area (ha)", "Non-forest (ha)", "Cost (mill US$)", "Development risk deviation",
#               "Town character conflict", "Network distance (km)")

###################################################
###### FUNCTION TO GENERATE FRONTIER POINTS #######
###################################################

frontPnts <- function(binsize, xVar, yVar, Yminmax, minX, maxX){
  
  # standardize binwidths between min and max of longest generations
  binwidth <- (maxX-minX)/binsize
  
  # placeholder for points along the frontier
  front <- as.data.frame(matrix(nrow=binsize,ncol=2))
  # name matrix columns
  names(front) <- c("x","y")
  
  #establish the cutoffs
  cutoffs<-seq(from=minX,to=maxX,by=(binwidth)) ######### XAXIS*2
  
  #identify min/max solutions within each bin
  for(i in 1:binsize){
    # lower value of the bin
    low <- cutoffs[i]
    # upper value of the bin
    upper <- cutoffs[i+1]
    # identify the solutions that occur within the bin
    binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) ######### XAXIS*2
    y <- Yminmax(binbest[,yVar]) ######### min or max Y variable, YAXIS
    x <- min(binbest[,xVar][binbest[,yVar]%in%y])
    front[i,1:2]<-c(x,y) ######### XAXIS, YAXIS
  }
  
  return(front)
}

setscale <- read.csv(paste(CFA,"_",gens[length(gens)],"gens_forR_",date,".csv",sep=''))

###################################################
###################################################
###################################################

# MinX <- function(x) {floor(min(setscale[x]))}
# MaxX <- function(x) {round(max(setscale[x]),1)}
MinX <- function(x) {min(setscale[x])}
MaxX <- function(x) {max(setscale[x])}

# objectives included in algorithm
objectives <- c("Area","Forest","Cost","Devel","Charact","Connect")
# number of intervals along the frontier
binsize = 20
# holds tracking outputs for each CFA (list of 5 matrices)
dfs <- list()

# loop through objectives (exclude Area, j=1)
for(j in 2:length(objectives)){

  # placeholder to max y values in each bin
  frontier <- matrix(data=NA, nrow=length(gens), ncol=binsize)
  # loop through generations
  for(i in 1:length(gens)){
    # get csv for the generatin of interest
    fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
    # round values
    fitness[,2:7]<-round(fitness[,2:7],5)
    # identify points along the frontier
    front <- frontPnts(binsize=binsize, xVar=objectives[j], yVar=objectives[1], Yminmax=max,
                       minX=MinX(objectives[j]), maxX=MaxX(objectives[j]))
    # replace '-Inf' caused by empty bins with 'NA'
    is.na(front) <- sapply(front, is.infinite)
    # save the best y values (x's never change since the bins are the same for each generation)
    frontier[i,] <- front$y
  }

  # placeholder for change in values for each generation
  frontierChanges <- matrix(data=NA, nrow=length(gens)-1, ncol=binsize)
  # loop through generations (skip generation 0)
  for(i in 1:length(gens)-1){
    # vector of y values at generation t
    genPrev <- frontier[i,]
    # vector of y values at generation t=1
    genNow <- frontier[i+1,]
    # calculate raw change between t+1 and t
    change <- genNow - genPrev
    # convert to percent change between t+1 and t
    percent <- round((change / genNow) * 100, 2)
    # change NA's to 100 percent to penelize for not having a value in that bin
    #   (penalize lack of spread along frontier)
    percent[is.na(percent)] <- 100
    # save vector of percent changes to matrix
    frontierChanges[i,] <- percent
  }

  # save matrix of changes for the objective to a list of matrices
  dfs[[j-1]] <- frontierChanges

}

### save frontier change tracking to an R data file
setwd("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns")
save(dfs, file=paste(CFA,"_ThresholdData_",date,".RData",sep=''))


################# THE FIGURE ################

### save the frontier change plots for the CFA
setwd("P:\\GeneticAlgorithm\\Figures\\StabilizationFigures")
# jpeg(filename="test.jpg",width=450,height=300,res=300) # units=pixels (equivalent to 6x4 inches)
todaydate <- gsub("-",".",Sys.Date()) # today's date (YYYY.MM.DD)
filename <- paste(CFA,"_Threshold_",todaydate,".jpg",sep='')
jpeg(filename=filename, width=5, height=4, units='in', res=300)

# ---------------------------------------- #
# names for legends
# objectives <- c("Area","Forest","Cost","Devel","Charact","Connect") # order of the dfs matrices
OBJnames <- c("Non-forest","Cost","Development risk","Town character","Network distance")
# colors for legends
colors <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")

# blank plot
plot(x=c(), xlim=c(0,1000), ylim=c(-15,1550), type="n",
     xlab="Generation", ylab="Cummulative change in the frontier")
# y=0 line
abline(h=0,lwd=1)

# placeholder to objective stabilization locations
objThresh <- c()
# add smoothed lines that track cummulative change along frontier
for(j in 1:length(dfs)){
  # specify the objective's (j) matrix
  frontierChanges <- dfs[[j]]
  # placeholder for sum of frontier change for each generation (i)
  track <- c()
  # loop through generations to calculate sum of percent change
  for(i in 1:(length(gens)-1)){
    value <- sum(frontierChanges[i,], na.rm=TRUE)
    track[i] <- value
  }
  # plot the smoothed lines based on track()
  y <- c(1:length(track))
  fit <- loess(track~y, span=0.1)
  predfit <- predict(fit)
  lines(predfit,col=colors[j],lwd=1)
  # identify the threshold for each objective (j)
  thresh <- min(which(predfit < 20))
  objThresh[j] <- thresh
}
# vertical stabilization point
abline(v=max(objThresh), lwd=1, lty=2)
# add title that indicates threshold value
text(x=max(objThresh), y=700, pos=2, paste("Stabilization = ",max(objThresh)," generations",sep=''),cex=0.7)
# add legend to the plot
legend("topright", legend=OBJnames, col=colors, lty=1, lwd=2, cex=0.7, 
       title = paste("CFA = ",toupper(CFA),sep=''))
# ---------------------------------------- #

# end plot save
dev.off()

#############################################

# save the threshold zoom in for the CFA
setwd("P:\\GeneticAlgorithm\\Figures\\StabilizationFigures")
# jpeg(filename="test.jpg",width=450,height=300,res=300) # units=pixels (equivalent to 6x4 inches)
todaydate <- gsub("-",".",Sys.Date()) # today's date (YYYY.MM.DD)
filename <- paste(CFA,"_ThresholdZoomed_",todaydate,".jpg",sep='')
jpeg(filename=filename, width=5, height=4, units='in', res=300)

# ---------------------------------------- #
### opportunity to zoom-in
# names for legends
OBJnames <- c("Non-forest","Cost","Development risk","Town character","Network distance", "Threshold")
# colors for legends
colors <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", "#999999")

# blank plot
xMin <-max(objThresh) - 30
plot(x=c(), xlim=c(xMin,1000), ylim=c(-30,1450), type="n",
     xlab="Generation", ylab="Cummulative change in the frontier")

# y=0 line
abline(h=0,lwd=1)

# add legend to the plot
legend("topright", legend=OBJnames[2:6], col=colors, lty=1, lwd=2, cex=0.7, 
       title = paste("CFA = ",toupper(CFA),sep=''))

for(j in 1:length(dfs)){
  # specify the objective's (j) matrix
  frontierChanges <- dfs[[j]]
  # placeholder for sum of frontier change for each generation (i)
  track <- c()
  # loop through generations to calculate sum of percent change
  for(i in 1:(length(gens)-1)){
    value <- sum(frontierChanges[i,], na.rm=TRUE)
    track[i] <- value
  }
  # plot the smoothed lines based on track()
  y <- c(1:length(track))
  fit <- loess(track~y, span=0.1)
  predfit <- predict(fit)
  lines(predfit,col=colors[j],lwd=1)
  # identify the threshold for each objective (j)
  thresh <- min(which(predfit < 20))
  objThresh[j] <- thresh
}

# threshold lines
abline(h=20, lty=2, col="grey")
abline(h=-20, lty=2, col="grey")

# vertical stabilization point
abline(v=max(objThresh), lwd=1, lty=2)
# add title that indicates threshold value
# text(x=max(objThresh), y=700, paste("Stabilization = ",max(objThresh)," generations",sep=''),cex=0.7)
legend(x=max(objThresh), y=700, paste("Stabilization = ",max(objThresh)," generations",sep=''), 
       box.col = "white", bg = "white", adj = 0.4, cex=0.8)
# add legend to the plot
legend("topright", legend=OBJnames, col=colors, lty=c(rep(1,5),2), lwd=2, cex=0.7, 
       title = paste("CFA = ",toupper(CFA),sep=''))
# ---------------------------------------- #

dev.off()

