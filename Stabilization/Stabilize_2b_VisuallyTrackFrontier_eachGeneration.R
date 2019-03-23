remove(list=ls())

## R Libraries ##
library(plotly)
library(ggplot2)
library(Rmisc)
library(cowplot)

CFA <- "Fort"
# CFA <- "Mill"
# CFA <- "Mascoma"
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

frontPnts <- function(binsize, xVar, yVar, Yminmax){
  rangeX <- range(fitness[,xVar]) ######### XAXIS
  binwidth <- (rangeX[2]-rangeX[1])/binsize
  
  numb <- binsize + 2
  front <- as.data.frame(matrix(nrow=numb,ncol=2))
  names(front) <- c("x","y")
  
  #establish the cutoffs
  cutoffs<-seq(from=min(fitness[,xVar]),to=max(fitness[,xVar]),by=(binwidth)) ######### XAXIS*2
  #identify min/max solutions within each bin
  for(i in 1:binsize){
    if(i==1){ ######### BIN + 1
      x <-cutoffs[i]
      y <- fitness[,yVar][fitness[,xVar]%in%x] ######### YAXIS, XAXIS
      front[i,1:2]<-c(x,y)
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) ######### XAXIS*2
      y <- Yminmax(binbest[,yVar]) ######### min or max Y variable, YAXIS
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y)
    }
    
    else if(i>1&i<binsize){
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) ######### XAXIS*2
      y <- Yminmax(binbest[,yVar]) ######### min or max Y variable, YAXIS
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y) ######### XAXIS, YAXIS
    }
    
    else if(i==binsize){ ######### BIN + 1
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) ######### XAXIS*2
      y <- Yminmax(binbest[,yVar]) ######### min or max Y variable, YAXIS
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y)
      x <- max(fitness[,xVar])
      y <- Yminmax(fitness[,yVar][fitness[,xVar]%in%x]) ######### YAXIS, XAXIS
      front[i+2,1:2]<-c(x,y)
    }
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










### EFFICIENCY FRONTIER & TRACK 3 SOLUTIONS

#########################################################################################################
#########################################################################################################

for(i in 1:length(gens)){
  
  # data location
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
  fitness[,2:7]<-round(fitness[,2:7],5)
  ## solutions along the Area(y)-Cost(x) frontier
  front <- frontPnts(binsize = 15, xVar = "Cost", yVar = "Area", Yminmax = max)
  a <- front$x[which.min(abs(front$x - min(fitness$Cost)))]
  b <- front$x[which.min(abs(front$x - median(fitness$Cost)))]
  c <- round(front$x[which.min(abs(front$x - max(fitness$Cost)))],5)
  soln1 <- fitness$Solution[fitness$Cost%in%a]
  soln2 <- fitness$Solution[fitness$Cost%in%b]
  soln3 <- fitness$Solution[round(fitness$Cost,5)==c]
  
  ### Ar-Fr
  front <- frontPnts(binsize = 20, xVar = "Forest", yVar = "Area", Yminmax = max)
  front[,2] <- t(t(front[,2]))
  Y <- objectives[1] # Area - max
  X <- objectives[2] # Forest
  ylabel <- Y_labels[1]; xlabel <- X_labels[2]
  ArFr <- ggplot(fitness, aes_string(x=X, y=Y)) +
    mypoints + myscale + myplot + ylab(ylabel) + xlab(xlabel) + mytheme_noY + 
    geom_smooth(data=front,aes(x=x,y=y), method="loess",se=FALSE,size=linesz,colour=linecol) +
    geom_point(aes(x=fitness[soln1,3],y=fitness[soln1,2]),size=size1, shape=shape1, stroke=stroke, colour=color) +
    geom_point(aes(x=fitness[soln2,3],y=fitness[soln2,2]),size=size2, shape=shape2, stroke=stroke, colour=color) +
    geom_point(aes(x=fitness[soln3,3],y=fitness[soln3,2]),size=size3, shape=shape3, stroke=stroke, colour=color)
  
  ### Ar-$$
  front <- frontPnts(binsize = 20, xVar = "Cost", yVar = "Area", Yminmax = max)
  front[,2] <- t(t(front[,2]))
  Y <- objectives[1] # Area - max
  X <- objectives[3] # Cost 
  ylabel <- Y_labels[1]; xlabel <- X_labels[3]
  ArCost <- ggplot(fitness, aes_string(x=X, y=Y)) +
    mypoints + myscale + myplot + mytheme_both + ylab(ylabel) + xlab(xlabel) +
    geom_smooth(data=front,aes(x=x,y=y), method="loess",se=FALSE,size=linesz,colour=linecol) +
    geom_point(aes(x=fitness[soln1,4],y=fitness[soln1,2]),size=size1, shape=shape1, stroke=stroke, colour=color) +
    geom_point(aes(x=fitness[soln2,4],y=fitness[soln2,2]),size=size2, shape=shape2, stroke=stroke, colour=color) +
    geom_point(aes(x=fitness[soln3,4],y=fitness[soln3,2]),size=size3, shape=shape3, stroke=stroke, colour=color)
  
  ### Ar-Dv
  front <- frontPnts(binsize = 20, xVar = "Devel", yVar = "Area", Yminmax = max)
  front[,2] <- t(t(front[,2]))
  Y <- objectives[1] # Area - max
  X <- objectives[4] # Development Probability
  ylabel <- Y_labels[1]; xlabel <- X_labels[4]
  ArDv <- ggplot(fitness, aes_string(x=X, y=Y)) +
    mypoints + myscale + myplot + mytheme_noY + ylab(ylabel) + xlab(xlabel) +
    geom_smooth(data=front,aes(x=x,y=y), method="loess",se=FALSE,size=linesz,colour=linecol) +
    geom_point(aes(x=fitness[soln1,5],y=fitness[soln1,2]),size=size1, shape=shape1, stroke=stroke, colour=color) +
    geom_point(aes(x=fitness[soln2,5],y=fitness[soln2,2]),size=size2, shape=shape2, stroke=stroke, colour=color) +
    geom_point(aes(x=fitness[soln3,5],y=fitness[soln3,2]),size=size3, shape=shape3, stroke=stroke, colour=color)
  
  ### Ar-Ch
  front <- frontPnts(binsize = 20, xVar = "Charact", yVar = "Area", Yminmax = max)
  front[,2] <- t(t(front[,2]))
  front <- front[-21,]
  Y <- objectives[1] # Area - max
  X <- objectives[5] # Character
  ylabel <- Y_labels[1]; xlabel <- X_labels[5]
  ArCh <- ggplot(fitness, aes_string(x=X, y=Y)) +
    mypoints + myscale + myplot + mytheme_noY + ylab(ylabel) + xlab(xlabel) +
    geom_smooth(data=front,aes(x=x,y=y), method="loess",se=FALSE,size=linesz,colour=linecol) +
    geom_point(aes(x=fitness[soln1,6],y=fitness[soln1,2]),size=size1, shape=shape1, stroke=stroke, colour=color) +
    geom_point(aes(x=fitness[soln2,6],y=fitness[soln2,2]),size=size2, shape=shape2, stroke=stroke, colour=color) +
    geom_point(aes(x=fitness[soln3,6],y=fitness[soln3,2]),size=size3, shape=shape3, stroke=stroke, colour=color)

    ### Ar-Co
  front <- frontPnts(binsize = 20, xVar = "Connect", yVar = "Area", Yminmax = max)
  front[,2] <- t(t(front[,2]))
  Y <- objectives[1] # Area - max
  X <- objectives[6] # Connectivity
  ylabel <- Y_labels[1]; xlabel <- X_labels[6]
  ArCo <- ggplot(fitness, aes_string(x=X, y=Y)) +
    mypoints + myscale + myplot + mytheme_noY + ylab(ylabel) + xlab(xlabel) +
    geom_smooth(data=front,aes(x=x,y=y), method="loess",se=FALSE,size=linesz,colour=linecol) +
    geom_point(aes(x=fitness[soln1,7],y=fitness[soln1,2]),size=size1, shape=shape1, stroke=stroke, colour=color) +
    geom_point(aes(x=fitness[soln2,7],y=fitness[soln2,2]),size=size2, shape=shape2, stroke=stroke, colour=color) +
    geom_point(aes(x=fitness[soln3,7],y=fitness[soln3,2]),size=size3, shape=shape3, stroke=stroke, colour=color)
  
  # indicate direction of relationship
  guideMAX <- ggplot(front, aes(x = x, y = y)) + geom_blank() + 
    theme(text=element_text(family="serif", size=20), legend.position="none",axis.text.x=element_blank(), axis.text.y=element_blank()) +
    ylab("Maximize objective") + xlab("Minimize objective") +
    geom_segment(aes(xend=min(x), yend=max(y), x=max(x), y=min(y)), size=2, arrow = arrow(length = unit(0.2, "npc")))

  ###########################
  # folder with figures
  setwd(paste("P:\\GeneticAlgorithm\\Figures\\StabilizationFigures\\",CFA,"_GenPlots",sep=''))
  
  # make jpg
  pGEN <- plot_grid(guideMAX, ArCost, ArCh, ArCo, ArFr, ArDv,  
                    align="hv", nrow=1, ncol=6, rel_widths=c(0.9,1,1,1,1,1))
  save_plot(paste(CFA,"_",gens[i],"_Plot_FrontSoln.jpg",sep=''), pGEN, ncol=6, nrow=1)
  
  # reset directory
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
  ###########################
  
}

#########################################################################################################
#########################################################################################################



### ONLY EFFICIENCY FRONTIERS (no solution tracking)
#########################################################################################################
#########################################################################################################

# for(i in 1:length(gens)){
#   
#   # data location
#   setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA,"_forR", sep=''))
#   fitness <- read.csv(paste(CFA,"_",gens[i],"gens_forR_",date,".csv",sep=''))
#   
#   ## solutions along the Area(y)-Cost(x) frontier
#   front <- frontPnts(binsize = 15, xVar = "Cost", yVar = "Area", Yminmax = max)
#   a <- front$x[which.min(abs(front$x - min(fitness$Cost)))]
#   b <- front$x[which.min(abs(front$x - median(fitness$Cost)))]
#   c <- front$x[which.min(abs(front$x - max(fitness$Cost)))]
#   soln1 <- fitness$Solution[fitness$Cost%in%a]
#   soln2 <- fitness$Solution[fitness$Cost%in%b]
#   soln3 <- fitness$Solution[fitness$Cost%in%c]
#   
#   ### Ar-Fr
#   front <- frontPnts(binsize = 20, xVar = "Forest", yVar = "Area", Yminmax = max)
#   front[,2] <- t(t(front[,2]))
#   Y <- objectives[1] # Area - max
#   X <- objectives[2] # Forest
#   ylabel <- Y_labels[1]; xlabel <- X_labels[2]
#   ArFr <- ggplot(fitness, aes_string(x=X, y=Y)) +
#     mypoints + myscale + myplot + ylab(ylabel) + xlab(xlabel) + mytheme_noY + 
#     geom_point(aes(x=fitness[soln1,3],y=fitness[soln1,2]),size=size1, shape=shape1, stroke=stroke, colour=color) +
#     geom_point(aes(x=fitness[soln2,3],y=fitness[soln2,2]),size=size2, shape=shape2, stroke=stroke, colour=color) +
#     geom_point(aes(x=fitness[soln3,3],y=fitness[soln3,2]),size=size3, shape=shape3, stroke=stroke, colour=color)
#   
#   ### Ar-$$
#   front <- frontPnts(binsize = 20, xVar = "Cost", yVar = "Area", Yminmax = max)
#   front[,2] <- t(t(front[,2]))
#   Y <- objectives[1] # Area - max
#   X <- objectives[3] # Cost 
#   ylabel <- Y_labels[1]; xlabel <- X_labels[3]
#   ArCost <- ggplot(fitness, aes_string(x=X, y=Y)) +
#     mypoints + myscale + myplot + mytheme_both + ylab(ylabel) + xlab(xlabel) +
#     geom_point(aes(x=fitness[soln1,4],y=fitness[soln1,2]),size=size1, shape=shape1, stroke=stroke, colour=color) +
#     geom_point(aes(x=fitness[soln2,4],y=fitness[soln2,2]),size=size2, shape=shape2, stroke=stroke, colour=color) +
#     geom_point(aes(x=fitness[soln3,4],y=fitness[soln3,2]),size=size3, shape=shape3, stroke=stroke, colour=color)
#   
#   ### Ar-Dv
#   front <- frontPnts(binsize = 20, xVar = "Devel", yVar = "Area", Yminmax = max)
#   front[,2] <- t(t(front[,2]))
#   Y <- objectives[1] # Area - max
#   X <- objectives[4] # Development Probability
#   ylabel <- Y_labels[1]; xlabel <- X_labels[4]
#   ArDv <- ggplot(fitness, aes_string(x=X, y=Y)) +
#     mypoints + myscale + myplot + mytheme_noY + ylab(ylabel) + xlab(xlabel) +
#     geom_point(aes(x=fitness[soln1,5],y=fitness[soln1,2]),size=size1, shape=shape1, stroke=stroke, colour=color) +
#     geom_point(aes(x=fitness[soln2,5],y=fitness[soln2,2]),size=size2, shape=shape2, stroke=stroke, colour=color) +
#     geom_point(aes(x=fitness[soln3,5],y=fitness[soln3,2]),size=size3, shape=shape3, stroke=stroke, colour=color)
#   
#   ### Ar-Ch
#   front <- frontPnts(binsize = 20, xVar = "Charact", yVar = "Area", Yminmax = max)
#   front[,2] <- t(t(front[,2]))
#   front <- front[-21,]
#   Y <- objectives[1] # Area - max
#   X <- objectives[5] # Character
#   ylabel <- Y_labels[1]; xlabel <- X_labels[5]
#   ArCh <- ggplot(fitness, aes_string(x=X, y=Y)) +
#     mypoints + myscale + myplot + mytheme_noY + ylab(ylabel) + xlab(xlabel) +
#     geom_point(aes(x=fitness[soln1,6],y=fitness[soln1,2]),size=size1, shape=shape1, stroke=stroke, colour=color) +
#     geom_point(aes(x=fitness[soln2,6],y=fitness[soln2,2]),size=size2, shape=shape2, stroke=stroke, colour=color) +
#     geom_point(aes(x=fitness[soln3,6],y=fitness[soln3,2]),size=size3, shape=shape3, stroke=stroke, colour=color)
#   
#   ### Ar-Co
#   front <- frontPnts(binsize = 20, xVar = "Connect", yVar = "Area", Yminmax = max)
#   front[,2] <- t(t(front[,2]))
#   Y <- objectives[1] # Area - max
#   X <- objectives[6] # Connectivity
#   ylabel <- Y_labels[1]; xlabel <- X_labels[6]
#   ArCo <- ggplot(fitness, aes_string(x=X, y=Y)) +
#     mypoints + myscale + myplot + mytheme_noY + ylab(ylabel) + xlab(xlabel) +
#     geom_point(aes(x=fitness[soln1,7],y=fitness[soln1,2]),size=size1, shape=shape1, stroke=stroke, colour=color) +
#     geom_point(aes(x=fitness[soln2,7],y=fitness[soln2,2]),size=size2, shape=shape2, stroke=stroke, colour=color) +
#     geom_point(aes(x=fitness[soln3,7],y=fitness[soln3,2]),size=size3, shape=shape3, stroke=stroke, colour=color)
#   
#   # indicate direction of relationship
#   guideMAX <- ggplot(front, aes(x = x, y = y)) + geom_blank() + 
#     theme(text=element_text(family="serif", size=20), legend.position="none",axis.text.x=element_blank(), axis.text.y=element_blank()) +
#     ylab("Maximize objective") + xlab("Minimize objective") +
#     geom_segment(aes(xend=min(x), yend=max(y), x=max(x), y=min(y)), size=2, arrow = arrow(length = unit(0.2, "npc")))
#   
#   ###########################
#   # folder with figures
#   setwd(paste("P:\\GeneticAlgorithm\\Figures\\StabilizationFigures\\",CFA,"_GenPlots",sep=''))
#   
#   # make jpg
#   pGEN <- plot_grid(guideMAX, ArCost, ArCh, ArCo, ArFr, ArDv,  
#                     align="hv", nrow=1, ncol=6, rel_widths=c(0.9,1,1,1,1,1))
#   save_plot(paste(CFA,"_",gens[i],"_Plot_OnlySoln.jpg",sep=''), pGEN, ncol=6, nrow=1)
#   ###########################
#   
# }

#########################################################################################################
#########################################################################################################

