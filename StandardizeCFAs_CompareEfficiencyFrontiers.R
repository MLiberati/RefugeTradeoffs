# Produce Figures 3 in Liberati et al. (2019)
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

CFA_short <- c("Salmon", "Muddy", "Mill", "Fort", "Sprague", "Mascoma")
CFA_long_spaced <- c("Salmon River", "Muddy Brook", "Mill River", "Fort River", "Sprague Brook", "Mascoma River")

StabilizationGens <- c(745, 931, 891, 857, 896, 770)
date <- "2017.10.18"
todaydate <- gsub("-",".",Sys.Date()) # today's date (YYYY.MM.DD)

objectives <- c("Area", "Forest", "Connect", "Devel", "Charact", "Cost")
direction <-c("max", rep("min",5))
Y_labels <- c("Area (ha)", "Non-forest (ha)", "Network distance (km)",
              "Development risk deviation", "Town character conflict", "Cost (mill $US)")
X_labels <- c("Area (ha)", "Non-forest (ha)", "Network distance (km)",
              "Development risk deviation", "Town character conflict", "Cost (mill $US)")

###################################################
###### FUNCTION TO GENERATE FRONTIER POINTS #######
###################################################

frontPnts <- function(binsize, xVar, yVar, Yminmax){
  rangeX <- range(fitness[,xVar]) 
  binwidth <- (rangeX[2]-rangeX[1])/binsize
  
  numb <- binsize + 2
  front <- as.data.frame(matrix(nrow=numb,ncol=2))
  names(front) <- c("x","y")
  
  #establish the cutoffs
  cutoffs<-seq(from=min(fitness[,xVar]),to=max(fitness[,xVar]),by=(binwidth)) 
  #identify min/max solutions within each bin
  for(i in 1:binsize){
    if(i==1){ 
      x <-cutoffs[i]
      y <- fitness[,yVar][fitness[,xVar]%in%x] 
      front[i,1:2]<-c(x,y)
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) 
      y <- do.call(Yminmax, as.list(binbest[,yVar])) 
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y)
    }
    
    else if(i>1&i<binsize){
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) 
      y <- do.call(Yminmax, as.list(binbest[,yVar])) 
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y) 
    }
    
    else if(i==binsize){
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) 
      y <- do.call(Yminmax, as.list(binbest[,yVar])) 
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y)
      x <- max(fitness[,xVar])
      y <- do.call(Yminmax,as.list(fitness[,yVar][fitness[,xVar]%in%x])) 
      front[i+2,1:2]<-c(x,y)
    }
  }
  return(front)
}

###################################################
############## GGPLOT TEMPLATES ###################
###################################################

textsize <- 20

mytheme_both <- theme(text=element_text(family="serif", size=textsize), 
                      legend.position="none", 
                      legend.background = element_rect(fill = "grey95"),
                      axis.ticks.length=unit(0.3,"cm"),
                      axis.line = element_line(colour = 'black', size = 1))

mypoints <- geom_point(size=1, shape=20, color="grey80")

myscale <- scale_fill_gradient(low='white', high='red', space="Lab", guide="colourbar")

myplot <- theme_classic(base_family = "serif")


size1 <- 4; size2 <- 5; size3 <- 3

shape1 <- 0; shape2 <- 1; shape3 <- 2
shape4 <- 5; shape5 <- 6; shape6 <- 4

stroke <- 4
color <- "black"
color1 <- "dark green"
color2 <- "dark blue"

###################################################
###################################################
###################################################

CFALst <- list()

for(k in 1:length(CFA_short)){
  
  # Specify algorithm generation for the CFA
  setwd(paste("P:\\GeneticAlgorithm\\GAResults\\StabilizationRuns\\",CFA_short[k],"_forR", sep=''))
  fitness <- read.csv(paste(CFA_short[k],"_",StabilizationGens[k],"gens_forR_2017.10.18.csv", sep=''))
  fitness<-fitness[,c("Solution", "Area", "Forest", "Connect", "Devel", "Charact", "Cost")]
  
  bestArea <- fitness$Solution[which.max(fitness$Area)]
  bestForest <- fitness$Solution[which.min(fitness$Forest)]
  bestConnect <- fitness$Solution[which.min(fitness$Connect)]
  bestDevel <- fitness$Solution[which.min(fitness$Devel)]
  bestCharact <- fitness$Solution[which.min(fitness$Charact)]
  bestCost <- fitness$Solution[which.min(fitness$Cost)]
  
  for(l in 2:(length(objectives)+1)){
    fitness[,l] <- rescale(fitness[,l])
  }
  
  counter = 1
  plotLst <- list()
  i=1  
  for(j in length(objectives):(i+1)){
    
    plotfront <- frontPnts(binsize = 100, xVar = objectives[j], yVar = objectives[i], Yminmax = direction[i])
    is.na(plotfront) <- sapply(plotfront, is.infinite)
    
    Y <- objectives[i] 
    X <- objectives[j] 
    Ybreaks <- signif(seq(from=min(fitness[,i+1]),to=max(fitness[,i+1]),length.out=5), digits=3)
    Xbreaks <- signif(seq(from=min(fitness[,j+1]),to=max(fitness[,j+1]),length.out=5), digits=3)
    
    plt <- ggplot(fitness, aes_string(x=X, y=Y)) +
      mypoints + myscale + myplot + ylab(Y_labels[i]) + xlab(X_labels[j]) + mytheme_both + 
      geom_point(data=plotfront,aes(x=x,y=y), size=2, shape=20, color="black") +
      geom_point(aes_string(x=fitness[bestArea,j+1],y=fitness[bestArea,i+1]),size=size1, shape=shape1, stroke=stroke, colour=color1) +
      geom_point(aes_string(x=fitness[bestForest,j+1],y=fitness[bestForest,i+1]),size=size2, shape=shape2, stroke=stroke, colour=color1) +
      geom_point(aes_string(x=fitness[bestConnect,j+1],y=fitness[bestConnect,i+1]),size=size3, shape=shape3, stroke=stroke, colour=color1) +
      geom_point(aes_string(x=fitness[bestDevel,j+1],y=fitness[bestDevel,i+1]),size=size3, shape=shape4, stroke=stroke, colour=color2) +
      geom_point(aes_string(x=fitness[bestCharact,j+1],y=fitness[bestCharact,i+1]),size=size3, shape=shape5, stroke=stroke, colour=color2) +
      geom_point(aes_string(x=fitness[bestCost,j+1],y=fitness[bestCost,i+1]),size=size3, shape=shape6, stroke=stroke, colour=color2) +
      scale_y_continuous(breaks=Ybreaks) +
      scale_x_continuous(breaks=Xbreaks)
    
    plotLst[[counter]] <- plt
    counter=counter+1
  }
  
  # Create blank plot with CFA name
  CFA <- ggplot(fitness, aes(x=X, y=Y)) + geom_blank() + theme_void() + 
    annotate("text", label=CFA_long_spaced[k], x=1.3, y=1, size=13, family="serif", hjust=1)
  
  CFALst[[k]] <- plot_grid(CFA, plotLst[[1]], plotLst[[2]], plotLst[[3]], plotLst[[4]], plotLst[[5]], 
                           align="h", nrow=1, ncol=6)
}

# # Create plot with legend
# plt <- ggplot(fitness, aes_string(x=X, y=Y)) +
#   theme(legend.position = c(0.3, 0.4), 
#         legend.title=element_text(size=30,face="bold",family="serif"),
#         legend.text=element_text(size=20,family="serif"),
#         legend.background = element_rect(fill="white", size=1, linetype="solid", colour="black"),
#         legend.spacing.y=unit(10,"cm"))+
#   guides(shape=guide_legend(title="Objective",override.aes = list(stroke=1.25,size=5),ncol=1,keywidth=2,keyheight=2)) +
#   geom_point(aes_string(x=fitness[bestArea,j+1],y=fitness[bestArea,i+1],shape=as.factor(shape1)),
#              size=size1, stroke=stroke, colour=color)+
#   geom_point(aes_string(x=fitness[bestForest,j+1],y=fitness[bestForest,i+1],shape=as.factor(shape2)),
#              size=size2, stroke=stroke, colour=color) +
#   geom_point(aes_string(x=fitness[bestConnect,j+1],y=fitness[bestConnect,i+1],shape=as.factor(shape3)),
#              size=size3, stroke=stroke, colour=color) +
#   geom_point(aes_string(x=fitness[bestDevel,j+1],y=fitness[bestDevel,i+1],shape=as.factor(shape4)),
#              size=size3, stroke=stroke, colour=color) +
#   geom_point(aes_string(x=fitness[bestCharact,j+1],y=fitness[bestCharact,i+1],shape=as.factor(shape5)),
#              size=size3, stroke=stroke, colour=color) +
#   geom_point(aes_string(x=fitness[bestCost,j+1],y=fitness[bestCost,i+1],shape=as.factor(shape6)),
#              size=size3, stroke=stroke, colour=color) +
#   scale_shape_manual(values=c(shape1,shape2,shape3,shape4,shape5,shape6),
#                      labels=c("Area","Non-forest","Fragementation","Developement risk","Town conflict","Cost"))

legData <- c(fitness[bestArea,j+1], fitness[bestArea,i+1], shape1, color1,
             fitness[bestForest,j+1], fitness[bestForest,i+1], shape2, color1,
             fitness[bestConnect,j+1], fitness[bestConnect,i+1], shape3, color1,
             fitness[bestDevel,j+1], fitness[bestDevel,i+1], shape4, color2,
             fitness[bestCharact,j+1], fitness[bestCharact,i+1], shape5, color2,
             fitness[bestCost,j+1], fitness[bestCost,i+1], shape6, color2)
legDf <- data.frame(matrix(legData, ncol=4, byrow=TRUE))
colnames(legDf) <- c("X","Y","shp","col")

# Create plot with legend
plt <- ggplot(legDf, aes(x=X, y=Y, group=shp, color=shp, shape=shp)) +
  geom_point(size=5, stroke=1.25) +
  scale_shape_manual(name="Objectives",
                     values=c(shape1,shape2,shape3,shape4,shape5,shape6),
                     labels=c("Area","Non-forest","Network distance","Developement risk","Town conflict","Cost")) +
  scale_color_manual(name="Objectives",
                     values=c(color1,color1,color1,color2,color2,color2),
                     labels=c("Area","Non-forest","Network distance","Developement risk","Town conflict","Cost")) +
  theme(legend.position = c(0.3, 0.4),
        legend.title=element_text(size=30,face="bold",family="serif"),
        legend.text=element_text(size=20,family="serif"),
        legend.background = element_rect(fill="white", size=1, linetype="solid", colour="black"),
        legend.title.align=0.5) +
  guides(shape=guide_legend(override.aes = list(stroke=1.5,size=5),ncol=1,keywidth=2,keyheight=2))

# Extract the legend
legend <- get_legend(plt)

# Create blank plots with name of objective
objLst <- list()
obj_short <- c("Cost","Town conflict","Development risk","Network distance","Non-forest")
counter = 1
for(k in 1:length(obj_short)){
  
  obj <- ggplot(fitness, aes(x=X, y=Y)) + geom_blank() + theme_void() + 
    annotate("text", label=obj_short[k], x=1, y=0.6, size=12, family="serif",hjust=0.5)
  
  objLst[[counter]] <- obj
  counter=counter+1
}

# Create row with legend and row names
objrow <- plot_grid(legend, objLst[[1]], objLst[[2]], objLst[[3]], objLst[[4]], objLst[[5]], 
                    nrow=1, ncol=6, rel_widths=c(1.1,1,1,1,1,1))

# folder with figures
setwd("P:\\GeneticAlgorithm\\Figures\\EfficiencyFrontierPlots")

# make jpg
grid <- plot_grid(objrow, CFALst[[1]], CFALst[[2]], CFALst[[3]], CFALst[[4]], CFALst[[5]], CFALst[[6]],
                  align="v", nrow=7, ncol=1, scale=0.95)

save_plot("test.jpg", grid, nrow=7, ncol=1, base_width=30)

