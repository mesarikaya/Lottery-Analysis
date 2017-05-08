library(dplyr)
library(tidyr)
library(data.table)

# This aims to to transform the combination file
# Mainly it concatenates the draws and tries to get some more out of the data
# This part will only work with 6 letters draw
combStats <- function(combinations)
{       
        force(combinations)
        
        # Concatenate draw numbers 
        draw <- transform(combinations,draw=paste0(combinations[,1],combinations[,2],combinations[,3],combinations[,4],combinations[,5],combinations[,6]))
        
        combinations <- draw

        # Create new columns to find gaps between draw numbers
        combinations$gapBetween1 <- abs(combinations[,1]-combinations[,2])
        combinations$gapBetween2 <- abs(combinations[,2]-combinations[,3])
        combinations$gapBetween3 <- abs(combinations[,3]-combinations[,4])
        combinations$gapBetween4 <- abs(combinations[,4]-combinations[,5])
        combinations$gapBetween5 <- abs(combinations[,5]-combinations[,6])
        combinations$meanGap <- apply(combinations[,c("gapBetween1","gapBetween2","gapBetween3","gapBetween4","gapBetween5")],1,mean)
        combinations$minGap <- apply(combinations[,c("gapBetween1","gapBetween2","gapBetween3","gapBetween4","gapBetween5")],1,min)
        combinations$maxGap <- apply(combinations[,c("gapBetween1","gapBetween2","gapBetween3","gapBetween4","gapBetween5")],1,max)
        combinations$stdGap <- apply(combinations[,c("gapBetween1","gapBetween2","gapBetween3","gapBetween4","gapBetween5")],1,sd)

        # Report the number couples between draws
        combinations <- transform(combinations,firstCouple = paste0(combinations[,1],combinations[,2]))
        combinations <- transform(combinations,secondCouple = paste0(combinations[,2],combinations[,3]))
        combinations <- transform(combinations,thirdCouple = paste0(combinations[,3],combinations[,4]))
        combinations <- transform(combinations,fourthCouple = paste0(combinations[,4],combinations[,5]))
        combinations <- transform(combinations,fifthCouple = paste0(combinations[,5],combinations[,6]))
        colnames(combinations[,c(1:6)]) <- as.character(c(1:6))

        return(combinations)
        
}


# This is to be able to filter a data where the above provided function is executed
filterDraw <- function(data,prefNum,minTotal,maxTotal,minGapmin,minGapmax,maxGapmin,maxGapmax,meanGapmin,meanGapmax,drawLimit)
{
        force(data)
        data$draw<-as.numeric(data$draw)
        data$draw<data$draw/1000000
        data <- dplyr::filter(data,minGap <= minGapmax & minGap >= minGapmin
                        & maxGap <= maxGapmax & maxGap>=maxGapmin &
                        meanGap <= meanGapmax & meanGap>=meanGapmin & 
                        colTotals >= minTotal & colTotals <= maxTotal &
                        draw<=drawLimit & (data[,1]==prefNum | data[,2]==prefNum | data[,3]==prefNum | data[,4]==prefNum |
                                                   data[,5]==prefNum | data[,6]==prefNum))
        #subset(data,data$minGap<=minGapthreshold && data$maxGap<=maxGapthreshold && data$meanGap<=meanGap && data$drawTotal>=minTotal && data$drawTotal<=maxTotal  )
        
        return(data)
}

