library(dplyr)
library(tidyr)
library(data.table)
source('~/GitHub/Lottery Analysis/LotteryAnalysis/CoupleCombinationsSearch.R')

## This function creates a combination table for the seleccted number of pairs
## with given possible minimums and maximums
findCombinations <-function(pairCount,min,max){
        numbers<-seq(min,max,by=1)
        combinations<-combn(numbers,pairCount)
        combinations<-t(combinations)
        combinations<-data.table(combinations)
        colnames(combinations)<-as.character(c(1:pairCount))
        combinations$colTotals <- rowSums(combinations)
        combinations<-arrange(combinations,colTotals)
        return(combinations)  
}

## To search a combination that meats certain selection total criteria in an existing combination table
findCombInRange <- function(data,min,max)
{
        force(data)
        data <- as.data.frame(data)
        results<- dplyr::filter(filter(data,colTotals>=min),colTotals<=max)
        return(results)
}