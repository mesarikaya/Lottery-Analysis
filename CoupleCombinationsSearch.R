
matchCombinations<- function(lotterydata,diff,startLoc=0){
        results<-data.frame(matrix(ncol = 2, nrow = 0))
        colnames(results)<-c("results","locations")
        lotterydata<-as.data.frame(lotterydata,stringsAsFactors=FALSE,stringsAsFactors=FALSE)
        
        n<-0
        i<-0
        repeat{
                i<-i+1
                n<-n+1
                if(isTRUE(abs(lotterydata[i,1+startLoc]-lotterydata[i,2+startLoc]) == diff)){
                        results[n,"results"]<-paste0(lotterydata[i,1+startLoc],lotterydata[i,2+startLoc])
                        results[n,"locations"]<-paste0(i)
                        n<-n+1
                }
                if(isTRUE(abs(lotterydata[i,2+startLoc]-lotterydata[i,3+startLoc]) == diff)){
                        results[n,"results"]<-paste0(lotterydata[i,2+startLoc],lotterydata[i,3+startLoc])
                        results[n,"locations"]<-paste0(i)
                        n<-n+1
                }
                if(isTRUE(abs(lotterydata[i,3+startLoc]-lotterydata[i,4+startLoc]) == diff)){
                        results[n,"results"]<-paste0(lotterydata[i,3+startLoc],lotterydata[i,4+startLoc])
                        results[n,"locations"]<-paste0(i)
                        n<-n+1
                }
                if(isTRUE(abs(lotterydata[i,4+startLoc]-lotterydata[i,5+startLoc]) == diff)){
                        results[n,"results"]<-paste0(lotterydata[i,4+startLoc],lotterydata[i,5+startLoc])
                        results[n,"locations"]<-paste0(i)
                        n<-n+1
                }
                if(isTRUE(abs(lotterydata[i,5+startLoc]-lotterydata[i,6+startLoc]) == diff)){
                        results[n,"results"]<-paste0(lotterydata[i,5+startLoc],lotterydata[i,6+startLoc])
                        results[n,"locations"]<-paste0(i)
                        n<-n+1
                }
                
                
                if(i==nrow(lotterydata)){
                        break
                }
        }
        results<-na.omit(results)

        eval(results)
        
        
}