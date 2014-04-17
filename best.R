best <- function (state,condition){
        
        # Initialize dataframes & get basic info
        outcomedf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        stateList<-unique(outcomedf$State)
        conditionList<-c("heart attack","heart failure","pneumonia")
        condIndex<-0
        
        # validate input parameters

        if(!is.element(state,stateList)){
                
                stop("invalid state")
        }
        
        # 30 day mortality rates by condition
        # Col 11 - 30 day mortality rates for heart attack
        # Col 17 - 30 day mortality rates for heart failure
        # Col 23 - 30 day mortality rates for Pneumonia
        
                if(condition=="heart attack"){
                condIndex<-11
        }else if (condition=="heart failure") {
                condIndex<-17
        }else if (condition=="pneumonia"){
                condIndex<-23
        }else {
                stop("invalid outcome")
        }
        
        keyCols<-c(2,condIndex) # 2 is Hname & 7 is state
        bystate<-split(outcomedf[,keyCols],outcomedf$State)
        finaldf<- as.data.frame(bystate[state])
        finaldf[,2]<-as.numeric(finaldf[,2])
        finaldf<-finaldf[complete.cases(finaldf),]
        finaldf[order(finaldf[,2],finaldf[,1]),][1,1]
        
                
        
}