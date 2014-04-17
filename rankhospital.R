rankhospital <- function (state,condition,num ="best"){
        
        # Initialize dataframes & get basic info
        outcomedf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        stateList<-unique(outcomedf$State)
        conditionList<-c("heart attack","heart failure","pneumonia")
        condIndex<-0
        rank<-0
        
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
        
        
        # check rank parameter
        if(num=="best"){
                rank<-1
        }else if (num=="worst"){
                rank<- -1
        }else if(is.character(num)){
                stop("invalid rank")
        }else {
                rank<-num
        }
        
        
        keyCols<-c(2,condIndex) # 2,Hname & 7,state are col indices 
        bystate<-split(outcomedf[,keyCols],outcomedf$State)# split df by state
        finaldf<- as.data.frame(bystate[state]) # get the value for selected state
        finaldf[,2]<-as.numeric(finaldf[,2]) # coerce mortality rates into numeric
        finaldf<-finaldf[complete.cases(finaldf),] # remove NA
        
        if(rank==-1){rank<-nrow(finaldf)} # set rank index
        
        finaldf[order(finaldf[,2],finaldf[,1]),][rank,1] # sort values

        
}