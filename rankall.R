rankall <- function (condition,num ="best"){
        
        # Initialize dataframes & get basic info
        outcomedf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        #stateList<-unique(outcomedf$State)
        conditionList<-c("heart attack","heart failure","pneumonia")
        condIndex<-0
        rank<-0
        
        #Validate condition
        #30 day mortality rates by condition
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
        
        # validate  rank parameter
        if(num=="best"){
                rank<-1
        }else if (num=="worst"){
                rank<- -1
        }else if(is.character(num)){
                stop("invalid rank")
        }else {
                rank<-num
        }
        
        
        keyCols<-c(7,2,condIndex) # 2,Hname & 7,state are col indices 
        tmpdf<-outcomedf[,keyCols]# get a temp df going
        tmpdf<-tmpdf[complete.cases(tmpdf),]#remove NA
        tmpdf[,3]<-as.numeric(tmpdf[,3])
        bystate<-split(tmpdf,tmpdf$State)# split tmpdf by state
        # each element is a df of state,hname,cond
        
        processdf<-function(df,ranknum){
                
                if(ranknum==-1){
                        ranknum<-nrow(df)
                }
                
                df[order(df[,3],df[,2]),][ranknum,c(2,1)]
                
        }
        
        tmplst<-lapply(bystate,processdf,rank)
        resultdf<-data.frame(t(sapply(tmplst,c)))
        names(resultdf)<-c("hospital","state")
        resultdf
        
}