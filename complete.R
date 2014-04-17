complete <-function(directory,id=1:332){
 
        #STEP 1 - Initialize Variables 
        nMonitors<-length(id) # number of files that we need to cycle through
        resultdf<-data.frame(id=integer(0),nobs=integer(0))# final result 
        
        #STEP 2 - Define Helper functions 
        
        # defining a function that returns formatted name of csv file based on ID
        getFileName<- function(num){
                
                if(num<10){
                        # single digit name
                        
                        filename<-paste("/","0","0",num,".csv", sep="")
                        
                }else if (num>=10 && num <100){
                        # double  digit name
                        filename<-paste("/","0",num,".csv", sep="")
                }else {
                        
                        # triple  digit name
                        filename<-paste("/",num,".csv", sep="")
                }
                
                filename
                
        }
        
        #Step 3 - Loop through refered files and figure out # complete obs
        for (i in 1:nMonitors){
                
                tmpdf<-read.csv(paste(directory,getFileName(id[i]),sep=""))# temp placeholder
                good<-complete.cases(tmpdf)
                resultdf<-rbind(resultdf,data.frame(id=id[i],nobs=sum(good)))
                
        }
                
        resultdf
        
}