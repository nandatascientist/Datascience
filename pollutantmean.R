pollutantmean <-function(directory,pollutant,id=1:332){
 
        #STEP 1 - Initialize Variables 
        
        nMonitors<-length(id) # number of files that we need to cycle through
        
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
        
        #Step 3 - Loop through refered files and build a large dataframe
        
        # first file is read
        resultdf<-read.csv(paste(directory,getFileName(id[1]),sep=""))
        
        # read other files and append to resultdf
        if (nMonitors>1){
                
                
                for (i in 2:nMonitors){
                        
                        resultdf<-rbind(resultdf,read.csv(paste(directory,getFileName(id[i]),sep="")))
                        
                }
        }
                
        mean(resultdf[,pollutant],na.rm=TRUE)
        
}