corr <-function(directory,threshold=0){
 
        #STEP 1 - Prepare basic data
        nobsdf<-complete(directory) #  count of compl. obs for all monitors
        numMonitors<-sum(nobsdf[,"nobs"]>threshold) # count of qual monitors
        corrVector<-numeric(numMonitors) # result we will return
        
        
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
        
        #Step 3 - Loop through qualifying datasets and build-out corrvector

        index = 1
        
        for (i in 1:332){
                
                if(nobsdf[i,"nobs"]>threshold){
                        
                        workingdf<-read.csv(paste(directory,getFileName(i),sep=""))
                        good<-complete.cases(workingdf)
                        corrVector[index]<- cor(workingdf[good,"sulfate"],workingdf[good,"nitrate"])
                        index<-index+1
                        
                }
                
                
        }
        
        corrVector
}