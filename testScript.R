# Returns function above 
above<- function(vect,threshold=10){
        vect[vect>threshold]
}

# mean of each of col in Dataframe assuming it is all numeric
colmean <- function(df, removeNA=TRUE){
        
        numcols<-ncol(df)
        colmeans<-numeric(numcols)
        
        
        for (i in 1:numcols){
                
                colmeans[i] = mean(df[,i],na.rm=removeNA)
                
        }
        
        colmeans
        
}


# unbelievable function that returns other functions
make.power<-function(n){
        
        power<-function(x){
                
                x^n
                
        }
        
        power
}



# Testing lexical scoping in R

f <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        x + g(x)
}


# figuring out how to calc mean from a list
tfunction<- function(listval){
        
        df<-as.data.frame(listval)
        #print(names(df))
        colMeans(df[,1:3])
}

