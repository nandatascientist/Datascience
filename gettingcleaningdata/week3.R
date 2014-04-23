 ######## WEEK 3: GETTING AND CLEANING DATA ####################

######## FILE DOWNLOAD ########

fileUrl<-"https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./restbal.csv", method="curl", extra="-k")
restdf<-read.csv("restbal.csv")

######## INSPECT DATAFRAME ########

head(restdf,n=3)
tail(restdf,n=3)
str(restdf) # good meta data

######## SUBSETTING ########

data(airquality)
airquality[airquality$Month==5 & airquality$Day>20,] # VERY POWERFUL

######## SUMMARIZING ########

summary(restdf) # tries to summarize data

# default probabilities/ percentiles are  0,25,50,75 & 100
quantile(restdf$councilDistrict,na.rm=TRUE) 
 
# custom probabilities / percentiles can be given
quantile(restdf$councilDistrict,probs=c(0.5,0.65,0.85))

# table function returns a contingency table
table(restdf$zipCode, useNA="ifany")
table(restdf$councilDistrict,restdf$zipCode) # pretty darn cool pivot table like

######## CHECKING FOR MISSING VALUES ########

sum(is.na(restdf$councilDistrict)) # returns number of NA values in this col
any(is.na(restdf$councilDistrict)) # are there any NA values in this col?

######## CHECKING FOR CONDITIONS ########
all(restdf$zipCode>0)# are all zipcodes > 0
colSums(is.na(restdf))# how many NA values exist per column in df
all(colSums(is.na(restdf))==0) # do we have NA values anywhere in our dataset

table(restdf$zipCode %in% c("21212","21213")) 
# we can group by number of records that match condition and do not match

restdf[restdf$zipCode %in% c("21212","21213"),]
data(UCBAdmissions)
df<-as.data.frame(UCBAdmissions)
summary(df) # provides high-lvel info on the df

######## CROSS TABULATION ########

xtabs(Freq~Gender+Admit,data=df) # holy cow!!! excellent summary
#xtabs can be done on all cols ~. in which case "ftable" on the output will 
# flatten it for better usage.

######## VARIABLE CREATION ########
# Missingness indicators, cutting up quantitative variables, applying transforms
restdf$ID<-seq(1:nrow(restdf)) # adding ID col

# adding a col to the DF on which locations are "of interest"
restdf$OfInterest<-restdf$neighborhood %in% c("Roland Park","Homeland")
# we can get contingency counts of restdf$OfInterest
table(restdf$OfInterest)

restdf$zipWrong<-ifelse(restdf$zipCode<0,TRUE,FALSE)

library(Hmisc)
restdf$zipGroups<-cut2(restdf$zipCode,g=4)

# plyr looks really cool and promising. I need to read more.
library(plyr)
restdf2<-mutate(restdf,ZipGroups=cut2(zipCode,g=4))

######## RESHAPING DATA ########
library(reshape2)
data(mtcars)
mtcars$carName<-rownames(mtcars) # just assigning col name for name
carMelt<-melt(mtcars,id=c("carName","gear","cyl"),measure.vars=c("mpg","hp"))
# the above code makes the data set tall and skinny and it merges the 
# cols "mpg" and "hp" into one "variable" col with their values melted 
# into "values" col

cylData<-dcast(carMelt,cyl~variable)
cylData<-dcast(carMelt,cyl~variable,mean)
# The melted data can be recast! in the second line above, cyl becomes the rows
# and the melted variable col is recast as its constituents with the aggregate 
# function "mean"

# names(InsectSprays) >>  "count" "spray"
tapply(InsectSprays$count,InsectSprays$spray,sum) # summarizes count by spray
sapply(split(InsectSprays$count,InsectSprays$spray),sum) # same as above

#plyr package
ddply(InsectSprays,.(spray),summarize,sum=sum(count)) # does same as above

######## EXERCISE CODE ########
# #1
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url,destfile="./uscom.csv", method="curl", extra="-k")
mydf<-read.csv("uscom.csv")
#ACR is acreage AGS is the $ of agricultural products sold
agricultureLogical<-mydf$ACR==3 & mydf$AGS==6 # simpy the row wise indicator
head(mydf[which(agricultureLogical),],n=3)

# #2
#img<-readJPEG(system.file("img","getdata-jeff.jpg",package="jpeg"))
img<-readJPEG("getdata-jeff.jpg", native=TRUE)
quantile(img,probs=c(0.3,0.8))

# #3 SUPER GOOD PROBLEM.
rawdf<-read.csv("GDP.csv", header=TRUE,strip.white=TRUE,skip=3) 
# get the file read into rawdf. Skip first 3 lines 
# strip white spaces while reading > like TRIM()

names(rawdf)[names(rawdf)=="X"]<-"CountryCode"
# rename the col X to CountryCode

toberemoved<-which(is.na(rawdf$Ranking)| rawdf$Ranking=="" |rawdf$CountryCode=="" ) 
# missingness index is made  robust to filter out incompete rows
# we are saving from checking for multiple whitespaces here,
# since we stripped them off while reading file

tail(rawdf[-toberemoved,]) 
# shows me that the index is good and we are getting only good rows.

gdpdf<-rawdf[-toberemoved,c(1,2,4,5)]
# gets all rows except ones indexed above + the correct cols

edstatsdf<-read.csv("EDSTATS_Country.csv",strip.white=TRUE)
# the other data set is read in as usual.

mergedf<-merge(gdpdf,edstatsdf,by.x="CountryCode",by.y="CountryCode", all=TRUE)
# look for rows that have matching keys  
# I tried this with all= TRUE & FALSE. 
# Quiz options matched only for all=TRUE.

nrow(mergedf)-sum(is.na(mergedf$Ranking))

# this expression will subtract number of NA values in "Ranking" field
# from total # of rows. The result is the "matched rows" across both dataframes
# NA values in Ranking field correspond to countries from edstatsdf
# that dont have values in gdpdf

tmpv<-as.character(mergedf$Ranking)
# This was a MAJOR HEADACHE to figure out!!
# Ranking is stored as a factor and directly conveting to numeric completely
# messes up the actual value, since the numeric value corresponds to the factor
# levels. The character coercion uses the actual values and *not* the levels ,
# which we then coerce back to numeric to get the sortable rank as below!

mergedf$SortRank<-as.numeric(tmpv)
mergedf[,c("Ranking","SortRank")]
# visual inspection to make sure everything is in order

sorteddf<- mergedf[order(mergedf$SortRank,decreasing=TRUE),c(1,2,3,6,35)]
#PHEW!!!

 # #4 
naindex<-which(is.na(sorteddf$Ranking))
csdf<-sorteddf[-naindex,] # assigns non-NA values to csdf
tapply(csdf$SortRank,csdf$Income.Group,mean) 
# calculates average of rank y Income group

# #5
 tmpgroups<-cut2(csdf$SortRank,g=5)
 # cutting rank in 5 quantiles as requested in exercise
 table(tmpgroups,csdf$Income.Group)
 # cross tabulates rank quantiles Vs Income group
 