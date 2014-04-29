###### Week 4 ########3

## String manupulations 
data(airquality)
testdf<-airquality

# Original names >> "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day"   
tolower(names(testdf)) # converts to lower case

# lower case names>> "ozone"   "solar.r" "wind"    "temp"    "month"   "day"    

# toupper() works similarly

namelist<-strsplit(names(testdf),"\\.")

namelist[[2]][1] # returns Solar since we split by period

firstelement<- function(x){ x[1]} # returns first element of list
sapply(namelist,firstelement) # returned the first part of col names post splitting

testdf<-airquality

gsub("\\.","",names(testdf)) # replaces all occurences of first parm by second in third
names(testdf)<-tolower(names(testdf))
#grep searches for pattern match
#grepl returns vector of true/false based on pattern match 
# grepl can be used in indexing
#length(grepl)==0 is a way to check for occurence
# stringr package
# nchar> count number of char
# substr(start,end,string)
#paste0 > paste without spaces
#str_trim > trim leading & trailing spaces

## REGULAR EXPRESSIONS

# ^test starts with test
# test$ ends with test
# [Bb][Uu][Ss][Hh] matches BUSH, bush, Bush, bushfire etc
# ^[Ii] am matches starts with I am or i am
# ^[0-9][a-zA-Z] is starts with number followed by letter e.g. 7th
# [^?.]$ says return all sentences that dont end with ? or . ^ behaves differently here
# 9.11> matches 9-11,9/11,9.11,9:11,911
# | is the alternatives>> so flood | fire >> flood or fire
# ^[Gg]ood | bad
# [Gg]eorge ([Ww]\.)? [Bb]ush >>> optionally looks for the W. or w. 
# * any char any number of times including none 
# + is at least one of item

#(.*) matches anystring within braces including empty braces

# [0-9]+ (.*) [0-9]+ looks for two numbers seperated by something or nothing 

# [Bb]ush ( +(^ )+ +){1,5} debate looks for bush followed (by at least once space followed 
# by at least one not space followed by at least one space ) repeated anywhere between 1-5 times 
# then debates

# {m,n} at least m not more than n
# m is exactly = m
# m, is at least =m

# +([a-zA-z]+) +\1 + matches space + <chars> + space +<same chars again> 

# ^s(.*)s > matches whole sentences that begin and end with letter s. so it is a greedy match
# ^s(.*?)s$ > less greedy

######## EXERCISES

#1 
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url,destfile="./housing.csv",method="curl", extra="-k")
housingdf<-read.csv("housing.csv")
splitlist<-strsplit(names(housingdf),"wgtp")

#2
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url,destfile="./GDP.csv",method="curl", extra="-k")
rawdf<-read.csv("GDP.csv",skip=3, header=TRUE, blank.lines.skip=TRUE)
names(rawdf)[names(rawdf)=="X"]<-"CountryCode"
toberemoved<-which(is.na(rawdf$Ranking)| rawdf$Ranking=="" |rawdf$CountryCode=="" ) 
gdpdf<-rawdf[-toberemoved,c(1,2,4,5)]
names(gdpdf)<-tolower(gsub("\\.","",names(gdpdf)))
gdpdf$usd<-gsub(",","",gdpdf$usdollars)
gdpdf$usd<-as.numeric(gdpdf$usd)

mean(gdpdf$usd)

#3
grep("^[Uu]nited",gdpdf$economy,value=TRUE)

#4
# load GDP data as above
# load edstads data
url1<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url1,destfile="./edstats1.csv",method="curl", extra="-k")
edstatsdf<-read.csv("edstats1.csv",strip.white=TRUE)
names(edstatsdf)<-tolower(names(edstatsdf))
names(edstatsdf)<-gsub("\\.","",names(edstatsdf))
mergedf<-merge(gdpdf,edstatsdf,by.x="countrycode",by.y="countrycode", all=TRUE)

# fiscal year end data is provided in the special notes column.

length(grep("^(.*)[Ff]iscal+ [Yy]ear+ [Ee]nd(.*)[Jj]un",mergedf$specialnotes))

#5 
amzn <- getSymbols("AMZN",auto.assign=FALSE)
sampleTimes<-index(amzn)
in2012<-which(format(sampleTimes,"%Y")=="2012Mon")
length(in2012)
in2012mon<-which(format(sampleTimes,"%Y%a")=="2012Mon")
length(in2012mon)