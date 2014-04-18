## Week 2 Getting & Cleaning data assigments

#1 Question
# Obtaining the repo list  info from github to check for created date of 
# datasharing repo

# This code needs to be exeuted from R Prompt for the authentication to occur

library(httr)

#    Use http://localhost:1410 as the callback url
 myapp <- oauth_app("github", <client_ID from Github application>,<client_secret
                   from github application)

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

req <- GET("https://api.github.com/users/jtleek/repos", config(token = github_token))

json1<-content(req) #json1 is a list
json2<JSONlite::fromJSON(toJSON(json1)) #json2 is a dataframe
subset(json2[,"created_at"],json2$name=="datasharing") # filters the dataframe

#2 & #3 Question
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl,destfile="./acs.csv",method="curl", extra="-k")
# assignment involves a couple of queries around this dataset that are elementary

#4 readLines function
content<-readLines("http://biostat.jhsph.edu/~jleek/contact.html")
nchar(content[nn])

#5 
text <- readLines("tabdelim.tab",encoding="UTF-8")
newtext<-str_replace_all(text,"-"," ")
filecon<-file("cleanfile.tab")
writeLines(newtext,filecon,sep="\n")
close(filecon)
df<-read.table("cleanfile.tab",skip=3,quote="",fill=TRUE)
df[,4]<-as.numeric(df[,4])
sum(df[,4])
