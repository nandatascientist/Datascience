getwd()
# Download the file if we don't have it
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "./data/smartphone.zip"

if (!file.exists(fileName)) {
        download.file(fileUrl, destfile = fileName, method="curl", method= "-k")
        list.files("./data")
}

# Make sure directory is alreay unzipped
phoneDir <- "./data/UCI HAR Dataset/"
if (!file.exists(phoneDir)) {
        unzip(fileName, exdir="./data")    
        list.files(phoneDir)
}

library(tools)
# Read into data.frame all the relevant data
testFiles <- list.files(paste(c(phoneDir, "test"), collapse=''),
                        full.names=TRUE,
                        pattern="*.txt")
trainFiles <- list.files(paste(c(phoneDir, "train"), collapse=''),
                         full.names=TRUE,
                         pattern="*.txt")
# First get all the files in a list so we could iterate
dataFiles <- c(testFiles, trainFiles,
               paste(c(phoneDir, "features.txt"), collapse=''),
               paste(c(phoneDir, "activity_labels.txt"), collapse=''))

for ( fs in dataFiles) {
        assign(basename(file_path_sans_ext(fs)), read.table(fs))
}
ls()

# Combine test and trainig label data
A <- cbind(y_test, subject_test)
B <- cbind(y_train, subject_train)
colnames(A) <- c("Activity", "Subject")
colnames(B) <- c("Activity", "Subject")
C <- rbind(A,B)

# Combine test and training set data
D <- rbind(X_train, X_test)
colnames(D) <- features[,2]

# We only care about std and mean
E <- subset(D, select = grep(".*(mean|std)\\(\\).*", features[,2]))
F <- cbind(C, E)

# A trick to get mean from the rows found in discussion forum
# http://stackoverflow.com/questions/14937165/using-dynamic-column-names-in-data-table
library(data.table)
dt <- data.table(F)
finaltbl <- dt[, lapply(.SD, mean), by=list(Activity,Subject)]

# Replace the number with the activity labels
nameCols <- finaltbl$Activity
for (i in 1:6) {
        nameCols <- sub(i,activity_labels[i,2], nameCols, fixed=TRUE)
}
finaltbl$Activity <- nameCols

# Finally write out the tidy data set
write.table(finaltbl, file="tidyDataSet.txt", row.names=FALSE, quote=FALSE)