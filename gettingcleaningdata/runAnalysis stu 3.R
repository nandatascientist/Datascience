## run_analysis.R
##
## Analyses Human Activity Recognition (HAR) data and
## converts it into a tidy data set.
##
## Unzip the dataset into a directory created by default
## called "UCI HAR Dataset" before running this analysis.



## Required libaries
library(reshape2)



## List of paths containing data files.
## !!!!    CAUTION    !!!!
## Ensure that the paths are in the same order for all datasets.
## Otherwise table rows would be merged incorrectly across tables.
## !!!!    *******    !!!!

# Data files storing subject information.
subjectDataPaths <- c("./UCI HAR Dataset/train/subject_train.txt",
                      "./UCI HAR Dataset/test/subject_test.txt")

# Data files storing activity information.
activityDataPaths <- c("./UCI HAR Dataset/train/y_train.txt",
                       "./UCI HAR Dataset/test/y_test.txt")

# Data files storing readings of various sensors.
readingsDataPaths <- c("./UCI HAR Dataset/train/X_train.txt",
                       "./UCI HAR Dataset/test/X_test.txt")

# Data file storing activity labels
labelsDataPath <- "./UCI HAR Dataset/activity_labels.txt"

# Data file storing feature names
featuresDataPath <- "./UCI HAR Dataset/features.txt"



## Read and prepare the subject data

# Read data files into a table
subjectTable <- do.call(rbind, lapply(subjectDataPaths, read.table))

# Assign informative column name
colnames(subjectTable) <- "Subject"



## Read and prepare the activity data.
## Replace activity codes in the raw activities table with
## descriptive activity labels.

# Read data files into a table
activityTableRaw <- do.call(rbind, lapply(activityDataPaths, read.table, colClasses=c("factor")))

# Get the labels of activities
labelTable <- read.table(labelsDataPath)

# Conversion function from activity code to label
getLabel <- function(actCode = numeric()) {
        labelTable[labelTable$V1 == actCode,]$V2
}

# Construct new table with activity labels
activityTable <- sapply(activityTableRaw[,1], getLabel)



## Read and prepare the readings of the sensors.
## Keep the data limited to our requirements and do away
## with unnecessary columns.

# Get the list of features from the features table
featuresTable <- read.table(featuresDataPath)
features <- featuresTable[,2]

# We are only interested in the readings of 'mean()' and 'std()'
# so remove all unwanted columns.
colsReqdNames <- grepl("mean\\()", features) | grepl("std\\()", features)
colsReqd <- ifelse(colsReqdNames, "numeric", "NULL")

# Read data files into a table reading only the required part
readingsTable <- do.call(rbind, lapply(readingsDataPaths, read.table, colClasses=colsReqd))

# Assign the column names to the readings table using the features list
colnames(readingsTable) <- features[colsReqdNames]



## Merge the tables and summarize important fields

# Merge the 3 tables into a single table
mergedTable <- cbind(subjectTable, activityTable, readingsTable)
colnames(mergedTable)[2] <- "Activity"

# Reshape the table so that we have each reading's value
# associated with subject and activity.
mergedTableMolten <- melt(mergedTable, id=c("Subject", "Activity"))

# Group the rows based on subject and activity and collapse them
# to display just each reading's mean
finalTable <- dcast(mergedTableMolten, Subject + Activity ~ variable, mean)



## Export the cleaned up and tidied data to a file
write.table(finalTable, "./tidydata.txt", row.names=FALSE)