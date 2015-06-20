# setwd("D:/Data Science/3. Getting & Cleaning Data/Week 3/Assignment")

# Create Directory to hold data
if (!file.exists("data")) {
    dir.create("data") 
}

# download data file from internet 
if (!file.exists("data/HARData.zip")) {
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, "data/HARData.zip")
}

# unzip downloaded data file  
unzip("data/HARData.zip", overwrite = TRUE, exdir = "data/HARData")


# Utility function useful to build data from files
buildData <- function (testDir, trainDir, ...)
{
    testListOfFiles <- list.files(testDir, full.names = TRUE, ...)
    trainListOfFiles <- list.files(trainDir, full.names = TRUE, ...)
    listOfFiles <- c(trainListOfFiles, testListOfFiles)
    listOfFiles
    dataList <- lapply(listOfFiles, function(x){
        data <- read.table(file=x,header=FALSE) 
    }
    )        
    do.call(rbind, dataList)
}

# 1 Merges the training and the test sets to create one data set.
XData <- buildData("data/HARData/UCI HAR Dataset/test", 
                   "data/HARData/UCI HAR Dataset/train", pattern="X_")

YData <- buildData("data/HARData/UCI HAR Dataset/test", 
                   "data/HARData/UCI HAR Dataset/train", pattern="y_")

SData <- buildData("data/HARData/UCI HAR Dataset/test", 
                   "data/HARData/UCI HAR Dataset/train", pattern="subject_")

# 2 Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("data/HARData/UCI HAR Dataset/features.txt")
indices_of_good_features <- grep("-mean|-std", features[, 2])
XData <- XData[, indices_of_good_features]

names(XData) <- features[indices_of_good_features, 2]
names(XData) <- tolower(names(XData))

# 3 Uses descriptive activity names to name the activities in the data set.

activities <- read.table("data/HARData/UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
YData[,1] = activities[YData[,1], 2]
names(YData) <- "activity"

# 4 Appropriately labels the data set with descriptive activity names.

names(SData) <- "subject"
cleanData <- cbind(SData, YData, XData)

# 5 Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(SData)[,1]
numSubjects = length(unique(SData)[,1])
numActivities = length(activities[,1])
numCols = dim(cleanData)[2]

result = cleanData[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = activities[a, 2]
        tmp <- cleanData[cleanData$subject==s & cleanData$activity==activities[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "data_set_with_the_averages.txt",  row.name=FALSE )

