setwd("~/Desktop/R/datasciencecoursera")

library(data.table)
library(dplyr)

featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)


##Merges the training and the test sets to create one data set.


subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

mergeddata <- cbind(features,activity,subject)

##Extracts only the measurements on the mean and standard deviation for each measurement. 


tidydata <- mergeddata %>% 
        select(grep(".*Mean.*|.*Std.*", names(mergeddata), ignore.case=TRUE), Activity, Subject) 

##Uses descriptive activity names to name the activities in the data set


tidydata$Activity <- as.character(tidydata$Activity)
for (i in 1:6){
        tidydata$Activity[tidydata$Activity == i] <- as.character(activityLabels[i,2])
}

##Appropriately labels the data set with descriptive variable names. 


names(tidydata)<-gsub("Acc", "Accelerometer", names(tidydata))
names(tidydata)<-gsub("Gyro", "Gyroscope", names(tidydata))
names(tidydata)<-gsub("BodyBody", "Body", names(tidydata))
names(tidydata)<-gsub("Mag", "Magnitude", names(tidydata))
names(tidydata)<-gsub("^t", "Time", names(tidydata))
names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
names(tidydata)<-gsub("tBody", "TimeBody", names(tidydata))
names(tidydata)<-gsub("-mean()", "Mean", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-std()", "STD", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-freq()", "Frequency", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("angle", "Angle", names(tidydata))
names(tidydata)<-gsub("gravity", "Gravity", names(tidydata))

##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

seconddata <- tidydata %>%
        group_by(Subject, Activity) %>%
        summarise_all(funs(mean))
