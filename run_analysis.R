
library("reshape2", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")

filename <- "Project_Data.zip"

# Downloading file
if (!file.exists(filename))
{
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  
  download.file(fileURL, filename)
  
}  


# Unzipping File
if (!file.exists("UCI HAR Dataset")) 
{ 
  unzip(filename) 
}


#reading train data
train_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")

#reading test data
test_x <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./UCI HAR Dataset/test/Y_test.txt")
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# read data description
variable_names <- read.table("./UCI HAR Dataset/features.txt")

# read activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")


# 1. Merges the training and the test sets to create one data set
mrg_train <- cbind(train_y, train_subject, train_x)
mrg_test <- cbind(test_y, test_subject, test_x)
setAllInOne <- rbind(mrg_train, mrg_test)


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
col_names <- colnames(setAllInOne)

mean_and_std <- (grepl("activityId" , col_names) | 
                   grepl("subjectId" , col_names) | 
                   grepl("mean.." , col_names) | 
                   grepl("std.." , col_names) 
                 )

setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]

#3. Uses descriptive activity names to name the activities in the data set
setWithActivityNames <- merge(setForMeanAndStd, activity_labels,
                              by='activityId',
                              all.x=TRUE)

#4. Appropriately labels the data set with descriptive variable names. 
colnames(train_x) <- variable_names[,2] 
colnames(train_y) <-"activityId"
colnames(train_subject) <- "subjectId"

colnames(test_x) <- variable_names[,2] 
colnames(test_y) <- "activityId"
colnames(test_subject) <- "subjectId"

colnames(activity_labels) <- c('activityId','activityType')

# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

write.table(secTidySet, "secTidySet.txt", row.name=FALSE)