## 1. Merges the training and the test sets to create one data set.
if(!file.exists("./data")) dir.create("./data")

## step 1 : download the dataset
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./data/cleandata.zip")

## step 2 : unzip the data
a <- unzip("./data/cleandata.zip", exdir = "./data")
## step 3 : load data into R
train_x <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
test_x <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
text_y <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
##
## step 4 : merge train and test data
train_data <- cbind(train_subject, train_y,train_x)
test_data <- cbind(test_subject,text_y,test_x)
full_dataa <- rbind(train_data,test_data)

## Extracts only the measurements on the mean and standard deviation for each measurement.

## step 1: load feature name into R
feature_name <- read.table("./data/UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[,2]

## step 2: extract mean and standard deviation of each measurment
feature_index <- grep(("mean\\(\\)|std\\(\\)"),feature_name)
final_data <- full_dataa[,c(1,2,feature_index+2)]
colnames(final_data) <- c("subject", "activity", feature_name[feature_index])

## Uses descriptive activity names to name the activities in the data set

## step 1 : load activity name into R
activity_name <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

## step 2 : repalce the numbers with activity names
final_data$activity <- factor(final_data$activity, levels = activity_name[,1], labels = activity_name[,2])

## Appropriately labels the data set with descriptive variable names.

names(final_data) <- gsub("\\()", "", names(final_data))
names(final_data) <- gsub("^t", "time", names(final_data))
names(final_data) <- gsub("^f", "frequency", names(final_data))
names(final_data) <- gsub("-mean", "Mean", names(final_data))
names(final_data) <- gsub("-std", "Std", names(final_data))

## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
group_data <- final_data %>% 
        group_by(subject, activity) %>%
        summarise_each(mean)