# Instal Packages
install.packages("dplyr")

#Load Packages
library(dplyr)

#Set working folder
setwd("../Getting and Cleaning Data Course Project/")

#Download file and save on local machine
#Set Filename
filename <- "Coursera_Project.zip"

# Checking if archieve already exists or else download from location and save on local machine.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists and unzip the downloaded files
Unzipeedfilename <- "UCI HAR Dataset"
if (!file.exists(Unzipeedfilename)) { 
  unzip(filename)
}


### Read data from unzipped file and assigning all data frames
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# view table details
str(features)
head(features)
str(activities)
head(activities)
str(subject_test)
head(subject_test)
str(x_test)
head(x_test)
str(y_test)
head(y_test)
str(subject_train)
head(subject_train)
str(x_train)
head(x_train)
str(y_train)
head(y_train)

# 1. Merge train and test dataset into one dataset.

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

# view table details
head(Subject)
str(Subject)
head(Merged_Data)
str(Merged_Data)

# 2. Extracts mean and standard deviation for each measurement.
TidyData <- Merged_Data %>% select(subject, code, contains("std"), contains("mean"))

#Check TidyData set - activities in the data set.
head(TidyData$code)
#Check Names of Tidy Dataset, pre transformation
names(TidyData)

# 3. Uses descriptive activity names to name the activities in the data set
TidyData$code <- activities[TidyData$code, 2]

#4. labels the data set with descriptive variable names.
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("^t", "Time", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("^f", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData), ignore.case = TRUE)

#Check Names of Tidy Dataset, post transformation
names(TidyData)

# 5. creates a second, independent tidy data set with the average of each variable for each activity and each subject.
FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

str(FinalData)
head(FinalData)
FinalData
write.table(FinalData, "FinalData.txt", row.name=FALSE)